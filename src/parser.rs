#![allow(unused)]
use crate::{
    Lexer, Token,
    ast::{Expression, Identifier, LetStatement, Program, ReturnStatement, Statement},
};

type PrefixParseFn = Box<dyn Fn() -> Box<dyn Expression>>;
type InfixParseFn = Box<dyn Fn() -> Box<dyn Expression>>;

struct Parser<'a> {
    lexer: &'a mut Lexer,
    current_token: Token,
    peek_token: Token,
    errors: Vec<String>,
    prefix_parse_fns: std::collections::HashMap<Token, PrefixParseFn>,
    infix_parse_fns: std::collections::HashMap<Token, InfixParseFn>,
}

impl<'a> Parser<'a> {
    pub fn errors(&self) -> Vec<String> {
        self.errors.clone()
    }

    pub fn peek_error(&mut self, token: Token) {
        self.errors.push(format!(
            "Expected token of type `{:?}`, but got `{:?}`.",
            token, self.peek_token
        ))
    }

    pub fn new(input: &'a mut Lexer) -> Parser<'a> {
        let current_token = input.next_token();
        let peek_token = input.next_token();
        let lexer = input;
        Self {
            lexer,
            current_token,
            peek_token,
            errors: Vec::new(),
            prefix_parse_fns: std::collections::HashMap::new(),
            infix_parse_fns: std::collections::HashMap::new(),
        }
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn expect_peek(&mut self, expected_token: Token) -> bool {
        use Token::*;
        let out = match expected_token.clone() {
            Identifier(_) => matches!(self.peek_token, Identifier(_)),
            Number(_) => matches!(self.peek_token, Number(_)),
            tok => self.peek_token == tok,
        };

        if out {
            self.next_token();
        } else {
            self.peek_error(expected_token);
        }

        out
    }

    pub fn parse_let_statement(&mut self) -> Option<LetStatement> {
        if !self.expect_peek(Token::Identifier("".to_string())) {
            return None;
        }
        let ident = self.current_token.clone();

        let Token::Identifier(ref val) = ident else {
            panic!("We should have an Token::Identifier at this point!");
        };

        let identifier = Identifier {
            token: Token::Identifier(val.clone()),
            value: val.clone(),
        };
        if !self.expect_peek(Token::Assign) {
            return None;
        }

        while self.current_token != Token::Semicolon {
            self.next_token();
        }

        Some(LetStatement {
            token: Token::Let,
            name: identifier,
            value: Some(()),
        })
    }

    pub fn parse_return_statement(&mut self) -> Option<ReturnStatement> {
        while self.current_token != Token::Semicolon {
            self.next_token();
        }

        Some(ReturnStatement {
            token: Token::Return,
            value: Some(()),
        })
    }

    pub fn parse_statement(&mut self) -> Option<Box<dyn Statement>> {
        match &self.current_token {
            Token::Let => self
                .parse_let_statement()
                .map(|statement| Some(Box::new(statement) as Box<dyn Statement>))?,
            Token::Return => self
                .parse_return_statement()
                .map(|statement| Some(Box::new(statement) as Box<dyn Statement>))?,
            _ => None,
        }
    }

    pub fn parse_program(&mut self) -> Program {
        let mut out = Program { statements: vec![] };

        while self.current_token != Token::EndOfFile {
            if let Some(statement) = self.parse_statement() {
                out.statements.push(statement);
            }
            self.next_token(); // I think this is eating the semicolon?
        }

        out
    }
}

#[cfg(test)]
mod test {
    use std::any::Any;

    use crate::ast::{
        ExpressionStatement, Identifier, LetStatement, Node, ReturnStatement, Statement,
    };

    fn check_parser_errors(input: &Parser) {
        let errors = input.errors();
        if errors.is_empty() {
            return;
        }

        eprintln!("Found {} errors", errors.len());

        for error in errors {
            eprintln!("{error}");
        }

        panic!("Ran into errors!");
    }
    #[test]
    fn unhappy_identifier_expression() {
        let input = "foobar;";

        let mut lexer = Lexer::new(input).unwrap();
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();
        assert_eq!(program.statements.len(), 1);
        let statement = (&program.statements[0] as &dyn Any)
            .downcast_ref::<ExpressionStatement>()
            .expect("Couldn't parse as ExpressionStatemnt.");
        let identifier = (&*statement.expression as &dyn Any)
            .downcast_ref::<Identifier>()
            .expect("Couldn't parse as Identifier.");
        assert_eq!(identifier.value, "foobar");
    }

    #[test]
    fn unhappy_program_to_string() {
        let input = "let x = 5;";

        let mut lexer = Lexer::new(input).unwrap();
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();
        program.to_string() == "let x = 5;";
        assert_eq!(program.to_string(), "let x = 5;");
    }

    /// TODO: This needs to check the `value` eventually.
    /// NOTE: This name field isn't used correctly, but will need to be the value eventually.
    fn assert_return_statement(input: &dyn Statement, name: &str) {
        let statement = (input as &dyn Any)
            .downcast_ref::<ReturnStatement>()
            .unwrap();
        assert_eq!(statement.token_literal(), "Return");
        assert_eq!(statement.value, Some(()));
    }

    use super::*;
    #[test]
    fn return_statements() {
        let input = "\
return 5;
return 10;

return 993322;";

        let mut lexer = Lexer::new(input).unwrap();
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();

        check_parser_errors(&parser);

        assert_eq!(program.statements.len(), 3);

        assert_return_statement(&*program.statements[0], "x");
        assert_return_statement(&*program.statements[1], "y");
        assert_return_statement(&*program.statements[2], "foobar");
    }

    /// TODO: This needs to check the `value` eventually.
    fn assert_let_statement(input: &dyn Statement, name: &str) {
        let statement = (input as &dyn Any).downcast_ref::<LetStatement>().unwrap();
        assert_eq!(statement.token_literal(), "Let");
        assert_eq!(
            statement.name,
            Identifier {
                token: Token::Identifier(name.to_string()),
                value: name.to_string()
            }
        );
    }

    #[test]
    fn let_statements() {
        let input = "\
let x = 5;
let y = 10;

let foobar = 838383;";

        let mut lexer = Lexer::new(input).unwrap();
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();

        check_parser_errors(&parser);

        assert_eq!(program.statements.len(), 3);

        assert_let_statement(&*program.statements[0], "x");
        assert_let_statement(&*program.statements[1], "y");
        assert_let_statement(&*program.statements[2], "foobar");
    }
}
