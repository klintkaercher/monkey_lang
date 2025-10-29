#![allow(unused)]
use crate::{
    Lexer, Token,
    ast::{
        Expression, ExpressionStatement, Identifier, LetStatement, Program, ReturnStatement,
        Statement,
    },
};

type PrefixParseFn = Box<dyn Fn(&Parser) -> Box<dyn Expression>>;
type InfixParseFn = Box<dyn Fn(&Parser) -> Box<dyn Expression>>;

struct Parser<'a> {
    lexer: &'a mut Lexer,
    current_token: Token,
    peek_token: Token,
    errors: Vec<String>,
    prefix_parse_fns: std::collections::HashMap<Token, PrefixParseFn>,
    infix_parse_fns: std::collections::HashMap<Token, InfixParseFn>,
}

impl<'a> Parser<'a> {
    fn parse_expression(&self, level: PrecedenceLevel) -> Option<Box<dyn Expression>> {
        let lookup_token = if let Token::Identifier(_) = &self.current_token {
            Token::Identifier("".to_string())
        } else if let Token::Number(_) = &self.current_token {
            Token::Number(0)
        } else {
            self.current_token.clone()
        };

        let prefix = self.prefix_parse_fns.get(&lookup_token)?;
        Some(prefix(self))
    }

    fn parse_expression_statement(&mut self) -> Option<ExpressionStatement> {
        let expression = self.parse_expression(PrecedenceLevel::Lowest)?;

        let mut stmt = ExpressionStatement {
            token: self.current_token.clone(),
            expression,
        };
        if self.peek_token == Token::Semicolon {
            self.next_token();
        }
        Some(stmt)
    }

    pub fn register_prefix(&mut self, token: Token, func: PrefixParseFn) {
        self.prefix_parse_fns.insert(token, func);
    }

    pub fn register_infix(&mut self, token: Token, func: InfixParseFn) {
        self.infix_parse_fns.insert(token, func);
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
        let mut out = Self {
            lexer,
            current_token,
            peek_token,
            errors: Vec::new(),
            prefix_parse_fns: std::collections::HashMap::new(),
            infix_parse_fns: std::collections::HashMap::new(),
        };
        out.register_prefix(
            Token::Identifier("".to_string()),
            Box::new(|parser| parser.parse_identifier()),
        );
        out
    }

    fn parse_identifier(&self) -> Box<dyn Expression> {
        let Token::Identifier(value) = self.current_token.clone() else {
            panic!("parsing something that isn't an identifier!");
        };
        Box::new(Identifier {
            token: self.current_token.clone(),
            value,
        }) as Box<dyn Expression>
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
            name: identifier.clone(),
            value: Box::new(identifier) as Box<dyn Expression>,
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

            // _ => None,
            // Not compiling yet.
            _ => self
                .parse_expression_statement()
                .map(|statement| Some(Box::new(statement) as Box<dyn Statement>))?,
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

    pub fn errors(&self) -> Vec<String> {
        self.errors.clone()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum PrecedenceLevel {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
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
    fn identifier_expression() {
        let input = "foobar;";

        let mut lexer = Lexer::new(input).unwrap();
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);
        assert_eq!(program.statements.len(), 1);
        let statement = (&*program.statements[0] as &dyn Any)
            .downcast_ref::<ExpressionStatement>()
            .expect("Couldn't parse as ExpressionStatement.");
        let identifier = (&*statement.expression as &dyn Any)
            .downcast_ref::<Identifier>()
            .expect("Couldn't parse as Identifier.");
        assert_eq!(identifier.value, "foobar");
        // Not sure if this is how this works with Identifiers?
        // It will probably give us like "Token::Identifier("foobar")" or something
        assert_eq!(identifier.token_literal(), r#"Identifier("foobar")"#);
    }

    #[test]
    fn unhappy_program_to_string() {
        // TODO: NEXT THING. This needs integer literals to work.
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
