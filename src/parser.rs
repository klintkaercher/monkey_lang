//! Traverses the tokens. Parses out statements and expressions.
//! This is the file doing most of the "work".
#![allow(unused)]
use std::collections::HashMap;

use crate::{
    Lexer, Token,
    ast::{
        BlockStatement, Boolean, CallExpression, Expression, ExpressionStatement, FunctionLiteral,
        Identifier, IfExpression, InfixExpression, IntegerLiteral, LetStatement, PrefixExpression,
        Program, ReturnStatement, Statement,
    },
};

type PrefixParseFn = fn(&mut Parser) -> Option<Box<dyn Expression>>;
type InfixParseFn = fn(&mut Parser, Box<dyn Expression>) -> Option<Box<dyn Expression>>;

struct Parser<'a> {
    lexer: &'a mut Lexer,
    current_token: Token,
    peek_token: Token,
    errors: Vec<String>,
    prefix_parse_fns: std::collections::HashMap<Token, PrefixParseFn>,
    infix_parse_fns: std::collections::HashMap<Token, InfixParseFn>,
}

impl<'a> Parser<'a> {
    /// This is the external facing function.
    pub fn parse_program(&mut self) -> Program {
        let mut output = Program { statements: vec![] };

        while self.current_token != Token::EndOfFile {
            if let Some(statement) = self.parse_statement() {
                output.statements.push(statement);
            }
            self.next_token(); // I think this is eating the semicolon?
        }

        output
    }

    // We can see that this is where we're going to be expanding when we add new types of
    // statements.
    fn parse_statement(&mut self) -> Option<Box<dyn Statement>> {
        // We can't factor out the map because then the match statement has different types in each
        // arm.
        match &self.current_token {
            Token::Let => self
                .parse_let_statement()
                .map(|statement| Some(Box::new(statement) as Box<dyn Statement>))?,
            Token::Return => self
                .parse_return_statement()
                .map(|statement| Some(Box::new(statement) as Box<dyn Statement>))?,
            _ => self
                .parse_expression_statement()
                .map(|statement| Some(Box::new(statement) as Box<dyn Statement>))?,
        }
    }

    fn parse_let_statement(&mut self) -> Option<LetStatement> {
        dbg!(&self.current_token);
        if !self.expect_peek(Token::Identifier("".to_string())) {
            return None;
        }
        let token = self.current_token.clone();

        let Token::Identifier(val) = token.clone() else {
            panic!("We should have an Token::Identifier at this point!");
        };

        let identifier = Identifier { token, value: val };

        if !self.expect_peek(Token::EqualSign) {
            return None;
        }

        self.next_token();

        let exp = self.parse_expression_statement()?;

        Some(LetStatement {
            token: Token::Let,
            name: identifier,
            value: Box::new(exp) as Box<dyn Expression>,
        })
    }

    fn parse_return_statement(&mut self) -> Option<ReturnStatement> {
        self.next_token();

        let value = self.parse_expression(PrecedenceLevel::Lowest)?;

        if self.peek_token == Token::Semicolon {
            self.next_token();
        }

        Some(ReturnStatement {
            token: Token::Return,
            value,
        })
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

    fn parse_blockstatement(&mut self) -> BlockStatement {
        // TAKING A FIRST CRACK AT IT.
        let mut statements = Vec::new();

        self.next_token();

        while self.current_token != Token::RCurly && self.current_token != Token::EndOfFile {
            if let Some(it) = self.parse_statement() {
                statements.push(it);
            }
            self.next_token();
        }

        BlockStatement {
            token: Token::LCurly,
            statements,
        }
    }
    fn parse_function_parameters(&mut self) -> Option<Vec<Box<Identifier>>> {
        let mut output = Vec::new();

        // Check if empty params
        if self.peek_token == Token::RParens {
            self.next_token();
            return Some(output);
        }

        self.next_token();

        if let Token::Identifier(ref it) = self.current_token {
            output.push(Box::new(Identifier {
                token: self.current_token.clone(),
                value: it.clone(),
            }));
        } else {
            panic!(
                "Next value is not an identifer! We have a(n) `{:?}`",
                self.current_token
            );
        }

        while self.peek_token == Token::Comma {
            self.next_token();
            self.next_token();
            if let Token::Identifier(ref it) = self.current_token {
                output.push(Box::new(Identifier {
                    token: self.current_token.clone(),
                    value: it.clone(),
                }));
            } else {
                panic!(
                    "Next value is not an identifer! We have a(n) `{:?}`",
                    self.current_token
                );
            }
        }
        if !self.expect_peek(Token::RParens) {
            return None;
        }

        Some(output)
    }

    fn parse_functionliteral(&mut self) -> Option<Box<dyn Expression>> {
        // 1. Eat LParens
        if !self.expect_peek(Token::LParens) {
            return None;
        }

        // 2. Eat Params
        let parameters = self.parse_function_parameters()?;

        // 3. Eat LCurly
        if !self.expect_peek(Token::LCurly) {
            return None;
        }
        // 4. Eat BlockStatement
        let body = Box::new(self.parse_blockstatement());

        Some(Box::new(FunctionLiteral {
            token: Token::Fn,
            parameters,
            body,
        }) as Box<dyn Expression>)
    }

    fn parse_if_expression(&mut self) -> Option<Box<dyn Expression>> {
        // 1. eat lparens
        if !self.expect_peek(Token::LParens) {
            return None;
        }

        self.next_token();

        // 2. eat expression
        let condition = self.parse_expression(PrecedenceLevel::Lowest)?;

        // 3. eat rparens
        if !self.expect_peek(Token::RParens) {
            return None;
        }

        // 4. eat lcurly
        if !self.expect_peek(Token::LCurly) {
            return None;
        }

        // 5. eat statements
        // 6. eat rcurly
        let consequence = self.parse_blockstatement();

        let mut alternative = None;
        if self.peek_token == Token::Else {
            // 7. eat else
            self.next_token();
            // 8. eat else lcurly
            if !self.expect_peek(Token::LCurly) {
                return None;
            }
            // 9. eat statements
            // 10. eat rcurly
            alternative = Some(self.parse_blockstatement());
        }

        Some(Box::new(IfExpression {
            token: Token::If,
            condition,
            consequence,
            alternative,
        }) as Box<dyn Expression>)
    }

    fn parse_expression(&mut self, precedence: PrecedenceLevel) -> Option<Box<dyn Expression>> {
        // We need to do this because we're using these enumerations that need a 0 value.
        // We won't find Identifier("foobar") in the map.
        let lookup_token = if let Token::Identifier(_) = &self.current_token {
            Token::Identifier("".to_string())
        } else if let Token::Number(_) = &self.current_token {
            Token::Number(0)
        } else {
            self.current_token.clone()
        };

        let Some(prefix_fn) = self.prefix_parse_fns.get(&lookup_token) else {
            self.errors.push(format!(
                "No prefix parse function for {lookup_token:?} found."
            ));
            return None;
        };

        let mut left = prefix_fn(self);

        while self.peek_token != Token::Semicolon && precedence < self.peek_precedence() {
            let mut infix_fn;
            if let Some(func) = self.infix_parse_fns.get(&self.peek_token) {
                infix_fn = func.clone();
            } else {
                return left;
            }
            self.next_token();
            left = infix_fn(self, left.expect("We should have a left at this point."));
        }

        left
    }

    fn register_prefix(&mut self, token: Token, func: PrefixParseFn) {
        self.prefix_parse_fns.insert(token, func);
    }

    fn register_infix(&mut self, token: Token, func: InfixParseFn) {
        self.infix_parse_fns.insert(token, func);
    }

    fn peek_error(&mut self, token: Token) {
        self.errors.push(format!(
            "Expected token of type `{:?}`, but got `{:?}`.",
            token, self.peek_token
        ))
    }

    fn parse_call_arguments(&mut self) -> Option<Vec<Box<dyn Expression>>> {
        use Token::*;
        if self.peek_token == RParens {
            self.next_token();
            self.next_token();
            return Some(Vec::new());
        }

        self.next_token();

        let mut output = vec![
            self.parse_expression(PrecedenceLevel::Lowest)
                .expect("There should be an expression here."),
        ];

        while self.peek_token == Comma {
            self.next_token();
            self.next_token();
            output.push(
                self.parse_expression(PrecedenceLevel::Lowest)
                    .expect("There should be an expression here."),
            );
        }

        // ===================
        if !self.expect_peek(RParens) {
            return None;
        }
        Some(output)
    }

    fn parse_call_expression(
        &mut self,
        function: Box<dyn Expression>,
    ) -> Option<Box<dyn Expression>> {
        Some(Box::new(CallExpression {
            token: self.current_token.clone(),
            function,
            arguments: self.parse_call_arguments()?,
        }) as Box<dyn Expression>)
    }

    pub fn new(input: &'a mut Lexer) -> Parser<'a> {
        let current_token = input.next_token();
        let peek_token = input.next_token();
        let lexer = input;
        let mut output = Self {
            lexer,
            current_token,
            peek_token,
            errors: Vec::new(),
            prefix_parse_fns: std::collections::HashMap::new(),
            infix_parse_fns: std::collections::HashMap::new(),
        };

        output.register_prefix(Token::Identifier("".to_string()), |parser: &mut Parser| {
            parser.parse_identifier()
        });
        output.register_prefix(Token::Number(0), |parser| parser.parse_integer_literal());
        output.register_prefix(Token::Bang, |parser| parser.parse_prefix_expression());
        output.register_prefix(Token::Minus, |parser| parser.parse_prefix_expression());
        output.register_prefix(Token::True, |parser| parser.parse_boolean());
        output.register_prefix(Token::False, |parser| parser.parse_boolean());
        output.register_prefix(Token::LParens, |parser| parser.parse_grouped_expression());
        output.register_prefix(Token::If, |parser| parser.parse_if_expression());
        output.register_prefix(Token::Fn, |parser| parser.parse_functionliteral());

        output.register_infix(Token::Plus, |parser, lhs| {
            parser.parse_infix_expression(lhs)
        });
        output.register_infix(Token::Minus, |parser, lhs| {
            parser.parse_infix_expression(lhs)
        });
        output.register_infix(Token::Slash, |parser, lhs| {
            parser.parse_infix_expression(lhs)
        });
        output.register_infix(Token::Asterisk, |parser, lhs| {
            parser.parse_infix_expression(lhs)
        });
        output.register_infix(Token::DoubleEqualSign, |parser, lhs| {
            parser.parse_infix_expression(lhs)
        });
        output.register_infix(Token::NotEquals, |parser, lhs| {
            parser.parse_infix_expression(lhs)
        });
        output.register_infix(Token::LT, |parser, lhs| parser.parse_infix_expression(lhs));
        output.register_infix(Token::GT, |parser, lhs| parser.parse_infix_expression(lhs));
        output.register_infix(Token::LParens, |parser, lhs| {
            parser.parse_call_expression(lhs)
        });

        output
    }

    fn parse_grouped_expression(&mut self) -> Option<Box<dyn Expression>> {
        self.next_token();

        let exp = self
            .parse_expression(PrecedenceLevel::Lowest)
            .expect("We expect there to be a thing here.");

        if !self.expect_peek(Token::RParens) {
            return None;
        }

        Some(exp)
    }

    fn parse_infix_expression(
        &mut self,
        input: Box<dyn Expression>,
    ) -> Option<Box<dyn Expression>> {
        let token = self.current_token.clone();
        let operator = token.to_string();

        let precedence = precedences(&self.current_token);

        self.next_token();

        Some(Box::new(InfixExpression {
            token,
            operator,
            lhs: input,
            rhs: self.parse_expression(precedence)?,
        }) as Box<dyn Expression>)
    }

    fn parse_prefix_expression(&mut self) -> Option<Box<dyn Expression>> {
        let token = self.current_token.clone();
        let operator = token.to_string();
        self.next_token();

        Some(Box::new(PrefixExpression {
            token,
            operator,
            rhs: self.parse_expression(PrecedenceLevel::Prefix)?,
        }) as Box<dyn Expression>)
    }

    fn parse_boolean(&mut self) -> Option<Box<dyn Expression>> {
        let token = match &self.current_token {
            &Token::True => Token::True,
            &Token::False => Token::False,
            _ => return None,
        };

        let value = (token == Token::True);

        Some(Box::new(Boolean { token, value }))
    }

    fn parse_identifier(&mut self) -> Option<Box<dyn Expression>> {
        let Token::Identifier(value) = self.current_token.clone() else {
            self.errors
                .push("parsing something that isn't an identifier!".to_string());
            return None;
        };

        Some(Box::new(Identifier {
            token: self.current_token.clone(),
            value,
        }) as Box<dyn Expression>)
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn expect_peek(&mut self, expected_token: Token) -> bool {
        use Token::*;
        let output = match expected_token.clone() {
            Identifier(_) => matches!(self.peek_token, Identifier(_)),
            Number(_) => matches!(self.peek_token, Number(_)),
            tok => self.peek_token == tok,
        };

        if output {
            self.next_token();
        } else {
            self.peek_error(expected_token);
        }

        output
    }

    fn parse_integer_literal(&mut self) -> Option<Box<dyn Expression>> {
        let Token::Number(ref n) = self.current_token else {
            self.errors
                .push("Could not parse current token as an integer.".to_string());
            return None;
        };

        Some(Box::new(IntegerLiteral {
            token: self.current_token.clone(),
            value: *n,
        }) as Box<dyn Expression>)
    }

    /// This function is a little wonky because `precedences` is supposed to be a map and we check
    /// it here, then we default to PrecedenceLevel::Lowest, but instead of a global map, we're
    /// just doing a function with match statement and then defaulting to Lowest.
    fn curr_precedence(&self) -> PrecedenceLevel {
        precedences(&self.current_token)
    }

    /// See documentation for [`curr_precedence`].
    fn peek_precedence(&self) -> PrecedenceLevel {
        precedences(&self.peek_token)
    }
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum PrecedenceLevel {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

fn precedences(input: &Token) -> PrecedenceLevel {
    use PrecedenceLevel::*;
    use Token::*;
    #[rustfmt::skip]
    return match input {
        DoubleEqualSign | NotEquals => Equals,
        LT              | GT        => LessGreater,
        Plus            | Minus     => Sum,
        Slash           | Asterisk  => Product,
        LParens                     => Call,
        _                           => Lowest,
    };
}

#[cfg(test)]
mod test {
    use super::*;
    use std::any::Any;

    use crate::ast::{
        Boolean, CallExpression, ExpressionStatement, FunctionLiteral, Identifier, IfExpression,
        InfixExpression, LetStatement, Node, PrefixExpression, ReturnStatement, Statement,
    };
    #[test]
    fn callexpression_parameter_parsing() {
        let inputs = [
            ("add();", "add", vec![]),
            ("add(1);", "add", vec!["1"]),
            (
                "add(1, 2 * 3, 4 + 5);",
                "add",
                vec!["1", "(2 * 3)", "(4 + 5)"],
            ),
        ];
        for input in inputs {
            let mut lexer = Lexer::new(input.0).unwrap();
            let mut parser = Parser::new(&mut lexer);
            let program = parser.parse_program();
            check_parser_errors(&mut parser);
            assert_eq!(program.statements.len(), 1);
            let statement = (&*program.statements[0] as &dyn Any)
                .downcast_ref::<ExpressionStatement>()
                .expect("Couldn't parse as ExpressionStatement.");
            let call_exp = (&*statement.expression as &dyn Any)
                .downcast_ref::<CallExpression>()
                .expect("Couldn't parse as CallExpression.");
            assert_eq!(call_exp.function.to_string(), input.1);
            for (idx, argument) in call_exp.arguments.iter().enumerate() {
                assert_eq!(argument.to_string(), input.2[idx]);
            }
        }
    }

    #[test]
    fn callexpression_parsing() {
        let input = "add(1, 2 * 3, 4 + 5);";

        let mut lexer = Lexer::new(input).unwrap();
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();
        // dbg!(&program);
        check_parser_errors(&mut parser);
        assert_eq!(program.statements.len(), 1);
        let statement = (&*program.statements[0] as &dyn Any)
            .downcast_ref::<ExpressionStatement>()
            .expect("Couldn't parse as ExpressionStatement.");
        let call_exp = (&*statement.expression as &dyn Any)
            .downcast_ref::<CallExpression>()
            .expect("Couldn't parse as FunctionLiteral.");
        assert_eq!(call_exp.function.to_string(), "add");
        assert_eq!(call_exp.arguments.len(), 3);
        assert_eq!(call_exp.arguments[0].to_string(), "1");
        assert_eq!(call_exp.arguments[1].to_string(), "(2 * 3)");
        assert_eq!(call_exp.arguments[2].to_string(), "(4 + 5)");
    }

    #[test]
    fn function_argument_parsing() {
        let inputs = [
            ("fn() {}", vec![]),
            ("fn(x) {}", vec!["x"]),
            ("fn(x, y, z) {}", vec!["x", "y", "z"]),
        ];

        for input in inputs {
            let mut lexer = Lexer::new(input.0).unwrap();
            let mut parser = Parser::new(&mut lexer);
            let program = parser.parse_program();
            check_parser_errors(&mut parser);
            assert_eq!(program.statements.len(), 1);
            let statement = (&*program.statements[0] as &dyn Any)
                .downcast_ref::<ExpressionStatement>()
                .expect("Couldn't parse as ExpressionStatement.");
            let func_lit = (&*statement.expression as &dyn Any)
                .downcast_ref::<FunctionLiteral>()
                .expect("Couldn't parse as FunctionLiteral.");
            assert_eq!(func_lit.parameters.len(), input.1.len());
            for (idx, param) in func_lit.parameters.iter().enumerate() {
                assert_eq!(param.value, input.1[idx]);
            }
        }
    }

    #[test]
    fn functionliteral_parsing() {
        let input = "fn(x, y) { x + y; }";

        let mut lexer = Lexer::new(input).unwrap();
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();
        check_parser_errors(&mut parser);
        assert_eq!(program.statements.len(), 1);
        let statement = (&*program.statements[0] as &dyn Any)
            .downcast_ref::<ExpressionStatement>()
            .expect("Couldn't parse as ExpressionStatement.");
        let func_lit = (&*statement.expression as &dyn Any)
            .downcast_ref::<FunctionLiteral>()
            .expect("Couldn't parse as FunctionLiteral.");

        assert_eq!(func_lit.parameters.len(), 2, "Expecting 2 params.");
        assert_eq!(func_lit.parameters[0].to_string(), "x");
        assert_eq!(func_lit.parameters[1].to_string(), "y");
        assert_eq!(func_lit.body.statements.len(), 1);
        assert_eq!(func_lit.body.statements[0].to_string(), "(x + y)");
    }

    #[test]
    fn if_else_expression() {
        let input = "if (x < y) { x } else { y }";

        let mut lexer = Lexer::new(input).unwrap();
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();
        check_parser_errors(&mut parser);
        assert_eq!(program.statements.len(), 1);
        let statement = (&*program.statements[0] as &dyn Any)
            .downcast_ref::<ExpressionStatement>()
            .expect("Couldn't parse as ExpressionStatement.");
        let if_exp = (&*statement.expression as &dyn Any)
            .downcast_ref::<IfExpression>()
            .expect("Couldn't parse as IfExpression.");

        let condition = (&*if_exp.condition as &dyn Any)
            .downcast_ref::<InfixExpression>()
            .expect("Couldn't parse as InfixExpression.");
        assert_eq!(condition.to_string(), "(x < y)");
        assert_eq!(if_exp.consequence.statements.len(), 1);
        let x_expression = (&*if_exp.consequence.statements[0] as &dyn Any)
            .downcast_ref::<ExpressionStatement>()
            .expect("Couldn't parse as  ExpressionStatement.");
        let x_ident = (&*x_expression.expression as &dyn Any)
            .downcast_ref::<Identifier>()
            .expect("Couldn't parse as Identifier.");
        assert_eq!(x_ident.to_string(), "x");

        if let Some(ref alternative) = if_exp.alternative {
            assert_eq!(alternative.statements.len(), 1);
            let y_expression = (&*alternative.statements[0] as &dyn Any)
                .downcast_ref::<ExpressionStatement>()
                .expect("Couldn't parse as  ExpressionStatement.");
            let y_ident = (&*y_expression.expression as &dyn Any)
                .downcast_ref::<Identifier>()
                .expect("Couldn't parse as Identifier.");
            assert_eq!(y_ident.to_string(), "y");
        } else {
            panic!("Didn't parse alternative.");
        }
    }

    #[test]
    fn if_expression() {
        let input = "if (x < y) { x }";

        let mut lexer = Lexer::new(input).unwrap();
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();
        check_parser_errors(&mut parser);
        assert_eq!(program.statements.len(), 1);
        let statement = (&*program.statements[0] as &dyn Any)
            .downcast_ref::<ExpressionStatement>()
            .expect("Couldn't parse as ExpressionStatement.");
        let if_exp = (&*statement.expression as &dyn Any)
            .downcast_ref::<IfExpression>()
            .expect("Couldn't parse as IfExpression.");
        let condition = (&*if_exp.condition as &dyn Any)
            .downcast_ref::<InfixExpression>()
            .expect("Couldn't parse as InfixExpression.");
        assert_eq!(condition.to_string(), "(x < y)");
        assert_eq!(if_exp.consequence.statements.len(), 1);
        let x_expression = (&*if_exp.consequence.statements[0] as &dyn Any)
            .downcast_ref::<ExpressionStatement>()
            .expect("Couldn't parse as  ExpressionStatement.");
        let x_ident = (&*x_expression.expression as &dyn Any)
            .downcast_ref::<Identifier>()
            .expect("Couldn't parse as Identifier.");
        assert_eq!(x_ident.to_string(), "x");
        assert!(if_exp.alternative.is_none());
    }

    #[test]
    fn boolean_literal() {
        let input = "false;";

        let mut lexer = Lexer::new(input).unwrap();
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();
        check_parser_errors(&mut parser);
        assert_eq!(program.statements.len(), 1);
        let statement = (&*program.statements[0] as &dyn Any)
            .downcast_ref::<ExpressionStatement>()
            .expect("Couldn't parse as ExpressionStatement.");
        let identifier = (&*statement.expression as &dyn Any)
            .downcast_ref::<Boolean>()
            .expect("Couldn't parse as Boolean.");
        assert_eq!(identifier.value, false);
        assert_eq!(identifier.token_literal(), "false");

        let input = "true;";

        let mut lexer = Lexer::new(input).unwrap();
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();
        check_parser_errors(&mut parser);
        assert_eq!(program.statements.len(), 1);
        let statement = (&*program.statements[0] as &dyn Any)
            .downcast_ref::<ExpressionStatement>()
            .expect("Couldn't parse as ExpressionStatement.");
        let identifier = (&*statement.expression as &dyn Any)
            .downcast_ref::<Boolean>()
            .expect("Couldn't parse as Boolean.");
        assert_eq!(identifier.value, true);
        assert_eq!(identifier.token_literal(), "true");
    }

    #[test]
    #[rustfmt::skip]
    fn operator_precedence_parsing() {
        let inputs = [
            ("-a * b",                     "((-a) * b)"),
            ("!-a",                        "(!(-a))"),
            ("a + b + c",                  "((a + b) + c)"),
            ("a + b - c",                  "((a + b) - c)"),
            ("a * b * c",                  "((a * b) * c)"),
            ("a * b / c",                  "((a * b) / c)"),
            ("a + b / c",                  "(a + (b / c))"),
            ("a + b * c + d / e - f",      "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4;-5 * 5",               "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4",             "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4",             "((5 < 4) != (3 > 4))"),
            ("3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"),
            ("true",                       "true"),
            ("false",                      "false"),
            ("3 > 5 == false",             "((3 > 5) == false)"),
            ("3 < 5 == true",              "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4",            "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2",                "((5 + 5) * 2)"),
            ("2 / (5 + 5)",                "(2 / (5 + 5))"),
            ("-(5 + 5)",                   "(-(5 + 5))"),
            ("!(true == true)",            "(!(true == true))"),

            // New ones
            ("a + add(b * c) + d",                        "((a + add((b * c))) + d)"),
            ("add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))", "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))"),
            ("add(a + b + c * d / f + g)",                "add((((a + b) + ((c * d) / f)) + g))"),
        ];

        for input in inputs {
            let mut lexer = Lexer::new(input.0).unwrap();
            let mut parser = Parser::new(&mut lexer);
            let program = parser.parse_program();
            check_parser_errors(&mut parser);
            assert_eq!(program.to_string(), input.1);
        }
    }

    #[test]
    #[rustfmt::skip]
    fn parsing_infix_expressions() {
        let inputs = [
            ("5 + 5;", 5, "+", 5),
            ("5 - 5;", 5, "-", 5),
            ("5 * 5;", 5, "*", 5),
            ("5 / 5;", 5, "/", 5),
            ("5 > 5;", 5, ">", 5),
            ("5 < 5;", 5, "<", 5),
            ("5 == 5;", 5, "==", 5),
            ("5 != 5;", 5, "!=", 5),
        ];

        for input in inputs {
            let mut lexer = Lexer::new(input.0).unwrap();
            let mut parser = Parser::new(&mut lexer);
            let program = parser.parse_program();
            dbg!(&program);
            check_parser_errors(&mut parser);
            assert_eq!(program.statements.len(), 1);
            let statement = (&*program.statements[0] as &dyn Any)
                .downcast_ref::<ExpressionStatement>()
                .expect("Couldn't parse as ExpressionStatement.");
            let infix_expression = (&*statement.expression as &dyn Any)
                .downcast_ref::<InfixExpression>()
                .expect("Couldn't parse as InfixExpression.");
            assert_eq!(infix_expression.operator, input.2);
            let lhs = (&*infix_expression.lhs as &dyn Any)
                .downcast_ref::<IntegerLiteral>()
                .expect("Couldn't parse as IntegerLiteral.");
            assert_eq!(lhs.value, input.1);
            assert_eq!(infix_expression.operator, input.2);
            let rhs = (&*infix_expression.rhs as &dyn Any)
                .downcast_ref::<IntegerLiteral>()
                .expect("Couldn't parse as IntegerLiteral.");
            assert_eq!(rhs.value, input.3);
        }

        let inputs = [
            ("true == true;", true, "==", true),
            ("true != false;", true, "!=", false),
            ("false == false;", false, "==", false),
        ];

        for input in inputs {
            let mut lexer = Lexer::new(input.0).unwrap();
            let mut parser = Parser::new(&mut lexer);
            let program = parser.parse_program();
            dbg!(&program);
            check_parser_errors(&mut parser);
            assert_eq!(program.statements.len(), 1);
            let statement = (&*program.statements[0] as &dyn Any)
                .downcast_ref::<ExpressionStatement>()
                .expect("Couldn't parse as ExpressionStatement.");
            let infix_expression = (&*statement.expression as &dyn Any)
                .downcast_ref::<InfixExpression>()
                .expect("Couldn't parse as InfixExpression.");
            assert_eq!(infix_expression.operator, input.2);
            let lhs = (&*infix_expression.lhs as &dyn Any)
                .downcast_ref::<Boolean>()
                .expect("Couldn't parse as Boolean.");
            assert_eq!(lhs.value, input.1);
            assert_eq!(infix_expression.operator, input.2);
            let rhs = (&*infix_expression.rhs as &dyn Any)
                .downcast_ref::<Boolean>()
                .expect("Couldn't parse as Boolean.");
            assert_eq!(rhs.value, input.3);
        }
    }

    #[test]
    fn parsing_prefix_expressions() {
        let inputs = [("!5;", "!", 5), ("-15;", "-", 15)];
        for input in inputs {
            let mut lexer = Lexer::new(input.0).unwrap();
            let mut parser = Parser::new(&mut lexer);
            let program = parser.parse_program();
            check_parser_errors(&mut parser);
            assert_eq!(program.statements.len(), 1);
            dbg!(&program);
            let statement = (&*program.statements[0] as &dyn Any)
                .downcast_ref::<ExpressionStatement>()
                .expect("Couldn't parse as ExpressionStatement.");
            let prefix_expression = (&*statement.expression as &dyn Any)
                .downcast_ref::<PrefixExpression>()
                .expect("Couldn't parse as PrefixExpression.");
            assert_eq!(prefix_expression.operator, input.1);
            let integer_literal = (&*prefix_expression.rhs as &dyn Any)
                .downcast_ref::<IntegerLiteral>()
                .expect("Couldn't parse as IntegerLiteral.");
            assert_eq!(integer_literal.value, input.2);
        }

        let inputs = [("!true;", "!", true), ("!false;", "!", false)];
        for input in inputs {
            let mut lexer = Lexer::new(input.0).unwrap();
            let mut parser = Parser::new(&mut lexer);
            let program = parser.parse_program();
            check_parser_errors(&mut parser);
            assert_eq!(program.statements.len(), 1);
            dbg!(&program);
            let statement = (&*program.statements[0] as &dyn Any)
                .downcast_ref::<ExpressionStatement>()
                .expect("Couldn't parse as ExpressionStatement.");
            let prefix_expression = (&*statement.expression as &dyn Any)
                .downcast_ref::<PrefixExpression>()
                .expect("Couldn't parse as PrefixExpression.");
            assert_eq!(prefix_expression.operator, input.1);
            let integer_literal = (&*prefix_expression.rhs as &dyn Any)
                .downcast_ref::<Boolean>()
                .expect("Couldn't parse as Boolean.");
            assert_eq!(integer_literal.value, input.2);
        }
    }

    #[test]
    fn program_to_string() {
        let input = "let x = 5;";

        let mut lexer = Lexer::new(input).unwrap();
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();
        dbg!(&program);
        check_parser_errors(&mut parser);
        assert_eq!(program.to_string(), "let x = 5;");
        assert_eq!(program.statements.len(), 1);
        let let_statement = (&*program.statements[0] as &dyn Any)
            .downcast_ref::<LetStatement>()
            .expect("Couldn't parse as LetStatement.");
        assert_eq!(let_statement.name.to_string(), "x");
        assert_eq!(let_statement.value.to_string(), "5");
    }

    #[test]
    fn integer_literal() {
        let input = "5;";

        let mut lexer = Lexer::new(input).unwrap();
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();
        dbg!(&program);
        check_parser_errors(&mut parser);
        assert_eq!(program.statements.len(), 1);
        let statement = (&*program.statements[0] as &dyn Any)
            .downcast_ref::<ExpressionStatement>()
            .expect("Couldn't parse as ExpressionStatement.");
        let integer_literal = (&*statement.expression as &dyn Any)
            .downcast_ref::<IntegerLiteral>()
            .expect("Couldn't parse as Identifier.");
        assert_eq!(integer_literal.value, 5);
        assert_eq!(integer_literal.to_string(), "5");
    }

    #[test]
    fn identifier_expression() {
        let input = "foobar;";

        let mut lexer = Lexer::new(input).unwrap();
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();
        check_parser_errors(&mut parser);
        assert_eq!(program.statements.len(), 1);
        let statement = (&*program.statements[0] as &dyn Any)
            .downcast_ref::<ExpressionStatement>()
            .expect("Couldn't parse as ExpressionStatement.");
        let identifier = (&*statement.expression as &dyn Any)
            .downcast_ref::<Identifier>()
            .expect("Couldn't parse as Identifier.");
        assert_eq!(identifier.value, "foobar");
        assert_eq!(identifier.token_literal(), r#"Identifier("foobar")"#);
    }

    fn assert_return_statement(input: &dyn Statement, value: &str) {
        let statement = (input as &dyn Any)
            .downcast_ref::<ReturnStatement>()
            .unwrap();
        assert_eq!(statement.token_literal(), "Return");
        assert_eq!(statement.value.to_string(), value);
    }

    #[test]
    fn return_statements() {
        let inputs = [
            ("return 5;", "5"),
            ("return true;", "true"),
            ("return foobar;", "foobar"),
        ];

        for input in inputs {
            let mut lexer = Lexer::new(input.0).unwrap();
            let mut parser = Parser::new(&mut lexer);
            let program = parser.parse_program();
            check_parser_errors(&mut parser);
            assert_eq!(program.to_string(), input.0);
            assert_return_statement(&*program.statements[0], input.1);
        }
    }

    fn assert_let_statement(input: &dyn Statement, name: &str, value: &str) {
        let statement = (input as &dyn Any).downcast_ref::<LetStatement>().unwrap();
        assert_eq!(statement.token_literal(), "Let");
        assert_eq!(
            statement.name,
            Identifier {
                token: Token::Identifier(name.to_string()),
                value: name.to_string()
            }
        );
        assert_eq!(statement.value.to_string(), value);
    }

    #[test]
    fn let_statements() {
        let inputs = [
            ("let x = 5;", "x", "5"),
            ("let y = true;", "y", "true"),
            ("let foobar = y;", "foobar", "y"),
        ];

        for input in inputs {
            let mut lexer = Lexer::new(input.0).unwrap();
            let mut parser = Parser::new(&mut lexer);
            let program = parser.parse_program();
            check_parser_errors(&mut parser);
            assert_eq!(program.to_string(), input.0);
            assert_let_statement(&*program.statements[0], input.1, input.2);
        }
    }

    /// Helper function.
    fn check_parser_errors(input: &mut Parser) {
        if input.errors.is_empty() {
            return;
        }

        eprintln!("Found {} errors", input.errors.len());

        for error in &input.errors {
            eprintln!("{error}");
        }

        panic!("Ran into errors!");
    }

    /// Helper function.
    /// There's supposed to be a test_literal function that does some Go magic to run this function
    /// and the test_identifier functions, but I'm disregarding that.
    /// TODO: There's some more helper functions on page 100 that could be implemented eventually,
    /// but it'll require an annoying amount of Any and downcast checking.
    fn test_integer_literal(input: Box<dyn Expression>, value: i64) -> bool {
        let Some(integer_literal) = (&*input as &dyn Any).downcast_ref::<IntegerLiteral>() else {
            eprintln!("Expression is not an IntegerLiteral.");
            return false;
        };

        if integer_literal.value != value {
            eprintln!(
                "IntegerLiteral.value is {}, not {value}.",
                integer_literal.value
            );
            return false;
        }

        return true;
    }

    /// Helper function.
    fn test_identifier(input: Box<dyn Expression>, value: String) -> bool {
        let Some(identifier) = (&*input as &dyn Any).downcast_ref::<Identifier>() else {
            eprintln!("Expression is not an Identifier.");
            return false;
        };
        if identifier.value != value {
            eprintln!("Identifier.value is {}, not {value}.", identifier.value);
            return false;
        }

        return true;
    }
    fn test_boolean(input: Box<dyn Expression>, value: bool) -> bool {
        let Some(boolean) = (&*input as &dyn Any).downcast_ref::<Boolean>() else {
            eprintln!("Expression is not an Boolean.");
            return false;
        };
        if boolean.value != value {
            eprintln!("Boolean.value is {}, not {value}.", boolean.value);
            return false;
        }

        return true;
    }
}
