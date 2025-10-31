//! Defines all the forms of different statements and expressions that make up a program.
//! TODO: Honestly, this whole section should probably be using enums instead.
//! Golang has better ergonomics when doing this type of thing and force casting
//! into the underlying struct if you explicitly know the type.
//!
//! Look at using empty traits. I don't think we need `statement_node` and `expression_node`
//! functions.
#![allow(unused)]

use std::{
    any::Any,
    fmt::{Debug, write},
};

use crate::Token;

pub trait Node {
    fn token_literal(&self) -> String;
    fn to_string(&self) -> String;
}

pub trait Statement: Any + Debug + Node {
    fn statement_node(&self);
}

pub trait Expression: Any + Debug + Node {
    fn expression_node(&self);
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
}

impl Node for Program {
    fn token_literal(&self) -> String {
        if !self.statements.is_empty() {
            self.statements[0].token_literal()
        } else {
            String::new()
        }
    }

    fn to_string(&self) -> String {
        let mut out = String::new();
        for statement in &self.statements {
            out += statement.to_string().as_str();
        }
        out
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        format!("{:?}", self.token)
    }

    fn to_string(&self) -> String {
        self.value.clone()
    }
}

impl Statement for Identifier {
    fn statement_node(&self) {}
}

impl Expression for Identifier {
    fn expression_node(&self) {}
}

#[derive(Debug)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Box<dyn Expression>,
}

impl Expression for ExpressionStatement {
    fn expression_node(&self) {}
}

impl Node for ExpressionStatement {
    fn token_literal(&self) -> String {
        format!("{:?}", self.token)
    }

    fn to_string(&self) -> String {
        self.expression.to_string()
    }
}

impl Statement for ExpressionStatement {
    fn statement_node(&self) {}
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub token: Token,
    pub value: Box<dyn Expression>,
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        format!("{:?}", self.token)
    }

    fn to_string(&self) -> String {
        format!("return {};", self.value.to_string())
    }
}

impl Statement for ReturnStatement {
    fn statement_node(&self) {}
}

#[derive(Debug)]
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Box<dyn Expression>,
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        format!("{:?}", self.token)
    }

    fn to_string(&self) -> String {
        format!("let {} = {};", self.name.value, self.value.to_string())
    }
}

impl Statement for LetStatement {
    fn statement_node(&self) {}
}

#[derive(Debug)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> String {
        format!("{:?}", self.token)
    }

    fn to_string(&self) -> String {
        self.value.to_string()
    }
}

impl Statement for IntegerLiteral {
    fn statement_node(&self) {}
}

impl Expression for IntegerLiteral {
    fn expression_node(&self) {}
}

#[derive(Debug)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub rhs: Box<dyn Expression>,
}

impl Node for PrefixExpression {
    fn token_literal(&self) -> String {
        format!("{:?}", self.token)
    }

    fn to_string(&self) -> String {
        format!("({}{})", self.operator, self.rhs.to_string())
    }
}

impl Statement for PrefixExpression {
    fn statement_node(&self) {}
}

impl Expression for PrefixExpression {
    fn expression_node(&self) {}
}

#[derive(Debug)]
pub struct InfixExpression {
    pub token: Token,
    pub operator: String,
    pub lhs: Box<dyn Expression>,
    pub rhs: Box<dyn Expression>,
}

impl Node for InfixExpression {
    fn token_literal(&self) -> String {
        format!("{:?}", self.token)
    }

    fn to_string(&self) -> String {
        format!(
            "({} {} {})",
            self.lhs.to_string(),
            self.operator,
            self.rhs.to_string()
        )
    }
}

impl Statement for InfixExpression {
    fn statement_node(&self) {}
}

impl Expression for InfixExpression {
    fn expression_node(&self) {}
}

#[derive(Debug)]
pub struct Boolean {
    pub token: Token,
    pub value: bool,
}

impl Node for Boolean {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }

    fn to_string(&self) -> String {
        self.token.to_string()
    }
}

impl Statement for Boolean {
    fn statement_node(&self) {}
}

impl Expression for Boolean {
    fn expression_node(&self) {}
}

#[derive(Debug)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Box<dyn Expression>,
    pub consequence: BlockStatement, // This might end up being Box<dyn BlockStatement>
    pub alternative: Option<BlockStatement>,
}

impl Node for IfExpression {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }

    fn to_string(&self) -> String {
        let mut output = format!(
            "if ({}) {{ {} }}",
            self.condition.to_string(),
            self.consequence.to_string(),
        );
        if let Some(ref it) = self.alternative {
            output += &format!(" else {{ {} }}", it.to_string());
        }
        return output;
    }
}

impl Statement for IfExpression {
    fn statement_node(&self) {}
}

impl Expression for IfExpression {
    fn expression_node(&self) {}
}

#[derive(Debug)]
pub struct BlockStatement {
    pub token: Token, // the `{` token
    pub statements: Vec<Box<dyn Statement>>,
}

impl Node for BlockStatement {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }

    fn to_string(&self) -> String {
        let mut output = Vec::new();
        for statement in &self.statements {
            output.push(statement.to_string());
        }
        output.join("\n")
    }
}

impl Statement for BlockStatement {
    fn statement_node(&self) {}
}

impl Expression for BlockStatement {
    fn expression_node(&self) {}
}

#[derive(Debug)]
pub struct FunctionLiteral {
    pub token: Token, // the `{` token
    pub parameters: Vec<Box<Identifier>>,
    pub body: Box<BlockStatement>,
}

impl Node for FunctionLiteral {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }

    fn to_string(&self) -> String {
        let params = self
            .parameters
            .iter()
            .map(|it| it.to_string())
            .collect::<Vec<_>>()
            .join(", ");
        format!("fn ({}) {{ {} }}", params, self.body.to_string())
    }
}

impl Statement for FunctionLiteral {
    fn statement_node(&self) {}
}

impl Expression for FunctionLiteral {
    fn expression_node(&self) {}
}

#[derive(Debug)]
pub struct CallExpression {
    pub token: Token, // the `{` token
    pub function: Box<dyn Expression>,
    pub arguments: Vec<Box<dyn Expression>>,
}

impl Node for CallExpression {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }

    fn to_string(&self) -> String {
        format!(
            "{}({})",
            self.function.to_string(),
            self.arguments
                .iter()
                .map(|it| it.to_string())
                .collect::<Vec<_>>()
                .join(", "),
        )
    }
}

impl Statement for CallExpression {
    fn statement_node(&self) {}
}

impl Expression for CallExpression {
    fn expression_node(&self) {}
}
