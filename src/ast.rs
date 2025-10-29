#![allow(unused)]

use std::{
    any::Any,
    fmt::{Debug, write},
};

use crate::Token;

// TODO: Honestly, this whole section should probably be using enums instead.
// Golang has better ergonomics when doing this type of thing and force casting
// into the underlying struct if you explicitly know the type.
pub trait Node {
    fn token_literal(&self) -> String;
    fn to_string(&self) -> String;
}

pub trait Statement: Node + Any + Debug {
    fn statement_node(&self);
}

pub trait Expression: Node + Any + Debug {
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
    // This is probably redundant with `token`.
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
        format!("{:?}", self.token)
    }
}

impl Statement for ExpressionStatement {
    fn statement_node(&self) {}
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub token: Token,
    // // This should be the correct type
    // pub value: Box<dyn Expression>,
    // This is the filler type so at least fields get populated.
    pub value: Option<()>,
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        format!("{:?}", self.token)
    }

    fn to_string(&self) -> String {
        format!("return {:?};", self.value)
    }
}

impl Statement for ReturnStatement {
    fn statement_node(&self) {}
}

#[derive(Debug)]
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    // // This should be the correct type
    // pub value: Box<dyn Expression>,
    // This is the filler type so at least fields get populated.
    pub value: Box<dyn Expression>,
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        format!("{:?}", self.token)
    }

    fn to_string(&self) -> String {
        format!("let {} = {:?};", self.name.value, self.value)
    }
}

impl Statement for LetStatement {
    fn statement_node(&self) {}
}
