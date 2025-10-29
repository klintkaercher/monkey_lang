#![allow(unused)]

use std::{collections::HashMap, fmt};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Token {
    Equals,
    NotEquals,
    ThickArrow,
    Arrow,
    LArrow,
    Assign,
    Plus,
    Asterisk,
    Minus,
    Semicolon,
    Colon,
    LCurly,
    LSquare,
    LParens,
    LAngle,
    RCurly,
    RSquare,
    RParens,
    RAngle,
    Comma,
    If,
    Else,
    Let,
    Return,
    True,
    False,
    Fn,
    SingleQuote,
    DoubleQuote,
    QuestionMark,
    Bang,
    Slash,
    BackSlash,
    Identifier(String),
    Number(i64), // Currently only integers.
    Space,
    Tab,
    Newline,
    CarriageReturn,
    Illegal,
    EndOfFile,
}

impl Token {
    fn is_ident(input: &str) -> Option<Token> {
        if input.chars().all(|c| c.is_ascii_alphabetic() || c == '_') {
            Some(Token::Identifier(input.to_string()))
        } else {
            None
        }
    }
    fn is_number(input: &str) -> Option<Token> {
        if input.chars().all(|c| c.is_ascii_digit()) {
            Some(Token::Number(input.parse::<i64>().unwrap()))
        } else {
            None
        }
    }
}

/// NOTE: This could maybe be a lookup table?
impl From<&str> for Token {
    fn from(input: &str) -> Token {
        use Token::*;
        match input {
            "==" => Equals,
            "!=" => NotEquals,
            ">=" => NotEquals,
            "<=" => NotEquals,
            "=>" => ThickArrow,
            "->" => Arrow,
            "<-" => LArrow,
            "=" => Assign,
            "+" => Plus,
            "*" => Asterisk,
            "-" => Minus,
            ";" => Semicolon,
            ":" => Colon,
            "{" => LCurly,
            "[" => LSquare,
            "(" => LParens,
            "<" => LAngle,
            "}" => RCurly,
            "]" => RSquare,
            ")" => RParens,
            ">" => RAngle,
            "," => Comma,
            "if" => If,
            "else" => Else,
            "let" => Let,
            "return" => Return,
            "true" => True,
            "false" => False,
            "fn" => Fn,
            "'" => SingleQuote,
            "\"" => DoubleQuote,
            "?" => QuestionMark,
            "!" => Bang,
            "/" => Slash,
            "\\" => BackSlash,
            " " => Space,
            "\n" => Newline,
            "\t" => Tab,
            "\r" => CarriageReturn, // This one might be wrong? Maybe "\r\n"?
            "\0" => EndOfFile,
            _ => Illegal,
        }
    }
}
