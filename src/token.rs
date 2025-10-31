//! Defines all the tokens to be used in other parts of the program.
#![allow(unused)]

use std::{collections::HashMap, fmt};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Token {
    DoubleEqualSign,
    NotEquals,
    GreaterThanEquals,
    LessThanEquals,
    ThickArrow,
    Arrow,
    LArrow,
    EqualSign,
    Plus,
    Asterisk,
    Minus,
    Semicolon,
    Colon,
    LCurly,
    LSquare,
    LParens,
    LT,
    RCurly,
    RSquare,
    RParens,
    GT,
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
    pub fn to_string(&self) -> String {
        use Token::*;
        match self {
            Identifier(id) => id.clone(),
            Number(n) => n.to_string(),
            DoubleEqualSign => "==".to_string(),
            NotEquals => "!=".to_string(),
            GreaterThanEquals => ">=".to_string(),
            LessThanEquals => "<=".to_string(),
            ThickArrow => "=>".to_string(),
            Arrow => "->".to_string(),
            LArrow => "<-".to_string(),
            EqualSign => "=".to_string(),
            Plus => "+".to_string(),
            Asterisk => "*".to_string(),
            Minus => "-".to_string(),
            Semicolon => ";".to_string(),
            Colon => ":".to_string(),
            LCurly => "{".to_string(),
            LSquare => "[".to_string(),
            LParens => "(".to_string(),
            LT => "<".to_string(),
            RCurly => "}".to_string(),
            RSquare => "]".to_string(),
            RParens => ")".to_string(),
            GT => ">".to_string(),
            Comma => ",".to_string(),
            If => "if".to_string(),
            Else => "else".to_string(),
            Let => "let".to_string(),
            Return => "return".to_string(),
            True => "true".to_string(),
            False => "false".to_string(),
            Fn => "fn".to_string(),
            SingleQuote => "'".to_string(),
            DoubleQuote => "\"".to_string(),
            QuestionMark => "?".to_string(),
            Bang => "!".to_string(),
            Slash => "/".to_string(),
            BackSlash => "\\".to_string(),
            Space => " ".to_string(),
            Newline => "\n".to_string(),
            Tab => "\t".to_string(),
            CarriageReturn => "\r".to_string(), // This one might be wrong? Maybe "\r\n"?
            EndOfFile => "\0".to_string(),
            Illegal => panic!("Tried to stringify an Illegal token."),
        }
    }
}

/// NOTE: This could maybe be a lookup table?
impl From<&str> for Token {
    fn from(input: &str) -> Token {
        use Token::*;
        match input {
            "==" => DoubleEqualSign,
            "!=" => NotEquals,
            ">=" => GreaterThanEquals,
            "<=" => LessThanEquals,
            "=>" => ThickArrow,
            "->" => Arrow,
            "<-" => LArrow,
            "=" => EqualSign,
            "+" => Plus,
            "*" => Asterisk,
            "-" => Minus,
            ";" => Semicolon,
            ":" => Colon,
            "{" => LCurly,
            "[" => LSquare,
            "(" => LParens,
            "<" => LT,
            "}" => RCurly,
            "]" => RSquare,
            ")" => RParens,
            ">" => GT,
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
