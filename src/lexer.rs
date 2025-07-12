#![allow(unused)]

use crate::token::*;
///
/// TODO: Missing some concept of a range for the input[x..y] to pass to token parsing.
#[derive(Debug)]
pub struct Lexer {
    ch: char,
    input: Vec<char>,
    pos: usize,
    read_pos: usize,
}

impl Lexer {
    pub fn new(input: &str) -> Result<Self, &'static str> {
        if input.is_empty() {
            Err("Input is empty.")
        } else {
            let input = input.chars().collect::<Vec<_>>();
            let mut out = Self {
                ch: '\0',
                input,
                pos: 0,
                read_pos: 0,
            };
            out.read_char();
            Ok(out)
        }
    }
    /// TODO: This is copy paste of read_char. fix that and then integrate it.
    pub fn peek_char(&mut self) -> char {
        if self.read_pos >= self.input.len() {
            '\0'
        } else {
            self.input[self.read_pos]
        }
    }

    /// Advances the read position until the end.
    pub fn read_char(&mut self) {
        if self.read_pos >= self.input.len() {
            self.ch = '\0';
        } else {
            self.ch = self.input[self.read_pos];
        }

        self.pos = self.read_pos;
        self.read_pos += 1;
    }

    pub fn next_token(&mut self) -> Token {
        use Token::*;

        self.skip_whitespace();
        let mut out = Illegal;

        let tok = self.ch.to_string().as_str().into();

        if tok != Illegal {
            if self.read_pos == self.input.len() {
                out = tok;
            } else if tok == Bang && self.peek_char() == '=' {
                out = NotEquals;
                self.read_char();
            } else if tok == Assign && self.peek_char() == '=' {
                out = Equals;
                self.read_char();
            } else if tok == Assign && self.peek_char() == '>' {
                out = ThickArrow;
                self.read_char();
            } else if tok == Minus && self.peek_char() == '>' {
                out = Arrow;
                self.read_char();
            } else if tok == LAngle && self.peek_char() == '-' {
                out = LArrow;
                self.read_char();
            } else {
                out = tok;
            }

            self.read_char();
        } else if Lexer::is_char(self.ch) {
            let ident = self.read_identifier();
            let keyword = ident.as_str().into();

            if keyword != Token::Illegal {
                out = keyword;
            } else {
                out = Token::Identifier(ident);
            }
        } else if Lexer::is_digit(self.ch) {
            out = Token::Number(self.read_number());
        }

        out
    }

    pub fn read_number(&mut self) -> i64 {
        let start_pos = self.pos;
        loop {
            if Lexer::is_digit(self.ch) {
                self.read_char();
            } else {
                break;
            }
        }

        self.input[start_pos..self.pos]
            .iter()
            .collect::<String>()
            .parse()
            .unwrap()
    }

    pub fn read_identifier(&mut self) -> String {
        let start_pos = self.pos;
        loop {
            if Lexer::is_char(self.ch) {
                self.read_char();
            } else {
                break;
            }
        }

        self.input[start_pos..self.pos].iter().collect::<String>()
    }

    pub fn skip_whitespace(&mut self) {
        loop {
            if self.ch == ' ' || self.ch == '\t' || self.ch == '\n' || self.ch == '\r' {
                self.read_char();
            } else {
                break;
            }
        }
    }

    fn is_char(input: char) -> bool {
        matches!(input, 'a'..='z' | 'A'..='Z' | '_')
    }

    fn is_digit(input: char) -> bool {
        input.is_ascii_digit()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn next_token_long() {
        use Token::*;
        let mut lex = Lexer::new(
            "let five = 5;
let ten = 10;
let add = fn(x, y) {
  x + y; };

let result = add(five, ten);
!-/*5;
5 < 10 > 5;
if (5 < 10) {
    return true; }
else {
    return false;
}",
        )
        .unwrap();
        assert_eq!(lex.next_token(), Let);
        assert_eq!(lex.next_token(), Identifier("five".to_string()));
        assert_eq!(lex.next_token(), Assign);
        assert_eq!(lex.next_token(), Number(5));
        assert_eq!(lex.next_token(), Semicolon);

        assert_eq!(lex.next_token(), Let);
        assert_eq!(lex.next_token(), Identifier("ten".to_string()));
        assert_eq!(lex.next_token(), Assign);
        assert_eq!(lex.next_token(), Number(10));
        assert_eq!(lex.next_token(), Semicolon);

        assert_eq!(lex.next_token(), Let);
        assert_eq!(lex.next_token(), Identifier("add".to_string()));
        assert_eq!(lex.next_token(), Assign);
        assert_eq!(lex.next_token(), Fn);
        assert_eq!(lex.next_token(), LParens);
        assert_eq!(lex.next_token(), Identifier("x".to_string()));
        assert_eq!(lex.next_token(), Comma);
        assert_eq!(lex.next_token(), Identifier("y".to_string()));
        assert_eq!(lex.next_token(), RParens);
        assert_eq!(lex.next_token(), LCurly);
        assert_eq!(lex.next_token(), Identifier("x".to_string()));
        assert_eq!(lex.next_token(), Plus);
        assert_eq!(lex.next_token(), Identifier("y".to_string()));
        assert_eq!(lex.next_token(), Semicolon);
        assert_eq!(lex.next_token(), RCurly);
        assert_eq!(lex.next_token(), Semicolon);

        assert_eq!(lex.next_token(), Let);
        assert_eq!(lex.next_token(), Identifier("result".to_string()));
        assert_eq!(lex.next_token(), Assign);
        assert_eq!(lex.next_token(), Identifier("add".to_string()));
        assert_eq!(lex.next_token(), LParens);
        assert_eq!(lex.next_token(), Identifier("five".to_string()));
        assert_eq!(lex.next_token(), Comma);
        assert_eq!(lex.next_token(), Identifier("ten".to_string()));
        assert_eq!(lex.next_token(), RParens);
        assert_eq!(lex.next_token(), Semicolon);

        assert_eq!(lex.next_token(), Bang);
        assert_eq!(lex.next_token(), Minus);
        assert_eq!(lex.next_token(), Slash);
        assert_eq!(lex.next_token(), Asterisk);
        assert_eq!(lex.next_token(), Number(5));
        assert_eq!(lex.next_token(), Semicolon);
        assert_eq!(lex.next_token(), Number(5));
        assert_eq!(lex.next_token(), LAngle);
        assert_eq!(lex.next_token(), Number(10));
        assert_eq!(lex.next_token(), RAngle);
        assert_eq!(lex.next_token(), Number(5));
        assert_eq!(lex.next_token(), Semicolon);
        assert_eq!(lex.next_token(), If);
        assert_eq!(lex.next_token(), LParens);
        assert_eq!(lex.next_token(), Number(5));
        assert_eq!(lex.next_token(), LAngle);
        assert_eq!(lex.next_token(), Number(10));
        assert_eq!(lex.next_token(), RParens);
        assert_eq!(lex.next_token(), LCurly);
        assert_eq!(lex.next_token(), Return);
        assert_eq!(lex.next_token(), True);
        assert_eq!(lex.next_token(), Semicolon);
        assert_eq!(lex.next_token(), RCurly);
        assert_eq!(lex.next_token(), Else);
        assert_eq!(lex.next_token(), LCurly);
        assert_eq!(lex.next_token(), Return);
        assert_eq!(lex.next_token(), False);
        assert_eq!(lex.next_token(), Semicolon);
        assert_eq!(lex.next_token(), RCurly);
        assert_eq!(lex.next_token(), EndOfFile);
    }

    #[test]
    fn read_identifier() {
        let mut lex = Lexer::new("abc def").unwrap();
        assert_eq!(&lex.read_identifier(), "abc");
        lex.skip_whitespace();
        assert_eq!(&lex.read_identifier(), "def");

        let mut lex = Lexer::new("abc\tdef \t").unwrap();
        assert_eq!(&lex.read_identifier(), "abc");
        lex.skip_whitespace();
        assert_eq!(&lex.read_identifier(), "def");

        let mut lex = Lexer::new("test").unwrap();
        assert_eq!(&lex.read_identifier(), "test");

        let mut lex = Lexer::new("abcdef").unwrap();
        assert_eq!(&lex.read_identifier(), "abcdef");
    }

    #[test]
    fn next_token() {
        let mut lex = Lexer::new("+={}()[];:*").unwrap();
        use Token::*;
        assert_eq!(lex.next_token(), Plus);
        assert_eq!(lex.next_token(), Assign);
        assert_eq!(lex.next_token(), LCurly);
        assert_eq!(lex.next_token(), RCurly);
        assert_eq!(lex.next_token(), LParens);
        assert_eq!(lex.next_token(), RParens);
        assert_eq!(lex.next_token(), LSquare);
        assert_eq!(lex.next_token(), RSquare);
        assert_eq!(lex.next_token(), Semicolon);
        assert_eq!(lex.next_token(), Colon);
        assert_eq!(lex.next_token(), Asterisk);
        assert_eq!(lex.next_token(), EndOfFile);

        let mut lex = Lexer::new("123 456").unwrap();
        assert_eq!(lex.next_token(), Number(123));
        assert_eq!(lex.next_token(), Number(456));

        let mut lex = Lexer::new("!= == -> <- =>").unwrap();
        assert_eq!(lex.next_token(), NotEquals);
        assert_eq!(lex.next_token(), Equals);
        assert_eq!(lex.next_token(), Arrow);
        assert_eq!(lex.next_token(), LArrow);
        assert_eq!(lex.next_token(), ThickArrow);

        let mut lex = Lexer::new("let      x y z;").unwrap();
        assert_eq!(lex.next_token(), Let);
        assert_eq!(lex.next_token(), Identifier("x".to_string()));
        assert_eq!(lex.next_token(), Identifier("y".to_string()));
        assert_eq!(lex.next_token(), Identifier("z".to_string()));
        assert_eq!(lex.next_token(), Semicolon);

        let mut lex = Lexer::new("let x = 5;").unwrap();
        assert_eq!(lex.next_token(), Let);
        assert_eq!(lex.next_token(), Identifier("x".to_string()));
        assert_eq!(lex.next_token(), Assign);
        assert_eq!(lex.next_token(), Number(5));
        assert_eq!(lex.next_token(), Semicolon);
    }

    #[test]
    fn read_char() {
        let mut lex = Lexer::new("let x = 5;").unwrap();
        assert_eq!(lex.ch, 'l');
        lex.read_char();
        assert_eq!(lex.ch, 'e');
        lex.read_char();
        assert_eq!(lex.ch, 't');
        lex.read_char();
        assert_eq!(lex.ch, ' ');
        lex.read_char();
        assert_eq!(lex.ch, 'x');
        lex.read_char();
        assert_eq!(lex.ch, ' ');
        lex.read_char();
        assert_eq!(lex.ch, '=');
        lex.read_char();
        assert_eq!(lex.ch, ' ');
        lex.read_char();
        assert_eq!(lex.ch, '5');
        lex.read_char();
        assert_eq!(lex.ch, ';');
        lex.read_char();
        assert_eq!(lex.ch, '\0');
        lex.read_char();
        assert_eq!(lex.ch, '\0');
        lex.read_char();
        assert_eq!(lex.ch, '\0');
    }
}
