use std::io::Write;

use monkey_interpreter::*;

fn main() {
    let mut input = String::new();
    loop {
        print!(">> ");
        std::io::stdout().flush().unwrap();
        std::io::stdin().read_line(&mut input).unwrap();
        let line = input.trim();
        if line == "exit" || line == "quit" || line.is_empty() {
            break;
        }
        let mut lexer = Lexer::new(line).unwrap();
        loop {
            let tok = lexer.next_token();
            if tok == Token::EndOfFile {
                break;
            }
            println!("{tok:?}");
        }
        input.clear();
    }
}
