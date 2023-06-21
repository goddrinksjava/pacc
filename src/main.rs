use std::{env, fs};

use parser::Parser;

use crate::{code_gen::CodeGenerator, lexer::Lexer};

mod ast;
mod code_gen;
mod lexer;
mod parser;
mod symbols;

fn main() {
    let args: Vec<String> = env::args().collect();

    let src = fs::read_to_string(args.get(1).unwrap_or(&"test/test.txt".to_string()))
        .expect("Could not read file.");

    let token_stream = match Lexer::new(&src).lex() {
        Ok(ts) => ts,
        Err(e) => {
            println!("{}", e);
            std::process::exit(1);
        }
    };

    let ast = match Parser::new(token_stream).parse() {
        Ok(ast) => ast,
        Err(pe) => {
            println!("{}", pe);
            std::process::exit(1);
        }
    };

    CodeGenerator::generate(&ast);

    println!("Compilación exitosa");
}
