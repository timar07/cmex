mod errors;
mod lexer;
mod parser;
mod ast;
use std::{env, fs};

use ast::ast_dump::AstDumper;
use lexer::Lexer;
use parser::Parser;

fn main() {
    let args: Vec<String> = env::args().collect();
    let file = fs::read_to_string(&args[1])
        .expect(&format!("unable to read file `{}`", args[0]));
    let lexer = Lexer::from(file.as_str());
    let mut parser = Parser::new(lexer);
    println!("{}", AstDumper::new(&parser.parse().unwrap()));
}
