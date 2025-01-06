use std::{env, fs};

use cmex_ast::ast_dump::AstDumper;
use cmex_errors::ErrorBuilder;
use cmex_lexer::Lexer;
use cmex_parser::Parser;

fn main() {
    let args: Vec<String> = env::args().collect();
    let file = fs::read_to_string(&args[1])
        .unwrap_or_else(|_| panic!("unable to read file `{}`", args[0]));
    let lexer = Lexer::from(file.as_str());
    let mut parser = Parser::new(lexer);

    match &parser.parse() {
        Ok(ast) => {
            println!("{}", AstDumper::new(ast));
        }
        Err(errors) => {
            errors.iter().for_each(|err| {
                eprintln!(
                    "{}",
                    ErrorBuilder::new()
                        .filename(args[1].clone())
                        .tag("ParseError")
                        .info(format!("{}", err.0))
                        .context(file.as_str(), err.1)
                        .build()
                )
            });
        }
    }
}
