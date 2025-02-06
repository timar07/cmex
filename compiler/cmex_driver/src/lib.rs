use std::fs::File;
use std::io::BufWriter;
use std::{env, fs};
use tracing_subscriber::EnvFilter;

use cmex_ast::{ast_dump::AstDumper, token::TokenTag, TranslationUnit};
use cmex_compile::Compiler;
use cmex_errors::{ErrorBuilder, ErrorEmitter};
use cmex_lexer::{Lexer, Tokens};
use cmex_macros::MacroExpander;
use cmex_parser::{ParseOptions, Parser};

/// TODO: maybe it worth using clap?
fn print_help() {
    println!("Usage: cmex [options] input");
    println!("Options:");
    println!(
        "
    -h, --help     Print help
    -ast-dump      Dump parsed AST
    -E             Expand macros in dumped tree
    "
    );
}

fn compile_file(path: &String, ast: &TranslationUnit) -> std::io::Result<()> {
    let mut f = BufWriter::new(File::create(path)?);
    Compiler::new(&mut f, ast).compile();
    Ok(())
}

pub fn main() {
    tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_default_env())
        .init();

    let args: Vec<String> = env::args().collect();

    if args.contains(&"-h".into()) {
        print_help();
        return;
    }

    let file = fs::read_to_string(&args[1])
        .unwrap_or_else(|_| panic!("unable to read file `{}`", args[0]));
    let lexer = Lexer::from(file.as_str());
    let emitter = ErrorEmitter::new(
        file.as_str(),
        ErrorBuilder::new().filename(args[1].clone()),
    );
    let tokens = Tokens(
        lexer
            .spanned()
            .map(|(res, span)| match res {
                Ok(tok) => (tok, span),
                Err(e) => {
                    emitter.emit(&(e, span));
                    (TokenTag::Error, span)
                }
            })
            .collect(),
    );
    let mut parser = Parser::new(
        &tokens,
        &emitter,
        ParseOptions {
            allow_comma_op: true,
        },
    );
    let mut expander = MacroExpander::new(&emitter);

    match &mut parser.parse() {
        Ok(ast) => {
            if args.contains(&"-E".into()) || args.contains(&"-o".into()) {
                if let Err(err) = expander.expand_ast(ast) {
                    emitter.emit(&err);
                    return;
                }
            }

            if args.contains(&"-ast-dump".into()) {
                println!("{}", AstDumper::new(ast));
                return;
            }

            let mut iter = args.iter();
            if iter.any(|flag| *flag == "-o") {
                match iter.next() {
                    Some(path) => {
                        compile_file(path, ast).unwrap();
                    }
                    None => {
                        panic!("missing filename after `-o` option");
                    }
                }
            }
        }
        Err(errors) => {
            errors.iter().for_each(|err| {
                emitter.emit(err);
            });
        }
    }
}
