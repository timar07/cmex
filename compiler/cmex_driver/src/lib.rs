use clap::Parser as CliParser;
use std::fs::{self, File};
use std::io::BufWriter;
use tracing_subscriber::EnvFilter;

use cmex_ast::{ast_dump::AstDumper, token::TokenTag, TranslationUnit};
use cmex_compile::Compiler;
use cmex_errors::{ErrorBuilder, ErrorEmitter};
use cmex_lexer::{Lexer, Tokens};
use cmex_macros::MacroExpander;
use cmex_parser::{ParseOptions, Parser};

#[derive(CliParser)]
#[command(name = "cmex")]
#[command(version, about = "The C programming language macro extension")]
struct Cli {
    /// Input file
    src: Option<String>,

    /// Sets an output file, defaults to `out.c`
    #[arg(short, long, value_name = "OUTPUT")]
    output: Option<String>,

    /// Dump parsed AST
    #[arg(long)]
    ast_dump: bool,

    /// Expand macros in dumped AST
    #[arg(short, long)]
    expand: bool,
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

    let cli = Cli::parse();

    let src = cli.src.expect("file name must be specified");
    let file = fs::read_to_string(&src)
        .unwrap_or_else(|_| panic!("unable to read file `{}`", src));
    let lexer = Lexer::from(file.as_str());
    let emitter = ErrorEmitter::new(
        file.as_str(),
        ErrorBuilder::new().filename(src.clone()),
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
            if cli.expand || cli.output.is_some() {
                if let Err(err) = expander.expand_ast(ast) {
                    emitter.emit(&err);
                    return;
                }
            }

            if cli.ast_dump {
                println!("{}", AstDumper::new(ast));
                return;
            }

            if let Some(path) = cli.output {
                compile_file(&path, ast).unwrap();
            }
        }
        Err(errors) => {
            errors.iter().for_each(|err| {
                emitter.emit(err);
            });
        }
    }
}
