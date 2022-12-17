use std::path::PathBuf;

use clap::{command, Parser};

#[derive(Parser, Debug)]
#[command(author, version, about)]
struct Args {
    file: PathBuf,
    #[arg(short, long, default_value_t = String::from("out.ll"))]
    output: String,
    #[arg(short, long)]
    ast_print: bool,
    #[arg(short, long)]
    tokens_print: bool,
    #[arg(short = 'x', long)]
    typed_ast_print: bool,
    #[arg(short, long)]
    ir_print: bool,
    #[arg(short, long)]
    debug: bool,
    #[arg(short, long)]
    jit_exec: bool,
    #[arg(short, long)]
    compile: bool,
}

impl Args {
    pub fn validate(self) -> Result<ArgsValidated, Box<dyn std::error::Error>> {
        Ok(ArgsValidated {
            file: self.file,
            output: PathBuf::from(self.output),
            ast_print: self.ast_print,
            tokens_print: self.tokens_print,
            ir_print: self.ir_print,
            debug: self.debug,
            jit_exec: self.jit_exec,
            compile: self.compile,
            typed_ast_print: self.typed_ast_print,
        })
    }
}

#[derive(Debug)]
pub struct ArgsValidated {
    pub file: PathBuf,
    pub output: PathBuf,
    pub ast_print: bool,
    pub tokens_print: bool,
    pub ir_print: bool,
    pub debug: bool,
    pub jit_exec: bool,
    pub compile: bool,
    pub typed_ast_print: bool,
}

pub fn run() -> Result<ArgsValidated, Box<dyn std::error::Error>> {
    let args = Args::parse().validate()?;
    Ok(args)
}
