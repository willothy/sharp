use std::path::PathBuf;

use clap::{command, Parser};

#[derive(Parser, Debug)]
#[command(author, version, about)]
struct Args {
    file: PathBuf,
    #[arg(short='o', long, default_value_t = String::from("a.out"))]
    output: String,
    #[arg(short = 'i', long)]
    ir_print: bool,
    #[arg(short = 'd', long)]
    debug: bool,
    #[arg(short = 'e', long)]
    jit_exec: bool,
    #[arg(short = 'c', long, conflicts_with = "output_asm")]
    output_obj: bool,
    #[arg(short = 's', long, conflicts_with = "output_obj")]
    output_asm: bool,
}

impl Args {
    pub fn validate(self) -> Result<ArgsValidated, Box<dyn std::error::Error>> {
        Ok(ArgsValidated {
            file: self.file,
            output: PathBuf::from(self.output),
            ir_print: self.ir_print,
            debug: self.debug,
            jit_exec: self.jit_exec,
            output_obj: self.output_obj,
            output_asm: self.output_asm,
        })
    }
}

#[derive(Debug)]
pub struct ArgsValidated {
    pub file: PathBuf,
    pub output: PathBuf,
    pub ir_print: bool,
    pub debug: bool,
    pub jit_exec: bool,
    pub output_obj: bool,
    pub output_asm: bool,
}

pub fn run() -> Result<ArgsValidated, Box<dyn std::error::Error>> {
    let args = Args::parse().validate()?;
    Ok(args)
}
