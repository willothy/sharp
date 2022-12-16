// Author: Will Hopkins

use std::{
    fs::{self, File},
    io::Read,
};

use crate::{codegen::generator::CodeGenerator, tokenizer::Span};

mod ast;
mod cli;
mod codegen;
mod parser;
mod tokenizer;
mod typechecker;

const DEBUG_COMPILE_MODULE: bool = true;
const DEBUG_WRITE_IR_FILE: bool = true;

pub static mut DEBUG: bool = false;

#[macro_export]
macro_rules! debug {
    ($name:expr) => {
        if unsafe { crate::DEBUG } {
            println!("{}", $name);
        }
    };
}

#[macro_export]
macro_rules! debugln {
    () => {
        if unsafe { crate::DEBUG } {
            println!("{}", line!());
        }
    };
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = cli::run()?;

    if args.debug {
        unsafe {
            crate::DEBUG = true;
        }
    }

    let source_path = args.file;
    let source = fs::read_to_string(&source_path)?;

    let source_span = Span::from(source.as_str());
    let Some(tokens) = tokenizer::tokenize(source_span) else {
        return Err("Failed to tokenize source".into());
    };

    if args.tokens_print {
        println!("{:#?}", tokens);
    }

    let module = parser::parse(tokens, source.to_string(), "main".into())?;

    if args.ast_print {
        println!("{:#?}", module);
    }

    let mut tc = typechecker::TypeChecker::new();
    let checked = tc.typecheck_module(&module)?;

    let llvm_ctx = inkwell::context::Context::create();
    let generator = CodeGenerator::new(&checked, &llvm_ctx);
    let generated_mod = generator.codegen_module()?;

    if args.ir_print {
        println!("{}", generated_mod.print_to_string().to_string());
    }

    let module_str = generated_mod.to_string();
    let mod_path = args.output;
    if DEBUG_WRITE_IR_FILE || DEBUG_COMPILE_MODULE {
        std::fs::write(&mod_path, module_str)?;
        //generated_mod.write_bitcode_to_path(mod_path.as_path());
    }

    if DEBUG_COMPILE_MODULE {
        let mod_path_str = mod_path.to_str().unwrap();
        let out_path = mod_path.with_file_name("test");
        let output = std::process::Command::new("clang")
            .arg(mod_path_str)
            .arg("-o")
            .arg(out_path)
            .arg("-fcolor-diagnostics")
            .output()?;

        let out = snailquote::unescape(String::from_utf8(output.stdout)?.as_str())?;
        let err = snailquote::unescape(String::from_utf8(output.stderr)?.as_str())?;

        println!("{}", out);
        println!("{}", err);
    }

    Ok(())
}
