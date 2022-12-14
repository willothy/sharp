// Author: Will Hopkins

use std::fs::{self, File};

use crate::{codegen::generator::CodeGenerator, tokenizer::Span};

mod ast;
mod codegen;
mod parser;
mod tokenizer;
mod typechecker;

const DEBUG_COMPILE_MODULE: bool = true;
const DEBUG_WRITE_IR_FILE: bool = true;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let source_path = std::env::current_dir()?.join("test.sharp");
    let source_file = fs::read_to_string(&source_path)?;
    let source = source_file;

    let source_span = Span::from(source.as_str());
    let Some(tokens) = tokenizer::tokenize(source_span) else {
        return Err("Failed to tokenize source".into());
    };

    let module = parser::parse(tokens, source.to_string(), "main".into())?;
    println!("{:#?}", module);
    let mut tc = typechecker::TypeChecker::new();
    let checked = tc.typecheck_module(&module)?;

    let llvm_ctx = inkwell::context::Context::create();
    let generator = CodeGenerator::new(&checked, &llvm_ctx);
    let generated_mod = generator.codegen_module()?;

    println!("{}", generated_mod.print_to_string().to_string());

    let module_str = generated_mod.to_string();
    let mod_path = std::env::current_dir()?.join("out.ll");
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
            .output()?;

        println!("{}", String::from_utf8(output.stdout)?);
        println!("{}", String::from_utf8(output.stderr)?);
    }

    Ok(())
}
