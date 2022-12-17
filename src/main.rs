// Author: Will Hopkins

use std::fs;

use crate::{codegen::generator::CodeGenerator, tokenizer::Span};

mod ast;
mod cli;
mod codegen;
mod parser;
mod tokenizer;
mod typechecker;

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

// TODO: Pointer and primitive casting (As expression)
// TODO: Struct member functions
// TODO: Modules and imports
// TODO: Arrays
// TODO: Traits

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
    let checked = tc.typecheck_module(module)?;

    if args.typed_ast_print {
        println!("{:#?}", checked.module);
    }

    let llvm_ctx = inkwell::context::Context::create();
    let generator = CodeGenerator::new(&checked, &llvm_ctx);
    let generated_mod = generator.codegen_module()?;

    if args.ir_print {
        println!("{}", generated_mod.print_to_string().to_string());
    }

    let module_str = generated_mod.to_string();
    let mod_path = args.output;
    if DEBUG_WRITE_IR_FILE || args.compile {
        std::fs::write(&mod_path, module_str)?;
        //generated_mod.write_bitcode_to_path(mod_path.as_path());
    }

    if args.compile {
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

    if args.jit_exec {
        let jit = generated_mod.create_jit_execution_engine(inkwell::OptimizationLevel::Default)?;
        unsafe {
            let Some(main) = generated_mod.get_function("main") else {
                return Err("Failed to get main function".into());
            };
            jit.run_function(main, &[]);
        }
    }

    Ok(())
}
