// Author: Will Hopkins

use std::{borrow::Borrow, fs, path::PathBuf};

use crate::tokenizer::Span;

use self::codegen::generator::CodeGenerator;

mod ast;
mod cli;
mod codegen;
mod lowering;
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

// TODO: Static struct member functions (::new(), etc.)
// TODO: Primitive member functions
// TODO: Modules and imports - In progress
// TODO: Arrays
// TODO: Traits

fn run<'a>() -> Result<(), Box<dyn std::error::Error>> {
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
        /* println!("{:#?}", module.borrow_mut().clone()); */
        /* println!(
            "lower: {:#?}",
            module_res::exports::lower(module.clone(), None)
                .borrow_mut()
                .clone()
        ) */
    }

    let mut intermediate = lowering::IntermediateProgram::new();
    let lowered = intermediate.lower(module, None)?;
    //let combined = intermediate.combine_modules()?;

    if args.ast_print {
        if args.json_ast {
            /* for module in &intermediate.modules {
                println!("{}", serde_json::to_string_pretty(module)?);
            } */
        } else {
            println!("{:#?}", lowered);
        };
    }

    let mut tc = typechecker::TypeChecker::new(intermediate);
    let checked = tc.typecheck()?;

    if args.typed_ast_print {
        if args.json_ast {
            let json = serde_json::to_string_pretty(&checked.modules)?;
            println!("{}", json);
            if let Some(ast_path) = args.write_ast_to {
                let ast_path = PathBuf::from(ast_path);
                std::fs::write(ast_path, json)?;
            }
        } else {
            println!("{:#?}", checked);
        }
    }

    let llvm_ctx = inkwell::context::Context::create();
    let mut generator = CodeGenerator::new(checked.clone(), &llvm_ctx);
    let generated_mod = generator.codegen()?;
    //let generated_mod = generator.codegen_module(checked)?;

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

fn main() -> Result<(), Box<dyn std::error::Error>> {
    run()?;

    Ok(())
}
