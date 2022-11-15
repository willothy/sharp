// Author: Will Hopkins

use std::{cell::RefCell, rc::Rc};

use crate::{codegen::generator::CodeGenerator, tokenizer::Span};

mod ast;
mod codegen;
mod parser;
mod tokenizer;
mod typechecker;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let source = r#"
fn main() -> i64 {
    let i64 a = 1;
    return a;
}
    "#;
    let source = Span::from(source);
    let (remaining, tokens) = tokenizer::tokenize(source)?;
    assert!(remaining.is_empty());
    //println!("{:?}", remaining);
    //println!("{:#?}", tokens);
    let module = parser::parse(tokens, "main".into())?;
    //println!("{:#?}", module);
    let checked = typechecker::typecheck_module(&module)?;

    let llvm_ctx = inkwell::context::Context::create();
    let generator = CodeGenerator::new(&checked, &llvm_ctx);
    let generated_mod = generator.codegen_module(&module)?;

    println!("{}", generated_mod.to_string());
    //println!("{:#?}", checked.names);
    //println!("{:#?}", checked.types);
    //println!("{:#?}", checked);
    Ok(())
}
