// Author: Will Hopkins

use crate::{codegen::generator::CodeGenerator, tokenizer::Span};

mod ast;
mod codegen;
mod parser;
mod tokenizer;
mod typechecker;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let source = r#"
fn printf(str format) -> i32;

struct Num {
    i32 value
}

fn main() -> i32 {
    let Num n = Num { value: 5 };
    printf("Hello, %d!", n.value);
    return n.value;
}
    "#;
    let source = Span::from(source);
    let (remaining, tokens) = tokenizer::tokenize(source)?;
    assert!(remaining.is_empty());
    //println!("{:?}", remaining);
    //println!("{:#?}", tokens);
    let module = parser::parse(tokens, "main".into())?;
    println!("{:#?}", module);
    let mut tc = typechecker::TypeChecker::new();
    let checked = tc.typecheck_module(&module)?;

    //println!("{:#?}", checked.module);

    let llvm_ctx = inkwell::context::Context::create();
    let generator = CodeGenerator::new(&checked, &llvm_ctx);
    let generated_mod = generator.codegen_module()?;

    println!("{}", generated_mod.to_string());
    //println!("{:#?}", checked.names);
    //println!("{:#?}", checked.types);
    //println!("{:#?}", checked);
    Ok(())
}
