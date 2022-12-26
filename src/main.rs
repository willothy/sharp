// Author: Will Hopkins

use std::{fs, path::PathBuf};

use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};

use crate::tokenizer::Span;

use self::codegen::generator::CodeGenerator;

mod ast;
mod cli;
mod codegen;
mod debug;
mod lowering;
mod parser;
mod tokenizer;
mod typechecker;

const DEBUG_WRITE_IR_FILE: bool = true;

// TODO: Static struct member functions (::new(), etc.)
// TODO: Primitive member functions
// TODO: Modules and imports - In progress
// TODO: Arrays
// TODO: Traits

fn run<'a>() -> Result<(), Box<dyn std::error::Error>> {
    let args = cli::run()?;

    if args.debug {
        unsafe {
            crate::debug::DEBUG = true;
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
        let out_path = mod_path.with_file_name("test.s");
        //Target::initialize_aarch64(&InitializationConfig::default());
        Target::initialize_native(&InitializationConfig::default())?;
        let triple = TargetMachine::get_default_triple();
        let t = Target::from_triple(&triple)?;
        let Some(target_machine) = t.create_target_machine(
        &triple,
        TargetMachine::get_host_cpu_name().to_str().unwrap(),
        TargetMachine::get_host_cpu_features().to_str().unwrap(),
        inkwell::OptimizationLevel::Aggressive,
            RelocMode::Default,
            CodeModel::Default,
        ) else {
            return Err("Failed to create target machine".into());
        };
        generated_mod.set_triple(&triple);
        generated_mod.set_data_layout(&target_machine.get_target_data().get_data_layout());
        generated_mod.verify()?;
        target_machine.write_to_file(&generated_mod, FileType::Assembly, &out_path)?;
        let output = std::process::Command::new("clang")
            .arg(mod_path_str)
            .arg("-o")
            .arg(out_path.with_extension(""))
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

#[cfg(test)]
mod tests {
    use super::*;

    fn run_test(source: &str) -> Result<(), Box<dyn std::error::Error>> {
        let source_span = Span::from(source);
        let Some(tokens) = tokenizer::tokenize(source_span) else {
            return Err("Failed to tokenize source".into());
        };

        let module = parser::parse(tokens, source.to_string(), "main".into())?;

        let mut intermediate = lowering::IntermediateProgram::new();
        intermediate.lower(module, None)?;

        let mut tc = typechecker::TypeChecker::new(intermediate);
        let checked = tc.typecheck()?;

        let llvm_ctx = inkwell::context::Context::create();
        let mut generator = CodeGenerator::new(checked.clone(), &llvm_ctx);
        let generated_mod = generator.codegen()?;

        Target::initialize_native(&InitializationConfig::default())?;
        let triple = TargetMachine::get_default_triple();
        let t = Target::from_triple(&triple)?;
        let Some(target_machine) = t.create_target_machine(
        &triple,
        TargetMachine::get_host_cpu_name().to_str().unwrap(),
        TargetMachine::get_host_cpu_features().to_str().unwrap(),
        inkwell::OptimizationLevel::Aggressive,
            RelocMode::Default,
            CodeModel::Default,
        ) else {
            return Err("Failed to create target machine".into());
        };
        generated_mod.set_triple(&triple);
        generated_mod.set_data_layout(&target_machine.get_target_data().get_data_layout());
        generated_mod.verify()?;
        Ok(())
    }

    #[test]
    fn integration_test1() -> Result<(), Box<dyn std::error::Error>> {
        let source = r#"
fn printf(str format, ...) -> i32;
fn malloc(i64 size) -> i8*;
fn free(i8* ptr);

struct node {
    i32 value,
    node* next,
}

fn new_node(i32 value) -> node* {
    let node* ptr = malloc(sizeof(node)) as node*;
    *ptr.value = value;
    return ptr;
}

fn push_front(node* head, i32 value) -> node* {
    let node* ptr = new_node(value);
    *ptr.next = head;
    return ptr;
}

fn node_test() {
    let node* head = new_node(1);

    let i32 a = 2;
    let i32 b = 3;

    head = push_front(head, a);
    head = push_front(head, b);

    printf("head: %d\n", *head.value);
    printf("head.next: %d\n", *(*head.next).value);
    printf("head.next.next: %d\n", *(*(*head.next).next).value);

    free((*(*head).next).next as i8*);
    free((*head).next as i8*);
    free(head as i8*);
    return;
}

fn main() -> i32 {
    node_test();

    return 0;
}
        "#;
        run_test(source)
    }

    #[test]
    fn integration_test2() -> Result<(), Box<dyn std::error::Error>> {
        let source = r#"
fn printf(str format, ...) -> i32;
fn malloc(i64 size) -> i8*;
fn free(i8* ptr);

fn main() -> i32 {
    let i32* i = malloc(sizeof(i32)) as i32*;
    *i = 2;
    let i32 a = *i;
    free(i as i8*);
    return a;
}
        "#;
        run_test(source)
    }

    #[test]
    fn integration_test3() -> Result<(), Box<dyn std::error::Error>> {
        let source = r#"
fn printf(str format, ...) -> i32;
fn malloc(i64 size) -> i8*;
fn free(i8* ptr);

struct Test {
    i32 a,
    Test* next
}

fn main() -> i32 {
    let Test* t = malloc(sizeof(Test)) as Test*;
    *t.a = 5;
    *t.next = t;

    printf("t.a = %d\n", *t.a);
    let Test* n = *t.next;
    let i32 a = *n.a;
    printf("t.a 2 = %d\n", a);
    return 0;
}
        "#;
        run_test(source)
    }

    #[test]
    fn integration_test4() -> Result<(), Box<dyn std::error::Error>> {
        let source = r#"
use self::bingles::Test3;
//use self::bingles::printf;

fn malloc(i64 size) -> i8*;
fn free(i8* ptr);
fn printf(str format, ...) -> i32;


struct Test {
    i32 a,
    i32 b,
}

impl Test {
    fn new(i32 a, i32 b) -> Test {
        return Test {
            a,
            b,
        };
    }

    fn set_a(self, i32 a) {
        *self.a = a;
        return;
    }
}


struct Test2 {
    Test3 a,
    Test b,
}

impl Test2 {
    fn set_a(self, Test3 a) {
        *self.a.a = a;
        return;
    }
}

fn main() -> i32 {
    let Test2 t = Test2 {
        a: Test3 {
            a: 0,
            b: 0,
        },
        b: Test::new(9, 5),
    };
    printf("t.a.a: %d\n", t.a.a);
    printf("t.b.a: %d\n", t.b.a);
    t.set_a(Test3 {
        a: 10,
        b: 0,
    });

    t.b.set_a(25);
    
    
    printf("t.a.a: %d\n", t.a.a);
    printf("t.b.a: %d\n", t.b.a);
    return 0;
}

mod bingles {
    struct Test3 {
        i32 a,
        i32 b,
    }
    
}
        "#;
        run_test(source)
    }

    #[test]
    fn integration_test5() -> Result<(), Box<dyn std::error::Error>> {
        let source = r#"
use self::bingles::three;
use self::bingles::Test;
use self::bingles::printf;

mod bingles {
    #(no-mangle)
    fn printf(str fmt, ...) -> i32;

    fn three() -> i32 {
        printf("three: %d\n", 3);
        return 3;
    }

    struct Test {
        i32 a,
        i32 b,
        i32 c
    }
}

fn main() -> i32 {
    let Test a = Test{
        a: 1,
        b: 2,
        c: three(),
    };
    printf("%d\n", a.c);
    return 0;
}
        "#;
        run_test(source)
    }

    /* #[test]
    fn integration_testX() -> Result<(), Box<dyn std::error::Error>> {
        let source = r#""#;
        run_test(source)
    } */
}
