// Author: Will Hopkins

use crate::tokenizer::Span;

mod ast;
mod parser;
mod tokenizer;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let source = r#"
struct thing {
    i32 a,
    i32 b,
}

mod test {
    fn test2(int a) -> thing {
        return thing {
            a,
            b: 2,
        };
    }
}
    "#;
    let source = Span::from(source);
    let (remaining, tokens) = tokenizer::tokenize(source)?;
    //println!("{:?}", remaining);
    println!("{:#?}", tokens);
    let module = parser::parse(tokens, "main".into())?;
    println!("{:#?}", module);
    Ok(())
}
