use crate::{
    debug,
    tokenizer::Literal,
    typechecker::{
        context::{LocalTypecheckContext, TypeSig},
        typed_ast::TypedLiteral,
        TypeChecker,
    },
};

impl<'tc> TypeChecker<'tc> {
    pub fn typecheck_literal(
        &self,
        literal: &Literal,
        local_ctx: &LocalTypecheckContext<'tc>,
    ) -> Result<TypedLiteral<'tc>, String> {
        debug!(format!("typechecker::typecheck_literal: {:?}", literal));
        match literal {
            Literal::Str(s, pos) => Ok(TypedLiteral {
                ty: self.ctx.get_type("str".to_string())?,
                literal: Literal::Str(s.clone(), pos.clone()),
            }),
            Literal::Int(i, pos) => match &local_ctx.result_type {
                Some(t) => {
                    if t.sig() == self.ctx.get_type("i32".into())?.sig() {
                        Ok(TypedLiteral {
                            ty: t.clone(),
                            literal: Literal::Int(*i, pos.clone()),
                        })
                    } else if t.sig() == self.ctx.get_type("i64".into())?.sig() {
                        Ok(TypedLiteral {
                            ty: t.clone(),
                            literal: Literal::Int(*i, pos.clone()),
                        })
                    } else if t.sig() == self.ctx.get_type("i8".into())?.sig() {
                        Ok(TypedLiteral {
                            ty: t.clone(),
                            literal: Literal::Int(*i, pos.clone()),
                        })
                    } else if t.sig() == self.ctx.get_type("i16".into())?.sig() {
                        Ok(TypedLiteral {
                            ty: t.clone(),
                            literal: Literal::Int(*i, pos.clone()),
                        })
                    } else {
                        Err(format!(
                            "Invalid int literal: Expected type {:?}, found {:?} on {}",
                            local_ctx.result_type,
                            *t,
                            literal.position()
                        ))
                    }
                }
                None => Ok(TypedLiteral {
                    ty: self.ctx.get_type("i32".into())?,
                    literal: Literal::Int(*i, pos.clone()),
                }),
            },
            Literal::Float(f, pos) => match &local_ctx.result_type {
                Some(t) => {
                    if *t == self.ctx.get_type("f32".into())? {
                        Ok(TypedLiteral {
                            ty: t.clone(),
                            literal: Literal::Float(*f, pos.clone()),
                        })
                    } else if *t == self.ctx.get_type("f64".into())? {
                        Ok(TypedLiteral {
                            ty: t.clone(),
                            literal: Literal::Float(*f, pos.clone()),
                        })
                    } else {
                        Err(format!(
                            "Invalid float literal: Expected type {:?}, found {:?}",
                            local_ctx.result_type, t
                        ))
                    }
                }
                None => return Err("Cannot infer type of float literal".to_string()),
            },
            Literal::Char(c, pos) => {
                if local_ctx.result_type == Some(self.ctx.get_type("char".into())?) {
                    Ok(TypedLiteral {
                        ty: self.ctx.get_type("char".into())?,
                        literal: Literal::Char(*c, pos.clone()),
                    })
                } else {
                    Err(format!(
                        "Invalid char literal: Expected type {:?}, found {:?}",
                        local_ctx.result_type,
                        self.ctx.get_type("char".into())?
                    ))
                }
            }
            Literal::Bool(b, pos) => {
                if local_ctx.result_type == Some(self.ctx.get_type("bool".into())?) {
                    Ok(TypedLiteral {
                        ty: self.ctx.get_type("bool".into())?,
                        literal: Literal::Bool(*b, pos.clone()),
                    })
                } else {
                    Err(format!(
                        "Invalid bool literal: Expected type {:?}, found {:?}",
                        local_ctx.result_type,
                        self.ctx.get_type("bool".into())?
                    ))
                }
            }
        }
    }
}
