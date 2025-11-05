use crate::{
    ast::{self, Expression, ScopeResolution},
    debug, debugln,
    typechecker::{
        context::{new_type, LocalTypecheckContext, TypeSig},
        type_sig::{FunctionType, Name, PointerType, Type, TypeSignature},
        typed_ast::{
            TypedExportType, TypedExpression, TypedExpressionData, TypedFunctionCall, TypedImport,
        },
        TypeChecker,
    },
};

impl<'tc> TypeChecker<'tc> {
    pub fn typecheck_fn_call(
        &self,
        fn_call: &ast::FunctionCall,
        mut local_ctx: LocalTypecheckContext<'tc>,
    ) -> Result<TypedExpression<'tc>, String> {
        debug!("typechecker::typecheck_fn_call");
        let mut callee = None;
        let (fn_type, fn_name, member_self_type) = match *fn_call.callee.clone() {
            Expression::Identifier { name, .. } => (
                local_ctx.names.get(&name).ok_or(format!(
                    "Function {} is not defined in this scope ({})",
                    name, fn_call.span
                ))?,
                name,
                None,
            ),
            Expression::MemberAccess { member_access } => {
                debugln!();
                let object = self.typecheck_expression(&member_access.object, local_ctx.clone())?;

                let Some(object_type) = &object.ty else {
                    return Err(format!("Cannot access member of non-existant type"));
                };
                let object_sig = object_type.sig();

                let Some(struct_ty) = (match &object_sig {
                    TypeSignature::Struct(s) => self.ctx.struct_types.get(&s),
                    TypeSignature::Pointer(PointerType {
                        target,
                    }) => {
                        match target.as_ref() {
                            TypeSignature::Struct(s_id) => {
                                self.ctx.struct_types.get(&s_id)
                            },
                            _ => return Err(format!("Cannot access member of non-struct type {:?}", object_sig)),
                        }
                    }
                    _ => return Err(format!("Cannot access member of non-struct type {:?}", object_sig)),
                }) else {
                    return Err(format!("Struct type does not exist"));
                };

                let Expression::Identifier { name: member_name, span: _ } = *member_access.member else {
                    return Err(format!("Member access must be an identifier"));
                };

                let member_method = struct_ty.methods.get(&member_name).ok_or(format!(
                    "Struct {} does not have a method {}",
                    struct_ty.name, member_name
                ))?;

                let member_fn_ty = member_method.fn_ty.clone();
                let full_name = if let TypeSignature::Function(f) = member_fn_ty.sig() {
                    if f.has_self_param {
                        struct_ty.name.clone() + "." + &member_name
                    } else {
                        struct_ty.name.clone() + "::" + &member_name
                    }
                } else {
                    return Err(format!("Member function must be a function"));
                };

                local_ctx
                    .names
                    .insert(full_name.clone(), Name { ty: member_fn_ty });
                let name = local_ctx.names.get(&full_name).unwrap();
                (name, full_name, Some(object))
            }
            Expression::ScopeResolution { scope_resolution } => {
                let ScopeResolution {
                    object,
                    member,
                    span: _,
                } = scope_resolution;
                debugln!();
                let mut object = *object;
                let mut path = Vec::new();
                while let Expression::ScopeResolution { scope_resolution } = object {
                    let ScopeResolution {
                        object: new_object,
                        member,
                        span: _,
                    } = scope_resolution;
                    if let Expression::Identifier { name, .. } = *member {
                        path.push(name);
                    } else {
                        return Err(format!("Unsupported scope resolution member"));
                    }
                    object = *new_object;
                }
                if let Expression::Identifier { name, .. } = object {
                    path.push(name);
                } else {
                    return Err(format!("Unsupported scope resolution object"));
                }

                let Expression::Identifier { name: fn_name, .. } = *member else {
                    return Err(format!("Unsupported scope resolution member"));
                };

                let first = path.first().unwrap();

                // Resolve type of object
                if let Ok(object_type) = self.ctx.get_type(first.clone()) {
                    // It's a struct
                    let object_sig = object_type.sig();

                    let Some(struct_ty) = (match &object_sig {
                        TypeSignature::Struct(s) => self.ctx.struct_types.get(&s),
                        TypeSignature::Pointer(PointerType {
                            target,
                        }) => {
                            match target.as_ref() {
                                TypeSignature::Struct(s_id) => {
                                    self.ctx.struct_types.get(&s_id)
                                },
                                _ => return Err(format!("Cannot access member of non-struct type {:?}", object_sig)),
                            }
                        }
                        _ => return Err(format!("Cannot access member of non-struct type {:?}", object_sig)),
                    }) else {
                        return Err(format!("Struct type does not exist"));
                    };

                    let member_method = struct_ty.methods.get(&fn_name).ok_or(format!(
                        "Struct {} does not have a method {}",
                        struct_ty.name, fn_name
                    ))?;

                    let member_fn_ty = member_method.fn_ty.clone();
                    let full_name = if let TypeSignature::Function(f) = member_fn_ty.sig() {
                        if f.has_self_param {
                            struct_ty.name.clone() + "." + &fn_name
                        } else {
                            struct_ty.name.clone() + "::" + &fn_name
                        }
                    } else {
                        return Err(format!("Member function must be a function"));
                    };
                    local_ctx
                        .names
                        .insert(full_name.clone(), Name { ty: member_fn_ty });
                    let name = local_ctx.names.get(&full_name).unwrap();
                    (name, full_name, None)
                } else {
                    // It's a module
                    let mut item_path = path.clone();
                    item_path.reverse();

                    let mod_id = self
                        .module_ids
                        .get(&self.canonicalize_path(&item_path))
                        .ok_or(format!("Module {:?} does not exist", item_path))?;

                    let module = self.modules.get(*mod_id).unwrap().clone();

                    let mod_borrowed = module.borrow();

                    let func = mod_borrowed.exports.get(&fn_name).ok_or(format!(
                        "Module {} does not have a function {}",
                        first, fn_name
                    ))?;

                    /* let full_name = if let TypedExportType::Function(f) = &func.ty {
                        if f.has_self_param {
                            struct_ty.name.clone() + "." + &fn_name
                        } else {
                            struct_ty.name.clone() + "::" + &fn_name
                        }
                    } else {
                        return Err(format!("Member function must be a function"));
                    }; */

                    /*  let full_name =  {
                        if f.has_self_param {
                            struct_ty.name.clone() + "." + &fn_name
                        } else {
                            struct_ty.name.clone() + "::" + &fn_name
                        }
                    } else {
                        return Err(format!("Member function must be a function"));
                    }; */

                    let curr_mod_id = self.module_ids.get(&self.current_module_path).unwrap();
                    let curr_mod = self.modules.get(*curr_mod_id).unwrap().clone();
                    curr_mod.borrow_mut().dependencies.push(TypedImport {
                        name: fn_name.clone(),
                        source_module: mod_borrowed.id,
                        local: true,
                        ty: func.ty.clone(),
                    });

                    if let TypedExportType::Function(f) = &func.ty {
                        /* /* struct_ty.name.clone() + "." +  */fn_name.clone() */
                        let full_name = /* mod_borrowed.path.join("::") + "::" + & */fn_name;
                        local_ctx.names.insert(
                            full_name.clone(),
                            Name {
                                ty: new_type(Type {
                                    sig: TypeSignature::Function(f.clone()),
                                }),
                            },
                        );
                        let name = local_ctx.names.get(&full_name).unwrap();
                        callee = Some(TypedExpression {
                            ty: Some(new_type(Type {
                                sig: TypeSignature::Function(f.clone()),
                            })),
                            expr: TypedExpressionData::Identifier {
                                name: full_name.clone(),
                            },
                        });
                        (name, full_name, None)
                    } else {
                        return Err(format!(
                            "Module {} does not have a function {}",
                            first, fn_name
                        ));
                    }

                    /* if let Some(def) = def {
                        let member_fn_ty = def.fn_ty.clone();
                        let full_name = /* struct_ty.name.clone() + "." +  */fn_name.clone();
                        local_ctx
                            .names
                            .insert(full_name.clone(), Name { ty: member_fn_ty });
                        let name = local_ctx.names.get(&full_name).unwrap();
                        (name, full_name, None)
                    } else if let Some(decl) = decl {
                        let member_fn_ty = decl.fn_ty.clone();
                        let full_name = /* struct_ty.name.clone() + "." +  */fn_name.clone();
                        local_ctx
                            .names
                            .insert(full_name.clone(), Name { ty: member_fn_ty });
                        let name = local_ctx.names.get(&full_name).unwrap();
                        (name, full_name, None)
                    } else {
                        return Err(format!(
                            "Module {} does not have a function {}",
                            module.get_name(),
                            fn_name
                        ));
                    } */
                }
            }
            _ => return Err(format!("Unsupported callee type {:?}", fn_call.callee)),
        };

        let ret_type = match fn_type.ty.sig() {
            TypeSignature::Function(FunctionType { return_type, .. }) => return_type,
            _ => return Err(format!("{:?} is not a function", fn_type.ty.sig())),
        };
        let mut args = Vec::new();
        if let Some(t) = member_self_type {
            args.push(t);
        }
        for (arg_idx, arg) in fn_call.args.iter().enumerate() {
            let param_type = match fn_type.ty.sig() {
                TypeSignature::Function(FunctionType {
                    params,
                    variadic,
                    has_self_param,
                    ..
                }) => {
                    let t = match params
                        .iter()
                        .find(|(_, p)| p.idx as usize == (arg_idx + (has_self_param as usize)))
                    {
                        Some((_, t)) => Some(t.ty.clone()),
                        None => {
                            if arg_idx > (params.len() - 1) {
                                if variadic {
                                    self.typecheck_expression(
                                        &arg.clone(),
                                        local_ctx.expect_result(None),
                                    )?
                                    .ty
                                } else {
                                    return Err(format!(
                                        "Function {} does not have a parameter at index {} and it's not variadic",
                                        fn_name, arg_idx
                                    ));
                                }
                            } else {
                                return Err(format!(
                                    "Function {} does not have a parameter at index {}",
                                    fn_name, arg_idx
                                ));
                            }
                        }
                    };
                    t.clone()
                }
                _ => return Err(format!("{} is not a function", fn_name)),
            };
            args.push(self.typecheck_expression(arg, local_ctx.expect_result(param_type))?);
        }
        if callee.is_none() {
            callee = Some(self.typecheck_expression(&fn_call.callee, local_ctx.clone())?);
        }
        Ok(TypedExpression {
            ty: ret_type,
            expr: TypedExpressionData::FnCall {
                fn_call: TypedFunctionCall {
                    callee: Box::from(callee.unwrap()),
                    args,
                    fn_ty: fn_type.ty.clone(),
                },
            },
        })
    }
}
