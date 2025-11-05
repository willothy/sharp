use std::collections::HashMap;

use crate::{
    ast::{self, MemberAccess},
    debug, debugln,
    typechecker::{
        context::{new_type, LocalTypecheckContext, TypeSig},
        type_sig::{Name, Type, TypeSignature},
        typed_ast::{TypedExpression, TypedExpressionData, TypedMemberAccess},
        TypeChecker,
    },
};

impl<'tc> TypeChecker<'tc> {
    pub fn typecheck_member_access(
        &self,
        member_access: &ast::MemberAccess,
        local_ctx: LocalTypecheckContext<'tc>,
    ) -> Result<TypedExpression<'tc>, String> {
        debug!("typechecker::typecheck_member_access");
        let MemberAccess {
            object,
            member,
            computed,
            ..
        } = member_access;

        if *computed {
            unimplemented!("Computed member access is not supported yet");
        }

        //let object_type = self.typecheck_expression(object, local_ctx.clone())?;
        //let member_type = self.typecheck_expression(member, local_ctx)?;
        let object = *(object.clone());
        let member = *(member.clone());
        let checked_object = self.typecheck_expression(&object, local_ctx.clone())?;
        //let checked_member = self.typecheck_expression(&member, local_ctx.clone())?;

        let Some(ty) = &checked_object.ty else {
            return Err(format!("Cannot dereference {:?}", checked_object));
        };
        let sig = ty.sig();

        if let TypeSignature::Struct(id) = sig {
            debugln!();
            let structure = self
                .ctx
                .struct_types
                .get(&id)
                .ok_or(format!("Struct {} is not defined", id))?;
            let mut struct_ctx = LocalTypecheckContext {
                names: HashMap::new(),
                result_type: local_ctx.result_type.clone(),
                return_type: local_ctx.return_type.clone(),
                in_loop: local_ctx.in_loop,
                impl_ctx: None,
            };

            // add struct fields to internal context
            for (field_name, field_type) in &structure.fields {
                struct_ctx.names.insert(
                    field_name.clone(),
                    Name {
                        ty: field_type.ty.clone(),
                    },
                );
            }

            // add struct methods to internal context
            for (method_name, method_type) in &structure.methods {
                struct_ctx.names.insert(
                    method_name.clone(),
                    Name {
                        ty: method_type.fn_ty.clone(),
                    },
                );
            }

            let checked_member = self.typecheck_expression(&member, struct_ctx)?;
            Ok(TypedExpression {
                ty: checked_member.ty.clone(),
                expr: TypedExpressionData::MemberAccess {
                    member_access: TypedMemberAccess {
                        object: Box::from(checked_object),
                        member: Box::from(checked_member),
                        computed: *computed,
                    },
                },
            })
        } else if let TypeSignature::Pointer(pointer) = &sig {
            if let TypeSignature::Struct(id) = pointer.target.as_ref() {
                let mut struct_ctx = LocalTypecheckContext {
                    names: HashMap::new(),
                    result_type: local_ctx.result_type.clone(),
                    return_type: local_ctx.return_type.clone(),
                    in_loop: local_ctx.in_loop,
                    impl_ctx: None,
                };

                debugln!();
                let s = self
                    .ctx
                    .struct_types
                    .get(id)
                    .ok_or(format!("Struct {} is not defined", id))?;

                // add struct fields to internal context
                for (field_name, field_type) in &s.fields {
                    struct_ctx.names.insert(
                        field_name.clone(),
                        Name {
                            ty: field_type.ty.clone(),
                        },
                    );
                }

                // add struct methods to internal context
                for (method_name, method_type) in &s.methods {
                    struct_ctx.names.insert(
                        method_name.clone(),
                        Name {
                            ty: method_type.fn_ty.clone(),
                        },
                    );
                }

                let checked_member = self.typecheck_expression(&member, struct_ctx)?;
                let Some(ty) = checked_member.ty.clone() else {
                    return Err(format!("Cannot access member of {:?}", sig));
                };
                Ok(TypedExpression {
                    ty: Some(new_type(Type {
                        sig: ty.sig().wrap_in_ptr(),
                    })),
                    expr: TypedExpressionData::MemberAccess {
                        member_access: TypedMemberAccess {
                            object: Box::from(checked_object),
                            member: Box::from(checked_member),
                            computed: *computed,
                        },
                    },
                })
            } else {
                return Err(format!("Cannot access member of {:?}", sig));
            }
        } else {
            return Err(format!("Cannot access member of {:?}", sig));
        }
    }
}
