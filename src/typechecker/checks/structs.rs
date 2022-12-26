use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    lowering::IntermediateModule,
    typechecker::{
        context::{LocalTypecheckContext, TypeSig},
        type_sig::{StructType, TypeSignature, TypedStructField},
        typed_ast::TypedStructDeclaration,
        TypeChecker,
    },
};

impl<'tc> TypeChecker<'tc> {
    pub fn typecheck_struct_methods(
        &mut self,
        module: Rc<RefCell<IntermediateModule>>,
    ) -> Result<(), String> {
        for structure in &module.borrow().structs {
            for method in &structure.methods {
                let Ok(type_ref) = self.ctx.get_type(structure.name.clone()) else {
                    return Err("".into())
                };
                let struct_id = if let TypeSignature::Struct(id) = type_ref.sig() {
                    id
                } else {
                    return Err(format!("{} is not a struct", structure.name));
                };

                // function def
                let func = self.typecheck_fn_def(
                    method,
                    LocalTypecheckContext::impl_method(type_ref.clone()),
                )?;

                let struct_ty = self.ctx.struct_types.get_mut(&struct_id).unwrap();
                struct_ty.methods.insert(func.name.clone(), func);
            }
        }
        Ok(())
    }

    pub fn typecheck_structs(
        &mut self,
        module: Rc<RefCell<IntermediateModule>>,
    ) -> Result<Vec<TypedStructDeclaration<'tc>>, String> {
        let mut structs = Vec::new();
        for structure in &module.borrow().structs {
            /* let id = if let TypeSignature::Struct(id) =
                self.ctx.get_type(structure.name.clone())?.sig()
            {
                id
            } else {
                return Err(format!("{} is not a struct", structure.name));
            };

            let mut fields = HashMap::new();
            for field in &structure.fields {
                let Ok(type_val) = self.ctx.get_type(field.type_name.clone()) else {
                    return Err(format!("{} is not a valid type 590", field.type_name));
                };

                fields.insert(
                    field.name.clone(),
                    TypedStructField {
                        name: field.name.clone(),
                        ty: type_val.clone(),
                        idx: field.idx,
                    },
                );
            }

            let mut struct_type = StructType {
                name: structure.name.clone(),
                fields: fields.clone(),
                methods: HashMap::new(),
                id,
            };

            let Ok(type_ref) = self.ctx.get_type(structure.name.clone()) else {
                return Err("".into())
            };
            // *type_ref.borrow_mut() = Type::new(TypeSignature::Struct(id));

            self.ctx.struct_types.insert(id, struct_type.clone());

            for method in &structure.methods {
                // function def
                let func = self.typecheck_fn_def(
                    method,
                    LocalTypecheckContext::impl_method(type_ref.clone()),
                )?;
                struct_type.methods.insert(method.name.clone(), func);
            }

            structs.push(TypedStructDeclaration {
                name: structure.name.clone(),
                fields: fields.values().map(|field| field.clone()).collect(),
                ty: type_ref,
                id,
                methods: struct_type.methods.clone(),
            }); */

            let id = if let TypeSignature::Struct(id) =
                self.ctx.get_type(structure.name.clone())?.sig()
            {
                id
            } else {
                return Err(format!("{} is not a struct", structure.name));
            };

            let mut fields = HashMap::new();
            for field in &structure.fields {
                let type_val = self.ctx.get_type(field.type_name.clone())?;
                fields.insert(
                    field.name.clone(),
                    TypedStructField {
                        name: field.name.clone(),
                        ty: type_val.clone(),
                        idx: field.idx,
                    },
                );
            }

            let struct_type = StructType {
                name: structure.name.clone(),
                fields: fields.clone(),
                methods: HashMap::new(),
                id,
            };

            let Ok(type_ref) = self.ctx.get_type(structure.name.clone()) else {
                return Err("".into())
            };
            //*type_ref.borrow_mut() = Type::new(TypeSignature::Struct(id));

            self.ctx.struct_types.insert(id, struct_type.clone());

            //self.ctx.struct_types.insert(id, struct_type.clone());

            structs.push(TypedStructDeclaration {
                name: structure.name.clone(),
                fields: fields.values().map(|field| field.clone()).collect(),
                ty: type_ref,
                id,
                methods: struct_type.methods.clone(),
            });
        }

        Ok(structs)
    }
}
