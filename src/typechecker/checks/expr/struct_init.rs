use crate::{
    ast::{StructInitializer, StructInitializerField},
    debug,
    typechecker::{
        context::{LocalTypecheckContext, TypeSig},
        type_sig::TypeSignature,
        typed_ast::{
            TypedExpression, TypedExpressionData, TypedStructInitializer,
            TypedStructInitializerField,
        },
        TypeChecker,
    },
};

impl<'tc> TypeChecker<'tc> {
    pub fn typecheck_struct_init(
        &self,
        struct_init: &StructInitializer,
        mut local_ctx: LocalTypecheckContext<'tc>,
    ) -> Result<TypedExpression<'tc>, String> {
        debug!("typechecker::typecheck_struct_init");
        let StructInitializer {
            struct_name,
            fields,
            ..
        } = struct_init;
        let struct_type = self.ctx.get_type(struct_name.clone())?;
        let struct_sig = struct_type.sig();

        let decl_fields = match &struct_sig {
            TypeSignature::Struct(id) => match self.ctx.struct_types.get(id) {
                Some(s) => s.fields.clone(),
                None => return Err(format!("Struct {} is not defined", struct_name)),
            },
            _ => unreachable!(),
        };

        let mut typed_fields = Vec::new();
        for field in fields {
            let StructInitializerField {
                field_name, value, ..
            } = field;

            let expected_field = decl_fields.get(field_name).ok_or(format!(
                "Field {} does not exist in struct {}",
                field_name, struct_name
            ))?;
            // TODO: Possible issue
            local_ctx.result_type = Some(expected_field.ty.clone());

            let typed_value = self.typecheck_expression(value, local_ctx.clone())?;

            if typed_value.ty != Some(expected_field.ty.clone()) {
                return Err(format!(
                    "Expected field {} to be of type {:?}, but got {:?}",
                    field_name, expected_field.ty, typed_value.ty
                ));
            }

            let typed_field = TypedStructInitializerField {
                field_name: field_name.clone(),
                value: typed_value,
                idx: expected_field.idx,
            };
            typed_fields.push(typed_field);
        }

        Ok(TypedExpression {
            ty: Some(struct_type.clone()),
            expr: TypedExpressionData::StructInitializer {
                struct_init: TypedStructInitializer {
                    struct_name: struct_name.clone(),
                    fields: typed_fields,
                    struct_ty: struct_type,
                },
            },
        })
    }
}
