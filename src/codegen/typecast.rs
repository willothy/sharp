use std::error::Error;

use inkwell::{
    types::BasicTypeEnum,
    values::{BasicValueEnum, FloatValue, IntValue, PointerValue},
};

use super::generator::CodeGenerator;

impl<'gen> CodeGenerator<'gen> {
    pub fn build_int_cast(
        &self,
        from: IntValue<'gen>,
        to: BasicTypeEnum<'gen>,
    ) -> Result<BasicValueEnum<'gen>, Box<dyn Error>> {
        match to {
            BasicTypeEnum::ArrayType(_) => unreachable!("Cannot cast int to array"),
            BasicTypeEnum::FloatType(to_float) => {
                let cast =
                    self.ctx
                        .ir_builder
                        .build_signed_int_to_float(from, to_float, "int_to_float");
                Ok(BasicValueEnum::FloatValue(cast))
            }
            BasicTypeEnum::IntType(to_int) => {
                let cast = self.ctx.ir_builder.build_int_cast(from, to_int, "int_cast");
                Ok(BasicValueEnum::IntValue(cast))
            }
            BasicTypeEnum::PointerType(to_ptr) => {
                let cast = self
                    .ctx
                    .ir_builder
                    .build_int_to_ptr(from, to_ptr, "int_to_ptr");
                Ok(BasicValueEnum::PointerValue(cast))
            }
            BasicTypeEnum::StructType(_) => unreachable!("Cannot cast int to struct"),
            BasicTypeEnum::VectorType(_) => unreachable!("Cannot cast int to vector"),
        }
    }

    pub fn build_float_cast(
        &self,
        from: FloatValue<'gen>,
        to: BasicTypeEnum<'gen>,
    ) -> Result<BasicValueEnum<'gen>, Box<dyn Error>> {
        match to {
            BasicTypeEnum::ArrayType(_) => unreachable!("Cannot cast float to array"),
            BasicTypeEnum::FloatType(to_float) => {
                let cast = self
                    .ctx
                    .ir_builder
                    .build_float_cast(from, to_float, "float_cast");
                Ok(BasicValueEnum::FloatValue(cast))
            }
            BasicTypeEnum::IntType(to_int) => {
                let cast =
                    self.ctx
                        .ir_builder
                        .build_float_to_signed_int(from, to_int, "float_to_int");
                Ok(BasicValueEnum::IntValue(cast))
            }
            BasicTypeEnum::PointerType(_) => unreachable!("Cannot cast float to pointer"),
            BasicTypeEnum::StructType(_) => unreachable!("Cannot cast float to struct"),
            BasicTypeEnum::VectorType(_) => unreachable!("Cannot cast float to vector"),
        }
    }

    pub fn build_ptr_cast(
        &self,
        from: PointerValue<'gen>,
        to: BasicTypeEnum<'gen>,
    ) -> Result<BasicValueEnum<'gen>, Box<dyn Error>> {
        match to {
            BasicTypeEnum::ArrayType(_) => unreachable!("Cannot cast pointer to array"),
            BasicTypeEnum::FloatType(_) => unreachable!("Cannot cast pointer to float"),
            BasicTypeEnum::IntType(to_int) => {
                let cast = self
                    .ctx
                    .ir_builder
                    .build_ptr_to_int(from, to_int, "ptr_to_int");
                Ok(BasicValueEnum::IntValue(cast))
            }
            BasicTypeEnum::PointerType(to_ptr) => {
                let cast = self
                    .ctx
                    .ir_builder
                    .build_pointer_cast(from, to_ptr, "ptr_cast");
                Ok(BasicValueEnum::PointerValue(cast))
            }
            BasicTypeEnum::StructType(_) => unreachable!("Cannot cast pointer to struct"),
            BasicTypeEnum::VectorType(_) => unreachable!("Cannot cast pointer to vector"),
        }
    }
}
