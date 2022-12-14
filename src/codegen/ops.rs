use inkwell::{
    values::{BasicValue, BasicValueEnum},
    FloatPredicate, IntPredicate,
};

use super::generator::CodeGenerator;

impl<'gen> CodeGenerator<'gen> {
    pub fn build_add(
        &self,
        left: BasicValueEnum<'gen>,
        right: BasicValueEnum<'gen>,
    ) -> BasicValueEnum<'gen> {
        match left {
            BasicValueEnum::ArrayValue(_) => todo!(),
            BasicValueEnum::IntValue(i) => self
                .ctx
                .ir_builder
                .build_int_add(i, right.into_int_value(), "iaddtmp")
                .as_basic_value_enum(),
            BasicValueEnum::FloatValue(f) => self
                .ctx
                .ir_builder
                .build_float_add(f, right.into_float_value(), "faddtmp")
                .as_basic_value_enum(),
            BasicValueEnum::PointerValue(_) => todo!(),
            BasicValueEnum::StructValue(_) => todo!(),
            BasicValueEnum::VectorValue(_) => todo!(),
        }
    }

    pub fn build_sub(
        &self,
        left: BasicValueEnum<'gen>,
        right: BasicValueEnum<'gen>,
    ) -> BasicValueEnum<'gen> {
        match left {
            BasicValueEnum::ArrayValue(_) => todo!(),
            BasicValueEnum::IntValue(i) => self
                .ctx
                .ir_builder
                .build_int_sub(i, right.into_int_value(), "isubtmp")
                .as_basic_value_enum(),
            BasicValueEnum::FloatValue(f) => self
                .ctx
                .ir_builder
                .build_float_sub(f, right.into_float_value(), "fsubtmp")
                .as_basic_value_enum(),
            BasicValueEnum::PointerValue(_) => todo!(),
            BasicValueEnum::StructValue(_) => todo!(),
            BasicValueEnum::VectorValue(_) => todo!(),
        }
    }

    pub fn build_mul(
        &self,
        left: BasicValueEnum<'gen>,
        right: BasicValueEnum<'gen>,
    ) -> BasicValueEnum<'gen> {
        match left {
            BasicValueEnum::ArrayValue(_) => todo!(),
            BasicValueEnum::IntValue(i) => self
                .ctx
                .ir_builder
                .build_int_mul(i, right.into_int_value(), "isubtmp")
                .as_basic_value_enum(),
            BasicValueEnum::FloatValue(f) => self
                .ctx
                .ir_builder
                .build_float_mul(f, right.into_float_value(), "fsubtmp")
                .as_basic_value_enum(),
            BasicValueEnum::PointerValue(_) => todo!(),
            BasicValueEnum::StructValue(_) => todo!(),
            BasicValueEnum::VectorValue(_) => todo!(),
        }
    }

    pub fn build_div(
        &self,
        left: BasicValueEnum<'gen>,
        right: BasicValueEnum<'gen>,
    ) -> BasicValueEnum<'gen> {
        match left {
            BasicValueEnum::ArrayValue(_) => todo!(),
            BasicValueEnum::IntValue(i) => self
                .ctx
                .ir_builder
                .build_int_signed_div(i, right.into_int_value(), "isubtmp")
                .as_basic_value_enum(),
            BasicValueEnum::FloatValue(f) => self
                .ctx
                .ir_builder
                .build_float_div(f, right.into_float_value(), "fsubtmp")
                .as_basic_value_enum(),
            BasicValueEnum::PointerValue(_) => todo!(),
            BasicValueEnum::StructValue(_) => todo!(),
            BasicValueEnum::VectorValue(_) => todo!(),
        }
    }

    pub fn build_rem(
        &self,
        left: BasicValueEnum<'gen>,
        right: BasicValueEnum<'gen>,
    ) -> BasicValueEnum<'gen> {
        match left {
            BasicValueEnum::ArrayValue(_) => todo!(),
            BasicValueEnum::IntValue(i) => self
                .ctx
                .ir_builder
                .build_int_signed_rem(i, right.into_int_value(), "isubtmp")
                .as_basic_value_enum(),
            BasicValueEnum::FloatValue(f) => self
                .ctx
                .ir_builder
                .build_float_rem(f, right.into_float_value(), "fsubtmp")
                .as_basic_value_enum(),
            BasicValueEnum::PointerValue(_) => todo!(),
            BasicValueEnum::StructValue(_) => todo!(),
            BasicValueEnum::VectorValue(_) => todo!(),
        }
    }

    pub fn build_eq(
        &self,
        left: BasicValueEnum<'gen>,
        right: BasicValueEnum<'gen>,
    ) -> BasicValueEnum<'gen> {
        match left {
            BasicValueEnum::ArrayValue(_) => todo!(),
            BasicValueEnum::IntValue(i) => self
                .ctx
                .ir_builder
                .build_int_compare(IntPredicate::EQ, i, right.into_int_value(), "isubtmp")
                .as_basic_value_enum(),
            BasicValueEnum::FloatValue(f) => self
                .ctx
                .ir_builder
                .build_float_compare(FloatPredicate::OEQ, f, right.into_float_value(), "fsubtmp")
                .as_basic_value_enum(),
            BasicValueEnum::PointerValue(_) => todo!(),
            BasicValueEnum::StructValue(_) => todo!(),
            BasicValueEnum::VectorValue(_) => todo!(),
        }
    }

    pub fn build_ne(
        &self,
        left: BasicValueEnum<'gen>,
        right: BasicValueEnum<'gen>,
    ) -> BasicValueEnum<'gen> {
        match left {
            BasicValueEnum::ArrayValue(_) => todo!(),
            BasicValueEnum::IntValue(i) => self
                .ctx
                .ir_builder
                .build_int_compare(IntPredicate::NE, i, right.into_int_value(), "isubtmp")
                .as_basic_value_enum(),
            BasicValueEnum::FloatValue(f) => self
                .ctx
                .ir_builder
                .build_float_compare(FloatPredicate::ONE, f, right.into_float_value(), "fsubtmp")
                .as_basic_value_enum(),
            BasicValueEnum::PointerValue(_) => todo!(),
            BasicValueEnum::StructValue(_) => todo!(),
            BasicValueEnum::VectorValue(_) => todo!(),
        }
    }

    pub fn build_lt(
        &self,
        left: BasicValueEnum<'gen>,
        right: BasicValueEnum<'gen>,
    ) -> BasicValueEnum<'gen> {
        match left {
            BasicValueEnum::ArrayValue(_) => todo!(),
            BasicValueEnum::IntValue(i) => self
                .ctx
                .ir_builder
                .build_int_compare(IntPredicate::SLT, i, right.into_int_value(), "isubtmp")
                .as_basic_value_enum(),
            BasicValueEnum::FloatValue(f) => self
                .ctx
                .ir_builder
                .build_float_compare(FloatPredicate::OLT, f, right.into_float_value(), "fsubtmp")
                .as_basic_value_enum(),
            BasicValueEnum::PointerValue(_) => todo!(),
            BasicValueEnum::StructValue(_) => todo!(),
            BasicValueEnum::VectorValue(_) => todo!(),
        }
    }

    pub fn build_le(
        &self,
        left: BasicValueEnum<'gen>,
        right: BasicValueEnum<'gen>,
    ) -> BasicValueEnum<'gen> {
        match left {
            BasicValueEnum::ArrayValue(_) => todo!(),
            BasicValueEnum::IntValue(i) => self
                .ctx
                .ir_builder
                .build_int_compare(IntPredicate::SLE, i, right.into_int_value(), "isubtmp")
                .as_basic_value_enum(),
            BasicValueEnum::FloatValue(f) => self
                .ctx
                .ir_builder
                .build_float_compare(FloatPredicate::OLE, f, right.into_float_value(), "fsubtmp")
                .as_basic_value_enum(),
            BasicValueEnum::PointerValue(_) => todo!(),
            BasicValueEnum::StructValue(_) => todo!(),
            BasicValueEnum::VectorValue(_) => todo!(),
        }
    }

    pub fn build_gt(
        &self,
        left: BasicValueEnum<'gen>,
        right: BasicValueEnum<'gen>,
    ) -> BasicValueEnum<'gen> {
        match left {
            BasicValueEnum::ArrayValue(_) => todo!(),
            BasicValueEnum::IntValue(i) => self
                .ctx
                .ir_builder
                .build_int_compare(IntPredicate::SGT, i, right.into_int_value(), "isubtmp")
                .as_basic_value_enum(),
            BasicValueEnum::FloatValue(f) => self
                .ctx
                .ir_builder
                .build_float_compare(FloatPredicate::OGT, f, right.into_float_value(), "fsubtmp")
                .as_basic_value_enum(),
            BasicValueEnum::PointerValue(_) => todo!(),
            BasicValueEnum::StructValue(_) => todo!(),
            BasicValueEnum::VectorValue(_) => todo!(),
        }
    }

    pub fn build_ge(
        &self,
        left: BasicValueEnum<'gen>,
        right: BasicValueEnum<'gen>,
    ) -> BasicValueEnum<'gen> {
        match left {
            BasicValueEnum::ArrayValue(_) => todo!(),
            BasicValueEnum::IntValue(i) => self
                .ctx
                .ir_builder
                .build_int_compare(IntPredicate::SGE, i, right.into_int_value(), "isubtmp")
                .as_basic_value_enum(),
            BasicValueEnum::FloatValue(f) => self
                .ctx
                .ir_builder
                .build_float_compare(FloatPredicate::OGE, f, right.into_float_value(), "fsubtmp")
                .as_basic_value_enum(),
            BasicValueEnum::PointerValue(_) => todo!(),
            BasicValueEnum::StructValue(_) => todo!(),
            BasicValueEnum::VectorValue(_) => todo!(),
        }
    }

    pub fn build_and(
        &self,
        left: BasicValueEnum<'gen>,
        right: BasicValueEnum<'gen>,
    ) -> BasicValueEnum<'gen> {
        match left {
            BasicValueEnum::ArrayValue(_) => todo!(),
            BasicValueEnum::IntValue(i) => self
                .ctx
                .ir_builder
                .build_and(i, right.into_int_value(), "andtmp")
                .as_basic_value_enum(),
            BasicValueEnum::FloatValue(_) => todo!(),
            BasicValueEnum::PointerValue(_) => todo!(),
            BasicValueEnum::StructValue(_) => todo!(),
            BasicValueEnum::VectorValue(_) => todo!(),
        }
    }

    pub fn build_or(
        &self,
        left: BasicValueEnum<'gen>,
        right: BasicValueEnum<'gen>,
    ) -> BasicValueEnum<'gen> {
        match left {
            BasicValueEnum::ArrayValue(_) => todo!(),
            BasicValueEnum::IntValue(i) => self
                .ctx
                .ir_builder
                .build_or(i, right.into_int_value(), "ortmp")
                .as_basic_value_enum(),
            BasicValueEnum::FloatValue(_) => todo!(),
            BasicValueEnum::PointerValue(_) => todo!(),
            BasicValueEnum::StructValue(_) => todo!(),
            BasicValueEnum::VectorValue(_) => todo!(),
        }
    }
}
