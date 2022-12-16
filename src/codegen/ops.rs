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
            BasicValueEnum::ArrayValue(_) => todo!("build_add: ArrayValue"),
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
            BasicValueEnum::PointerValue(_) => todo!("build_add: PointerValue"),
            BasicValueEnum::StructValue(_) => todo!("build_add: StructValue"),
            BasicValueEnum::VectorValue(_) => todo!("build_add: VectorValue"),
        }
    }

    pub fn build_sub(
        &self,
        left: BasicValueEnum<'gen>,
        right: BasicValueEnum<'gen>,
    ) -> BasicValueEnum<'gen> {
        match left {
            BasicValueEnum::ArrayValue(_) => todo!("build_sub: ArrayValue"),
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
            BasicValueEnum::PointerValue(_) => todo!("build_sub: PointerValue"),
            BasicValueEnum::StructValue(_) => todo!("build_sub: StructValue"),
            BasicValueEnum::VectorValue(_) => todo!("build_sub: VectorValue"),
        }
    }

    pub fn build_mul(
        &self,
        left: BasicValueEnum<'gen>,
        right: BasicValueEnum<'gen>,
    ) -> BasicValueEnum<'gen> {
        match left {
            BasicValueEnum::ArrayValue(_) => todo!("build_mul: ArrayValue"),
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
            BasicValueEnum::PointerValue(_) => todo!("build_mul: PointerValue"),
            BasicValueEnum::StructValue(_) => todo!("build_mul: StructValue"),
            BasicValueEnum::VectorValue(_) => todo!("build_mul: VectorValue"),
        }
    }

    pub fn build_div(
        &self,
        left: BasicValueEnum<'gen>,
        right: BasicValueEnum<'gen>,
    ) -> BasicValueEnum<'gen> {
        match left {
            BasicValueEnum::ArrayValue(_) => todo!("build_div: ArrayValue"),
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
            BasicValueEnum::PointerValue(_) => todo!("build_div: PointerValue"),
            BasicValueEnum::StructValue(_) => todo!("build_div: StructValue"),
            BasicValueEnum::VectorValue(_) => todo!("build_div: VectorValue"),
        }
    }

    pub fn build_rem(
        &self,
        left: BasicValueEnum<'gen>,
        right: BasicValueEnum<'gen>,
    ) -> BasicValueEnum<'gen> {
        match left {
            BasicValueEnum::ArrayValue(_) => todo!("build_rem: ArrayValue"),
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
            BasicValueEnum::PointerValue(_) => todo!("build_rem: PointerValue"),
            BasicValueEnum::StructValue(_) => todo!("build_rem: StructValue"),
            BasicValueEnum::VectorValue(_) => todo!("build_rem: VectorValue"),
        }
    }

    pub fn build_eq(
        &self,
        left: BasicValueEnum<'gen>,
        right: BasicValueEnum<'gen>,
    ) -> BasicValueEnum<'gen> {
        match left {
            BasicValueEnum::ArrayValue(_) => todo!("build_eq: ArrayValue"),
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
            BasicValueEnum::PointerValue(_) => todo!("build_eq: PointerValue"),
            BasicValueEnum::StructValue(_) => todo!("build_eq: StructValue"),
            BasicValueEnum::VectorValue(_) => todo!("build_eq: VectorValue"),
        }
    }

    pub fn build_ne(
        &self,
        left: BasicValueEnum<'gen>,
        right: BasicValueEnum<'gen>,
    ) -> BasicValueEnum<'gen> {
        match left {
            BasicValueEnum::ArrayValue(_) => todo!("build_ne: ArrayValue"),
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
            BasicValueEnum::PointerValue(_) => todo!("build_ne: PointerValue"),
            BasicValueEnum::StructValue(_) => todo!("build_ne: StructValue"),
            BasicValueEnum::VectorValue(_) => todo!("build_ne: VectorValue"),
        }
    }

    pub fn build_lt(
        &self,
        left: BasicValueEnum<'gen>,
        right: BasicValueEnum<'gen>,
    ) -> BasicValueEnum<'gen> {
        match left {
            BasicValueEnum::ArrayValue(_) => todo!("build_lt: ArrayValue"),
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
            BasicValueEnum::PointerValue(_) => todo!("build_lt: PointerValue"),
            BasicValueEnum::StructValue(_) => todo!("build_lt: StructValue"),
            BasicValueEnum::VectorValue(_) => todo!("build_lt: VectorValue"),
        }
    }

    pub fn build_le(
        &self,
        left: BasicValueEnum<'gen>,
        right: BasicValueEnum<'gen>,
    ) -> BasicValueEnum<'gen> {
        match left {
            BasicValueEnum::ArrayValue(_) => todo!("build_le: ArrayValue"),
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
            BasicValueEnum::PointerValue(_) => todo!("build_le: PointerValue"),
            BasicValueEnum::StructValue(_) => todo!("build_le: StructValue"),
            BasicValueEnum::VectorValue(_) => todo!("build_le: VectorValue"),
        }
    }

    pub fn build_gt(
        &self,
        left: BasicValueEnum<'gen>,
        right: BasicValueEnum<'gen>,
    ) -> BasicValueEnum<'gen> {
        match left {
            BasicValueEnum::ArrayValue(_) => todo!("build_gt: ArrayValue"),
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
            BasicValueEnum::PointerValue(_) => todo!("build_gt: PointerValue"),
            BasicValueEnum::StructValue(_) => todo!("build_gt: StructValue"),
            BasicValueEnum::VectorValue(_) => todo!("build_gt: VectorValue"),
        }
    }

    pub fn build_ge(
        &self,
        left: BasicValueEnum<'gen>,
        right: BasicValueEnum<'gen>,
    ) -> BasicValueEnum<'gen> {
        match left {
            BasicValueEnum::ArrayValue(_) => todo!("build_ge: ArrayValue"),
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
            BasicValueEnum::PointerValue(_) => todo!("build_ge: PointerValue"),
            BasicValueEnum::StructValue(_) => todo!("build_ge: StructValue"),
            BasicValueEnum::VectorValue(_) => todo!("build_ge: VectorValue"),
        }
    }

    pub fn build_and(
        &self,
        left: BasicValueEnum<'gen>,
        right: BasicValueEnum<'gen>,
    ) -> BasicValueEnum<'gen> {
        match left {
            BasicValueEnum::ArrayValue(_) => todo!("build_and: ArrayValue"),
            BasicValueEnum::IntValue(i) => self
                .ctx
                .ir_builder
                .build_and(i, right.into_int_value(), "andtmp")
                .as_basic_value_enum(),
            BasicValueEnum::FloatValue(_) => todo!("build_and: FloatValue"),
            BasicValueEnum::PointerValue(_) => todo!("build_and: PointerValue"),
            BasicValueEnum::StructValue(_) => todo!("build_and: StructValue"),
            BasicValueEnum::VectorValue(_) => todo!("build_and: VectorValue"),
        }
    }

    pub fn build_or(
        &self,
        left: BasicValueEnum<'gen>,
        right: BasicValueEnum<'gen>,
    ) -> BasicValueEnum<'gen> {
        match left {
            BasicValueEnum::ArrayValue(_) => todo!("build_or: ArrayValue"),
            BasicValueEnum::IntValue(i) => self
                .ctx
                .ir_builder
                .build_or(i, right.into_int_value(), "ortmp")
                .as_basic_value_enum(),
            BasicValueEnum::FloatValue(_) => todo!("build_or: FloatValue"),
            BasicValueEnum::PointerValue(_) => todo!("build_or: PointerValue"),
            BasicValueEnum::StructValue(_) => todo!("build_or: StructValue"),
            BasicValueEnum::VectorValue(_) => todo!("build_or: VectorValue"),
        }
    }
}
