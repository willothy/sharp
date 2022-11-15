// Author: Will Hopkins

use std::{
    borrow::Borrow,
    cell::RefCell,
    collections::{HashMap, HashSet},
    hash::Hash,
    rc::Rc,
};

use crate::{
    ast::{self, Block, Declaration, Expression, FunctionDeclaration, StructDeclaration},
    tokenizer::{AssignmentOperator, Operator},
};

#[derive(Debug, Clone)]
pub struct Type<'t> {
    pub type_: TypeSignature<'t>,
    pub interfaces: TypeInterfaces<'t>,
}

impl<'t> PartialEq for Type<'t> {
    fn eq(&self, other: &Self) -> bool {
        self.type_ == other.type_
    }
}

impl<'t> Eq for Type<'t> {}

impl<'t> Type<'t> {
    pub fn new(type_: TypeSignature<'t>) -> Self {
        Self {
            type_,
            interfaces: TypeInterfaces::default(),
        }
    }

    pub fn new_with_interfaces(type_: TypeSignature<'t>, interfaces: TypeInterfaces<'t>) -> Self {
        Self { type_, interfaces }
    }

    pub fn empty() -> Self {
        Self {
            type_: TypeSignature::Empty,
            interfaces: TypeInterfaces::default(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeSignature<'t> {
    Primitive(PrimitiveType),
    Struct(Option<StructType<'t>>),
    Function(FunctionType<'t>),
    Empty,
}

impl<'t> Hash for Type<'t> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.type_.hash(state);
    }
}

impl<'t> Hash for TypeSignature<'t> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            TypeSignature::Primitive(p) => p.hash(state),
            TypeSignature::Struct(s) => s.hash(state),
            TypeSignature::Function(f) => f.hash(state),
            TypeSignature::Empty => 0.hash(state),
        }
    }
}

impl Hash for PrimitiveType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
    }
}

impl<'t> Hash for StructType<'t> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

impl<'t> Hash for FunctionType<'t> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        if let Some(ret_type) = &self.return_type {
            let return_type = ret_type.borrow_mut().clone();
            return_type.hash(state);
        }
        self.params.iter().for_each(|(_, v)| v.hash(state));
    }
}

impl<'fn_param> Hash for FunctionParameter<'fn_param> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.param_type.borrow_mut().hash(state);
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PrimitiveType {
    I32,
    I64,
    F32,
    F64,
    Bool,
    Char,
    String,
}

impl TryFrom<String> for PrimitiveType {
    type Error = String;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        match value.as_str() {
            "i32" => Ok(PrimitiveType::I32),
            "i64" => Ok(PrimitiveType::I64),
            "f32" => Ok(PrimitiveType::F32),
            "f64" => Ok(PrimitiveType::F64),
            "bool" => Ok(PrimitiveType::Bool),
            "char" => Ok(PrimitiveType::Char),
            "string" => Ok(PrimitiveType::String),
            _ => Err(format!("{} is not a valid primitive type", value)),
        }
    }
}

#[derive(Debug, Clone)]
pub struct StructType<'struct_type> {
    pub name: String,
    pub fields: HashMap<String, StructField<'struct_type>>,
}

impl<'struct_type> PartialEq for StructType<'struct_type> {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl<'struct_type> Eq for StructType<'struct_type> {}

#[derive(Debug, Clone)]
pub struct StructField<'field> {
    pub name: String,
    pub field_type: TypeRef<'field>,
}

#[derive(Debug, Clone)]
pub struct FunctionType<'fn_type> {
    pub return_type: Option<TypeRef<'fn_type>>,
    pub params: HashMap<String, FunctionParameter<'fn_type>>,
}

impl<'fn_type> PartialEq for FunctionType<'fn_type> {
    fn eq(&self, other: &Self) -> bool {
        if let Some(ret_type) = &self.return_type {
            let return_type = ret_type.borrow_mut().clone();
            if let Some(other_ret_type) = &other.return_type {
                let other_return_type = other_ret_type.borrow_mut().clone();
                if return_type != other_return_type {
                    return false;
                }
            } else {
                return false;
            }
        } else if other.return_type.is_some() {
            return false;
        }

        self.params.iter().all(|(k, v)| {
            if let Some(other_v) = other.params.get(k) {
                v == other_v
            } else {
                false
            }
        })
    }
}

impl<'fn_type> Eq for FunctionType<'fn_type> {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionParameter<'fn_param> {
    pub name: String,
    pub param_type: TypeRef<'fn_param>,
}

#[derive(Debug, Clone)]
pub struct Name<'name> {
    pub var_type: TypeRef<'name>,
}

#[derive(Debug, Clone)]
struct BlockResult<'block> {
    pub yield_type: Option<TypeRef<'block>>,
    pub return_type: Option<TypeRef<'block>>,
}

#[derive(Debug, Clone)]
pub struct TypeInterfaces<'type_interfaces> {
    pub interfaces: HashMap<TypeRef<'type_interfaces>, TypeInterface>,
}

impl<'type_interfaces> TypeInterfaces<'type_interfaces> {
    pub fn has_interface(&mut self, type_ref: &TypeRef<'type_interfaces>) -> bool {
        false
    }
}

pub type IBinaryExpr = fn(Expression, Expression);
pub type IUnaryExpr = fn(Expression);

#[derive(Debug, Clone)]
pub struct TypeInterface {
    pub add: Option<IBinaryExpr>,
    pub sub: Option<IBinaryExpr>,
    pub mul: Option<IBinaryExpr>,
    pub div: Option<IBinaryExpr>,
    pub modulus: Option<IBinaryExpr>,
    pub pow: Option<IBinaryExpr>,
    pub eq: Option<IBinaryExpr>,
    pub neq: Option<IBinaryExpr>,
    pub lt: Option<IBinaryExpr>,
    pub gt: Option<IBinaryExpr>,
    pub lte: Option<IBinaryExpr>,
    pub gte: Option<IBinaryExpr>,
    pub and: Option<IBinaryExpr>,
    pub or: Option<IBinaryExpr>,
    pub bitwise_and: Option<IBinaryExpr>,
    pub bitwise_or: Option<IBinaryExpr>,
    pub bitwise_xor: Option<IBinaryExpr>,
    pub bitwise_not: Option<IUnaryExpr>,
    pub bitwise_left_shift: Option<IBinaryExpr>,
    pub bitwise_right_shift: Option<IBinaryExpr>,
    pub not: Option<IUnaryExpr>,
    pub neg: Option<IUnaryExpr>,
}

impl<'interface> Default for TypeInterfaces<'interface> {
    fn default() -> Self {
        Self {
            interfaces: HashMap::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeCheckContext<'ctx> {
    pub names: HashMap<String, Name<'ctx>>,
    pub types: HashMap<String, TypeRef<'ctx>>,
    pub submodules: HashMap<String, TypeCheckContext<'ctx>>,
    pub return_type: Option<TypeRef<'ctx>>,
    pub yield_type: Option<TypeRef<'ctx>>,
    pub primitives: HashMap<String, TypeRef<'ctx>>,
}

pub type TypeRef<'type_ref> = Rc<RefCell<Type<'type_ref>>>;

impl<'ctx> TypeCheckContext<'ctx> {
    pub fn new() -> Self {
        let mut ctx = Self {
            names: HashMap::new(),
            types: HashMap::new(),
            submodules: HashMap::new(),
            return_type: None,
            yield_type: None,
            primitives: HashMap::new(),
        };
        ctx
    }

    pub fn enter_fn(self, fn_type: TypeRef<'ctx>) -> Self {
        let mut ctx = self.clone();
        let fn_type = &*fn_type.borrow_mut();
        match &fn_type.type_ {
            TypeSignature::Function(func) => {
                ctx.return_type = match &func.return_type {
                    Some(return_type) => Some(return_type.clone()),
                    None => None,
                };
                for param in func.params.values() {
                    ctx.names.insert(
                        param.name.clone(),
                        Name {
                            var_type: param.param_type.clone(),
                        },
                    );
                }
            }
            _ => panic!("Expected function type"),
        };

        ctx
    }

    pub fn exit_fn(self) -> Self {
        let mut ctx = self;
        ctx.return_type = None;
        ctx
    }

    pub fn allow_yield(&mut self, yield_type: TypeRef<'ctx>) -> &mut Self {
        self.yield_type = Some(yield_type);
        self
    }

    pub fn disallow_yield(self) -> Self {
        let mut ctx = self;
        ctx.yield_type = None;
        ctx
    }

    pub fn get_type(&self, type_name: String) -> Result<TypeRef<'ctx>, String> {
        if let Some(t) = self.primitives.get(&type_name) {
            return Ok(t.clone());
        }
        match self.types.get(&type_name) {
            Some(type_) => Ok(type_.clone()),
            None => Err(format!("{} is not a valid type", type_name)),
        }
    }
}

fn box_some<T>(t: T) -> Box<Option<T>> {
    Box::from(Some(t))
}

fn box_none<T>() -> Box<Option<T>> {
    Box::new(None)
}

pub fn new_type(t: Type) -> TypeRef {
    Rc::from(RefCell::from(t))
}

fn load_module_ctx<'ctx>(module: &ast::Module) -> Result<TypeCheckContext<'ctx>, String> {
    let mut ctx = TypeCheckContext::new();

    // Add primitive types
    ctx.primitives.insert(
        "i32".into(),
        new_type(Type::new(TypeSignature::Primitive(PrimitiveType::I32))),
    );
    ctx.primitives.insert(
        "i64".into(),
        new_type(Type::new(TypeSignature::Primitive(PrimitiveType::I64))),
    );
    ctx.primitives.insert(
        "f32".into(),
        new_type(Type::new(TypeSignature::Primitive(PrimitiveType::F32))),
    );
    ctx.primitives.insert(
        "f64".into(),
        new_type(Type::new(TypeSignature::Primitive(PrimitiveType::F64))),
    );
    ctx.primitives.insert(
        "bool".into(),
        new_type(Type::new(TypeSignature::Primitive(PrimitiveType::Bool))),
    );
    ctx.primitives.insert(
        "char".into(),
        new_type(Type::new(TypeSignature::Primitive(PrimitiveType::Char))),
    );

    Ok(ctx)
}

pub fn preload_types<'ctx>(
    declarations: &Vec<Declaration>,
    ctx: &mut TypeCheckContext<'ctx>,
) -> Result<(), String> {
    use Declaration::*;
    // Load all types without fields for resolving circular dependencies
    for decl in declarations {
        match decl {
            Struct(structure) => {
                ctx.types.insert(
                    structure.name.clone(),
                    new_type(Type {
                        type_: TypeSignature::Struct(None),
                        interfaces: TypeInterfaces::default(),
                    }),
                );
            }
            _ => (),
        }
    }
    Ok(())
}

pub fn resolve_fn_type<'ctx>(
    function: &FunctionDeclaration,
    ctx: &TypeCheckContext<'ctx>,
) -> Result<TypeRef<'ctx>, String> {
    let mut params = HashMap::new();
    for param in &function.params {
        params.insert(
            param.name.clone(),
            FunctionParameter {
                name: param.name.clone(),
                param_type: ctx.get_type(param.type_name.clone())?,
            },
        );
    }
    let return_type = if let Some(return_type) = &function.return_type {
        Some(ctx.get_type(return_type.clone())?)
    } else {
        None
    };
    Ok(new_type(Type {
        type_: TypeSignature::Function(FunctionType {
            return_type,
            params,
        }),
        interfaces: TypeInterfaces::default(),
    }))
}

pub fn resolve_types<'ctx>(
    declarations: &Vec<Declaration>,
    ctx: &mut TypeCheckContext<'ctx>,
) -> Result<(), String> {
    use Declaration::*;

    for decl in declarations {
        match decl {
            Struct(structure) => {
                resolve_struct(structure, ctx)?;
            }
            Function(function) => {
                ctx.names.insert(
                    function.name.clone(),
                    Name {
                        var_type: resolve_fn_type(function, ctx)?,
                    },
                );
            }
            _ => (),
        }
    }

    Ok(())
}

pub fn resolve_struct<'ctx>(
    structure: &StructDeclaration,
    ctx: &mut TypeCheckContext<'ctx>,
) -> Result<(), String> {
    let mut fields = HashMap::new();
    for field in &structure.fields {
        let Ok(type_val) = ctx.get_type(field.type_name.clone()) else {
            return Err(format!("{} is not a valid type", field.type_name));
        };

        fields.insert(
            field.name.clone(),
            StructField {
                name: field.name.clone(),
                field_type: type_val.clone(),
            },
        );
    }
    let Ok(type_ref) = ctx.get_type(structure.name.clone()) else {
        return Err("".into())
    };
    *type_ref.borrow_mut() = Type::new(TypeSignature::Struct(Some(StructType {
        name: structure.name.clone(),
        fields,
    })));
    Ok(())
}

pub fn typecheck_module<'ctx>(module: &ast::Module) -> Result<TypeCheckContext<'ctx>, String> {
    let mut ctx = load_module_ctx(&module)?;

    let declarations = module.body.clone();

    preload_types(&declarations, &mut ctx)?;
    resolve_types(&declarations, &mut ctx)?;
    typecheck_functions(&declarations, ctx.clone())?;

    Ok(ctx)
}

fn typecheck_functions<'ctx>(
    declarations: &[Declaration],
    ctx: TypeCheckContext<'ctx>,
) -> Result<(), String> {
    use Declaration::*;

    for decl in declarations {
        if let Function(function) = decl {
            typecheck_function(function, &ctx)?;
        }
    }

    Ok(())
}

fn typecheck_function<'ctx>(
    function: &FunctionDeclaration,
    ctx: &TypeCheckContext<'ctx>,
) -> Result<(), String> {
    let fn_type = if let Some(t) = ctx.names.get(&function.name) {
        t.var_type.clone()
    } else {
        return Err(format!("{} is not a valid function", function.name));
    };
    let mut function_ctx = ctx.clone().enter_fn(fn_type);
    for param in &function.params {
        function_ctx.names.insert(
            param.name.clone(),
            Name {
                var_type: ctx.get_type(param.type_name.clone())?,
            },
        );
    }
    typecheck_block(&function.body, function_ctx)?;
    Ok(())
}

fn typecheck_block<'ctx>(
    body: &Block,
    function_ctx: TypeCheckContext<'ctx>,
) -> Result<Option<TypeRef<'ctx>>, String> {
    let mut ctx = function_ctx;
    let mut yield_type = None;
    for stmt in &body.statements {
        if let Some(t) = typecheck_statement(stmt, &mut ctx)? {
            if let Some(yield_type) = yield_type.clone() {
                if yield_type != t {
                    return Err("yield type mismatch".into());
                }
            } else {
                yield_type = Some(t);
            }
        }
    }
    Ok(yield_type)
}

impl<'t> std::fmt::Display for Type<'t> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.type_)
    }
}

impl<'t> std::fmt::Display for TypeSignature<'t> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeSignature::Primitive(prim) => match prim {
                PrimitiveType::I32 => write!(f, "i32"),
                PrimitiveType::I64 => write!(f, "i64"),
                PrimitiveType::F32 => write!(f, "f32"),
                PrimitiveType::F64 => write!(f, "f64"),
                PrimitiveType::Bool => write!(f, "bool"),
                PrimitiveType::Char => write!(f, "char"),
                _ => write!(f, "unknown"),
            },
            TypeSignature::Struct(structure) => {
                if let Some(structure) = structure {
                    write!(f, "{}", structure.name)
                } else {
                    write!(f, "<unknown>")
                }
            }
            TypeSignature::Function(function) => {
                write!(f, "fn(")?;
                for (i, (name, param)) in function.params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", param.name, param.param_type.borrow_mut())?;
                }
                write!(f, ")")?;
                if let Some(return_type) = &function.return_type {
                    write!(f, " -> {}", *return_type.borrow_mut())?;
                }
                Ok(())
            }
            TypeSignature::Empty => write!(f, "()"),
        }
    }
}

fn typecheck_statement<'ctx>(
    stmt: &ast::Statement,
    ctx: &mut TypeCheckContext<'ctx>,
) -> Result<Option<TypeRef<'ctx>>, String> {
    use ast::Statement::*;
    match stmt {
        Variable(var_stmt) => {
            let var_type = ctx.get_type(var_stmt.type_name.clone())?;
            ctx.names.insert(
                var_stmt.name.clone(),
                Name {
                    var_type: var_type.clone(),
                },
            );
            if let Some(expr) = &var_stmt.initializer {
                let expr_type = typecheck_expression(expr, ctx.allow_yield(var_type.clone()))?;
                if expr_type != var_type {
                    return Err(format!(
                        "Cannot assign {} to {}",
                        (*expr_type).borrow(),
                        *var_type.borrow_mut()
                    ));
                }
            };
        }
        Expression(expr) => {
            typecheck_expression(expr, ctx)?;
        }
        Loop(loop_stmt) => typecheck_loop(loop_stmt, ctx.clone())?,
        Return(ret_stmt) => typecheck_return(ret_stmt, ctx.clone())?,
        Yield(yield_stmt) => return Ok(Some(typecheck_yield(yield_stmt, ctx.clone())?)),
        Continue => {}
        Break => {}
        other => {}
    }
    Ok(None)
}

fn typecheck_loop<'ctx>(
    loop_stmt: &ast::LoopStatement,
    ctx: TypeCheckContext<'ctx>,
) -> Result<(), String> {
    typecheck_block(&loop_stmt.body, ctx)?;
    Ok(())
}

fn typecheck_yield<'ctx>(
    yield_stmt: &ast::YieldStatement,
    mut ctx: TypeCheckContext<'ctx>,
) -> Result<TypeRef<'ctx>, String> {
    if let Some(yield_type) = ctx.yield_type.clone() {
        let yield_val_type =
            typecheck_expression(&yield_stmt.value, ctx.allow_yield(yield_type.clone()))?;
        if yield_val_type != yield_type {
            return Err(format!(
                "Cannot yield {}, expected type {}",
                *yield_val_type.borrow_mut(),
                *yield_type.borrow_mut()
            ));
        }
        Ok(yield_type)
    } else {
        return Err("Cannot yield in this context".into());
    }
}

fn typecheck_return<'ctx>(
    ret_stmt: &ast::ReturnStatement,
    mut ctx: TypeCheckContext<'ctx>,
) -> Result<(), String> {
    if let Some(t) = ctx.return_type.clone() {
        if let Some(expr) = &ret_stmt.value {
            let expr_type = typecheck_expression(expr, ctx.allow_yield(t.clone()))?;
            if expr_type != t {
                return Err(format!(
                    "Cannot return {} from {}",
                    (*expr_type).borrow(),
                    *t.borrow_mut()
                ));
            }
        } else {
            return Err("Cannot return void from non-void function".into());
        }
    } else {
        if let Some(_) = &ret_stmt.value {
            return Err("Cannot return from void function".into());
        }
    }
    Ok(())
}

fn typecheck_expression<'ctx>(
    expr: &Expression,
    ctx: &mut TypeCheckContext<'ctx>,
) -> Result<TypeRef<'ctx>, String> {
    use Expression::*;
    match expr {
        BinaryOp { left, right, op } => typecheck_binary_op(left, right, op, ctx),
        LogicalOp { left, right, op } => typecheck_logical_op(left, right, op, ctx),
        UnaryOp { expr, op } => typecheck_unary_op(expr, op, ctx),
        Identifier { name } => typecheck_identifier(name, ctx),
        Literal { literal } => typecheck_literal(literal, ctx),
        If { expr } => todo!(),
        Block { block } => match typecheck_block(block, ctx.clone()) {
            Ok(t) => {
                if let Some(t) = t {
                    Ok(t)
                } else {
                    Ok(new_type(Type::empty()))
                }
            }
            Err(e) => Err(e),
        },
        VarAssignment { var_assign } => typecheck_var_assignment(var_assign, ctx),
        FnCall { fn_call } => todo!(), //typecheck_fn_call(fn_call, ctx),
        MemberAccess { member_access } => todo!(),
        StructInitializer { struct_init } => typecheck_struct_initializer(struct_init, ctx),
    }
}

fn typecheck_struct_initializer<'ctx>(
    struct_init: &ast::StructInitializer,
    ctx: &mut TypeCheckContext<'ctx>,
) -> Result<TypeRef<'ctx>, String> {
    let struct_ptr = ctx.get_type(struct_init.struct_name.clone())?;
    let t = struct_ptr.borrow_mut();
    if let TypeSignature::Struct(Some(struct_type)) = t.type_.clone() {
        let mut field_types = Vec::new();
        for field in &struct_type.fields {
            let field_type = field.1.field_type.clone();
            field_types.push(field_type);
        }
        for (i, init_field) in struct_init.fields.iter().enumerate() {
            let Some(field_type) = field_types.get(i) else {
                return Err(format!(
                    "Too many fields in struct initializer for {}",
                    struct_init.struct_name
                ));
            };
            let expr_type =
                typecheck_expression(&init_field.value, ctx.allow_yield(field_type.clone()))?;
            if expr_type != *field_type {
                return Err(format!(
                    "Cannot assign {} to {}",
                    *expr_type.borrow_mut(),
                    *field_type.borrow_mut()
                ));
            }
        }
        Ok(struct_ptr.clone())
    } else {
        Err(format!(
            "{} is not a struct",
            struct_init.struct_name.clone()
        ))
    }
}

/* fn typecheck_fn_call<'ctx>(
    fn_call: &ast::FunctionCall,
    ctx: &mut TypeCheckContext<'ctx>,
) -> Result<TypeRef<'ctx>, String> {
    let ast::Expression::Identifier { name } = *fn_call.callee else {
        return Err("Cannot call non-function".into());
    };
    let callee = ctx
        .names
        .get(&name)
        .ok_or(format!("Unknown function {}", name))?;
    let callee_type = callee.var_type.clone();
    let callee_type = callee_type.borrow_mut();
    let callee_type = match &*callee_type {
        TypeSignature(f) => f,
        _ => return Err("Cannot call non-function".into()),
    };
} */

fn typecheck_var_assignment<'ctx>(
    var_assign: &ast::VarAssignment,
    ctx: &mut TypeCheckContext<'ctx>,
) -> Result<TypeRef<'ctx>, String> {
    let left_type = typecheck_expression(&*var_assign.left, ctx)?;
    let right_type = typecheck_expression(&*var_assign.right, ctx)?;
    if left_type != right_type {
        return Err(format!(
            "Cannot assign {} to {}",
            *right_type.borrow_mut(),
            *left_type.borrow_mut()
        ));
    }
    Ok(left_type)
}

fn typecheck_literal<'ctx>(
    literal: &crate::tokenizer::Literal,
    ctx: &mut TypeCheckContext<'ctx>,
) -> Result<TypeRef<'ctx>, String> {
    match literal {
        crate::tokenizer::Literal::String(_s) => Ok(ctx.get_type("string".into())?),
        crate::tokenizer::Literal::Int(_i) => {
            // determine if i32 or i64
            if let Some(t) = ctx.yield_type.clone() {
                match t.borrow_mut().clone().type_ {
                    TypeSignature::Primitive(PrimitiveType::I32) => {
                        return Ok(ctx.get_type("i32".into())?)
                    }
                    TypeSignature::Primitive(PrimitiveType::I64) => {
                        return Ok(ctx.get_type("i64".into())?)
                    }
                    _ => {}
                }
            } else if let Some(ret_type) = ctx.return_type.clone() {
                match ret_type.borrow_mut().clone().type_ {
                    TypeSignature::Primitive(PrimitiveType::I32) => {
                        return Ok(ctx.get_type("i32".into())?)
                    }
                    TypeSignature::Primitive(PrimitiveType::I64) => {
                        return Ok(ctx.get_type("i64".into())?)
                    }
                    _ => {}
                }
            };
            Ok(ctx.get_type("i64".into())?)
        }
        crate::tokenizer::Literal::Float(_f) => {
            // determine if f32 or f64
            if let Some(t) = ctx.yield_type.clone() {
                match t.borrow_mut().clone().type_ {
                    TypeSignature::Primitive(PrimitiveType::F32) => {
                        return Ok(ctx.get_type("f32".into())?)
                    }
                    TypeSignature::Primitive(PrimitiveType::F64) => {
                        return Ok(ctx.get_type("f64".into())?)
                    }
                    _ => {}
                }
            } else if let Some(ret_type) = ctx.return_type.clone() {
                match ret_type.borrow_mut().clone().type_ {
                    TypeSignature::Primitive(PrimitiveType::F32) => {
                        return Ok(ctx.get_type("f32".into())?)
                    }
                    TypeSignature::Primitive(PrimitiveType::F64) => {
                        return Ok(ctx.get_type("f64".into())?)
                    }
                    _ => {}
                }
            };
            Ok(ctx.get_type("f64".into())?)
        }
        crate::tokenizer::Literal::Char(_c) => Ok(ctx.get_type("char".into())?),
        crate::tokenizer::Literal::Bool(_b) => Ok(ctx.get_type("bool".into())?),
    }
}

fn typecheck_identifier<'ctx>(
    name: &str,
    ctx: &mut TypeCheckContext<'ctx>,
) -> Result<TypeRef<'ctx>, String> {
    if let Some(name) = ctx.names.get(name) {
        Ok(name.var_type.clone())
    } else {
        Err(format!("Unknown identifier {}", name))
    }
}

fn typecheck_unary_op<'ctx>(
    expr: &Expression,
    op: &Operator,
    ctx: &mut TypeCheckContext<'ctx>,
) -> Result<TypeRef<'ctx>, String> {
    let expr_type = typecheck_expression(expr, ctx)?;
    match op {
        Operator::Not => {
            if expr_type != ctx.get_type("bool".into())? {
                return Err(format!("Cannot apply ! to {}", *expr_type.borrow_mut()));
            }
        }
        Operator::Minus => {
            /* if expr_type != Type::Primitive(PrimitiveType::I32) {
                return Err(format!("Cannot apply - to {}", *expr_type.borrow_mut()));
            } */
        }
        _ => {
            return Err(format!(
                "Cannot apply {} to {}",
                op,
                *expr_type.borrow_mut()
            ));
        }
    }
    Ok(expr_type)
}

fn typecheck_logical_op<'ctx>(
    left: &Expression,
    right: &Expression,
    op: &Operator,
    ctx: &mut TypeCheckContext<'ctx>,
) -> Result<TypeRef<'ctx>, String> {
    let left_type = typecheck_expression(left, ctx)?;
    let right_type = typecheck_expression(right, ctx)?;
    if left_type != right_type {
        return Err(format!(
            "Cannot apply {} to {} and {}",
            op,
            *left_type.borrow_mut(),
            *right_type.borrow_mut()
        ));
    }
    let bool_type = ctx.types.get("bool").unwrap().clone();
    if left_type != bool_type {
        return Err(format!(
            "Cannot apply {} to {}",
            op,
            *left_type.borrow_mut()
        ));
    }
    Ok(bool_type)
}

fn typecheck_binary_op<'ctx>(
    left: &Expression,
    right: &Expression,
    op: &Operator,
    ctx: &mut TypeCheckContext<'ctx>,
) -> Result<TypeRef<'ctx>, String> {
    let left_type = typecheck_expression(left, ctx)?;
    let right_type = typecheck_expression(right, ctx)?;
    use Operator::*;
    match op {
        Plus | Minus | Times | Divide => {
            if left_type != right_type {
                return Err(format!(
                    "Cannot perform operation {} on {} and {}",
                    op,
                    *left_type.borrow_mut(),
                    *right_type.borrow_mut()
                ));
            }
            Ok(left_type)
        }
        Equals | NotEquals | LessThan | GreaterThan | LessThanEquals | GreaterThanEquals => {
            if left_type != right_type {
                return Err(format!(
                    "Cannot perform operation {} on {} and {}",
                    op,
                    *left_type.borrow_mut(),
                    *right_type.borrow_mut()
                ));
            }

            Ok(ctx.types.get("bool").unwrap().clone())
        }
        And | Or => {
            if left_type != right_type {
                return Err(format!(
                    "Cannot perform operation {} on {} and {}",
                    op,
                    *left_type.borrow_mut(),
                    *right_type.borrow_mut()
                ));
            }
            Ok(ctx.types.get("bool").unwrap().clone())
        }
        Assign(_op) => {
            if left_type != right_type {
                return Err(format!(
                    "Cannot assign {} to {}",
                    *right_type.borrow_mut(),
                    *left_type.borrow_mut()
                ));
            }
            Ok(left_type)
        }
        Modulo | Power | Not | BitwiseAnd | BitwiseOr | BitwiseXor | BitwiseNot
        | BitwiseLeftShift | BitwiseRightShift => {
            if left_type != right_type {
                return Err(format!(
                    "Cannot perform operation {} on {} and {}",
                    op,
                    *left_type.borrow_mut(),
                    *right_type.borrow_mut()
                ));
            }
            Ok(left_type)
        }
    }
}
