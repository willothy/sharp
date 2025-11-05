// Author: Will Hopkins

use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    ast::{self, ModulePath},
    lowering::{Intermediate, IntermediateProgram, ModuleId},
    typechecker::{
        context::new_type,
        type_sig::{Name, Type, TypeSignature},
    },
};

use super::{
    context::{ModuleTypeCheckCtx, StructId, TypeSig},
    typed_ast::{
        TypedExport, TypedExportType, TypedFunctionDeclaration, TypedFunctionDefinition,
        TypedImport, TypedModule, TypedStructDeclaration,
    },
};

#[derive(Debug, Clone)]
pub struct TypeChecker<'tc> {
    pub ctx: ModuleTypeCheckCtx<'tc>,
    pub intermediate: IntermediateProgram,
    pub modules: Vec<Rc<RefCell<TypedModule<'tc>>>>,
    pub module_ids: HashMap<ModulePath, ModuleId>,
    pub current_module_path: ModulePath,
    pub next_struct_id: StructId,
}

pub trait TCModule<'ctx> {
    fn get_fn_defs(&self) -> Vec<TypedFunctionDefinition<'ctx>>;
    fn get_fn_decls(&self) -> Vec<TypedFunctionDeclaration<'ctx>>;
    fn get_structs(&self) -> Vec<TypedStructDeclaration<'ctx>>;
    fn get_submodules(&self) -> HashMap<String, ModuleId>;
    fn get_requirements(&self) -> Vec<TypedImport<'ctx>>;
    fn get_parent_id(&self) -> Option<ModuleId>;
    fn get_path(&self) -> ast::ModulePath;
    fn get_name(&self) -> String;
    fn has_parent(&self) -> bool;
    fn get_ctx(&self) -> ModuleTypeCheckCtx<'ctx>;
}

impl<'ctx> TCModule<'ctx> for Rc<RefCell<TypedModule<'ctx>>> {
    fn get_fn_defs(&self) -> Vec<TypedFunctionDefinition<'ctx>> {
        self.borrow().fn_defs.clone()
    }

    fn get_fn_decls(&self) -> Vec<TypedFunctionDeclaration<'ctx>> {
        self.borrow().fn_decls.clone()
    }

    fn get_structs(&self) -> Vec<TypedStructDeclaration<'ctx>> {
        self.borrow().structs.clone()
    }

    fn get_submodules(&self) -> HashMap<String, ModuleId> {
        self.borrow().submodules.clone()
    }

    fn get_requirements(&self) -> Vec<TypedImport<'ctx>> {
        self.borrow().dependencies.clone()
    }

    fn get_parent_id(&self) -> Option<ModuleId> {
        self.borrow().parent.clone()
    }

    fn get_path(&self) -> ast::ModulePath {
        self.borrow().path.clone()
    }

    fn get_name(&self) -> String {
        self.borrow().name.clone()
    }

    fn has_parent(&self) -> bool {
        self.borrow().parent.is_some()
    }

    fn get_ctx(&self) -> ModuleTypeCheckCtx<'ctx> {
        self.borrow().ctx.clone()
    }
}

#[derive(Debug, Clone)]
pub struct TypeCheckerOutput<'tc> {
    pub modules: Vec<Rc<RefCell<TypedModule<'tc>>>>,
    pub module_ids: HashMap<ModulePath, ModuleId>,
}

impl<'tc> TypeChecker<'tc> {
    pub fn new(intermediate: IntermediateProgram) -> Self {
        Self {
            intermediate,
            ctx: ModuleTypeCheckCtx::new(),
            modules: Vec::new(),
            module_ids: HashMap::new(),
            current_module_path: Vec::new(),
            next_struct_id: 0,
        }
    }

    pub fn typecheck(&mut self) -> Result<TypeCheckerOutput<'tc>, String> {
        // Preload struct names
        for module in &self.intermediate.modules.clone() {
            self.current_module_path = module.borrow().path.clone();
            let module_id = module.borrow().id;
            self.module_ids
                .insert(module.borrow().path.clone(), module_id);

            let new_module = TypedModule {
                fn_defs: Vec::new(),
                fn_decls: Vec::new(),
                structs: Vec::new(),
                submodules: HashMap::new(),
                dependencies: Vec::new(),
                parent: None,
                exports: HashMap::new(),
                path: self.current_module_path.clone(),
                name: module.get_name(),
                id: module_id,
                ctx: ModuleTypeCheckCtx::new(),
            };

            self.modules.push(Rc::new(RefCell::new(new_module)));

            let Some(intermediate) = self.intermediate.modules.get(module_id).cloned() else {
                return Err(format!("Module with id {} not found", module_id));
            };

            let intermediate = intermediate.borrow();
            for structure in &intermediate.structs {
                let id = self.get_next_struct_id();
                self.ctx.types.insert(
                    structure.name.clone()/* intermediate.path.join("::") + "::" + &structure.name */,
                    new_type(Type {
                        sig: TypeSignature::Struct(id),
                    }),
                );
            }
        }

        for module in &self.intermediate.modules.clone() {
            self.current_module_path = module.borrow().path.clone();
            let module_id = module.borrow().id;
            let Some(module) = self.intermediate.modules.get(module_id).cloned() else {
                return Err(format!("Module with id {} not found", module_id));
            };

            let structs = self.typecheck_structs(module.clone())?;
            self.modules.get(module_id).unwrap().borrow_mut().structs = structs;
        }

        for module in &self.intermediate.modules.clone() {
            // resolve exports
            let inter_mod_ptr = module.borrow();
            let new_mod = self.modules.get(inter_mod_ptr.id).unwrap().clone();
            let mut new_mod_ptr = new_mod.borrow_mut();
            let mut exports = HashMap::new();
            for (name, export) in &inter_mod_ptr.exports {
                let ty = match &export.ty {
                    crate::lowering::ExportType::FunctionDef(f) => {
                        let ty = self.resolve_fn_def_type(f)?;
                        let TypeSignature::Function(f_type) = ty.sig else {
                            return Err(format!("Exported function {} is not a function", name));
                        };
                        TypedExportType::Function(f_type)
                    }
                    crate::lowering::ExportType::FunctionDecl(f) => {
                        let ty = self.resolve_fn_decl_type(f)?;
                        let TypeSignature::Function(f_type) = ty.sig else {
                            return Err(format!("Exported function {} is not a function", name));
                        };
                        TypedExportType::Function(f_type)
                    }
                    crate::lowering::ExportType::Struct(s) => {
                        let ty = new_mod_ptr
                            .structs
                            .iter()
                            .find(|s2| s2.name == s.name)
                            .unwrap()
                            .ty
                            .clone();
                        let TypeSignature::Struct(s_id) = ty.sig() else {
                            return Err(format!("Exported struct {} is not a struct", name));
                        };
                        TypedExportType::Struct(s_id)
                    }
                    crate::lowering::ExportType::Module(m) => TypedExportType::Module(*m),
                };
                exports.insert(
                    name.clone(),
                    TypedExport {
                        name: name.clone(),
                        ty,
                    },
                );
            }
            new_mod_ptr.exports = exports;
        }

        for module in &self.intermediate.modules.clone() {
            self.current_module_path = module.borrow().path.clone();
            let module_id = module.borrow().id;
            let Some(module) = self.intermediate.modules.get(module_id).cloned() else {
                return Err(format!("Module with id {} not found", module_id));
            };
            self.typecheck_struct_methods(module)?;
        }

        for module in &self.intermediate.modules.clone() {
            self.current_module_path = module.borrow().path.clone();
            let module_id = module.borrow().id;

            /* let Some(module) = self.intermediate.modules.get(module_id).cloned() else {
                return Err(format!("Module with id {} not found", module_id));
            }; */

            let dependencies = module
                .borrow()
                .dependencies
                .iter()
                .map(|dep| self.resolve_import_type(dep))
                .collect::<Vec<TypedImport>>();

            for dependency in &dependencies {
                // add to module context
                let ty = dependency.ty.clone();
                match ty {
                    TypedExportType::Function(f) => {
                        self.ctx.functions.insert(
                            dependency.name.clone(),
                            Name {
                                ty: new_type(Type {
                                    sig: TypeSignature::Function(f),
                                }),
                            },
                        );
                    }
                    TypedExportType::Struct(s) => {
                        self.ctx.types.insert(
                            dependency.name.clone(),
                            new_type(Type {
                                sig: TypeSignature::Struct(s),
                            }),
                        );
                    }
                    TypedExportType::Module(_) => todo!(),
                }
            }
            self.modules
                .get(module_id)
                .unwrap()
                .borrow_mut()
                .dependencies = dependencies;

            let (fn_decls, fn_defs) = self.typecheck_functions(module.clone())?;

            let submodules = module.borrow().submodules.clone();

            let parent = module.borrow().parent.clone();

            //let exports = self.resolve_exports(module.clone(), &submodules)?;

            let new_ctx = ModuleTypeCheckCtx::with_types(&self.ctx);
            let mut mod_ptr = self.modules.get(module_id).unwrap().borrow_mut();

            mod_ptr.fn_defs = fn_defs;
            mod_ptr.fn_decls = fn_decls;
            mod_ptr.submodules = submodules;
            mod_ptr.parent = parent;
            //mod_ptr.exports = exports;
            mod_ptr.ctx = std::mem::replace(&mut self.ctx, new_ctx);
        }

        Ok(TypeCheckerOutput {
            modules: self.modules.clone(),
            module_ids: self.module_ids.clone(),
        })
    }

    // Canonicalize a path to be relative to the current module
    // For example:
    //  Current module: main. convert "self::foo::bar" to "main::foo::bar"
    //  Current module: main::foo. convert "self::bar" to "main::foo::bar"
    //  Current module: main::foo. convert "super::bar" to "main::bar"
    pub fn canonicalize_path(&self, path: &ModulePath) -> ModulePath {
        let mut new_path = Vec::new();
        let mut current_module = self.current_module_path.clone();
        for segment in path {
            match segment.as_str() {
                "self" => {
                    new_path.extend(current_module.clone());
                }
                "super" => {
                    current_module.pop();
                    new_path.extend(current_module.clone());
                }
                name => {
                    new_path.push(name.to_owned());
                }
            }
        }
        new_path
    }

    fn resolve_import_type(&self, dep: &crate::lowering::Import) -> TypedImport<'tc> {
        let source_mod = self.modules.get(dep.source_module).unwrap().borrow();
        let export = source_mod.exports.get(&dep.name).unwrap();

        TypedImport {
            name: dep.name.clone(),
            source_module: dep.source_module.clone(),
            local: dep.local,
            ty: export.ty.clone(),
        }
    }

    /* pub(crate) fn resolve_exports(
        &self,
        module: Rc<RefCell<IntermediateModule>>,
        submodules: &HashMap<String, ModuleId>,
    ) -> Result<HashMap<String, TypedExport<'tc>>, String> {
        use crate::lowering::ExportType::*;
        let module_exports = module.borrow().exports.clone();

        let mut exports = HashMap::new();
        for (name, export) in module_exports {
            exports.insert(
                name.clone(),
                TypedExport {
                    name: name.clone(),
                    ty: match export.ty {
                        FunctionDef(_) | FunctionDecl(_) => {
                            let Some(fn_id) = self.ctx.functions.get(&name).cloned() else {
                                panic!("Export {} is not a function", name);
                            };
                            if let TypeSignature::Function(func) = fn_id.ty.sig() {
                                TypedExportType::Function(func)
                            } else {
                                panic!("Export {} is not a function", name);
                            }
                        }
                        crate::lowering::ExportType::Struct(_) => {
                            let Some(struct_t) = self.ctx.types.get(&name).cloned() else {
                                panic!("Export {} is not a struct", name);
                            };
                            if let TypeSignature::Struct(id) = struct_t.sig() {
                                TypedExportType::Struct(
                                    self.ctx
                                        .struct_types
                                        .get(&id)
                                        .expect("Struct not found")
                                        .clone(),
                                )
                            } else {
                                panic!("Export {} is not a struct", name);
                            }
                        }
                        crate::lowering::ExportType::Module(_) => {
                            if let Some(sub) = submodules.get(&name) {
                                TypedExportType::Module(*sub)
                            } else {
                                return Err(format!("Export {} is not a module", name));
                            }
                        }
                    },
                },
            );
        }
        Ok(exports)
    } */

    pub fn get_next_struct_id(&mut self) -> usize {
        let id = self.next_struct_id;
        self.next_struct_id += 1;
        id
    }
}
