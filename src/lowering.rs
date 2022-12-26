use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::ast::{FunctionDeclaration, FunctionDefinition, Module, ModulePath, StructDeclaration};

#[derive(Debug, Clone)]
pub struct Export {
    pub name: String,
    pub ty: ExportType,
}

#[derive(Debug, Clone)]
pub enum ExportType {
    FunctionDef(FunctionDefinition),
    FunctionDecl(FunctionDeclaration),
    Struct(StructDeclaration),
    Module(ModuleId),
}

#[derive(Debug, Clone)]
pub struct Import {
    pub name: String,
    pub source_module: ModuleId,
    pub local: bool,
}

#[derive(Debug, Clone)]
pub struct IntermediateModule {
    pub fn_defs: Vec<FunctionDefinition>,
    pub fn_decls: Vec<FunctionDeclaration>,
    pub structs: Vec<StructDeclaration>,
    pub submodules: HashMap<String, ModuleId>,
    pub dependencies: Vec<Import>,
    pub parent: Option<ModuleId>,
    pub exports: HashMap<String, Export>,
    pub name: String,
    pub path: ModulePath,
    pub id: ModuleId,
}

pub trait Intermediate {
    fn get_name(&self) -> String;
    fn get_path(&self) -> Vec<String>;
    fn get_parent(&self) -> Option<ModuleId>;
    fn get_submodules(&self) -> HashMap<String, ModuleId>;
}

impl Intermediate for Rc<RefCell<IntermediateModule>> {
    fn get_name(&self) -> String {
        self.borrow().name.clone()
    }

    fn get_path(&self) -> Vec<String> {
        self.borrow().path.clone()
    }

    fn get_parent(&self) -> Option<ModuleId> {
        self.borrow().parent.clone()
    }

    fn get_submodules(&self) -> HashMap<String, ModuleId> {
        self.borrow().submodules.clone()
    }
}

pub type ModuleId = usize;

#[derive(Debug, Clone)]
pub struct IntermediateProgram {
    pub root: Option<ModuleId>,
    pub modules: Vec<Rc<RefCell<IntermediateModule>>>,
    pub module_ids: HashMap<ModulePath, ModuleId>,
    pub current_module_idx: ModuleId,
}

impl IntermediateProgram {
    pub fn lower(root: Rc<RefCell<Module>>) -> Result<IntermediateProgram, String> {
        let mut prog = IntermediateProgram {
            root: None,
            modules: Vec::new(),
            module_ids: HashMap::new(),
            current_module_idx: 0,
        };
        prog.lower_module(root, None)?;
        Ok(prog)
    }

    fn add_module(&mut self, module: Rc<RefCell<IntermediateModule>>) -> ModuleId {
        let module_ref = module.borrow();
        self.module_ids
            .insert(module_ref.path.clone(), module_ref.id);
        self.modules.push(module.clone());
        self.current_module_idx += 1;
        module_ref.id
    }

    fn lower_module(
        &mut self,
        module: Rc<RefCell<Module>>,
        parent: Option<ModuleId>,
    ) -> Result<ModuleId, String> {
        let module = module.borrow();
        let intermediate = Rc::new(RefCell::new(IntermediateModule {
            fn_defs: module.fn_defs.clone(),
            fn_decls: module.fn_decls.clone(),
            structs: module.structs.clone(),
            submodules: HashMap::new(),
            dependencies: Vec::new(),
            parent,
            exports: HashMap::new(),
            name: module.name.clone(),
            path: module.path.clone(),
            id: self.current_module_idx,
        }));
        let this_module = self.add_module(intermediate.clone());
        if self.root.is_none() {
            self.root = Some(this_module);
        }

        let mut submodules = HashMap::new();

        // Add submodules to the intermediate program
        let submodule_exports = module
            .submodules
            .iter()
            .map(|submodule| {
                let lowered_id =
                    self.lower_module(submodule.clone(), Some(intermediate.borrow().id))?;
                let Some(lowered) = self.modules.get(lowered_id) else {
                        return Err(format!("Invalid module id: {}", lowered_id));
                    };
                let lowered = lowered.borrow();
                submodules.insert(lowered.name.clone(), lowered_id);
                Ok((
                    lowered.name.clone(),
                    Export {
                        name: lowered.name.clone(),
                        ty: ExportType::Module(lowered.id),
                    },
                ))
            })
            .collect::<Result<Vec<(String, Export)>, String>>()?;
        intermediate.borrow_mut().exports.extend(submodule_exports);

        let fn_def_exports = intermediate
            .borrow()
            .fn_defs
            .iter()
            .map(|x| {
                (
                    x.name.clone(),
                    Export {
                        name: x.name.clone(),
                        ty: ExportType::FunctionDef(x.clone()),
                    },
                )
            })
            .collect::<Vec<(String, Export)>>();
        intermediate.borrow_mut().exports.extend(fn_def_exports);

        let fn_decl_exports = intermediate
            .borrow()
            .fn_decls
            .iter()
            .map(|x| {
                (
                    x.name.clone(),
                    Export {
                        name: x.name.clone(),
                        ty: ExportType::FunctionDecl(x.clone()),
                    },
                )
            })
            .collect::<Vec<(String, Export)>>();
        intermediate.borrow_mut().exports.extend(fn_decl_exports);

        let struct_exports = intermediate
            .borrow()
            .structs
            .iter()
            .map(|struct_decl| {
                (
                    struct_decl.name.clone(),
                    Export {
                        name: struct_decl.name.clone(),
                        ty: ExportType::Struct(struct_decl.clone()),
                    },
                )
            })
            .collect::<Vec<(String, Export)>>();
        intermediate.borrow_mut().exports.extend(struct_exports);

        let dependencies = module
            .dependencies
            .iter()
            .map(|dep| {
                let mut path = dep.item_path.clone();
                let Some(name) = path.pop() else {
                        return Err(format!("Invalid dep: {:?}", dep));
                    };
                let source_module = if path.len() == 0 {
                    this_module
                } else {
                    self.get_module_id(&path)?
                };

                Ok(Import {
                    name,
                    source_module,
                    local: dep.local,
                })
            })
            .collect::<Result<Vec<Import>, String>>()?;

        intermediate.borrow_mut().dependencies.extend(dependencies);
        intermediate.borrow_mut().submodules.extend(submodules);

        Ok(this_module)
    }

    pub fn get_module_id(&self, path: &Vec<String>) -> Result<ModuleId, String> {
        let Some(id) = self.module_ids.get(path) else {
            return Err(format!("Module not found: {:?}", path));
        };
        Ok(*id)
    }
}
