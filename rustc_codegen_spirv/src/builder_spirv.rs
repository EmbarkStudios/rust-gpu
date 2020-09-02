use rspirv::dr::{Block, Builder, Module, Operand};
use rspirv::spirv::{AddressingModel, Capability, MemoryModel, Op, Word};
use rspirv::{binary::Assemble, binary::Disassemble};
use std::cell::{RefCell, RefMut};
use std::{fs::File, io::Write, path::Path, sync::Mutex};

/// This theoretically should contain the rspirv::Builder, but instead, we put that into the BuilderSpirv struct, and at
/// the end of compilation, move the Builder over here. The reason is that the WriteBackendMethods trait requires the
/// Module type to be Send+Sync, which necessitates a Mutex over a RefCell. So, we've split it up to not have the perf
/// cost of a Mutex, as accessing the builder is a rather hot path.
pub struct ModuleSpirv {
    pub module: Mutex<Option<Module>>,
}

impl ModuleSpirv {
    pub fn new() -> Self {
        Self {
            module: Mutex::new(None),
        }
    }
}

#[derive(Copy, Clone, Debug, Default, Ord, PartialOrd, Eq, PartialEq)]
pub struct SpirvValue {
    pub def: Word,
    pub ty: Word,
}

pub trait SpirvValueExt {
    fn with_type(self, ty: Word) -> SpirvValue;
}

impl SpirvValueExt for Word {
    fn with_type(self, ty: Word) -> SpirvValue {
        SpirvValue { def: self, ty }
    }
}

#[derive(Debug, Default, Copy, Clone)]
#[must_use = "BuilderCursor should usually be assigned to the Builder.cursor field"]
pub struct BuilderCursor {
    pub function: Option<usize>,
    pub block: Option<usize>,
}

pub struct BuilderSpirv {
    builder: RefCell<Builder>,
}

impl BuilderSpirv {
    pub fn new() -> Self {
        let mut builder = Builder::new();
        builder.capability(Capability::Shader);
        // Temp hack: Linkage allows us to get away with no OpEntryPoint
        builder.capability(Capability::Linkage);
        builder.memory_model(AddressingModel::Logical, MemoryModel::GLSL450);
        Self {
            builder: RefCell::new(builder),
        }
    }

    pub fn finalize(self) -> Module {
        self.builder.into_inner().module()
    }

    pub fn dump_module_str(&self) -> String {
        let mut module = self.builder.borrow().module_ref().clone();
        let mut header = rspirv::dr::ModuleHeader::new(0);
        header.set_version(0, 0);
        module.header = Some(header);
        module.disassemble()
    }

    /// Helper function useful to place right before a crash, to debug the module state.
    pub fn dump_module(&self, path: impl AsRef<Path>) {
        let mut module = self.builder.borrow().module_ref().clone();
        let mut header = rspirv::dr::ModuleHeader::new(0);
        header.set_version(0, 0);
        module.header = Some(header);
        let disas = module.disassemble();
        println!("{}", disas);
        let spirv_module = module.assemble();
        let spirv_module_u8: &[u8] = unsafe {
            std::slice::from_raw_parts(
                spirv_module.as_ptr() as *const u8,
                spirv_module.len() * std::mem::size_of::<u32>(),
            )
        };
        File::create(path)
            .unwrap()
            .write_all(spirv_module_u8)
            .unwrap();
    }

    /// Note: This Builder can be used by multiple things being built at the same time, so, we need to save and restore
    /// the current cursor every time we do something.
    pub fn builder(&self, cursor: BuilderCursor) -> RefMut<Builder> {
        let mut builder = self.builder.borrow_mut();
        // select_function does bounds checks and other relatively expensive things, so don't just call it
        // unconditionally.
        if builder.selected_function() != cursor.function {
            builder.select_function(cursor.function).unwrap();
        }
        if cursor.function.is_some() && builder.selected_block() != cursor.block {
            builder.select_block(cursor.block).unwrap();
        }
        builder
    }

    pub fn select_function_by_id(&self, id: Word) -> BuilderCursor {
        let mut builder = self.builder.borrow_mut();
        for (index, func) in builder.module_ref().functions.iter().enumerate() {
            if func.def.as_ref().and_then(|i| i.result_id) == Some(id) {
                builder.select_function(Some(index)).unwrap();
                return BuilderCursor {
                    function: Some(index),
                    block: None,
                };
            }
        }

        panic!("Function not found: {}", id);
    }

    pub fn constant_u32(&self, ty: Word, val: u32) -> Word {
        self.def_constant(ty, Operand::LiteralInt32(val))
    }

    pub fn constant_u64(&self, ty: Word, val: u64) -> Word {
        self.def_constant(ty, Operand::LiteralInt64(val))
    }

    pub fn constant_f32(&self, ty: Word, val: f32) -> Word {
        self.def_constant(ty, Operand::LiteralFloat32(val))
    }

    pub fn constant_f64(&self, ty: Word, val: f64) -> Word {
        self.def_constant(ty, Operand::LiteralFloat64(val))
    }

    fn def_constant(&self, ty: Word, val: Operand) -> Word {
        let mut builder = self.builder.borrow_mut();
        // TODO: Cache these instead of doing a search.
        for inst in &builder.module_ref().types_global_values {
            if inst.class.opcode == Op::Constant
                && inst.result_type == Some(ty)
                && inst.operands[0] == val
            {
                return inst.result_id.unwrap();
            }
        }
        match val {
            Operand::LiteralInt32(v) => builder.constant_u32(ty, v),
            Operand::LiteralInt64(v) => builder.constant_u64(ty, v),
            Operand::LiteralFloat32(v) => builder.constant_f32(ty, v),
            Operand::LiteralFloat64(v) => builder.constant_f64(ty, v),
            unknown => panic!("def_constant doesn't support constant type {}", unknown),
        }
    }

    pub fn lookup_const_u64(&self, def: Word) -> Result<u64, &'static str> {
        match self.lookup_const(def)? {
            Operand::LiteralInt32(v) => Ok(v as u64),
            Operand::LiteralInt64(v) => Ok(v),
            _ => Err("Literal value not Int32/64"),
        }
    }

    pub fn lookup_const(&self, def: Word) -> Result<Operand, &'static str> {
        let builder = self.builder.borrow();
        for inst in &builder.module_ref().types_global_values {
            if inst.result_id == Some(def) {
                return if inst.class.opcode == Op::Constant {
                    Ok(inst.operands[0].clone())
                } else {
                    Err("Instruction not OpConstant")
                };
            }
        }
        Err("Definition not found")
    }

    pub fn lookup_global_constant_variable(&self, def: Word) -> Result<Word, &'static str> {
        // TODO: Maybe assert that this indeed a constant?
        let builder = self.builder.borrow();
        for inst in &builder.module_ref().types_global_values {
            if inst.result_id == Some(def) {
                return if inst.class.opcode == Op::Variable {
                    if let Some(&Operand::IdRef(id_ref)) = inst.operands.get(1) {
                        Ok(id_ref)
                    } else {
                        Err("Instruction had no initializer")
                    }
                } else {
                    Err("Instruction not OpVariable")
                };
            }
        }
        Err("Definition not found")
    }

    pub fn find_global_constant_variable(&self, value: Word) -> Option<SpirvValue> {
        let builder = self.builder.borrow();
        for inst in &builder.module_ref().types_global_values {
            if inst.class.opcode == Op::Variable {
                if let Some(&Operand::IdRef(id_ref)) = inst.operands.get(1) {
                    if id_ref == value {
                        return Some(inst.result_id.unwrap().with_type(inst.result_type.unwrap()));
                    }
                }
            }
        }
        None
    }

    pub fn set_global_initializer(&self, global: Word, initialiezr: Word) {
        let mut builder = self.builder.borrow_mut();
        for inst in &mut builder.module_mut().types_global_values {
            if inst.class.opcode == Op::Variable {
                if let Some(&Operand::IdRef(id_ref)) = inst.operands.get(1) {
                    if id_ref == global {
                        assert_eq!(
                            inst.operands.len(),
                            1,
                            "global already has initializer defined: {}",
                            global
                        );
                        inst.operands.push(Operand::IdRef(initialiezr));
                    }
                }
            }
        }
    }

    pub fn select_block_by_id(&self, id: Word) -> BuilderCursor {
        fn block_matches(block: &Block, id: Word) -> bool {
            block.label.as_ref().and_then(|b| b.result_id) == Some(id)
        }

        let mut builder = self.builder.borrow_mut();
        let module = builder.module_ref();

        // The user is probably selecting a block in the current function, so search that first.
        if let Some(selected_function) = builder.selected_function() {
            // make no-ops really fast
            if let Some(selected_block) = builder.selected_block() {
                let block = &module.functions[selected_function].blocks[selected_block];
                if block_matches(block, id) {
                    return BuilderCursor {
                        function: Some(selected_function),
                        block: Some(selected_block),
                    };
                }
            }

            for (index, block) in module.functions[selected_function]
                .blocks
                .iter()
                .enumerate()
            {
                if block_matches(block, id) {
                    builder.select_block(Some(index)).unwrap();
                    return BuilderCursor {
                        function: Some(selected_function),
                        block: Some(index),
                    };
                }
            }
        }

        // Search the whole module.
        for (function_index, function) in module.functions.iter().enumerate() {
            for (block_index, block) in function.blocks.iter().enumerate() {
                if block_matches(block, id) {
                    builder.select_function(Some(function_index)).unwrap();
                    builder.select_block(Some(block_index)).unwrap();
                    return BuilderCursor {
                        function: Some(function_index),
                        block: Some(block_index),
                    };
                }
            }
        }

        panic!("Block not found: {}", id);
    }
}
