use bimap::BiHashMap;
use rspirv::dr::{Block, Builder, Module, Operand};
use rspirv::spirv::{AddressingModel, Capability, MemoryModel, Op, Word};
use rspirv::{binary::Assemble, binary::Disassemble};
use rustc_middle::bug;
use std::cell::{RefCell, RefMut};
use std::{fs::File, io::Write, path::Path};

#[derive(Copy, Clone, Debug, Default, Ord, PartialOrd, Eq, PartialEq, Hash)]
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

#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum SpirvConst {
    U32(Word, u32),
    U64(Word, u64),
    /// f32 isn't hash, so store bits
    F32(Word, u32),
    /// f64 isn't hash, so store bits
    F64(Word, u64),
    Bool(Word, bool),
    Composite(Word, Vec<Word>),
    Null(Word),
    Undef(Word),
}

/// Cursor system:
///
/// The LLVM module builder model (and therefore `codegen_ssa`) assumes that there is a central
/// module object, then, builder objects are created pointing at that central module object (e.g.
/// for adding instructions to a basic block).  Several of these builder objects can be live at the
/// same time, mutating the central module object all at once.  Unfortunately, rspirv doesn't work
/// like that. Instead, there is a single builder object, which owns a module and a "cursor". This
/// cursor indicates to the builder where to append instructions when an instruction is added -
/// e.g. if add() is called, then `OpAdd` is appended to the basic block pointed to by the cursor.
///
/// So! We emulate the LLVM system by treating the rspirv Builder as the "central module object",
/// then, when a "builder object" is created, we store a reference to a `RefCell<rspirv builder>`,
/// *as well as* a copy of the cursor for that particular builder. Whenever the `RefCell` is
/// borrowed, then we stomp over the rspirv cursor with our copy, causing the duration of that
/// `RefCell` borrow to use that cursor.
///
/// So, if you're writing code inside `crate::builder::Builder`, then `self.emit()` will use
/// `self.cursor` (the current basic block) as that "stomp-over" cursor and return a mutable
/// reference to the rspirv builder. If you're writing code elsewhere (`codegen_cx::CodegenCx`),
/// then `self.emit_global()` will use the generic "global cursor" and return a mutable reference
/// to the rspirv builder with no basic block nor function selected, i.e. any instructions emitted
/// will be in the global section.
#[derive(Debug, Default, Copy, Clone)]
#[must_use = "BuilderCursor should usually be assigned to the Builder.cursor field"]
pub struct BuilderCursor {
    pub function: Option<usize>,
    pub block: Option<usize>,
}

pub struct BuilderSpirv {
    builder: RefCell<Builder>,
    constants: RefCell<BiHashMap<SpirvConst, SpirvValue>>,
}

impl BuilderSpirv {
    pub fn new(
        version: Option<(u8, u8)>,
        memory_model: Option<MemoryModel>,
        kernel_mode: bool,
    ) -> Self {
        let mut builder = Builder::new();
        // Default to spir-v 1.3
        let version = version.unwrap_or((1, 3));
        builder.set_version(version.0, version.1);
        let memory_model = memory_model.unwrap_or(MemoryModel::Vulkan);
        if kernel_mode {
            builder.capability(Capability::Kernel);
        } else {
            builder.capability(Capability::Shader);
            if memory_model == MemoryModel::Vulkan {
                if version < (1, 5) {
                    builder.extension("SPV_KHR_vulkan_memory_model");
                }
                builder.capability(Capability::VulkanMemoryModel);
            }
            builder.capability(Capability::VariablePointers);
            if version < (1, 3) {
                builder.extension("SPV_KHR_variable_pointers");
            }
        }
        // The linker will always be ran on this module
        builder.capability(Capability::Linkage);
        // TODO: Remove these eventually?
        builder.capability(Capability::Int8);
        builder.capability(Capability::Int16);
        builder.capability(Capability::Int64);
        builder.capability(Capability::Float64);
        if kernel_mode {
            builder.capability(Capability::Addresses);
            builder.memory_model(AddressingModel::Physical32, MemoryModel::OpenCL);
        } else {
            builder.memory_model(AddressingModel::Logical, memory_model);
        }
        Self {
            builder: RefCell::new(builder),
            constants: Default::default(),
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
        File::create(path)
            .unwrap()
            .write_all(crate::slice_u32_to_u8(&spirv_module))
            .unwrap();
    }

    /// See comment on `BuilderCursor`
    pub fn builder(&self, cursor: BuilderCursor) -> RefMut<'_, Builder> {
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

    pub fn has_capability(&self, capability: Capability) -> bool {
        self.builder
            .borrow()
            .module_ref()
            .capabilities
            .iter()
            .any(|inst| {
                inst.class.opcode == Op::Capability
                    && inst.operands[0].unwrap_capability() == capability
            })
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

        bug!("Function not found: {}", id);
    }

    pub fn def_constant(&self, val: SpirvConst) -> SpirvValue {
        let mut builder = self.builder(BuilderCursor::default());
        if let Some(value) = self.constants.borrow_mut().get_by_left(&val) {
            return *value;
        }
        let id = match val {
            SpirvConst::U32(ty, v) => builder.constant_u32(ty, v).with_type(ty),
            SpirvConst::U64(ty, v) => builder.constant_u64(ty, v).with_type(ty),
            SpirvConst::F32(ty, v) => builder.constant_f32(ty, f32::from_bits(v)).with_type(ty),
            SpirvConst::F64(ty, v) => builder.constant_f64(ty, f64::from_bits(v)).with_type(ty),
            SpirvConst::Bool(ty, v) => {
                if v {
                    builder.constant_true(ty).with_type(ty)
                } else {
                    builder.constant_false(ty).with_type(ty)
                }
            }
            SpirvConst::Composite(ty, ref v) => builder
                .constant_composite(ty, v.iter().copied())
                .with_type(ty),
            SpirvConst::Null(ty) => builder.constant_null(ty).with_type(ty),
            SpirvConst::Undef(ty) => builder.undef(ty, None).with_type(ty),
        };
        self.constants
            .borrow_mut()
            .insert_no_overwrite(val, id)
            .unwrap();
        id
    }

    pub fn lookup_const(&self, def: SpirvValue) -> Option<SpirvConst> {
        self.constants.borrow().get_by_right(&def).cloned()
    }

    pub fn lookup_const_u64(&self, def: SpirvValue) -> Option<u64> {
        match self.lookup_const(def)? {
            SpirvConst::U32(_, v) => Some(v as u64),
            SpirvConst::U64(_, v) => Some(v),
            _ => None,
        }
    }

    pub fn set_global_initializer(&self, global: Word, initializer: Word) {
        let mut builder = self.builder.borrow_mut();
        let module = builder.module_mut();
        let index = module
            .types_global_values
            .iter()
            .enumerate()
            .find_map(|(index, inst)| {
                if inst.result_id == Some(global) {
                    Some(index)
                } else {
                    None
                }
            })
            .expect("set_global_initializer global not found");
        // Remove and push it to the end, to keep spir-v definition order.
        let mut inst = module.types_global_values.remove(index);
        assert_eq!(inst.class.opcode, Op::Variable);
        assert_eq!(
            inst.operands.len(),
            1,
            "global already has initializer defined: {}",
            global
        );
        inst.operands.push(Operand::IdRef(initializer));
        module.types_global_values.push(inst);
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

        bug!("Block not found: {}", id);
    }
}
