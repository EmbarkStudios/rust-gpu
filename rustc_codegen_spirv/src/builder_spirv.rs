use rspirv::dr::{Block, Builder, Module, Operand};
use rspirv::spirv::{AddressingModel, Capability, MemoryModel, Op, Word};
use rspirv::{binary::Assemble, binary::Disassemble};
use std::cell::{RefCell, RefMut};
use std::collections::hash_map::Entry;
use std::collections::HashMap;
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
/// The LLVM module builder model (and therefore codegen_ssa) assumes that there is a central module object, then,
/// builder objects are created pointing at that central module object (e.g. for adding instructions to a basic block).
/// Several of these builder objects can be live at the same time, mutating the central module object all at once.
/// Unfortunately, rspirv doesn't work like that. Instead, there is a single builder object, which owns a module and a
/// "cursor". This cursor indicates to the builder where to append instructions when an instruction is added - e.g. if
/// add() is called, then OpAdd is appended to the basic block pointed to by the cursor.
/// So! We emulate the LLVM system by treating the rspirv Builder as the "central module object", then, when a "builder
/// object" is created, we store a reference to a RefCell<rspirv builder>, *as well as* a copy of the cursor for that
/// particular builder. Whenever the RefCell is borrowed, then we stomp over the rspirv cursor with our copy, causing the
/// duration of that RefCell borrow to use that cursor.
/// So, if you're writing code inside crate::builder::Builder, then self.emit() will use self.cursor (the current basic
/// block) as that "stomp-over" cursor and return a mutable reference to the rspirv builder. If you're writing code
/// elsewhere (codegen_cx::CodegenCx), then self.emit_global() will use the generic "global cursor" and return a mutable
/// reference to the rspirv builder with no basic block nor function selected, i.e. any instructions emitted will be in
/// the global section.
#[derive(Debug, Default, Copy, Clone)]
#[must_use = "BuilderCursor should usually be assigned to the Builder.cursor field"]
pub struct BuilderCursor {
    pub function: Option<usize>,
    pub block: Option<usize>,
}

pub struct BuilderSpirv {
    builder: RefCell<Builder>,
    constants: RefCell<HashMap<SpirvConst, SpirvValue>>,
    constants_inverse: RefCell<HashMap<Word, SpirvConst>>,
}

impl BuilderSpirv {
    pub fn new() -> Self {
        let mut builder = Builder::new();
        // TODO: Flip this back to Shader once the structurizer is working.
        builder.capability(Capability::Kernel);
        // Temp hack: Linkage allows us to get away with no OpEntryPoint
        builder.capability(Capability::Linkage);
        // All the below capabilities are temp hacks to validate libcore with spirv-val
        builder.capability(Capability::GenericPointer);
        builder.capability(Capability::Int8);
        builder.capability(Capability::Int16);
        builder.capability(Capability::Int64);
        builder.capability(Capability::Float64);
        builder.capability(Capability::Addresses);
        // TODO: Physical pointer size
        builder.memory_model(AddressingModel::Physical32, MemoryModel::OpenCL);
        Self {
            builder: RefCell::new(builder),
            constants: Default::default(),
            constants_inverse: Default::default(),
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

    /// See comment on BuilderCursor
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

    pub fn def_constant(&self, val: SpirvConst) -> SpirvValue {
        match self.constants.borrow_mut().entry(val) {
            Entry::Occupied(entry) => *entry.get(),
            Entry::Vacant(entry) => {
                let id = match *entry.key() {
                    SpirvConst::U32(ty, v) => {
                        self.builder.borrow_mut().constant_u32(ty, v).with_type(ty)
                    }
                    SpirvConst::U64(ty, v) => {
                        self.builder.borrow_mut().constant_u64(ty, v).with_type(ty)
                    }
                    SpirvConst::F32(ty, v) => self
                        .builder
                        .borrow_mut()
                        .constant_f32(ty, f32::from_bits(v))
                        .with_type(ty),
                    SpirvConst::F64(ty, v) => self
                        .builder
                        .borrow_mut()
                        .constant_f64(ty, f64::from_bits(v))
                        .with_type(ty),
                    SpirvConst::Bool(ty, v) => {
                        if v {
                            self.builder.borrow_mut().constant_true(ty).with_type(ty)
                        } else {
                            self.builder.borrow_mut().constant_false(ty).with_type(ty)
                        }
                    }
                    SpirvConst::Composite(ty, ref v) => self
                        .builder
                        .borrow_mut()
                        .constant_composite(ty, v.iter().copied())
                        .with_type(ty),
                    SpirvConst::Null(ty) => {
                        self.builder.borrow_mut().constant_null(ty).with_type(ty)
                    }
                    SpirvConst::Undef(ty) => {
                        self.builder.borrow_mut().undef(ty, None).with_type(ty)
                    }
                };
                self.constants_inverse
                    .borrow_mut()
                    .insert(id.def, entry.key().clone());
                entry.insert(id);
                id
            }
        }
    }

    pub fn lookup_const(&self, def: Word) -> Option<SpirvConst> {
        self.constants_inverse.borrow().get(&def).cloned()
    }

    pub fn lookup_const_u64(&self, def: Word) -> Option<u64> {
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

        panic!("Block not found: {}", id);
    }
}
