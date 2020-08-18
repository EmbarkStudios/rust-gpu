use rspirv::dr::{Block, Builder, Module};
use rspirv::spirv::{AddressingModel, Capability, MemoryModel, Word};
use rustc_codegen_ssa::traits::{ModuleBufferMethods, ThinBufferMethods};
use std::cell::{RefCell, RefMut};
use std::sync::Mutex;

// uncategorized things, should be put into modules eventually

pub struct SpirvModuleBuffer;

impl ModuleBufferMethods for SpirvModuleBuffer {
    fn data(&self) -> &[u8] {
        todo!()
    }
}

pub struct SprivThinBuffer;

impl ThinBufferMethods for SprivThinBuffer {
    fn data(&self) -> &[u8] {
        todo!()
    }
}

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

#[derive(Default, Copy, Clone)]
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
