use rspirv::dr::Builder;
use spirv_headers::{AddressingModel, MemoryModel, Word};
use std::collections::HashMap;

macro_rules! impl_cache {
    ($name:ident) => {
        pub fn $name(&mut self) -> Word {
            match self.cache.$name {
                Some(value) => value,
                None => {
                    let value = self.builder.$name();
                    self.cache.$name = Some(value);
                    value
                }
            }
        }
    };
}

pub struct SpirvContext {
    pub builder: Builder,
    cache: Cache,
}

#[derive(Default)]
struct Cache {
    type_void: Option<Word>,
    type_bool: Option<Word>,
    type_int: HashMap<(u32, u32), Word>,
    type_float: HashMap<u32, Word>,
    type_function: HashMap<(Word, Vec<Word>), Word>,
}

impl SpirvContext {
    pub fn new() -> Self {
        let mut builder = Builder::new();
        builder.memory_model(AddressingModel::Logical, MemoryModel::GLSL450);
        SpirvContext {
            builder,
            cache: Default::default(),
        }
    }

    impl_cache! {type_void}
    impl_cache! {type_bool}

    pub fn type_int(&mut self, width: u32, signedness: u32) -> Word {
        let builder = &mut self.builder;
        *self
            .cache
            .type_int
            .entry((width, signedness))
            .or_insert_with(|| builder.type_int(width, signedness))
    }

    pub fn type_float(&mut self, width: u32) -> Word {
        let builder = &mut self.builder;
        *self
            .cache
            .type_float
            .entry(width)
            .or_insert_with(|| builder.type_float(width))
    }

    pub fn type_function(&mut self, return_type: Word, parameter_types: Vec<Word>) -> Word {
        let builder = &mut self.builder;
        *self
            .cache
            .type_function
            .entry((return_type, parameter_types))
            .or_insert_with_key(|(return_type, parameter_types)| {
                builder.type_function(*return_type, parameter_types)
            })
    }
}
