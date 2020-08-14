use rspirv::spirv::Word;

pub struct TypeTracker {}

impl TypeTracker {
    pub fn new() -> Self {
        Self {}
    }
}

impl SpirvType {
    pub fn def(&self) -> Word {
        match *self {
            SpirvType::Primitive(def) => def,
            SpirvType::Adt { def, .. } => def,
            SpirvType::Pointer { def, .. } => def,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SpirvType {
    /// A basic spir-v type with no interesting properties: an integer, bool, void, etc.
    Primitive(Word),
    /// This uses the rustc definition of "adt", i.e. a struct, enum, or union
    Adt {
        def: Word,
        // TODO: enums/unions
        field_types: Vec<SpirvType>,
    },
    Pointer {
        def: Word,
        pointee: Box<SpirvType>,
    },
}
