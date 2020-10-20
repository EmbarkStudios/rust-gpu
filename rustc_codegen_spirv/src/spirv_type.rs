use crate::abi::RecursivePointeeCache;
use crate::builder_spirv::SpirvValue;
use crate::codegen_cx::CodegenCx;
use bimap::BiHashMap;
use rspirv::dr::Operand;
use rspirv::spirv::{Capability, Decoration, StorageClass, Word};
use rustc_target::abi::{Align, Size};
use std::cell::RefCell;
use std::fmt;
use std::iter::once;
use std::lazy::SyncLazy;
use std::sync::Mutex;

/// Spir-v types are represented as simple Words, which are the `result_id` of instructions like
/// `OpTypeInteger`. Sometimes, however, we want to inspect one of these Words and ask questions
/// like "Is this an `OpTypeInteger`? How many bits does it have?". This struct holds all of that
/// information. All types that are emitted are registered in `CodegenCx`, so you can always look
/// up the definition of a `Word` via `cx.lookup_type`. Note that this type doesn't actually store
/// the `result_id` of the type declaration instruction, merely the contents.
#[derive(Clone, Debug, PartialEq, Eq, Ord, PartialOrd, Hash)]
pub enum SpirvType {
    Void,
    Bool,
    Integer(u32, bool),
    Float(u32),
    /// This uses the rustc definition of "adt", i.e. a struct, enum, or union
    Adt {
        name: String,
        align: Align,
        size: Option<Size>,
        field_types: Vec<Word>,
        field_offsets: Vec<Size>,
        field_names: Option<Vec<String>>,
    },
    Opaque {
        name: String,
    },
    Vector {
        element: Word,
        /// Note: vector count is literal.
        count: u32,
    },
    Array {
        element: Word,
        /// Note: array count is ref to constant.
        count: SpirvValue,
    },
    RuntimeArray {
        element: Word,
    },
    Pointer {
        storage_class: StorageClass,
        pointee: Word,
    },
    Function {
        return_type: Word,
        arguments: Vec<Word>,
    },
}

impl SpirvType {
    /// Note: `Builder::type_*` should be called *nowhere else* but here, to ensure
    /// `CodegenCx::type_defs` stays up-to-date
    pub fn def(self, cx: &CodegenCx<'_>) -> Word {
        if let Some(cached) = cx.type_cache.get(&self) {
            return cached;
        }
        let result = match self {
            Self::Void => cx.emit_global().type_void(),
            Self::Bool => cx.emit_global().type_bool(),
            Self::Integer(width, signedness) => {
                let result = cx
                    .emit_global()
                    .type_int(width, if signedness { 1 } else { 0 });
                match width {
                    8 if !cx.builder.has_capability(Capability::Int8) => {
                        cx.zombie_no_span(result, "u8 without OpCapability Int8")
                    }
                    16 if !cx.builder.has_capability(Capability::Int16) => {
                        cx.zombie_no_span(result, "u16 without OpCapability Int16")
                    }
                    64 if !cx.builder.has_capability(Capability::Int64) => {
                        cx.zombie_no_span(result, "u64 without OpCapability Int64")
                    }
                    8 | 16 | 32 | 64 => (),
                    128 => cx.zombie_no_span(result, "u128"),
                    other => cx
                        .tcx
                        .sess
                        .fatal(&format!("Integer width {} invalid for spir-v", other)),
                };
                result
            }
            Self::Float(width) => {
                let result = cx.emit_global().type_float(width);
                match width {
                    64 if !cx.builder.has_capability(Capability::Float64) => {
                        cx.zombie_no_span(result, "f64 without OpCapability Float64")
                    }
                    32 | 64 => (),
                    other => cx
                        .tcx
                        .sess
                        .fatal(&format!("Float width {} invalid for spir-v", other)),
                };
                result
            }
            Self::Adt {
                ref name,
                align: _,
                size: _,
                ref field_types,
                ref field_offsets,
                ref field_names,
            } => {
                let mut emit = cx.emit_global();
                // Ensure a unique struct is emitted each time, due to possibly having different OpMemberDecorates
                let id = emit.id();
                let result = emit.type_struct_id(Some(id), field_types.iter().cloned());
                emit.name(result, name);
                // The struct size is only used in our own sizeof_in_bits() (used in e.g. ArrayStride decoration)
                if !cx.kernel_mode {
                    // TODO: kernel mode can't do this??
                    for (index, offset) in field_offsets.iter().copied().enumerate() {
                        emit.member_decorate(
                            result,
                            index as u32,
                            Decoration::Offset,
                            [Operand::LiteralInt32(offset.bytes() as u32)]
                                .iter()
                                .cloned(),
                        );
                    }
                }
                if let Some(field_names) = field_names {
                    for (index, field_name) in field_names.iter().enumerate() {
                        emit.member_name(result, index as u32, field_name);
                    }
                }
                result
            }
            Self::Opaque { ref name } => cx.emit_global().type_opaque(name),
            Self::Vector { element, count } => cx.emit_global().type_vector(element, count),
            Self::Array { element, count } => {
                // ArrayStride decoration wants in *bytes*
                let element_size = cx
                    .lookup_type(element)
                    .sizeof(cx)
                    .expect("Element of sized array must be sized")
                    .bytes();
                let result = cx.emit_global().type_array(element, count.def);
                if !cx.kernel_mode {
                    // TODO: kernel mode can't do this??
                    cx.emit_global().decorate(
                        result,
                        Decoration::ArrayStride,
                        once(Operand::LiteralInt32(element_size as u32)),
                    );
                }
                result
            }
            Self::RuntimeArray { element } => {
                let result = cx.emit_global().type_runtime_array(element);
                if cx.kernel_mode {
                    cx.zombie_no_span(result, "RuntimeArray in kernel mode");
                }
                result
            }
            Self::Pointer {
                storage_class,
                pointee,
            } => {
                let result = cx.emit_global().type_pointer(None, storage_class, pointee);
                // no pointers to functions
                if let Self::Function { .. } = cx.lookup_type(pointee) {
                    cx.zombie_even_in_user_code(result, "pointer to function")
                }
                result
            }
            Self::Function {
                return_type,
                ref arguments,
            } => cx
                .emit_global()
                .type_function(return_type, arguments.iter().cloned()),
        };
        cx.type_cache.def(result, self);
        result
    }

    /// `def_with_id` is used by the `RecursivePointeeCache` to handle `OpTypeForwardPointer`: when
    /// emitting the subsequent `OpTypePointer`, the ID is already known and must be re-used.
    pub fn def_with_id(self, cx: &CodegenCx<'_>, id: Word) -> Word {
        if let Some(cached) = cx.type_cache.get(&self) {
            assert_eq!(cached, id);
            return cached;
        }
        let result = match self {
            Self::Pointer {
                storage_class,
                pointee,
            } => {
                let result = cx
                    .emit_global()
                    .type_pointer(Some(id), storage_class, pointee);
                // no pointers to functions
                if let Self::Function { .. } = cx.lookup_type(pointee) {
                    cx.zombie_even_in_user_code(result, "pointer to function")
                }
                result
            }
            ref other => cx
                .tcx
                .sess
                .fatal(&format!("def_with_id invalid for type {:?}", other)),
        };
        cx.type_cache.def(result, self);
        result
    }

    /// Use this if you want a pretty type printing that recursively prints the types within (e.g. struct fields)
    pub fn debug<'cx, 'tcx>(
        self,
        id: Word,
        cx: &'cx CodegenCx<'tcx>,
    ) -> SpirvTypePrinter<'cx, 'tcx> {
        SpirvTypePrinter { ty: self, id, cx }
    }

    pub fn sizeof<'tcx>(&self, cx: &CodegenCx<'tcx>) -> Option<Size> {
        let result = match *self {
            Self::Void => Size::ZERO,
            Self::Bool => Size::from_bytes(1),
            Self::Integer(width, _) => Size::from_bits(width),
            Self::Float(width) => Size::from_bits(width),
            Self::Adt { size, .. } => size?,
            Self::Opaque { .. } => Size::ZERO,
            Self::Vector { element, count } => {
                cx.lookup_type(element).sizeof(cx)? * count.next_power_of_two() as u64
            }
            Self::Array { element, count } => {
                cx.lookup_type(element).sizeof(cx)? * cx.builder.lookup_const_u64(count).unwrap()
            }
            Self::RuntimeArray { .. } => return None,
            Self::Pointer { .. } => cx.tcx.data_layout.pointer_size,
            Self::Function { .. } => cx.tcx.data_layout.pointer_size,
        };
        Some(result)
    }

    pub fn alignof<'tcx>(&self, cx: &CodegenCx<'tcx>) -> Align {
        match *self {
            Self::Void => Align::from_bytes(0).unwrap(),
            Self::Bool => Align::from_bytes(1).unwrap(),
            Self::Integer(width, _) => Align::from_bits(width as u64).unwrap(),
            Self::Float(width) => Align::from_bits(width as u64).unwrap(),
            Self::Adt { align, .. } => align,
            Self::Opaque { .. } => Align::from_bytes(0).unwrap(),
            // Vectors have size==align
            Self::Vector { .. } => Align::from_bytes(
                self.sizeof(cx)
                    .expect("alignof: Vectors must be sized")
                    .bytes(),
            )
            .expect("alignof: Vectors must have power-of-2 size"),
            Self::Array { element, .. } => cx.lookup_type(element).alignof(cx),
            Self::RuntimeArray { element } => cx.lookup_type(element).alignof(cx),
            Self::Pointer { .. } => cx.tcx.data_layout.pointer_align.abi,
            Self::Function { .. } => cx.tcx.data_layout.pointer_align.abi,
        }
    }
}

pub struct SpirvTypePrinter<'cx, 'tcx> {
    id: Word,
    ty: SpirvType,
    cx: &'cx CodegenCx<'tcx>,
}

/// Types can be recursive, e.g. a struct can contain a pointer to itself. So, we need to keep
/// track of a stack of what types are currently being printed, to not infinitely loop.
/// Unfortunately, unlike `fmt::Display`, we can't easily pass down the "stack" of
/// currently-being-printed types, so we use a global static.
static DEBUG_STACK: SyncLazy<Mutex<Vec<Word>>> = SyncLazy::new(|| Mutex::new(Vec::new()));

impl fmt::Debug for SpirvTypePrinter<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        {
            let mut debug_stack = DEBUG_STACK.lock().unwrap();
            if debug_stack.contains(&self.id) {
                return write!(f, "<recursive type id={}>", self.id);
            }
            debug_stack.push(self.id);
        }
        let res = match self.ty {
            SpirvType::Void => f.debug_struct("Void").field("id", &self.id).finish(),
            SpirvType::Bool => f.debug_struct("Bool").field("id", &self.id).finish(),
            SpirvType::Integer(width, signedness) => f
                .debug_struct("Integer")
                .field("id", &self.id)
                .field("width", &width)
                .field("signedness", &signedness)
                .finish(),
            SpirvType::Float(width) => f
                .debug_struct("Float")
                .field("id", &self.id)
                .field("width", &width)
                .finish(),
            SpirvType::Adt {
                ref name,
                align,
                size,
                ref field_types,
                ref field_offsets,
                ref field_names,
            } => {
                let fields = field_types
                    .iter()
                    .map(|&f| self.cx.debug_type(f))
                    .collect::<Vec<_>>();
                f.debug_struct("Adt")
                    .field("id", &self.id)
                    .field("name", &name)
                    .field("align", &align)
                    .field("size", &size)
                    .field("field_types", &fields)
                    .field("field_offsets", field_offsets)
                    .field("field_names", field_names)
                    .finish()
            }
            SpirvType::Opaque { ref name } => f
                .debug_struct("Opaque")
                .field("id", &self.id)
                .field("name", &name)
                .finish(),
            SpirvType::Vector { element, count } => f
                .debug_struct("Vector")
                .field("id", &self.id)
                .field("element", &self.cx.debug_type(element))
                .field("count", &count)
                .finish(),
            SpirvType::Array { element, count } => f
                .debug_struct("Array")
                .field("id", &self.id)
                .field("element", &self.cx.debug_type(element))
                .field(
                    "count",
                    &self
                        .cx
                        .builder
                        .lookup_const_u64(count)
                        .expect("Array type has invalid count value"),
                )
                .finish(),
            SpirvType::RuntimeArray { element } => f
                .debug_struct("RuntimeArray")
                .field("id", &self.id)
                .field("element", &self.cx.debug_type(element))
                .finish(),
            SpirvType::Pointer {
                storage_class,
                pointee,
            } => f
                .debug_struct("Pointer")
                .field("id", &self.id)
                .field("storage_class", &storage_class)
                .field("pointee", &self.cx.debug_type(pointee))
                .finish(),
            SpirvType::Function {
                return_type,
                ref arguments,
            } => {
                let args = arguments
                    .iter()
                    .map(|&a| self.cx.debug_type(a))
                    .collect::<Vec<_>>();
                f.debug_struct("Function")
                    .field("id", &self.id)
                    .field("return_type", &self.cx.lookup_type(return_type))
                    .field("arguments", &args)
                    .finish()
            }
        };
        {
            let mut debug_stack = DEBUG_STACK.lock().unwrap();
            debug_stack.pop();
        }
        res
    }
}

/// Types can be recursive, e.g. a struct can contain a pointer to itself. So, we need to keep
/// track of a stack of what types are currently being printed, to not infinitely loop. So, we only
/// use `fmt::Display::fmt` as an "entry point", and then call through to our own (recursive)
/// custom function that has a parameter for the current stack. Make sure to not call Display on a
/// type inside the custom function!
impl fmt::Display for SpirvTypePrinter<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.display(&mut Vec::new(), f)
    }
}

impl SpirvTypePrinter<'_, '_> {
    fn display(&self, stack: &mut Vec<Word>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn ty<'tcx>(
            cx: &CodegenCx<'tcx>,
            stack: &mut Vec<Word>,
            f: &mut fmt::Formatter<'_>,
            ty: Word,
        ) -> fmt::Result {
            if stack.contains(&ty) {
                f.write_str("<recursive type>")
            } else {
                stack.push(ty);
                let result = cx.debug_type(ty).display(stack, f);
                assert_eq!(ty, stack.pop().unwrap());
                result
            }
        }
        match self.ty {
            SpirvType::Void => f.write_str("void"),
            SpirvType::Bool => f.write_str("bool"),
            SpirvType::Integer(width, signedness) => {
                let prefix = if signedness { "i" } else { "u" };
                write!(f, "{}{}", prefix, width)
            }
            SpirvType::Float(width) => write!(f, "f{}", width),
            SpirvType::Adt {
                ref name,
                align: _,
                size: _,
                ref field_types,
                field_offsets: _,
                ref field_names,
            } => {
                write!(f, "struct {} {{ ", name)?;
                for (index, &field) in field_types.iter().enumerate() {
                    let suffix = if index + 1 == field_types.len() {
                        ""
                    } else {
                        ", "
                    };
                    if let Some(field_names) = field_names {
                        write!(f, "{}: ", field_names[index])?;
                    }
                    ty(self.cx, stack, f, field)?;
                    write!(f, "{}", suffix)?;
                }
                f.write_str(" }")
            }
            SpirvType::Opaque { ref name } => write!(f, "struct {}", name),
            SpirvType::Vector { element, count } => {
                ty(self.cx, stack, f, element)?;
                write!(f, "x{}", count)
            }
            SpirvType::Array { element, count } => {
                let len = self.cx.builder.lookup_const_u64(count);
                let len = len.expect("Array type has invalid count value");
                f.write_str("[")?;
                ty(self.cx, stack, f, element)?;
                write!(f, "; {}]", len)
            }
            SpirvType::RuntimeArray { element } => {
                f.write_str("[")?;
                ty(self.cx, stack, f, element)?;
                f.write_str("]")
            }
            SpirvType::Pointer {
                storage_class,
                pointee,
            } => {
                write!(f, "*{{{:?}}} ", storage_class)?;
                ty(self.cx, stack, f, pointee)
            }
            SpirvType::Function {
                return_type,
                ref arguments,
            } => {
                f.write_str("fn(")?;
                for (index, &arg) in arguments.iter().enumerate() {
                    let suffix = if index + 1 == arguments.len() {
                        ""
                    } else {
                        ", "
                    };
                    ty(self.cx, stack, f, arg)?;
                    write!(f, "{}", suffix)?;
                }
                f.write_str(") -> ")?;
                ty(self.cx, stack, f, return_type)
            }
        }
    }
}

#[derive(Default)]
pub struct TypeCache<'tcx> {
    /// Map between ID and structure
    pub type_defs: RefCell<BiHashMap<Word, SpirvType>>,
    /// Recursive pointer breaking
    pub recursive_pointee_cache: RecursivePointeeCache<'tcx>,
}

impl TypeCache<'_> {
    fn get(&self, ty: &SpirvType) -> Option<Word> {
        self.type_defs.borrow().get_by_right(ty).copied()
    }

    pub fn lookup(&self, word: Word) -> SpirvType {
        self.type_defs
            .borrow()
            .get_by_left(&word)
            .expect("Tried to lookup value that wasn't a type, or has no definition")
            .clone()
    }

    fn def(&self, word: Word, ty: SpirvType) {
        self.type_defs
            .borrow_mut()
            .insert_no_overwrite(word, ty)
            .unwrap();
    }
}
