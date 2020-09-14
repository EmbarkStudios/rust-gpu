use crate::abi::RecursivePointeeCache;
use crate::builder::Builder;
use crate::codegen_cx::CodegenCx;
use rspirv::dr::Operand;
use rspirv::spirv::{Decoration, StorageClass, Word};
use rustc_target::abi::{Align, Size};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::iter::once;
use std::lazy::SyncLazy;
use std::sync::Mutex;

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
    // see comment where Opaque is constructed
    #[allow(dead_code)]
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
        count: Word,
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

fn memset_fill_u16(b: u8) -> u16 {
    b as u16 | ((b as u16) << 8)
}

fn memset_fill_u32(b: u8) -> u32 {
    b as u32 | ((b as u32) << 8) | ((b as u32) << 16) | ((b as u32) << 24)
}

fn memset_fill_u64(b: u8) -> u64 {
    b as u64
        | ((b as u64) << 8)
        | ((b as u64) << 16)
        | ((b as u64) << 24)
        | ((b as u64) << 32)
        | ((b as u64) << 40)
        | ((b as u64) << 48)
        | ((b as u64) << 56)
}

fn memset_dynamic_scalar<'a, 'tcx>(
    builder: &Builder<'a, 'tcx>,
    fill_var: Word,
    byte_width: usize,
    is_float: bool,
) -> Word {
    let composite_type = SpirvType::Vector {
        element: SpirvType::Integer(8, false).def(builder),
        count: byte_width as u32,
    }
    .def(builder);
    let composite = builder
        .emit()
        .composite_construct(
            composite_type,
            None,
            std::iter::repeat(fill_var).take(byte_width),
        )
        .unwrap();
    let result_type = if is_float {
        SpirvType::Float(byte_width as u32 * 8)
    } else {
        SpirvType::Integer(byte_width as u32 * 8, false)
    };
    builder
        .emit()
        .bitcast(result_type.def(builder), None, composite)
        .unwrap()
}

impl SpirvType {
    /// Note: Builder::type_* should be called *nowhere else* but here, to ensure CodegenCx::type_defs stays up-to-date
    pub fn def<'tcx>(&self, cx: &CodegenCx<'tcx>) -> Word {
        if let Some(cached) = cx.type_cache.get(self) {
            return cached;
        }
        let result = match *self {
            SpirvType::Void => cx.emit_global().type_void(),
            SpirvType::Bool => cx.emit_global().type_bool(),
            SpirvType::Integer(width, signedness) => {
                let result = cx
                    .emit_global()
                    .type_int(width, if signedness { 1 } else { 0 });
                match width {
                    8 | 16 | 32 | 64 => (),
                    128 => cx.poison(result, "u128"),
                    other => panic!("Integer width {} invalid for spir-v", other),
                };
                result
            }
            SpirvType::Float(width) => {
                match width {
                    32 | 64 => (),
                    other => panic!("Float width {} invalid for spir-v", other),
                };
                cx.emit_global().type_float(width)
            }
            SpirvType::Adt {
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
            SpirvType::Opaque { ref name } => cx.emit_global().type_opaque(name),
            SpirvType::Vector { element, count } => cx.emit_global().type_vector(element, count),
            SpirvType::Array { element, count } => {
                // ArrayStride decoration wants in *bytes*
                let element_size = cx
                    .lookup_type(element)
                    .sizeof(cx)
                    .expect("Element of sized array must be sized")
                    .bytes();
                let result = cx.emit_global().type_array(element, count);
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
            SpirvType::RuntimeArray { element } => {
                let result = cx.emit_global().type_runtime_array(element);
                if cx.kernel_mode {
                    cx.poison(result, "RuntimeArray in kernel mode");
                }
                result
            }
            SpirvType::Pointer {
                storage_class,
                pointee,
            } => {
                let result = cx.emit_global().type_pointer(None, storage_class, pointee);
                // no pointers to functions
                if let SpirvType::Function { .. } = cx.lookup_type(pointee) {
                    cx.poison(result, "pointer to function")
                }
                result
            }
            SpirvType::Function {
                return_type,
                ref arguments,
            } => cx
                .emit_global()
                .type_function(return_type, arguments.iter().cloned()),
        };
        cx.type_cache.def(result, self);
        result
    }

    pub fn def_with_id<'tcx>(&self, cx: &CodegenCx<'tcx>, id: Word) -> Word {
        if let Some(cached) = cx.type_cache.get(self) {
            assert_eq!(cached, id);
            return cached;
        }
        let result = match *self {
            SpirvType::Pointer {
                storage_class,
                pointee,
            } => {
                let result = cx
                    .emit_global()
                    .type_pointer(Some(id), storage_class, pointee);
                // no pointers to functions
                if let SpirvType::Function { .. } = cx.lookup_type(pointee) {
                    cx.poison(result, "pointer to function")
                }
                result
            }
            ref other => panic!("def_with_id invalid for type {:?}", other),
        };
        cx.type_cache.def(result, self);
        result
    }

    pub fn debug<'cx, 'tcx>(
        self,
        id: Word,
        cx: &'cx CodegenCx<'tcx>,
    ) -> SpirvTypePrinter<'cx, 'tcx> {
        SpirvTypePrinter { ty: self, id, cx }
    }

    pub fn sizeof<'tcx>(&self, cx: &CodegenCx<'tcx>) -> Option<Size> {
        let result = match *self {
            SpirvType::Void => Size::ZERO,
            SpirvType::Bool => Size::from_bytes(1),
            SpirvType::Integer(width, _) => Size::from_bits(width),
            SpirvType::Float(width) => Size::from_bits(width),
            SpirvType::Adt { size, .. } => size?,
            SpirvType::Opaque { .. } => Size::ZERO,
            SpirvType::Vector { element, count } => {
                cx.lookup_type(element).sizeof(cx)? * count as u64
            }
            SpirvType::Array { element, count } => {
                cx.lookup_type(element).sizeof(cx)? * cx.builder.lookup_const_u64(count).unwrap()
            }
            SpirvType::RuntimeArray { .. } => return None,
            SpirvType::Pointer { .. } => cx.tcx.data_layout.pointer_size,
            SpirvType::Function { .. } => cx.tcx.data_layout.pointer_size,
        };
        Some(result)
    }

    pub fn alignof<'tcx>(&self, cx: &CodegenCx<'tcx>) -> Align {
        match *self {
            SpirvType::Void => Align::from_bytes(0).unwrap(),
            SpirvType::Bool => Align::from_bytes(1).unwrap(),
            SpirvType::Integer(width, _) => Align::from_bits(width as u64).unwrap(),
            SpirvType::Float(width) => Align::from_bits(width as u64).unwrap(),
            SpirvType::Adt { align, .. } => align,
            SpirvType::Opaque { .. } => Align::from_bytes(0).unwrap(),
            // TODO: Is this right? (must match rustc's understanding)
            SpirvType::Vector { element, .. } => cx.lookup_type(element).alignof(cx),
            SpirvType::Array { element, .. } => cx.lookup_type(element).alignof(cx),
            SpirvType::RuntimeArray { element } => cx.lookup_type(element).alignof(cx),
            SpirvType::Pointer { .. } => cx.tcx.data_layout.pointer_align.abi,
            SpirvType::Function { .. } => cx.tcx.data_layout.pointer_align.abi,
        }
    }

    pub fn memset_const_pattern<'tcx>(&self, cx: &CodegenCx<'tcx>, fill_byte: u8) -> Word {
        match *self {
            SpirvType::Void => panic!("memset invalid on void pattern"),
            SpirvType::Bool => panic!("memset invalid on bool pattern"),
            SpirvType::Integer(width, _signedness) => match width {
                8 => cx.constant_u8(fill_byte).def,
                16 => cx.constant_u16(memset_fill_u16(fill_byte)).def,
                32 => cx.constant_u32(memset_fill_u32(fill_byte)).def,
                64 => cx.constant_u64(memset_fill_u64(fill_byte)).def,
                _ => panic!("memset on integer width {} not implemented yet", width),
            },
            SpirvType::Float(width) => match width {
                32 => {
                    cx.constant_f32(f32::from_bits(memset_fill_u32(fill_byte)))
                        .def
                }
                64 => {
                    cx.constant_f64(f64::from_bits(memset_fill_u64(fill_byte)))
                        .def
                }
                _ => panic!("memset on float width {} not implemented yet", width),
            },
            SpirvType::Adt { .. } => panic!("memset on structs not implemented yet"),
            SpirvType::Opaque { .. } => panic!("memset on opaque type is invalid"),
            SpirvType::Vector { element, count } => {
                let elem_pat = cx.lookup_type(element).memset_const_pattern(cx, fill_byte);
                cx.emit_global()
                    .constant_composite(self.def(cx), vec![elem_pat; count as usize])
            }
            SpirvType::Array { element, count } => {
                let elem_pat = cx.lookup_type(element).memset_const_pattern(cx, fill_byte);
                let count = cx.builder.lookup_const_u64(count).unwrap() as usize;
                cx.emit_global()
                    .constant_composite(self.def(cx), vec![elem_pat; count])
            }
            SpirvType::RuntimeArray { .. } => {
                panic!("memset on runtime arrays not implemented yet")
            }
            SpirvType::Pointer { .. } => panic!("memset on pointers not implemented yet"),
            SpirvType::Function { .. } => panic!("memset on functions not implemented yet"),
        }
    }

    pub fn memset_dynamic_pattern<'a, 'tcx>(
        &self,
        builder: &Builder<'a, 'tcx>,
        fill_var: Word,
    ) -> Word {
        match *self {
            SpirvType::Void => panic!("memset invalid on void pattern"),
            SpirvType::Bool => panic!("memset invalid on bool pattern"),
            SpirvType::Integer(width, _signedness) => match width {
                8 => fill_var,
                16 => memset_dynamic_scalar(builder, fill_var, 2, false),
                32 => memset_dynamic_scalar(builder, fill_var, 4, false),
                64 => memset_dynamic_scalar(builder, fill_var, 8, false),
                _ => panic!("memset on integer width {} not implemented yet", width),
            },
            SpirvType::Float(width) => match width {
                32 => memset_dynamic_scalar(builder, fill_var, 4, true),
                64 => memset_dynamic_scalar(builder, fill_var, 8, true),
                _ => panic!("memset on float width {} not implemented yet", width),
            },
            SpirvType::Adt { .. } => panic!("memset on structs not implemented yet"),
            SpirvType::Opaque { .. } => panic!("memset on opaque type is invalid"),
            SpirvType::Array { element, count } => {
                let elem_pat = builder
                    .lookup_type(element)
                    .memset_dynamic_pattern(builder, fill_var);
                let count = builder.builder.lookup_const_u64(count).unwrap() as usize;
                builder
                    .emit()
                    .composite_construct(
                        self.def(builder),
                        None,
                        std::iter::repeat(elem_pat).take(count),
                    )
                    .unwrap()
            }
            SpirvType::Vector { element, count } => {
                let elem_pat = builder
                    .lookup_type(element)
                    .memset_dynamic_pattern(builder, fill_var);
                builder
                    .emit()
                    .composite_construct(
                        self.def(builder),
                        None,
                        std::iter::repeat(elem_pat).take(count as usize),
                    )
                    .unwrap()
            }
            SpirvType::RuntimeArray { .. } => {
                panic!("memset on runtime arrays not implemented yet")
            }
            SpirvType::Pointer { .. } => panic!("memset on pointers not implemented yet"),
            SpirvType::Function { .. } => panic!("memset on functions not implemented yet"),
        }
    }
}

pub struct SpirvTypePrinter<'cx, 'tcx> {
    id: Word,
    ty: SpirvType,
    cx: &'cx CodegenCx<'tcx>,
}

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
    /// Map from ID to structure
    pub type_defs: RefCell<HashMap<Word, SpirvType>>,
    /// Inverse of type_defs (used to cache generating types)
    pub type_cache: RefCell<HashMap<SpirvType, Word>>,
    /// Recursive pointer breaking
    pub recursive_pointee_cache: RecursivePointeeCache<'tcx>,
}

impl TypeCache<'_> {
    fn get(&self, ty: &SpirvType) -> Option<Word> {
        self.type_cache.borrow().get(ty).copied()
    }

    pub fn lookup(&self, word: Word) -> SpirvType {
        self.type_defs
            .borrow()
            .get(&word)
            .expect("Tried to lookup value that wasn't a type, or has no definition")
            .clone()
    }

    fn def(&self, word: Word, ty: &SpirvType) {
        // Change to expect_none if/when stabilized
        assert!(
            self.type_defs
                .borrow_mut()
                .insert(word, ty.clone())
                .is_none(),
            "type_defs already had entry, caching failed? {:#?}",
            ty
        );
        assert!(
            self.type_cache
                .borrow_mut()
                .insert(ty.clone(), word)
                .is_none(),
            "type_cache already had entry, caching failed? {:#?}",
            ty
        );
    }
}
