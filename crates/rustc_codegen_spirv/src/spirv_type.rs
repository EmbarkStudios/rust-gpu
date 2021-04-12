use crate::abi::{RecursivePointeeCache, TyLayoutNameKey};
use crate::builder_spirv::SpirvValue;
use crate::codegen_cx::CodegenCx;
use bimap::BiHashMap;
use indexmap::IndexSet;
use rspirv::dr::Operand;
use rspirv::spirv::{
    AccessQualifier, Capability, Decoration, Dim, ImageFormat, StorageClass, Word,
};
use rustc_data_structures::fx::FxHashMap;
use rustc_span::def_id::DefId;
use rustc_span::Span;
use rustc_target::abi::{Align, Size};
use std::cell::RefCell;
use std::fmt;
use std::iter;
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
        /// Not emitted into SPIR-V, but used to avoid too much deduplication,
        /// which could result in one SPIR-V `OpType*` having many names
        /// (not in itself an issue, but it makes error reporting harder).
        def_id: Option<DefId>,

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
        pointee: Word,
    },
    Function {
        return_type: Word,
        arguments: Vec<Word>,
    },
    Image {
        sampled_type: Word,
        dim: Dim,
        depth: u32,
        arrayed: u32,
        multisampled: u32,
        sampled: u32,
        image_format: ImageFormat,
        access_qualifier: Option<AccessQualifier>,
    },
    Sampler,
    SampledImage {
        image_type: Word,
    },

    /// `OpTypeStruct` decorated with `Block`, required by Vulkan (and OpenGL)
    /// for `PushConstant`, `Uniform` and `StorageBuffer` interface variables.
    InterfaceBlock {
        inner_type: Word,
    },
}

impl SpirvType {
    /// Note: `Builder::type_*` should be called *nowhere else* but here, to ensure
    /// `CodegenCx::type_defs` stays up-to-date
    pub fn def(self, def_span: Span, cx: &CodegenCx<'_>) -> Word {
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
                        cx.zombie_with_span(result, def_span, "u8 without OpCapability Int8")
                    }
                    16 if !cx.builder.has_capability(Capability::Int16) => {
                        cx.zombie_with_span(result, def_span, "u16 without OpCapability Int16")
                    }
                    64 if !cx.builder.has_capability(Capability::Int64) => {
                        cx.zombie_with_span(result, def_span, "u64 without OpCapability Int64")
                    }
                    8 | 16 | 32 | 64 => (),
                    128 => cx.zombie_with_span(result, def_span, "u128"),
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
                        cx.zombie_with_span(result, def_span, "f64 without OpCapability Float64")
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
                def_id: _,
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
                // The struct size is only used in our own sizeof_in_bits() (used in e.g. ArrayStride decoration)
                if !cx.target.is_kernel() {
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
                let result = cx.emit_global().type_array(element, count.def_cx(cx));
                if !cx.target.is_kernel() {
                    // TODO: kernel mode can't do this??
                    cx.emit_global().decorate(
                        result,
                        Decoration::ArrayStride,
                        iter::once(Operand::LiteralInt32(element_size as u32)),
                    );
                }
                result
            }
            Self::RuntimeArray { element } => {
                let result = cx.emit_global().type_runtime_array(element);
                // ArrayStride decoration wants in *bytes*
                let element_size = cx
                    .lookup_type(element)
                    .sizeof(cx)
                    .expect("Element of sized array must be sized")
                    .bytes();
                cx.emit_global().decorate(
                    result,
                    Decoration::ArrayStride,
                    iter::once(Operand::LiteralInt32(element_size as u32)),
                );
                if cx.target.is_kernel() {
                    cx.zombie_with_span(result, def_span, "RuntimeArray in kernel mode");
                }
                result
            }
            Self::Pointer { pointee } => {
                // NOTE(eddyb) we emit `StorageClass::Generic` here, but later
                // the linker will specialize the entire SPIR-V module to use
                // storage classes inferred from `OpVariable`s.
                let result = cx
                    .emit_global()
                    .type_pointer(None, StorageClass::Generic, pointee);
                // no pointers to functions
                if let Self::Function { .. } = cx.lookup_type(pointee) {
                    cx.zombie_even_in_user_code(
                        result,
                        def_span,
                        "function pointer types are not allowed",
                    )
                }
                result
            }
            Self::Function {
                return_type,
                ref arguments,
            } => cx
                .emit_global()
                .type_function(return_type, arguments.iter().cloned()),
            Self::Image {
                sampled_type,
                dim,
                depth,
                arrayed,
                multisampled,
                sampled,
                image_format,
                access_qualifier,
            } => cx.emit_global().type_image(
                sampled_type,
                dim,
                depth,
                arrayed,
                multisampled,
                sampled,
                image_format,
                access_qualifier,
            ),
            Self::Sampler => cx.emit_global().type_sampler(),
            Self::SampledImage { image_type } => cx.emit_global().type_sampled_image(image_type),

            Self::InterfaceBlock { inner_type } => {
                let mut emit = cx.emit_global();
                let id = emit.id();
                let result = emit.type_struct_id(Some(id), iter::once(inner_type));
                emit.decorate(result, Decoration::Block, iter::empty());
                emit.member_decorate(
                    result,
                    0,
                    Decoration::Offset,
                    [Operand::LiteralInt32(0)].iter().cloned(),
                );
                result
            }
        };
        cx.type_cache.def(result, self);
        result
    }

    /// `def_with_id` is used by the `RecursivePointeeCache` to handle `OpTypeForwardPointer`: when
    /// emitting the subsequent `OpTypePointer`, the ID is already known and must be re-used.
    pub fn def_with_id(self, cx: &CodegenCx<'_>, def_span: Span, id: Word) -> Word {
        if let Some(cached) = cx.type_cache.get(&self) {
            assert_eq!(cached, id);
            return cached;
        }
        let result = match self {
            Self::Pointer { pointee } => {
                // NOTE(eddyb) we emit `StorageClass::Generic` here, but later
                // the linker will specialize the entire SPIR-V module to use
                // storage classes inferred from `OpVariable`s.
                let result =
                    cx.emit_global()
                        .type_pointer(Some(id), StorageClass::Generic, pointee);
                // no pointers to functions
                if let Self::Function { .. } = cx.lookup_type(pointee) {
                    cx.zombie_even_in_user_code(
                        result,
                        def_span,
                        "function pointer types are not allowed",
                    )
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

    /// In addition to `SpirvType::def`, also name the resulting type (with `OpName`).
    pub fn def_with_name<'tcx>(
        self,
        cx: &CodegenCx<'tcx>,
        def_span: Span,
        name_key: TyLayoutNameKey<'tcx>,
    ) -> Word {
        let id = self.def(def_span, cx);

        // Only emit `OpName` if this is the first time we see this name.
        let mut type_names = cx.type_cache.type_names.borrow_mut();
        if type_names.entry(id).or_default().insert(name_key) {
            cx.emit_global().name(id, name_key.to_string());
        }

        id
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
            // Types that have a dynamic size, or no concept of size at all.
            Self::Void
            | Self::Opaque { .. }
            | Self::RuntimeArray { .. }
            | Self::Function { .. } => return None,

            Self::Bool => Size::from_bytes(1),
            Self::Integer(width, _) | Self::Float(width) => Size::from_bits(width),
            Self::Adt { size, .. } => size?,
            Self::Vector { element, count } => {
                cx.lookup_type(element).sizeof(cx)? * count.next_power_of_two() as u64
            }
            Self::Array { element, count } => {
                cx.lookup_type(element).sizeof(cx)? * cx.builder.lookup_const_u64(count).unwrap()
            }
            Self::Pointer { .. } => cx.tcx.data_layout.pointer_size,
            Self::Image { .. } | Self::Sampler | Self::SampledImage { .. } => Size::from_bytes(4),

            Self::InterfaceBlock { inner_type } => cx.lookup_type(inner_type).sizeof(cx)?,
        };
        Some(result)
    }

    pub fn alignof<'tcx>(&self, cx: &CodegenCx<'tcx>) -> Align {
        match *self {
            // Types that have no concept of size or alignment.
            Self::Void | Self::Opaque { .. } | Self::Function { .. } => {
                Align::from_bytes(0).unwrap()
            }

            Self::Bool => Align::from_bytes(1).unwrap(),
            Self::Integer(width, _) | Self::Float(width) => Align::from_bits(width as u64).unwrap(),
            Self::Adt { align, .. } => align,
            // Vectors have size==align
            Self::Vector { .. } => Align::from_bytes(
                self.sizeof(cx)
                    .expect("alignof: Vectors must be sized")
                    .bytes(),
            )
            .expect("alignof: Vectors must have power-of-2 size"),
            Self::Array { element, .. } | Self::RuntimeArray { element } => {
                cx.lookup_type(element).alignof(cx)
            }
            Self::Pointer { .. } => cx.tcx.data_layout.pointer_align.abi,
            Self::Image { .. } | Self::Sampler | Self::SampledImage { .. } => {
                Align::from_bytes(4).unwrap()
            }

            Self::InterfaceBlock { inner_type } => cx.lookup_type(inner_type).alignof(cx),
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
                def_id,
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
                    .field("def_id", &def_id)
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
            SpirvType::Pointer { pointee } => f
                .debug_struct("Pointer")
                .field("id", &self.id)
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
            SpirvType::Image {
                sampled_type,
                dim,
                depth,
                arrayed,
                multisampled,
                sampled,
                image_format,
                access_qualifier,
            } => f
                .debug_struct("Image")
                .field("id", &self.id)
                .field("sampled_type", &self.cx.debug_type(sampled_type))
                .field("dim", &dim)
                .field("depth", &depth)
                .field("arrayed", &arrayed)
                .field("multisampled", &multisampled)
                .field("sampled", &sampled)
                .field("image_format", &image_format)
                .field("access_qualifier", &access_qualifier)
                .finish(),
            SpirvType::Sampler => f.debug_struct("Sampler").field("id", &self.id).finish(),
            SpirvType::SampledImage { image_type } => f
                .debug_struct("SampledImage")
                .field("id", &self.id)
                .field("image_type", &self.cx.debug_type(image_type))
                .finish(),

            SpirvType::InterfaceBlock { inner_type } => f
                .debug_struct("InterfaceBlock")
                .field("id", &self.id)
                .field("inner_type", &self.cx.debug_type(inner_type))
                .finish(),
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
                def_id: _,
                align: _,
                size: _,
                ref field_types,
                field_offsets: _,
                ref field_names,
            } => {
                write!(f, "struct")?;

                // HACK(eddyb) use the first name (in insertion order, i.e.
                // from the first invocation of `def_with_name` for this type)
                // even when this may not be correct - a better solution could
                // be to pick the shortest name (which could work well when
                // newtypes are involved).
                let first_name = {
                    let type_names = self.cx.type_cache.type_names.borrow();
                    type_names
                        .get(&self.id)
                        .and_then(|names| names.iter().next().copied())
                };

                if let Some(name) = first_name {
                    write!(f, " {}", name)?;
                }

                f.write_str(" { ")?;
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
            SpirvType::Pointer { pointee } => {
                f.write_str("*")?;
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
            SpirvType::Image {
                sampled_type,
                dim,
                depth,
                arrayed,
                multisampled,
                sampled,
                image_format,
                access_qualifier,
            } => f
                .debug_struct("Image")
                .field("sampled_type", &self.cx.debug_type(sampled_type))
                .field("dim", &dim)
                .field("depth", &depth)
                .field("arrayed", &arrayed)
                .field("multisampled", &multisampled)
                .field("sampled", &sampled)
                .field("image_format", &image_format)
                .field("access_qualifier", &access_qualifier)
                .finish(),
            SpirvType::Sampler => f.write_str("Sampler"),
            SpirvType::SampledImage { image_type } => f
                .debug_struct("SampledImage")
                .field("image_type", &self.cx.debug_type(image_type))
                .finish(),

            SpirvType::InterfaceBlock { inner_type } => {
                f.write_str("interface block { ")?;
                ty(self.cx, stack, f, inner_type)?;
                f.write_str(" }")
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
    /// Set of names for a type (only `SpirvType::Adt` currently).
    /// The same `OpType*` may have multiple names if it's e.g. a generic
    /// `struct` where the generic parameters result in the same field types.
    type_names: RefCell<FxHashMap<Word, IndexSet<TyLayoutNameKey<'tcx>>>>,
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
