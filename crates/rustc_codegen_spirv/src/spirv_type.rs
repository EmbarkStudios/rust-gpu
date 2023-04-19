use crate::abi::{RecursivePointeeCache, TyLayoutNameKey};
use crate::builder_spirv::SpirvValue;
use crate::codegen_cx::CodegenCx;
use indexmap::IndexSet;
use rspirv::dr::Operand;
use rspirv::spirv::{Capability, Decoration, Dim, ImageFormat, StorageClass, Word};
use rustc_data_structures::fx::FxHashMap;
use rustc_middle::span_bug;
use rustc_span::def_id::DefId;
use rustc_span::{Span, Symbol};
use rustc_target::abi::{Align, Size};
use std::cell::RefCell;
use std::fmt;
use std::iter;
use std::sync::{LazyLock, Mutex};

/// Spir-v types are represented as simple Words, which are the `result_id` of instructions like
/// `OpTypeInteger`. Sometimes, however, we want to inspect one of these Words and ask questions
/// like "Is this an `OpTypeInteger`? How many bits does it have?". This struct holds all of that
/// information. All types that are emitted are registered in `CodegenCx`, so you can always look
/// up the definition of a `Word` via `cx.lookup_type`. Note that this type doesn't actually store
/// the `result_id` of the type declaration instruction, merely the contents.
//
// FIXME(eddyb) should `SpirvType`s be behind `&'tcx` from `tcx.arena.dropless`?
#[derive(Copy, Clone, Debug, PartialEq, Eq, Ord, PartialOrd, Hash)]
pub enum SpirvType<'tcx> {
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
        field_types: &'tcx [Word],
        field_offsets: &'tcx [Size],
        field_names: Option<&'tcx [Symbol]>,
    },
    Vector {
        element: Word,
        /// Note: vector count is literal.
        count: u32,
    },
    Matrix {
        element: Word,
        /// Note: matrix count is literal.
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
        arguments: &'tcx [Word],
    },
    Image {
        sampled_type: Word,
        dim: Dim,
        depth: u32,
        arrayed: u32,
        multisampled: u32,
        sampled: u32,
        image_format: ImageFormat,
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

    AccelerationStructureKhr,
    RayQueryKhr,
}

impl SpirvType<'_> {
    /// Note: `Builder::type_*` should be called *nowhere else* but here, to ensure
    /// `CodegenCx::type_defs` stays up-to-date
    pub fn def(self, def_span: Span, cx: &CodegenCx<'_>) -> Word {
        if let Some(cached) = cx.type_cache.get(&self) {
            return cached;
        }
        let id = Some(cx.emit_global().id());
        let result = match self {
            Self::Void => cx.emit_global().type_void_id(id),
            Self::Bool => cx.emit_global().type_bool_id(id),
            Self::Integer(width, signedness) => {
                let result = cx.emit_global().type_int_id(id, width, signedness as u32);
                let u_or_i = if signedness { "i" } else { "u" };
                match width {
                    8 if !cx.builder.has_capability(Capability::Int8) => cx.zombie_with_span(
                        result,
                        def_span,
                        &format!("`{u_or_i}8` without `OpCapability Int8`"),
                    ),
                    16 if !cx.builder.has_capability(Capability::Int16) => cx.zombie_with_span(
                        result,
                        def_span,
                        &format!("`{u_or_i}16` without `OpCapability Int16`"),
                    ),
                    64 if !cx.builder.has_capability(Capability::Int64) => cx.zombie_with_span(
                        result,
                        def_span,
                        &format!("`{u_or_i}64` without `OpCapability Int64`"),
                    ),
                    8 | 16 | 32 | 64 => {}
                    w => cx.zombie_with_span(
                        result,
                        def_span,
                        &format!("`{u_or_i}{w}` unsupported in SPIR-V"),
                    ),
                };
                result
            }
            Self::Float(width) => {
                let result = cx.emit_global().type_float_id(id, width);
                match width {
                    64 if !cx.builder.has_capability(Capability::Float64) => cx.zombie_with_span(
                        result,
                        def_span,
                        "`f64` without `OpCapability Float64`",
                    ),
                    32 | 64 => (),
                    other => cx.zombie_with_span(
                        result,
                        def_span,
                        &format!("`f{other}` unsupported in SPIR-V"),
                    ),
                };
                result
            }
            Self::Adt {
                def_id: _,
                align: _,
                size: _,
                field_types,
                field_offsets,
                field_names,
            } => {
                let mut emit = cx.emit_global();
                let result = emit.type_struct_id(id, field_types.iter().cloned());
                // The struct size is only used in our own sizeof_in_bits() (used in e.g. ArrayStride decoration)
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
                if let Some(field_names) = field_names {
                    for (index, field_name) in field_names.iter().enumerate() {
                        emit.member_name(result, index as u32, field_name.as_str());
                    }
                }
                result
            }
            Self::Vector { element, count } => cx.emit_global().type_vector_id(id, element, count),
            Self::Matrix { element, count } => cx.emit_global().type_matrix_id(id, element, count),
            Self::Array { element, count } => {
                // ArrayStride decoration wants in *bytes*
                let element_size = cx
                    .lookup_type(element)
                    .sizeof(cx)
                    .expect("Element of sized array must be sized")
                    .bytes();
                let mut emit = cx.emit_global();
                let result = emit.type_array_id(id, element, count.def_cx(cx));
                emit.decorate(
                    result,
                    Decoration::ArrayStride,
                    iter::once(Operand::LiteralInt32(element_size as u32)),
                );
                result
            }
            Self::RuntimeArray { element } => {
                let mut emit = cx.emit_global();
                let result = emit.type_runtime_array_id(id, element);
                // ArrayStride decoration wants in *bytes*
                let element_size = cx
                    .lookup_type(element)
                    .sizeof(cx)
                    .expect("Element of sized array must be sized")
                    .bytes();
                emit.decorate(
                    result,
                    Decoration::ArrayStride,
                    iter::once(Operand::LiteralInt32(element_size as u32)),
                );
                result
            }
            Self::Pointer { pointee } => {
                // NOTE(eddyb) we emit `StorageClass::Generic` here, but later
                // the linker will specialize the entire SPIR-V module to use
                // storage classes inferred from `OpVariable`s.
                let result = cx
                    .emit_global()
                    .type_pointer(id, StorageClass::Generic, pointee);
                // no pointers to functions
                if let SpirvType::Function { .. } = cx.lookup_type(pointee) {
                    // FIXME(eddyb) use the `SPV_INTEL_function_pointers` extension.
                    cx.zombie_with_span(result, def_span, "function pointer types are not allowed");
                }
                result
            }
            Self::Function {
                return_type,
                arguments,
            } => cx
                .emit_global()
                .type_function_id(id, return_type, arguments.iter().cloned()),
            Self::Image {
                sampled_type,
                dim,
                depth,
                arrayed,
                multisampled,
                sampled,
                image_format,
            } => cx.emit_global().type_image_id(
                id,
                sampled_type,
                dim,
                depth,
                arrayed,
                multisampled,
                sampled,
                image_format,
                None,
            ),
            Self::Sampler => cx.emit_global().type_sampler_id(id),
            Self::AccelerationStructureKhr => {
                cx.emit_global().type_acceleration_structure_khr_id(id)
            }
            Self::RayQueryKhr => cx.emit_global().type_ray_query_khr_id(id),
            Self::SampledImage { image_type } => {
                cx.emit_global().type_sampled_image_id(id, image_type)
            }

            Self::InterfaceBlock { inner_type } => {
                let mut emit = cx.emit_global();
                let result = emit.type_struct_id(id, iter::once(inner_type));
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
        cx.type_cache_def(result, self.tcx_arena_alloc_slices(cx), def_span);
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
                if let SpirvType::Function { .. } = cx.lookup_type(pointee) {
                    // FIXME(eddyb) use the `SPV_INTEL_function_pointers` extension.
                    cx.zombie_with_span(result, def_span, "function pointer types are not allowed");
                }
                result
            }
            ref other => cx
                .tcx
                .sess
                .fatal(format!("def_with_id invalid for type {other:?}")),
        };
        cx.type_cache_def(result, self.tcx_arena_alloc_slices(cx), def_span);
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

    pub fn sizeof(&self, cx: &CodegenCx<'_>) -> Option<Size> {
        let result = match *self {
            // Types that have a dynamic size, or no concept of size at all.
            Self::Void | Self::RuntimeArray { .. } | Self::Function { .. } => return None,

            Self::Bool => Size::from_bytes(1),
            Self::Integer(width, _) | Self::Float(width) => Size::from_bits(width),
            Self::Adt { size, .. } => size?,
            Self::Vector { element, count } => {
                cx.lookup_type(element).sizeof(cx)? * count.next_power_of_two() as u64
            }
            Self::Matrix { element, count } => cx.lookup_type(element).sizeof(cx)? * count as u64,
            Self::Array { element, count } => {
                cx.lookup_type(element).sizeof(cx)? * cx.builder.lookup_const_u64(count).unwrap()
            }
            Self::Pointer { .. } => cx.tcx.data_layout.pointer_size,
            Self::Image { .. }
            | Self::AccelerationStructureKhr
            | Self::RayQueryKhr
            | Self::Sampler
            | Self::SampledImage { .. } => Size::from_bytes(4),

            Self::InterfaceBlock { inner_type } => cx.lookup_type(inner_type).sizeof(cx)?,
        };
        Some(result)
    }

    pub fn alignof(&self, cx: &CodegenCx<'_>) -> Align {
        match *self {
            // Types that have no concept of size or alignment.
            Self::Void | Self::Function { .. } => Align::from_bytes(0).unwrap(),

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
            Self::Array { element, .. }
            | Self::RuntimeArray { element }
            | Self::Matrix { element, .. } => cx.lookup_type(element).alignof(cx),
            Self::Pointer { .. } => cx.tcx.data_layout.pointer_align.abi,
            Self::Image { .. }
            | Self::AccelerationStructureKhr
            | Self::RayQueryKhr
            | Self::Sampler
            | Self::SampledImage { .. } => Align::from_bytes(4).unwrap(),

            Self::InterfaceBlock { inner_type } => cx.lookup_type(inner_type).alignof(cx),
        }
    }

    /// Replace `&[T]` fields with `&'tcx [T]` ones produced by calling
    /// `tcx.arena.dropless.alloc_slice(...)` - this is done late for two reasons:
    /// 1. it avoids allocating in the arena when the cache would be hit anyway,
    ///    which would create "garbage" (as in, unreachable allocations)
    ///    (ideally these would also be interned, but that's even more refactors)
    /// 2. an empty slice is disallowed (as it's usually handled as a special
    ///    case elsewhere, e.g. `rustc`'s `ty::List` - sadly we can't use that)
    fn tcx_arena_alloc_slices<'tcx>(self, cx: &CodegenCx<'tcx>) -> SpirvType<'tcx> {
        fn arena_alloc_slice<'tcx, T: Copy>(cx: &CodegenCx<'tcx>, xs: &[T]) -> &'tcx [T] {
            if xs.is_empty() {
                &[]
            } else {
                cx.tcx.arena.dropless.alloc_slice(xs)
            }
        }

        match self {
            // FIXME(eddyb) these are all noop cases, could they be automated?
            SpirvType::Void => SpirvType::Void,
            SpirvType::Bool => SpirvType::Bool,
            SpirvType::Integer(width, signedness) => SpirvType::Integer(width, signedness),
            SpirvType::Float(width) => SpirvType::Float(width),
            SpirvType::Vector { element, count } => SpirvType::Vector { element, count },
            SpirvType::Matrix { element, count } => SpirvType::Matrix { element, count },
            SpirvType::Array { element, count } => SpirvType::Array { element, count },
            SpirvType::RuntimeArray { element } => SpirvType::RuntimeArray { element },
            SpirvType::Pointer { pointee } => SpirvType::Pointer { pointee },
            SpirvType::Image {
                sampled_type,
                dim,
                depth,
                arrayed,
                multisampled,
                sampled,
                image_format,
            } => SpirvType::Image {
                sampled_type,
                dim,
                depth,
                arrayed,
                multisampled,
                sampled,
                image_format,
            },
            SpirvType::Sampler => SpirvType::Sampler,
            SpirvType::SampledImage { image_type } => SpirvType::SampledImage { image_type },
            SpirvType::InterfaceBlock { inner_type } => SpirvType::InterfaceBlock { inner_type },
            SpirvType::AccelerationStructureKhr => SpirvType::AccelerationStructureKhr,
            SpirvType::RayQueryKhr => SpirvType::RayQueryKhr,

            // Only these variants have any slices to arena-allocate.
            SpirvType::Adt {
                def_id,
                align,
                size,
                field_types,
                field_offsets,
                field_names,
            } => SpirvType::Adt {
                def_id,
                align,
                size,
                field_types: arena_alloc_slice(cx, field_types),
                field_offsets: arena_alloc_slice(cx, field_offsets),
                field_names: field_names.map(|field_names| arena_alloc_slice(cx, field_names)),
            },
            SpirvType::Function {
                return_type,
                arguments,
            } => SpirvType::Function {
                return_type,
                arguments: arena_alloc_slice(cx, arguments),
            },
        }
    }
}

impl<'a> SpirvType<'a> {
    /// Use this if you want a pretty type printing that recursively prints the types within (e.g. struct fields)
    pub fn debug<'tcx>(self, id: Word, cx: &'a CodegenCx<'tcx>) -> SpirvTypePrinter<'a, 'tcx> {
        SpirvTypePrinter { ty: self, id, cx }
    }
}

pub struct SpirvTypePrinter<'a, 'tcx> {
    id: Word,
    ty: SpirvType<'a>,
    cx: &'a CodegenCx<'tcx>,
}

/// Types can be recursive, e.g. a struct can contain a pointer to itself. So, we need to keep
/// track of a stack of what types are currently being printed, to not infinitely loop.
/// Unfortunately, unlike `fmt::Display`, we can't easily pass down the "stack" of
/// currently-being-printed types, so we use a global static.
static DEBUG_STACK: LazyLock<Mutex<Vec<Word>>> = LazyLock::new(|| Mutex::new(Vec::new()));

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
                field_types,
                field_offsets,
                field_names,
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
                    .field("field_offsets", &field_offsets)
                    .field("field_names", &field_names)
                    .finish()
            }
            SpirvType::Vector { element, count } => f
                .debug_struct("Vector")
                .field("id", &self.id)
                .field("element", &self.cx.debug_type(element))
                .field("count", &count)
                .finish(),
            SpirvType::Matrix { element, count } => f
                .debug_struct("Matrix")
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
                arguments,
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
            SpirvType::AccelerationStructureKhr => f.debug_struct("AccelerationStructure").finish(),
            SpirvType::RayQueryKhr => f.debug_struct("RayQuery").finish(),
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
        fn ty(
            cx: &CodegenCx<'_>,
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
                write!(f, "{prefix}{width}")
            }
            SpirvType::Float(width) => write!(f, "f{width}"),
            SpirvType::Adt {
                def_id: _,
                align: _,
                size: _,
                field_types,
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
                    write!(f, " {name}")?;
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
                    write!(f, "{suffix}")?;
                }
                f.write_str(" }")
            }
            SpirvType::Vector { element, count } | SpirvType::Matrix { element, count } => {
                ty(self.cx, stack, f, element)?;
                write!(f, "x{count}")
            }
            SpirvType::Array { element, count } => {
                let len = self.cx.builder.lookup_const_u64(count);
                let len = len.expect("Array type has invalid count value");
                f.write_str("[")?;
                ty(self.cx, stack, f, element)?;
                write!(f, "; {len}]")
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
                arguments,
            } => {
                f.write_str("fn(")?;
                for (index, &arg) in arguments.iter().enumerate() {
                    let suffix = if index + 1 == arguments.len() {
                        ""
                    } else {
                        ", "
                    };
                    ty(self.cx, stack, f, arg)?;
                    write!(f, "{suffix}")?;
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
            } => f
                .debug_struct("Image")
                .field("sampled_type", &self.cx.debug_type(sampled_type))
                .field("dim", &dim)
                .field("depth", &depth)
                .field("arrayed", &arrayed)
                .field("multisampled", &multisampled)
                .field("sampled", &sampled)
                .field("image_format", &image_format)
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
            SpirvType::AccelerationStructureKhr => f.write_str("AccelerationStructureKhr"),
            SpirvType::RayQueryKhr => f.write_str("RayQuery"),
        }
    }
}

#[derive(Default)]
pub struct TypeCache<'tcx> {
    pub id_to_spirv_type: RefCell<FxHashMap<Word, SpirvType<'tcx>>>,
    pub spirv_type_to_id: RefCell<FxHashMap<SpirvType<'tcx>, Word>>,

    /// Recursive pointer breaking
    pub recursive_pointee_cache: RecursivePointeeCache<'tcx>,
    /// Set of names for a type (only `SpirvType::Adt` currently).
    /// The same `OpType*` may have multiple names if it's e.g. a generic
    /// `struct` where the generic parameters result in the same field types.
    type_names: RefCell<FxHashMap<Word, IndexSet<TyLayoutNameKey<'tcx>>>>,
}

impl<'tcx> TypeCache<'tcx> {
    fn get(&self, ty: &SpirvType<'_>) -> Option<Word> {
        self.spirv_type_to_id.borrow().get(ty).copied()
    }

    #[track_caller]
    pub fn lookup(&self, id: Word) -> SpirvType<'tcx> {
        *self
            .id_to_spirv_type
            .borrow()
            .get(&id)
            .expect("tried to lookup ID that wasn't a type, or has no definition")
    }
}

impl<'tcx> CodegenCx<'tcx> {
    fn type_cache_def(&self, id: Word, ty: SpirvType<'tcx>, def_span: Span) {
        if let Some(old_ty) = self.type_cache.id_to_spirv_type.borrow_mut().insert(id, ty) {
            span_bug!(
                def_span,
                "SPIR-V type with ID %{id} is being redefined\n\
                old type: {old_ty}\n\
                new type: {ty}",
                old_ty = old_ty.debug(id, self),
                ty = ty.debug(id, self)
            );
        }

        if let Some(old_id) = self.type_cache.spirv_type_to_id.borrow_mut().insert(ty, id) {
            span_bug!(
                def_span,
                "SPIR-V type is changing IDs (%{old_id} -> %{id}):\n\
                {ty}",
                ty = ty.debug(id, self)
            );
        }
    }
}
