#[cfg(not(feature = "use-installed"))]
pub(crate) mod compiled;
#[cfg(feature = "use-installed")]
mod tool;

#[cfg(not(feature = "use-installed"))]
pub use compiled::Validator;

#[cfg(feature = "use-installed")]
pub use tool::Optimizer;

#[derive(Default, Clone)]
pub struct ValidatorOptions {
    /// Record whether or not the validator should relax the rules on types for
    /// stores to structs.  When relaxed, it will allow a type mismatch as long as
    /// the types are structs with the same layout.  Two structs have the same layout
    /// if
    ///
    /// 1) the members of the structs are either the same type or are structs with
    /// same layout, and
    ///
    /// 2) the decorations that affect the memory layout are identical for both
    /// types.  Other decorations are not relevant.
    pub relax_store_struct: bool,
    /// Records whether or not the validator should relax the rules on pointer usage
    /// in logical addressing mode.
    ///
    /// When relaxed, it will allow the following usage cases of pointers:
    /// 1) OpVariable allocating an object whose type is a pointer type
    /// 2) OpReturnValue returning a pointer value
    pub relax_logical_pointer: bool,
    /// Records whether or not the validator should relax the rules because it is
    /// expected that the optimizations will make the code legal.
    ///
    /// When relaxed, it will allow the following:
    /// 1) It will allow relaxed logical pointers.  Setting this option will also
    ///    set that option.
    /// 2) Pointers that are pass as parameters to function calls do not have to
    ///    match the storage class of the formal parameter.
    /// 3) Pointers that are actaul parameters on function calls do not have to point
    ///    to the same type pointed as the formal parameter.  The types just need to
    ///    logically match.
    pub before_legalization: bool,
    /// Records whether the validator should use "relaxed" block layout rules.
    /// Relaxed layout rules are described by Vulkan extension
    /// VK_KHR_relaxed_block_layout, and they affect uniform blocks, storage blocks,
    /// and push constants.
    ///
    /// This is enabled by default when targeting Vulkan 1.1 or later.
    /// Relaxed layout is more permissive than the default rules in Vulkan 1.0.
    pub relax_block_layout: Option<bool>,
    /// Records whether the validator should use standard block layout rules for
    /// uniform blocks.
    pub uniform_buffer_standard_layout: bool,
    /// Records whether the validator should use "scalar" block layout rules.
    /// Scalar layout rules are more permissive than relaxed block layout.
    ///
    /// See Vulkan extnesion VK_EXT_scalar_block_layout.  The scalar alignment is
    /// defined as follows:
    /// - scalar alignment of a scalar is the scalar size
    /// - scalar alignment of a vector is the scalar alignment of its component
    /// - scalar alignment of a matrix is the scalar alignment of its component
    /// - scalar alignment of an array is the scalar alignment of its element
    /// - scalar alignment of a struct is the max scalar alignment among its
    ///   members
    ///
    /// For a struct in Uniform, StorageClass, or PushConstant:
    /// - a member Offset must be a multiple of the member's scalar alignment
    /// - ArrayStride or MatrixStride must be a multiple of the array or matrix
    ///   scalar alignment
    pub scalar_block_layout: bool,
    /// Records whether or not the validator should skip validating standard
    /// uniform/storage block layout.
    pub skip_block_layout: bool,
    /// Applies a maximum to one or more Universal limits
    pub max_limits: Vec<(spirv_tools_sys::val::ValidatorLimits, u32)>,
}
