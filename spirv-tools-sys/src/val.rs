use crate::shared;

#[repr(C)]
pub struct ValidatorOptions {
    _unused: [u8; 0],
}

#[derive(Copy, Clone, Debug)]
#[repr(C)]
pub enum ValidatorLimits {
    MaxStructMembers,
    MaxStructDepth,
    MaxLocalVariables,
    MaxGlobalVariables,
    MaxSwitchBranches,
    MaxFunctionArgs,
    MaxControlFlowNestingDepth,
    MaxAccessChainIndexes,
    MaxIdBound,
}

extern "C" {
    /// Validates a raw SPIR-V binary for correctness. Any errors will be written
    /// into *diagnostic if diagnostic is non-null, otherwise the context's message
    /// consumer will be used.
    #[link_name = "spvValidateBinary"]
    pub fn validate(
        tool: *const shared::ToolContext,
        binary: *const u32,
        size: usize,
        options: *const ValidatorOptions,
        diagnostic: *mut *mut crate::diagnostics::Diagnostic,
    ) -> crate::shared::SpirvResult;

    /// Creates a Validator options object with default options. Returns a valid
    /// options object. The object remains valid until it is passed into
    /// spvValidatorOptionsDestroy.
    #[link_name = "spvValidatorOptionsCreate"]
    pub fn validator_options_create() -> *mut ValidatorOptions;

    /// Destroys the given Validator options object.
    #[link_name = "spvValidatorOptionsDestroy"]
    pub fn validator_options_destroy(opts: *mut ValidatorOptions);

    /// Records the maximum Universal Limit that is considered valid in the given
    /// Validator options object. <options> argument must be a valid options object.
    #[link_name = "spvValidatorOptionsSetUniversalLimit"]
    pub fn validator_options_set_limit(
        opts: *mut ValidatorOptions,
        limit_type: ValidatorLimits,
        limit: u32,
    );

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
    #[link_name = "spvValidatorOptionsSetRelaxStoreStruct"]
    pub fn validator_options_set_relax_store_struct(opts: *mut ValidatorOptions, toggle: bool);

    /// Records whether or not the validator should relax the rules on pointer usage
    /// in logical addressing mode.
    ///
    /// When relaxed, it will allow the following usage cases of pointers:
    /// 1) OpVariable allocating an object whose type is a pointer type
    /// 2) OpReturnValue returning a pointer value
    #[link_name = "spvValidatorOptionsSetRelaxLogicalPointer"]
    pub fn validator_options_set_relax_logical_pointer(opts: *mut ValidatorOptions, toggle: bool);

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
    #[link_name = "spvValidatorOptionsSetBeforeHlslLegalization"]
    pub fn validator_options_set_before_legalization(opts: *mut ValidatorOptions, toggle: bool);

    /// Records whether the validator should use "relaxed" block layout rules.
    /// Relaxed layout rules are described by Vulkan extension
    /// VK_KHR_relaxed_block_layout, and they affect uniform blocks, storage blocks,
    /// and push constants.
    ///
    /// This is enabled by default when targeting Vulkan 1.1 or later.
    /// Relaxed layout is more permissive than the default rules in Vulkan 1.0.
    #[link_name = "spvValidatorOptionsSetRelaxBlockLayout"]
    pub fn validator_options_set_relax_block_layout(opts: *mut ValidatorOptions, toggle: bool);

    /// Records whether the validator should use standard block layout rules for
    /// uniform blocks.
    #[link_name = "spvValidatorOptionsSetUniformBufferStandardLayout"]
    pub fn validator_options_set_uniform_buffer_standard_layout(
        opts: *mut ValidatorOptions,
        toggle: bool,
    );

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
    #[link_name = "spvValidatorOptionsSetScalarBlockLayout"]
    pub fn validator_options_set_scalar_block_layout(opts: *mut ValidatorOptions, toggle: bool);

    /// Records whether or not the validator should skip validating standard
    /// uniform/storage block layout.
    #[link_name = "spvValidatorOptionsSetSkipBlockLayout"]
    pub fn validator_options_set_skip_block_layout(opts: *mut ValidatorOptions, toggle: bool);
}
