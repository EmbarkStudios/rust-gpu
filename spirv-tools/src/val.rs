use spirv_tools_sys::{shared, val};

pub struct ValidatorOptsBuilder {
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
    pub relax_block_layout: bool,
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
}

impl ValidatorOptsBuilder {
    pub fn into_opts(self) -> ValidatorOptions {
        self.into_opts_with_limits(std::iter::empty())
    }

    pub fn into_opts_with_limits(
        self,
        limits: impl Iterator<Item = (val::ValidatorLimits, u32)>,
    ) -> ValidatorOptions {
        unsafe {
            let vopts = val::validator_options_create();

            // AFAICT this is the only one that _may_ default to true, so
            // we always set it
            val::validator_options_set_relax_block_layout(vopts, self.relax_block_layout);

            if self.relax_store_struct {
                val::validator_options_set_relax_store_struct(vopts, self.relax_store_struct);
            }

            if self.relax_logical_pointer {
                val::validator_options_set_relax_logical_pointer(vopts, self.relax_logical_pointer);
            }

            if self.before_legalization {
                val::validator_options_set_before_legalization(vopts, self.before_legalization);
            }

            if self.uniform_buffer_standard_layout {
                val::validator_options_set_uniform_buffer_standard_layout(
                    vopts,
                    self.uniform_buffer_standard_layout,
                );
            }

            if self.scalar_block_layout {
                val::validator_options_set_scalar_block_layout(vopts, self.scalar_block_layout);
            }

            if self.skip_block_layout {
                val::validator_options_set_skip_block_layout(vopts, self.skip_block_layout);
            }

            for (limit, val) in limits {
                val::validator_options_set_limit(vopts, limit, val);
            }

            ValidatorOptions { inner: vopts }
        }
    }
}

pub struct ValidatorOptions {
    inner: *mut val::ValidatorOptions,
}

impl Default for ValidatorOptions {
    fn default() -> Self {
        Self {
            inner: unsafe { val::validator_options_create() },
        }
    }
}

impl Drop for ValidatorOptions {
    fn drop(&mut self) {
        unsafe { val::validator_options_destroy(self.inner) }
    }
}

pub struct Validator {
    inner: *mut shared::ToolContext,
}

impl Validator {
    pub fn new(target_env: shared::TargetEnv) -> Self {
        Self {
            inner: unsafe { shared::context_create(target_env) },
        }
    }

    pub fn validate(
        &self,
        binary: &[u32],
        options: &ValidatorOptions,
    ) -> Result<(), crate::error::Error> {
        unsafe {
            let mut diagnostic = std::ptr::null_mut();
            let res = val::validate(
                self.inner,
                binary.as_ptr(),
                binary.len(),
                options.inner,
                &mut diagnostic,
            );

            use std::convert::TryFrom;
            let diagnostic = crate::error::Diagnostic::try_from(diagnostic).ok();

            match res {
                shared::SpirvResult::Success => Ok(()),
                other => Err(crate::error::Error {
                    inner: other,
                    diagnostic,
                }),
            }
        }
    }
}

impl Drop for Validator {
    fn drop(&mut self) {
        unsafe { shared::context_destroy(self.inner) }
    }
}
