use spirv_tools_sys::{shared, val};

pub struct Options {
    pub(crate) inner: *mut val::ValidatorOptions,
}

impl From<super::ValidatorOptions> for Options {
    fn from(vo: super::ValidatorOptions) -> Self {
        unsafe {
            let inner = val::validator_options_create();

            // This is AFAICT the only one that _can_ default to true based on our target
            // so we treat it differently
            if let Some(relax) = vo.relax_block_layout {
                val::validator_options_set_relax_block_layout(inner, relax);
            }

            if vo.relax_struct_store {
                val::validator_options_set_relax_store_struct(inner, true);
            }

            if vo.relax_logical_pointer {
                val::validator_options_set_relax_logical_pointer(inner, true);
            }

            if vo.before_legalization {
                val::validator_options_set_before_legalization(inner, true);
            }

            if vo.uniform_buffer_standard_layout {
                val::validator_options_set_uniform_buffer_standard_layout(inner, true);
            }

            if vo.scalar_block_layout {
                val::validator_options_set_scalar_block_layout(inner, true);
            }

            if vo.skip_block_layout {
                val::validator_options_set_skip_block_layout(inner, true);
            }

            for (limit, val) in vo.max_limits {
                val::validator_options_set_limit(inner, limit, val);
            }

            Self { inner }
        }
    }
}

impl Drop for Options {
    fn drop(&mut self) {
        unsafe { val::validator_options_destroy(self.inner) }
    }
}

pub struct CompiledValidator {
    inner: *mut shared::ToolContext,
}

use super::Validator;

impl Validator for CompiledValidator {
    fn with_env(target_env: crate::TargetEnv) -> Self {
        Self {
            inner: unsafe { shared::context_create(target_env) },
        }
    }

    fn validate(
        &self,
        binary: &[u32],
        options: Option<super::ValidatorOptions>,
    ) -> Result<(), crate::error::Error> {
        unsafe {
            let mut diagnostic = std::ptr::null_mut();

            let options = options.map(Options::from);

            let input = shared::Binary {
                code: binary.as_ptr(),
                size: binary.len(),
            };

            let res = match options {
                Some(opts) => {
                    val::validate_with_options(self.inner, opts.inner, &input, &mut diagnostic)
                }
                None => val::validate(self.inner, &input, &mut diagnostic),
            };

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

impl Default for CompiledValidator {
    fn default() -> Self {
        Self::with_env(crate::TargetEnv::default())
    }
}

impl Drop for CompiledValidator {
    fn drop(&mut self) {
        unsafe { shared::context_destroy(self.inner) }
    }
}
