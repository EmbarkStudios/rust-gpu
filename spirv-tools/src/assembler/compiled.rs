use spirv_tools_sys::{assembler, shared};

pub struct CompiledAssembler {
    inner: *mut shared::ToolContext,
}

use super::Assembler;

impl Assembler for CompiledAssembler {
    fn with_env(target_env: crate::TargetEnv) -> Self {
        Self {
            inner: unsafe { shared::context_create(target_env) },
        }
    }

    fn assemble(
        &self,
        text: &str,
        options: super::AssemblerOptions,
    ) -> Result<crate::binary::Binary, crate::error::Error> {
        unsafe {
            let mut binary = std::ptr::null_mut();
            let mut diagnostic = std::ptr::null_mut();

            let res = assembler::assemble(
                self.inner,
                text.as_ptr() as *const _,
                text.len(),
                options.into(),
                &mut binary,
                &mut diagnostic,
            );

            // Always wrap diagnostic, it's fine if it's null
            use std::convert::TryFrom;
            let diagnostic = crate::error::Diagnostic::try_from(diagnostic).ok();

            match res {
                shared::SpirvResult::Success => {
                    if binary.is_null() {
                        return Err(crate::error::Error {
                            inner: shared::SpirvResult::InternalError,
                            diagnostic: Some("spirv assemble indicated success but did not return a valid binary".to_owned().into()),
                        });
                    }

                    let bin = crate::binary::external::ExternalBinary::new(binary);
                    Ok(crate::binary::Binary::External(bin))
                }
                other => Err(crate::error::Error {
                    inner: other,
                    diagnostic,
                }),
            }
        }
    }
}

impl Default for CompiledAssembler {
    fn default() -> Self {
        Self::with_env(crate::TargetEnv::default())
    }
}

impl Drop for CompiledAssembler {
    fn drop(&mut self) {
        unsafe {
            shared::context_destroy(self.inner);
        }
    }
}
