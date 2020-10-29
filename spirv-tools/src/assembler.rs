use spirv_tools_sys::{assembler, shared};

#[derive(Copy, Clone, Default)]
pub struct AssemblerOptions {
    /// Numeric IDs in the binary will have the same values as in the source.
    /// Non-numeric IDs are allocated by filling in the gaps, starting with 1
    /// and going up.
    pub preserve_numeric_ids: bool,
}

impl Into<u32> for AssemblerOptions {
    fn into(self) -> u32 {
        // This is weird, the "none" is 1, so I'm not sure if that means having
        // it disables all other options or...?
        let mut res = 0; //assembler::BinaryOptions::None as u32;

        if self.preserve_numeric_ids {
            res |= assembler::BinaryOptions::PreserveNumberIds as u32;
        }

        res
    }
}

pub struct Assembler {
    inner: *mut shared::ToolContext,
}

impl Assembler {
    pub fn new(target_env: shared::TargetEnv) -> Self {
        Self {
            inner: unsafe { shared::context_create(target_env) },
        }
    }

    pub fn assemble(
        &self,
        text: &str,
        options: AssemblerOptions,
    ) -> Result<crate::shared::Binary, crate::error::Error> {
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
                            diagnostics: vec![crate::error::Diagnostic {
                                line: 0,
                                column: 0,
                                index: 0,
                                message: "spirv assemble indicated success but did not return a valid binary".to_owned(),
                                is_text: true,
                            }],
                        });
                    }

                    Ok(crate::shared::Binary::new(binary))
                }
                other => Err(crate::error::Error {
                    inner: other,
                    diagnostics: vec![diagnostic],
                }),
            }
        }
    }
}

impl Drop for Assembler {
    fn drop(&mut self) {
        unsafe {
            shared::context_destroy(self.inner);
        }
    }
}
