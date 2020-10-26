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
