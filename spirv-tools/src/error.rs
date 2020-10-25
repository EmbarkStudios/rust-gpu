use spirv_tools_sys::shared;

#[derive(Debug, PartialEq)]
pub struct Error {
    pub inner: shared::SpirvResult,
    pub diagnostic: Option<Diagnostic>,
}

use std::fmt;

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.diagnostic {
            Some(diag) => f.write_fmt(format_args!(
                "{}:{}:{} - {}",
                self.inner, diag.line, diag.column, diag.message
            )),
            None => f.write_fmt(format_args!("{}", self.inner)),
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        Some(&self.inner)
    }
}

#[derive(Debug, PartialEq)]
pub struct Diagnostic {
    pub line: usize,
    pub column: usize,
    pub index: usize,
    pub message: String,
    pub is_text: bool,
}

impl std::convert::TryFrom<*mut shared::Diagnostic> for Diagnostic {
    type Error = shared::SpirvResult;

    #[allow(clippy::not_unsafe_ptr_arg_deref)]
    fn try_from(diag: *mut shared::Diagnostic) -> Result<Self, Self::Error> {
        unsafe {
            if diag.is_null() {
                return Err(shared::SpirvResult::Success);
            }

            let message = std::ffi::CStr::from_ptr((*diag).error)
                .to_string_lossy()
                .to_string();

            let res = Self {
                line: (*diag).position.line,
                column: (*diag).position.column,
                index: (*diag).position.index,
                message,
                is_text: (*diag).is_text_source,
            };

            shared::diagnostic_destroy(diag);
            Ok(res)
        }
    }
}
