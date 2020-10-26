use spirv_tools_sys::{diagnostics, shared};

pub use diagnostics::MessageLevel;

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

impl std::convert::TryFrom<*mut diagnostics::Diagnostic> for Diagnostic {
    type Error = shared::SpirvResult;

    #[allow(clippy::not_unsafe_ptr_arg_deref)]
    fn try_from(diag: *mut diagnostics::Diagnostic) -> Result<Self, Self::Error> {
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

            diagnostics::diagnostic_destroy(diag);
            Ok(res)
        }
    }
}

pub struct Message<'a> {
    pub level: MessageLevel,
    pub source: std::borrow::Cow<'a, str>,
    pub line: usize,
    pub column: usize,
    pub index: usize,
    pub message: std::borrow::Cow<'a, str>,
}

impl<'a> Message<'a> {
    pub(crate) fn from_parts(
        level: MessageLevel,
        source: *const std::os::raw::c_char,
        source_pos: *const diagnostics::Position,
        msg: *const std::os::raw::c_char,
    ) -> Self {
        unsafe {
            let source = std::ffi::CStr::from_ptr(source).to_string_lossy();
            let message = std::ffi::CStr::from_ptr(msg).to_string_lossy();

            let (line, column, index) = if source_pos.is_null() {
                (0, 0, 0)
            } else {
                (
                    (*source_pos).line,
                    (*source_pos).column,
                    (*source_pos).index,
                )
            };

            Self {
                level,
                source,
                line,
                column,
                index,
                message,
            }
        }
    }

    pub(crate) fn parse(_s: &str) -> Self {
        unimplemented!()
    }
}

pub trait MessageCallback {
    fn on_message(&mut self, msg: Message<'_>);
}

impl<F> MessageCallback for F
where
    F: FnMut(Message<'_>),
{
    fn on_message(&mut self, msg: Message<'_>) {
        self(msg)
    }
}
