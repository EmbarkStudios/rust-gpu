use spirv_tools_sys::{diagnostics, shared};

pub use diagnostics::MessageLevel;
pub use shared::SpirvResult;

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

#[cfg(feature = "use-compiled-tools")]
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

impl From<String> for Diagnostic {
    fn from(message: String) -> Self {
        Self {
            line: 0,
            column: 0,
            index: 0,
            is_text: false,
            message,
        }
    }
}

impl From<Message> for Diagnostic {
    fn from(msg: Message) -> Self {
        Self {
            line: msg.line,
            column: msg.column,
            index: msg.index,
            message: msg.message,
            is_text: false,
        }
    }
}

#[derive(Debug)]
pub struct Message {
    pub level: MessageLevel,
    pub source: Option<String>,
    pub line: usize,
    pub column: usize,
    pub index: usize,
    pub message: String,
}

impl Message {
    #[cfg(feature = "use-installed-tools")]
    pub(crate) fn fatal(message: String) -> Self {
        Self {
            level: MessageLevel::Fatal,
            source: None,
            line: 0,
            column: 0,
            index: 0,
            message,
        }
    }

    #[cfg(feature = "use-compiled-tools")]
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
                source: if source.is_empty() {
                    None
                } else {
                    Some(source.into_owned())
                },
                line,
                column,
                index,
                message: message.into_owned(),
            }
        }
    }

    #[cfg(feature = "use-installed-tools")]
    pub(crate) fn parse(s: &str) -> Option<Self> {
        s.find(": ")
            .and_then(|i| {
                let level = match &s[..i] {
                    "error" => MessageLevel::Error,
                    "warning" => MessageLevel::Warning,
                    "info" => MessageLevel::Info,
                    _ => return None,
                };

                Some((level, i))
            })
            .and_then(|(level, i)| {
                s[i + 7..]
                    .find(": ")
                    .and_then(|i2| {
                        s[i + 7..i + 7 + i2]
                            .parse::<usize>()
                            .ok()
                            .map(|index| (index, i2))
                    })
                    .map(|(index, i2)| (level, index, i + 7 + i2 + 2))
            })
            .map(|(level, index, last)| Self {
                level,
                index,
                message: s[last..].to_owned(),
                source: None,
                line: 0,
                column: 0,
            })
    }
}

pub trait MessageCallback {
    fn on_message(&mut self, msg: Message);
}

impl<F> MessageCallback for F
where
    F: FnMut(Message),
{
    fn on_message(&mut self, msg: Message) {
        self(msg)
    }
}
