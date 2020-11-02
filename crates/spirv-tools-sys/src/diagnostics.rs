#[repr(C)]
pub struct Position {
    pub line: usize,
    pub column: usize,
    pub index: usize,
}

#[repr(C)]
pub struct Diagnostic {
    pub position: Position,
    pub error: *const std::os::raw::c_char,
    pub is_text_source: bool,
}

#[derive(Copy, Clone, PartialEq, Debug)]
#[repr(C)]
pub enum MessageLevel {
    /// Unrecoverable error due to environment.
    /// Will exit the program immediately. E.g.,
    /// out of memory.
    Fatal,
    /// Unrecoverable error due to SPIRV-Tools
    /// internals.
    /// Will exit the program immediately. E.g.,
    /// unimplemented feature.
    InternalError,
    /// Normal error due to user input.
    Error,
    /// Warning information.
    Warning,
    /// General information.
    Info,
    /// Debug information.
    Debug,
}

pub type MessageCallback = extern "C" fn(
    MessageLevel,                // level
    *const std::os::raw::c_char, // source
    *const Position,             // source position
    *const std::os::raw::c_char, // the actual message
    *mut std::ffi::c_void,       // context we use for mapping
);

extern "C" {
    /// Destroys a diagnostic object.  This is a no-op if diagnostic is a null
    /// pointer.
    #[link_name = "spvDiagnosticDestroy"]
    pub fn diagnostic_destroy(diag: *mut Diagnostic);
}
