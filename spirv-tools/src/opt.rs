#[cfg(not(feature = "use-installed"))]
mod compiled;
#[cfg(feature = "use-installed")]
mod tool;

#[cfg(not(feature = "use-installed"))]
pub use compiled::Optimizer;

#[cfg(feature = "use-installed")]
pub use tool::Optimizer;

pub use spirv_tools_sys::opt::Passes;

/// Options for specifying the behavior of the optimizer
#[derive(Default)]
pub struct Options {
    /// Records the validator options that should be passed to the validator,
    /// the validator will run with the options before optimizer.
    pub validator_options: Option<crate::val::ValidatorOptions>,
    /// Records the maximum possible value for the id bound.
    pub max_id_bound: Option<u32>,
    /// Records whether all bindings within the module should be preserved.
    pub preserve_bindings: bool,
    /// Records whether all specialization constants within the module
    /// should be preserved.
    pub preserve_spec_constants: bool,
}
