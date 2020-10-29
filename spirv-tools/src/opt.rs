#[cfg(feature = "use-compiled-tools")]
pub mod compiled;
#[cfg(feature = "use-installed-tools")]
pub mod tool;

pub use spirv_tools_sys::opt::Passes;

/// Options for specifying the behavior of the optimizer
#[derive(Default, Clone)]
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

pub trait Optimizer {
    fn with_env(target_env: crate::TargetEnv) -> Self;

    fn optimize<MC: crate::error::MessageCallback>(
        &self,
        input: &[u32],
        msg_callback: &mut MC,
        options: Option<Options>,
    ) -> Result<crate::binary::Binary, crate::Error>;

    /// Register a single pass with the the optimizer.
    fn register_pass(&mut self, pass: Passes) -> &mut Self;
    /// Registers passes that attempt to improve performance of generated code.
    /// This sequence of passes is subject to constant review and will change
    /// from time to time.
    fn register_performance_passes(&mut self) -> &mut Self;
    /// Registers passes that attempt to improve the size of generated code.
    /// This sequence of passes is subject to constant review and will change
    /// from time to time.
    fn register_size_passes(&mut self) -> &mut Self;
    /// Registers passes that attempt to legalize the generated code.
    ///
    /// Note: this recipe is specially designed for legalizing SPIR-V. It should be
    /// used by compilers after translating HLSL source code literally. It should
    /// *not* be used by general workloads for performance or size improvement.
    ///
    /// This sequence of passes is subject to constant review and will change
    /// from time to time.
    fn register_hlsl_legalization_passes(&mut self) -> &mut Self;
}

pub fn create(te: Option<crate::TargetEnv>) -> impl Optimizer {
    let target_env = te.unwrap_or_default();

    #[cfg(feature = "use-compiled-tools")]
    {
        compiled::CompiledOptimizer::with_env(target_env)
    }

    #[cfg(all(feature = "use-installed-tools", not(feature = "use-compiled-tools")))]
    {
        tool::ToolOptimizer::with_env(target_env)
    }
}
