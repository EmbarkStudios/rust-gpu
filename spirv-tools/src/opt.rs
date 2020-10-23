mod options;

use spirv_tools_sys::opt;

pub use opt::{Passes, RunResult};
pub use options::Options;

pub trait OptimizeCallback {
    fn optimized(&mut self, data: &[u32]);
}

impl<F> OptimizeCallback for F
where
    F: FnMut(&[u32]),
{
    fn optimized(&mut self, data: &[u32]) {
        self(data)
    }
}

pub struct Optimizer {
    inner: *mut opt::Optimizer,
}

impl Optimizer {
    #[inline]
    pub fn new(target: spirv_tools_sys::shared::TargetEnv) -> Self {
        Self {
            inner: unsafe { opt::optimizer_create(target) },
        }
    }

    #[inline]
    pub fn optimize<F: OptimizeCallback>(
        &self,
        input: &[u32],
        oc: &mut F,
        options: Option<&Options>,
    ) -> Result<(), RunResult> {
        unsafe {
            struct Ctx<'a> {
                cb: &'a mut dyn OptimizeCallback,
            }

            let mut ctx = Ctx { cb: oc };

            let cb_ctx: *mut std::ffi::c_void = std::mem::transmute(&mut ctx);

            extern "C" fn callback(data: *const u32, len: usize, ctx: *mut std::ffi::c_void) {
                unsafe {
                    let optimized_binary = std::slice::from_raw_parts(data, len);
                    let ctx: &mut Ctx<'_> = &mut *(ctx as *mut Ctx);

                    ctx.cb.optimized(optimized_binary);
                }
            }

            let options = match options {
                Some(opts) => opts.inner,
                None => std::ptr::null(),
            };

            let result = opt::optimizer_run(
                self.inner,
                input.as_ptr(),
                input.len(),
                callback,
                cb_ctx,
                options,
            );

            if result != opt::RunResult::OptimizerSucceeded {
                return Err(result);
            }
        }

        Ok(())
    }

    #[inline]
    pub fn optimize_to_buffer(
        &self,
        input: &[u32],
        options: Option<&Options>,
    ) -> Result<Vec<u32>, RunResult> {
        let mut buffer = None;
        self.optimize(
            input,
            &mut |data: &[u32]| {
                buffer = Some(data.to_vec());
            },
            options,
        )?;

        buffer.ok_or(RunResult::OptimizerFailed)
    }

    /// Register a single pass with the the optimizer.
    #[inline]
    pub fn register_pass(&mut self, pass: Passes) -> &mut Self {
        unsafe { opt::optimizer_register_pass(self.inner, pass) }
        self
    }

    /// Registers passes that attempt to improve performance of generated code.
    /// This sequence of passes is subject to constant review and will change
    /// from time to time.
    #[inline]
    pub fn register_performance_passes(&mut self) -> &mut Self {
        unsafe { opt::optimizer_register_performance_passes(self.inner) }
        self
    }

    /// Registers passes that attempt to improve the size of generated code.
    /// This sequence of passes is subject to constant review and will change
    /// from time to time.
    #[inline]
    pub fn register_size_passes(&mut self) -> &mut Self {
        unsafe { opt::optimizer_register_size_passes(self.inner) }
        self
    }

    /// Registers passes that have been prescribed for converting from Vulkan to
    /// WebGPU. This sequence of passes is subject to constant review and will
    /// change from time to time.
    #[inline]
    pub fn register_vulkan_to_webgpu_passes(&mut self) -> &mut Self {
        unsafe { opt::optimizer_register_vulkan_to_webgpu_passes(self.inner) }
        self
    }

    /// Registers passes that have been prescribed for converting from WebGPU to
    /// Vulkan. This sequence of passes is subject to constant review and will
    /// change from time to time.
    #[inline]
    pub fn register_webgpu_to_vulkan_passes(&mut self) -> &mut Self {
        unsafe { opt::optimizer_register_webgpu_to_vulkan_passes(self.inner) }
        self
    }

    /// Registers passes that attempt to legalize the generated code.
    ///
    /// Note: this recipe is specially designed for legalizing SPIR-V. It should be
    /// used by compilers after translating HLSL source code literally. It should
    /// *not* be used by general workloads for performance or size improvement.
    ///
    /// This sequence of passes is subject to constant review and will change
    /// from time to time.
    #[inline]
    pub fn register_legalization_passes(&mut self) -> &mut Self {
        unsafe { opt::optimizer_register_legalization_passes(self.inner) }
        self
    }
}

impl Drop for Optimizer {
    #[inline]
    fn drop(&mut self) {
        unsafe { opt::optimizer_destroy(self.inner) }
    }
}
