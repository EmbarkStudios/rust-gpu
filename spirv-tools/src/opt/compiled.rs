use crate::error;
use spirv_tools_sys::opt;

pub struct Options {
    pub(crate) inner: *mut opt::OptimizerOptions,
}

impl From<super::Options> for Options {
    fn from(o: super::Options) -> Self {
        unsafe {
            let inner = opt::optimizer_options_create();

            if let Some(vopts) = o.validator_options {
                let vopts = crate::val::compiled::Options::from(vopts);

                opt::optimizer_options_run_validator(inner, true);

                // The validator options are copied, so it's fine to drop vopts
                // after this call
                opt::optimizer_options_set_validator_options(inner, vopts.inner);
            }

            if let Some(max_bound) = o.max_id_bound {
                opt::optimizer_options_set_max_id_bound(inner, max_bound);
            }

            if o.preserve_bindings {
                opt::optimizer_options_preserve_bindings(inner, true);
            }

            if o.preserve_spec_constants {
                opt::optimizer_options_preserve_spec_constants(inner, true);
            }

            Self { inner }
        }
    }
}

impl Drop for Options {
    #[inline]
    fn drop(&mut self) {
        unsafe { opt::optimizer_options_destroy(self.inner) }
    }
}

pub struct Optimizer {
    inner: *mut opt::Optimizer,
}

impl Optimizer {
    #[inline]
    pub fn new(target: crate::TargetEnv) -> Self {
        Self {
            inner: unsafe { opt::optimizer_create(target) },
        }
    }

    #[inline]
    pub fn optimize<MC: error::MessageCallback>(
        &self,
        input: &[u32],
        msg_callback: &mut MC,
        options: Option<super::Options>,
    ) -> Result<crate::shared::Binary, crate::Error> {
        unsafe {
            struct Ctx<'a> {
                cb: &'a mut dyn error::MessageCallback,
            }

            let mut ctx = Ctx { cb: msg_callback };

            let cb_ctx: *mut std::ffi::c_void = std::mem::transmute(&mut ctx);

            extern "C" fn callback(
                level: spirv_tools_sys::diagnostics::MessageLevel,
                source: *const std::os::raw::c_char,
                source_pos: *const spirv_tools_sys::diagnostics::Position,
                msg: *const std::os::raw::c_char,
                ctx: *mut std::ffi::c_void,
            ) {
                unsafe {
                    let ctx: &mut Ctx<'_> = &mut *(ctx as *mut Ctx);

                    let msg = error::Message::from_parts(level, source, source_pos, msg);

                    ctx.cb.on_message(msg);
                }
            }

            let mut binary = std::ptr::null_mut();

            let options = options.map(Options::from);

            let options = match options {
                Some(opts) => opts.inner,
                None => std::ptr::null(),
            };

            let res = opt::optimizer_run(
                self.inner,
                input.as_ptr(),
                input.len(),
                &mut binary,
                callback,
                cb_ctx,
                options,
            );

            match res {
                spirv_tools_sys::shared::SpirvResult::Success => {
                    if binary.is_null() {
                        return Err(error::Error {
                            inner: spirv_tools_sys::shared::SpirvResult::InternalError,
                            diagnostic: Some(crate::error::Diagnostic {
                                line: 0,
                                column: 0,
                                index: 0,
                                message: "spirv optimizer indicated success but did not return a valid binary".to_owned(),
                                is_text: false,
                            }),
                        });
                    }

                    Ok(crate::shared::Binary::new(binary))
                }
                other => Err(error::Error {
                    inner: other,
                    diagnostic: None,
                }),
            }
        }
    }

    /// Register a single pass with the the optimizer.
    #[inline]
    pub fn register_pass(&mut self, pass: super::Passes) -> &mut Self {
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
