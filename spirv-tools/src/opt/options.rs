use spirv_tools_sys::opt;

pub struct Options {
    pub(crate) inner: *mut opt::OptimizerOptions,
}

impl Options {
    #[inline]
    pub fn new() -> Self {
        Self {
            inner: unsafe { opt::create_optimizer_options() },
        }
    }

    #[inline]
    pub fn run_validator(&mut self, toggle: bool) {
        unsafe { opt::optimizer_options_run_validator(self.inner, toggle) }
    }

    #[inline]
    pub fn set_max_id_bounds(&mut self, max: u32) {
        unsafe { opt::optimizer_options_set_max_id_bound(self.inner, max) }
    }

    #[inline]
    pub fn preserve_bindings(&mut self, toggle: bool) {
        unsafe { opt::optimizer_options_preserve_bindings(self.inner, toggle) }
    }

    #[inline]
    pub fn preserve_spec_constants(&mut self, toggle: bool) {
        unsafe { opt::optimizer_options_preserve_spec_constants(self.inner, toggle) }
    }
}

impl Default for Options {
    fn default() -> Self {
        Self::new()
    }
}

impl Drop for Options {
    #[inline]
    fn drop(&mut self) {
        unsafe { opt::destroy_optimizer_options(self.inner) }
    }
}
