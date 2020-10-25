#[cfg(feature = "opt")]
pub mod opt;
#[cfg(feature = "val")]
pub mod val;

pub use spirv_tools_sys::shared;

pub mod error;

pub use error::Error;

/// Gets the default target environment, currently Universal_1_5
pub fn default_target_env() -> shared::TargetEnv {
    shared::TargetEnv::default()
}
