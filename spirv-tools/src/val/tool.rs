
pub struct Validator {
    target_env: crate::TargetEnv,
}

impl Validator {
    pub fn new(target_env: crate::TargetEnv) -> Self {
        Self {
            target_env,
        }
    }

    pub fn validate(
        &self,
        binary: &[u32],
        options: Option<super::ValidatorOptions>,
    ) -> Result<(), crate::error::Error> {
        use std::process::Command;


        let mut cmd = Command::new("spirv-val");
        
        cmd.arg("--target-env")
            .arg(self.target_env.to_string());

        if let Some(opts) = options {

        }

        cmd.status()
    }
}

impl Drop for Validator {
    fn drop(&mut self) {
        unsafe { shared::context_destroy(self.inner) }
    }
}
