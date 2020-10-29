pub struct ToolAssembler {
    target_env: crate::TargetEnv,
}

use super::Assembler;

impl Assembler for ToolAssembler {
    fn with_env(target_env: crate::TargetEnv) -> Self {
        Self { target_env }
    }

    fn assemble(
        &self,
        text: &str,
        options: super::AssemblerOptions,
    ) -> Result<crate::binary::Binary, crate::error::Error> {
        let mut cmd = std::process::Command::new("spirv-as");
        cmd.arg("--target-env").arg(self.target_env.to_string());

        if options.preserve_numeric_ids {
            cmd.arg("--preserve-numeric-ids");
        }

        let cmd_output =
            crate::cmd::exec(cmd, Some(text.as_bytes()), crate::cmd::Output::Retrieve)?;

        use std::convert::TryFrom;
        crate::binary::Binary::try_from(cmd_output.binary)
    }
}

impl Default for ToolAssembler {
    fn default() -> Self {
        Self::with_env(crate::TargetEnv::default())
    }
}
