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
        use crate::cmd::CmdError;

        let mut cmd = std::process::Command::new("spirv-as");
        cmd.arg("--target-env").arg(self.target_env.to_string());

        if options.preserve_numeric_ids {
            cmd.arg("--preserve-numeric-ids");
        }

        let temp_dir = tempfile::tempdir().map_err(CmdError::Io)?;
        let output_path = temp_dir.path().join("code.spv");
        cmd.arg("-o").arg(&output_path);

        // Input file
        let input_path = temp_dir.path().join("code.txt");
        std::fs::write(&input_path, text).map_err(CmdError::Io)?;
        cmd.arg(&input_path);

        let _cmd_output =
            crate::cmd::exec(cmd, Some(text.as_bytes()), crate::cmd::Output::Ignore)?;

        let binary = std::fs::read(&output_path).map_err(CmdError::Io)?;

        use std::convert::TryFrom;
        crate::binary::Binary::try_from(binary)
    }
}

impl Default for ToolAssembler {
    fn default() -> Self {
        Self::with_env(crate::TargetEnv::default())
    }
}
