use crate::error::Message;
use std::process::{Command, Stdio};

pub enum CmdError {
    /// The binary failed to spawn, probably because it's not installed
    /// or not in PATH
    BinaryNotFound(std::io::Error),
    /// An I/O error occurred accessing the process' pipes
    Io(std::io::Error),
    /// The binary ran, but returned a non-zero exit code and (hopefully)
    /// diagnostics
    ToolErrors {
        exit_code: i32,
        /// Messages that were parsed from the output
        messages: Vec<Message>,
    },
}

impl From<CmdError> for crate::error::Error {
    fn from(ce: CmdError) -> Self {
        use crate::SpirvResult;

        match ce {
            CmdError::BinaryNotFound(e) => Self {
                inner: SpirvResult::Unsupported,
                diagnostic: Some(format!("failed spawn executable: {}", e).into()),
            },
            CmdError::Io(e) => Self {
                inner: SpirvResult::EndOfStream,
                diagnostic: Some(
                    format!("i/o error occurred communicating with executable: {}", e).into(),
                ),
            },
            CmdError::ToolErrors {
                exit_code,
                messages,
            } => {
                // The C API just puts the last message as the diagnostic, so just do the
                // same for now
                let diagnostic = messages
                    .into_iter()
                    .last()
                    .map(crate::error::Diagnostic::from)
                    .unwrap_or_else(|| {
                        crate::error::Diagnostic::from(format!(
                            "tool exited with code {} and no output",
                            exit_code
                        ))
                    });

                Self {
                    inner: SpirvResult::InternalError, // this isn't really correct
                    diagnostic: Some(diagnostic),
                }
            }
        }
    }
}

pub struct CmdOutput {
    /// The output the command is actually supposed to give back
    pub binary: Vec<u8>,
    /// Warning or Info level diagnostics that were gathered during execution
    pub messages: Vec<Message>,
}

#[derive(PartialEq, Copy, Clone)]
pub enum Output {
    /// Doesn't try to read stdout for tool output (other than diagnostics)
    Ignore,
    /// Attempts to retrieve the tool's output from stdout
    Retrieve,
}

pub fn exec(
    mut cmd: Command,
    input: Option<&[u8]>,
    retrieve_output: Output,
) -> Result<CmdOutput, CmdError> {
    cmd.stdout(Stdio::piped()).stderr(Stdio::piped());

    // Create a temp dir for the input and/or output of the tool
    let temp_dir = tempfile::tempdir().map_err(CmdError::Io)?;

    // Output
    let output_path = temp_dir.path().join("output");
    if retrieve_output == Output::Retrieve {
        cmd.arg("-o").arg(&output_path);
    }

    // Input
    if let Some(input) = input {
        let input_path = temp_dir.path().join("input");
        std::fs::write(&input_path, input).map_err(CmdError::Io)?;

        cmd.arg(&input_path);
    }

    let child = cmd.spawn().map_err(CmdError::BinaryNotFound)?;

    let output = child.wait_with_output().map_err(CmdError::Io)?;

    let code = match output.status.code() {
        Some(code) => code,
        None => {
            #[cfg(unix)]
            let message = {
                use std::os::unix::process::ExitStatusExt;
                format!(
                    "process terminated by signal: {}",
                    output.status.signal().unwrap_or(666)
                )
            };
            #[cfg(not(unix))]
            let message = "process ended in an unknown state".to_owned();

            return Err(CmdError::ToolErrors {
                exit_code: -1,
                messages: vec![Message::fatal(message)],
            });
        }
    };

    // stderr should only ever contain error+ level diagnostics
    if code != 0 {
        let messages: Vec<_> = match String::from_utf8(output.stderr) {
            Ok(errors) => errors
                .lines()
                .filter_map(|line| crate::error::Message::parse(line))
                .collect(),
            Err(e) => vec![Message::fatal(format!(
                "unable to read stderr ({}) but process exited with code {}",
                e, code
            ))],
        };

        return Err(CmdError::ToolErrors {
            exit_code: code,
            messages,
        });
    }

    fn split(haystack: &[u8], needle: u8) -> impl Iterator<Item = &[u8]> {
        struct Split<'a> {
            haystack: &'a [u8],
            needle: u8,
        }

        impl<'a> Iterator for Split<'a> {
            type Item = &'a [u8];

            fn next(&mut self) -> Option<&'a [u8]> {
                if self.haystack.is_empty() {
                    return None;
                }
                let (ret, remaining) = match memchr::memchr(self.needle, self.haystack) {
                    Some(pos) => (&self.haystack[..pos], &self.haystack[pos + 1..]),
                    None => (self.haystack, &[][..]),
                };
                self.haystack = remaining;
                Some(ret)
            }
        }

        Split { haystack, needle }
    }

    let binary = match retrieve_output {
        Output::Retrieve => std::fs::read(&output_path).map_err(CmdError::Io)?,
        Output::Ignore => Vec::new(),
    };

    // Since we are retrieving the results via stdout, but it can also contain
    // diagnostic messages, we need to be careful
    let mut messages = Vec::new();

    for line in split(&output.stdout, b'\n') {
        if let Ok(s) = std::str::from_utf8(line) {
            if let Some(msg) = crate::error::Message::parse(s) {
                messages.push(msg);
                continue;
            }
        }

        break;
    }

    Ok(CmdOutput { binary, messages })
}
