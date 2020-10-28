use crate::error::Diagnostic;
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
        /// Diagnostics that were parsed from the output
        diagnostics: Vec<Diagnostic>,
    },
}

impl From<CmdError> for crate::error::Error {
    fn from(ce: CmdError) -> Self {}
}

pub struct CmdOutput {
    /// The output the command is actually supposed to give back
    pub binary: Vec<u8>,
    /// Warning or Info level diagnostics that were gathered during execution
    pub diagnostics: Vec<Diagnostic>,
}

#[derive(PartialEq, Copy, Clone)]
pub enum Output {
    /// Doesn't try to read stdout for tool output (other than diagnostics)
    Ignore,
    /// Attempts to retrieve the tool's output from stdout
    Retrieve,
}

pub fn exec(
    cmd: Command,
    input: Option<&[u8]>,
    retrieve_output: Output,
) -> Result<CmdOutput, CmdError> {
    if input.is_some() {
        cmd.stdin(Stdio::piped());
    }

    cmd.stdout(Stdio::piped()).stderr(Stdio::piped());

    let mut child = cmd.spawn().map_err(|e| CmdError::BinaryNotFound(e))?;

    if let Some(input) = input {
        use std::io::Write;

        child
            .stdin
            .take()
            .unwrap()
            .write_all(input)
            .map_err(|e| CmdError::Io(e))?;
    }

    let output = child.wait_with_output().map_err(|e| CmdError::Io(e))?;

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
                diagnostics: vec![Diagnostic {
                    line: 0,
                    column: 0,
                    index: 0,
                    message,
                    is_text: false,
                }],
            });
        }
    };

    // stderr should only ever contain error+ level diagnostics
    if code != 0 {
        let diagnostics: Vec<_> = match String::from_utf8(output.stderr) {
            Ok(errors) => errors
                .lines()
                .filter_map(|line| crate::error::Message::parse(line).map(Diagnostic::from))
                .collect(),
            Err(e) => vec![Diagnostic {
                line: 0,
                column: 0,
                index: 0,
                message: format!(
                    "unable to read stderr ({}) but process exited with code {}",
                    e, code
                ),
                is_text: false,
            }],
        };

        return Err(CmdError::ToolErrors {
            exit_code: code,
            diagnostics,
        });
    }

    fn split<'a>(haystack: &'a [u8], needle: u8) -> impl Iterator<Item = &'a [u8]> + 'a {
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

    let retrieve_output = retrieve_output == Output::Retrieve;

    // Since we are retrieving the results via stdout, but it can also contain
    // diagnostic messages, we need to be careful
    let mut diagnostics = Vec::new();
    let mut binary = Vec::with_capacity(if retrieve_output { 1024 } else { 0 });

    let mut iter = split(&output.stdout, b'\n');
    let mut maybe_diagnostic = true;
    for line in iter {
        if maybe_diagnostic {
            if let Some(s) = std::str::from_utf8(line).ok() {
                if let Some(msg) = crate::error::Message::parse(s) {
                    diagnostics.push(Diagnostic::from(msg));
                    continue;
                }
            }
        }

        if retrieve_output {
            binary.extend_from_slice(line);
        }
        maybe_diagnostic = false;
    }

    Ok(CmdOutput {
        binary,
        diagnostics,
    })
}
