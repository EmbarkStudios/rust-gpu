use std::path::PathBuf;

/// Defines the source to use for `rustc_codegen_spirv` and `sysroot`.
#[derive(Debug, Default)]
pub struct Source {
    /// Whether to attempt to compile the source provided by `kind`. If `false`
    /// expects the source to point to a prebuilt sysroot containing the
    /// `spirv-unknown-unknown` sysroot and codegen. Errors if `true`, and
    /// using `SourceKind::Environment`.
    pub compile_source: bool,
    pub kind: SourceKind,
}

impl Source {
    pub fn get(&self) -> eyre::Result<PathBuf> {
        if self.compile_source && self.kind == SourceKind::Environment {
            return Err(eyre::eyre!(
                "Invalid source configuration. Environment \
                                    can't use compile_source."
            ));
        }

        Ok(match &self.kind {
            SourceKind::Environment => PathBuf::from("spirv"),
            SourceKind::Path(path) => {
                std::fs::create_dir_all(path)?;
                let mut path = path
                    .canonicalize()
                    .map_err(|e| eyre::eyre!("Path: `{}`\n{}", path.display(), e))?;
                crate::utils::fix_canon_paths(&mut path);
                path
            }
            SourceKind::Git {
                repository,
                commitish,
                into,
            } => {
                std::fs::create_dir_all(&into)?;
                let repo = git2::build::RepoBuilder::new().clone(&repository, &into)?;

                if let Some(commitish) = commitish {
                    repo.set_head(commitish)?;
                }

                into
            }
        })
    }
}

/// Where the source of a component is located.
#[derive(Debug, Eq, PartialEq)]
pub enum SourceKind {
    /// Clones a git repository for the source.
    Git {
        /// The git repository.
        repository: String,
        /// The branch or commit to checkout Default: default branch of the
        /// repository.
        commitish: Option<String>,
        /// The directory to clone the repository into.
        into: PathBuf,
    },
    /// Uses the path provided for the source.
    Path(PathBuf),
    /// Assume that `rustc_codegen_spirv` and `spirv-unknown-unknown` are
    /// already available in the environment.
    Environment,
}

impl SourceKind {
    pub fn is_environment(&self) -> bool {
        *self == Self::Environment
    }
}

impl Default for SourceKind {
    fn default() -> Self {
        Self::Environment
    }
}
