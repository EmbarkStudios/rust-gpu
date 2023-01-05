use std::{collections::HashSet, sync::mpsc::sync_channel};

use notify::{Event, RecursiveMode, Watcher};
use rustc_codegen_spirv_types::CompileResult;

use crate::{leaf_deps, SpirvBuilder, SpirvBuilderError};

impl SpirvBuilder {
    /// Watches the module for changes using [`notify`](https://crates.io/crates/notify),
    /// and rebuild it upon changes
    ///
    /// Returns the result of the first successful compilation, then calls
    /// `on_compilation_finishes` for each subsequent compilation.
    pub fn watch(
        mut self,
        mut on_compilation_finishes: impl FnMut(CompileResult) + Send + 'static,
    ) -> Result<CompileResult, SpirvBuilderError> {
        self.validate_running_conditions()?;
        if !matches!(self.print_metadata, crate::MetadataPrintout::None) {
            return Err(SpirvBuilderError::WatchWithPrintMetadata);
        }
        let metadata_result = crate::invoke_rustc(&self);
        // Load the dependencies of the thing
        let metadata_file = if let Ok(path) = metadata_result {
            path
        } else {
            let (tx, rx) = sync_channel(0);
            // Fall back to watching from the crate root if the inital compilation fails
            let mut watcher =
                notify::recommended_watcher(move |event: notify::Result<Event>| match event {
                    Ok(e) => match e.kind {
                        notify::EventKind::Access(_) => (),
                        notify::EventKind::Any
                        | notify::EventKind::Create(_)
                        | notify::EventKind::Modify(_)
                        | notify::EventKind::Remove(_)
                        | notify::EventKind::Other => {
                            let _ = tx.try_send(());
                        }
                    },
                    Err(e) => println!("notify error: {e:?}"),
                })
                .expect("Could create watcher");
            // This is likely to notice changes in the `target` dir, however, given that `cargo watch` doesn't seem to handle that,
            watcher
                .watch(&self.path_to_crate, RecursiveMode::Recursive)
                .expect("Could watch crate root");
            loop {
                rx.recv().expect("Watcher still alive");
                let metadata_file = crate::invoke_rustc(&self);
                if let Ok(f) = metadata_file {
                    break f;
                }
            }
        };
        let metadata = self.parse_metadata_file(&metadata_file)?;
        let first_result = metadata;

        let thread = std::thread::spawn(move || {
            let mut watched_paths = HashSet::new();
            let (tx, rx) = sync_channel(0);
            let mut watcher =
                notify::recommended_watcher(move |event: notify::Result<Event>| match event {
                    Ok(e) => match e.kind {
                        notify::EventKind::Access(_) => (),
                        notify::EventKind::Any
                        | notify::EventKind::Create(_)
                        | notify::EventKind::Modify(_)
                        | notify::EventKind::Remove(_)
                        | notify::EventKind::Other => {
                            let _ = tx.try_send(());
                        }
                    },
                    Err(e) => println!("notify error: {e:?}"),
                })
                .expect("Could create watcher");
            leaf_deps(&metadata_file, |it| {
                let path = it.to_path().unwrap();
                if watched_paths.insert(path.to_owned()) {
                    watcher
                        .watch(it.to_path().unwrap(), RecursiveMode::NonRecursive)
                        .expect("Cargo dependencies are valid files");
                }
            })
            .expect("Could read dependencies file");
            loop {
                rx.recv().expect("Watcher still alive");
                let metadata_result = crate::invoke_rustc(&self);
                if let Ok(file) = metadata_result {
                    let metadata = self
                        .parse_metadata_file(&file)
                        .expect("Metadata file is correct");

                    leaf_deps(&file, |it| {
                        let path = it.to_path().unwrap();
                        if watched_paths.insert(path.to_owned()) {
                            watcher
                                .watch(it.to_path().unwrap(), RecursiveMode::NonRecursive)
                                .expect("Cargo dependencies are valid files");
                        }
                    })
                    .expect("Could read dependencies file");

                    on_compilation_finishes(metadata);
                }
            }
        });
        std::mem::forget(thread);
        Ok(first_result)
    }
}
