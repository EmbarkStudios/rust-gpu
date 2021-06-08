use std::{collections::HashSet, sync::mpsc::sync_channel};

use notify::{Event, RecursiveMode, Watcher};
use rustc_codegen_spirv::CompileResult;

use crate::{leaf_deps, SpirvBuilder, SpirvBuilderError};

impl SpirvBuilder {
    /// Watches the module for changes using [`notify`](https://crates.io/crates/notify).
    ///
    /// This is a blocking operation, wand should never return in the happy path
    pub fn watch(
        mut self,
        on_compilation_finishes: impl Fn(CompileResult),
    ) -> Result<(), SpirvBuilderError> {
        self.validate_running_conditions()?;
        if !matches!(self.print_metadata, crate::MetadataPrintout::None) {
            return Err(SpirvBuilderError::WatchWithPrintMetadata);
        }
        let metadata_result = crate::invoke_rustc(&self);
        // Load the dependencies of the thing
        let metadata_file = match metadata_result {
            Ok(path) => path,
            Err(_) => {
                let (tx, rx) = sync_channel(0);
                // Fall back to watching from the crate root if the inital compilation fails
                let mut watcher =
                    notify::immediate_watcher(move |event: notify::Result<Event>| match event {
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
                        Err(e) => println!("notify error: {:?}", e),
                    })
                    .expect("Could create watcher");
                // This is likely to notice changes in the `target` dir, however, given that `cargo watch` doesn't seem to handle that,
                watcher
                    .watch(&self.path_to_crate, RecursiveMode::Recursive)
                    .expect("Could watch crate root");
                loop {
                    rx.recv().expect("Watcher still alive");
                    let metadata_file = crate::invoke_rustc(&self);
                    match metadata_file {
                        Ok(f) => break f,
                        Err(_) => (), // Continue the loop until compilation succeeds
                    }
                }
            }
        };
        let metadata = self.parse_metadata_file(&metadata_file)?;
        on_compilation_finishes(metadata);
        let mut watched_paths = HashSet::new();
        let (tx, rx) = sync_channel(0);
        let mut watcher =
            notify::immediate_watcher(move |event: notify::Result<Event>| match event {
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
                Err(e) => println!("notify error: {:?}", e),
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
            match metadata_result {
                Ok(file) => {
                    // We can bubble this error up because it's an internal error  (e.g. rustc_codegen_spirv's version of CompileResult is somehow out of sync)
                    let metadata = self.parse_metadata_file(&file)?;

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
                Err(_) => (), // Continue until compilation succeeds
            }
        }
    }
}
