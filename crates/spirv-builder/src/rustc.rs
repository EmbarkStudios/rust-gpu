use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::process::Command;

use raw_string::{RawStr, RawString};

pub fn cargo() -> Command {
    let mut cmd = Command::new(std::env::var("CARGO").unwrap_or_else(|_| String::from("cargo")));
    cmd.env_clear();
    cmd.env("PATH", std::env::var("PATH").unwrap());
    cmd
}

#[derive(serde::Deserialize)]
struct RustcOutput {
    reason: String,
    filenames: Option<Vec<String>>,
}

pub fn get_artifacts_from_output(out: &str) -> Vec<PathBuf> {
    let filenames = out
        .lines()
        .filter_map(|line| match serde_json::from_str::<RustcOutput>(line) {
            Ok(line) => Some(line),
            Err(_) => {
                // Pass through invalid lines
                println!("{}", line);
                None
            }
        })
        .filter(|line| line.reason == "compiler-artifact")
        .flat_map(|line| line.filenames.unwrap())
        .filter(|v| v.ends_with(".spv"))
        .map(Into::into)
        .collect::<Vec<_>>();

    if filenames.is_empty() {
        panic!("Crate had no .spv artifacts");
    }

    filenames
}

pub fn print_deps_of(artifact: &Path) {
    let deps_file = artifact.with_extension("d");
    let mut deps_map = HashMap::new();
    crate::depfile::read_deps_file(&deps_file, |item, deps| {
        deps_map.insert(item, deps);
        Ok(())
    })
    .expect("Could not read dep file");
    fn recurse(map: &HashMap<RawString, Vec<RawString>>, artifact: &RawStr) {
        match map.get(artifact) {
            Some(entries) => {
                for entry in entries {
                    recurse(map, entry)
                }
            }
            None => println!("cargo:rerun-if-changed={}", artifact),
        }
    }
    recurse(&deps_map, artifact.to_str().unwrap().into());
}
