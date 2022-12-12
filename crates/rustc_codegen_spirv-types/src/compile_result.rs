use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::fmt::Write;
use std::path::{Path, PathBuf};

#[derive(Debug, Serialize, Deserialize)]
#[serde(untagged)]
pub enum ModuleResult {
    SingleModule(PathBuf),
    MultiModule(BTreeMap<String, PathBuf>),
}

impl ModuleResult {
    pub fn unwrap_single(&self) -> &Path {
        match self {
            ModuleResult::SingleModule(result) => result,
            ModuleResult::MultiModule(_) => {
                panic!("called `ModuleResult::unwrap_single()` on a `MultiModule` result")
            }
        }
    }

    pub fn unwrap_multi(&self) -> &BTreeMap<String, PathBuf> {
        match self {
            ModuleResult::MultiModule(result) => result,
            ModuleResult::SingleModule(_) => {
                panic!("called `ModuleResult::unwrap_multi()` on a `SingleModule` result")
            }
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct CompileResult {
    pub entry_points: Vec<String>,
    pub module: ModuleResult,
}

impl CompileResult {
    pub fn codegen_entry_point_strings(&self) -> String {
        let trie = Trie::create_from(self.entry_points.iter().map(|x| x as &str));
        let mut builder = String::new();
        trie.emit(&mut builder, String::new(), 0);
        builder
    }
}

#[derive(Default)]
struct Trie<'a> {
    present: bool,
    children: BTreeMap<&'a str, Trie<'a>>,
}

impl<'a> Trie<'a> {
    fn create_from(entry_points: impl IntoIterator<Item = &'a str>) -> Self {
        let mut result = Trie::default();
        for entry in entry_points {
            result.insert(entry.split("::"));
        }
        result
    }

    fn insert(&mut self, mut sequence: impl Iterator<Item = &'a str>) {
        match sequence.next() {
            None => self.present = true,
            Some(next) => self.children.entry(next).or_default().insert(sequence),
        }
    }

    fn emit(&self, builder: &mut String, full_name: String, indent: usize) {
        let mut children = self.children.iter().collect::<Vec<_>>();
        children.sort_unstable_by(|(k1, _), (k2, _)| k1.cmp(k2));
        for (child_name, child) in children {
            let full_child_name = if full_name.is_empty() {
                (*child_name).to_string()
            } else {
                format!("{}::{}", full_name, child_name)
            };
            if child.present {
                assert!(child.children.is_empty());
                writeln!(
                    builder,
                    "{:indent$}#[allow(non_upper_case_globals)]",
                    "",
                    indent = indent * 4
                )
                .unwrap();
                writeln!(
                    builder,
                    "{:indent$}pub const {}: &str = \"{}\";",
                    "",
                    child_name,
                    full_child_name,
                    indent = indent * 4
                )
                .unwrap();
            } else {
                writeln!(
                    builder,
                    "{:indent$}pub mod {} {{",
                    "",
                    child_name,
                    indent = indent * 4
                )
                .unwrap();
                child.emit(builder, full_child_name, indent + 1);
                writeln!(builder, "{:indent$}}}", "", indent = indent * 4).unwrap();
            }
        }
    }
}

#[allow(non_upper_case_globals)]
pub const a: &str = "x::a";

#[cfg(test)]
mod test {
    use super::*;

    fn test<const N: usize>(arr: [&str; N], expected: &str) {
        let trie = Trie::create_from(IntoIterator::into_iter(arr));
        let mut builder = String::new();
        trie.emit(&mut builder, String::new(), 0);
        assert_eq!(builder, expected);
    }

    #[test]
    fn basic() {
        test(
            ["a", "b"],
            r#"#[allow(non_upper_case_globals)]
pub const a: &str = "a";
#[allow(non_upper_case_globals)]
pub const b: &str = "b";
"#,
        );
    }

    #[test]
    fn modules() {
        test(
            ["a", "x::a", "x::b", "x::y::a", "y::z::a"],
            r#"#[allow(non_upper_case_globals)]
pub const a: &str = "a";
pub mod x {
    #[allow(non_upper_case_globals)]
    pub const a: &str = "x::a";
    #[allow(non_upper_case_globals)]
    pub const b: &str = "x::b";
    pub mod y {
        #[allow(non_upper_case_globals)]
        pub const a: &str = "x::y::a";
    }
}
pub mod y {
    pub mod z {
        #[allow(non_upper_case_globals)]
        pub const a: &str = "y::z::a";
    }
}
"#,
        );
    }
}
