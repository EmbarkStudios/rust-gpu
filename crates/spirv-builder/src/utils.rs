use std::path::PathBuf;

/// Currently cargo doesn't work well with UNC (`\\?\`) paths on Windows
/// so we have to remove it.
pub fn fix_canon_paths(path: &mut PathBuf) {
    if cfg!(windows) {
        let prefix = path
            .components()
            .next()
            .and_then(|p| p.as_os_str().to_str())
            .map(|s| s.replace(r"\\?\", ""))
            .unwrap();

        *path = vec![
            prefix.into(),
            path.components().skip(1).collect::<PathBuf>(),
        ]
        .into_iter()
        .collect();
    }
}
