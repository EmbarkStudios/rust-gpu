use std::{env, fs, io::Write as _, process::Command};

struct Group(bool);

impl Group {
    fn new(group: &str) -> Self {
        let is_gh = env::var_os("CI").is_some();
        if is_gh {
            println!("::group::{group}");
        } else {
            println!("{group}");
        }
        Self(is_gh)
    }
}

impl Drop for Group {
    fn drop(&mut self) {
        if self.0 {
            println!("::endgroup::");
        }
    }
}

fn main() {
    let (triple, release, td) = {
        let mut args = env::args().skip(1);
        let triple = args.next().expect("expected target triple");
        let release = args.next().expect("expected release tag name");
        let td = args.next().expect("expected output directory");

        (triple, release, td)
    };

    let compressed = {
        let _s = Group::new(&format!("downloading {triple} tarball"));
        let mut cmd = Command::new("curl");
        cmd.args(["-f", "-L"])
        .arg(format!("https://github.com/EmbarkStudios/spirv-tools-rs/releases/download/{release}/{triple}.tar.zst"))
        .stdout(std::process::Stdio::piped());

        let output = cmd
            .spawn()
            .expect("curl is not installed")
            .wait_with_output()
            .expect("failed to wait for curl");

        if !output.status.success() {
            panic!("failed to download tarball via curl");
        }

        output.stdout
    };

    let decoded = {
        let _s = Group::new(&format!("decompressing {triple} tarball"));
        // All archives are <8MiB decompressed
        let uncompressed = Vec::with_capacity(8 * 1024 * 1024);
        let mut decoder =
            zstd::stream::write::Decoder::new(uncompressed).expect("failed to create decoder");
        decoder
            .write_all(&compressed)
            .expect("failed to decompress");
        decoder.flush().expect("failed to flush decompress stream");

        decoder.into_inner()
    };

    {
        let _s = Group::new(&format!("untarring {triple} tarball"));
        {
            let mut tar = tar::Archive::new(std::io::Cursor::new(&decoded));

            if tar
                .entries()
                .expect("failed to retrieve entries")
                .filter(|ent| ent.is_ok())
                .count()
                == 0
            {
                panic!("no valid entries found in tarball");
            }
        }

        let mut tar = tar::Archive::new(std::io::Cursor::new(decoded));
        tar.unpack(&td).expect("failed to untar files");
    }

    if let Some(gh_path) = env::var_os("GITHUB_PATH") {
        let _s = Group::new(&format!("adding '{td}' to $GITHUB_PATH ({gh_path:?})"));

        // emulate >> for both empty and non-empty files
        let has_contents = fs::metadata(&gh_path).map_or(false, |md| md.len() > 0);

        let mut file = fs::OpenOptions::new()
            .append(true)
            .open(gh_path)
            .expect("failed to open $GITHUB_PATH");

        let td = if has_contents {
            format!("\n{td}\n")
        } else {
            td
        };

        file.write_all(td.as_bytes())
            .expect("failed to write to $GITHUB_PATH");
    }
}
