/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{exit, Command};
use std::sync::LazyLock;
use walkdir::WalkDir;

#[cfg(feature = "gecko")]
mod build_gecko;

#[cfg(not(feature = "gecko"))]
mod build_gecko {
    pub fn generate() {}
}

pub static PYTHON: LazyLock<String> = LazyLock::new(|| {
    env::var("PYTHON3").ok().unwrap_or_else(|| {
        let candidates = if cfg!(windows) {
            ["python.exe"]
        } else {
            ["python3"]
        };
        for &name in &candidates {
            if Command::new(name)
                .arg("--version")
                .output()
                .ok()
                .map_or(false, |out| out.status.success())
            {
                return name.to_owned();
            }
        }
        panic!(
            "Can't find python (tried {})! Try fixing PATH or setting the PYTHON3 env var",
            candidates.join(", ")
        )
    })
});

fn generate_properties(engine: &str) {
    for entry in WalkDir::new("properties") {
        let entry = entry.unwrap();
        match entry.path().extension().and_then(|e| e.to_str()) {
            Some("mako") | Some("rs") | Some("py") | Some("zip") | Some("toml") => {
                println!("cargo:rerun-if-changed={}", entry.path().display());
            },
            _ => {},
        }
    }

    let script = Path::new(&env::var_os("CARGO_MANIFEST_DIR").unwrap())
        .join("properties")
        .join("build.py");

    let status = Command::new(&*PYTHON)
        // `cargo publish` isn't happy with the `__pycache__` files that are created
        // when we run the property generator.
        //
        // TODO(mrobinson): Is this happening because of how we run this script? It
        // would be better to ensure are just placed in the output directory.
        .env("PYTHONDONTWRITEBYTECODE", "1")
        .arg(&script)
        .arg(engine)
        .arg("style-crate")
        .status()
        .unwrap();
    if !status.success() {
        exit(1)
    }
}

fn copy_prebuilt_servo_properties() {
    let manifest_dir =
        PathBuf::from(env::var_os("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR must be set"));
    let out_dir = PathBuf::from(env::var_os("OUT_DIR").expect("OUT_DIR must be set"));
    let source_dir = manifest_dir.join("properties/generated/servo");

    for filename in [
        "properties.rs",
        "css-properties.html",
        "css-properties.json",
    ] {
        let source = source_dir.join(filename);
        let destination = out_dir.join(filename);
        println!("cargo:rerun-if-changed={}", source.display());
        fs::copy(&source, &destination).unwrap_or_else(|error| {
            panic!(
                "failed to copy {} to {}: {error}",
                source.display(),
                destination.display()
            )
        });
    }

    // Keep the generated rustdoc property inventory in the same place as the
    // upstream generator. OUT_DIR is target/<profile>/build/stylo-*/out.
    let doc_dir = out_dir.join("../../../../doc/stylo");
    fs::create_dir_all(&doc_dir)
        .unwrap_or_else(|error| panic!("failed to create {}: {error}", doc_dir.display()));
    for filename in ["css-properties.html", "css-properties.json"] {
        let source = source_dir.join(filename);
        let destination = doc_dir.join(filename);
        fs::copy(&source, &destination).unwrap_or_else(|error| {
            panic!(
                "failed to copy {} to {}: {error}",
                source.display(),
                destination.display()
            )
        });
    }
}

fn main() {
    let gecko = cfg!(feature = "gecko");
    let servo = cfg!(feature = "servo");
    let engine = match (gecko, servo) {
        (true, false) => "gecko",
        (false, true) => "servo",
        _ => panic!(
            "\n\n\
             The style crate requires enabling one of its 'servo' or 'gecko' feature flags. \
             \n\n"
        ),
    };
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:out_dir={}", env::var("OUT_DIR").unwrap());
    if servo {
        copy_prebuilt_servo_properties();
    } else {
        generate_properties(engine);
    }
    build_gecko::generate();
}
