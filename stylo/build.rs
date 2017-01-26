/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#[cfg(feature = "gecko")]
#[macro_use]
extern crate lazy_static;
#[cfg(feature = "bindgen")]
extern crate bindgen;
#[cfg(feature = "bindgen")]
extern crate regex;
extern crate walkdir;
extern crate phf_codegen;

use std::env;
use std::fs::File;
use std::io::{BufWriter, BufReader, BufRead, Write};
use std::path::Path;
use std::process::{Command, exit};
use walkdir::WalkDir;

#[cfg(feature = "gecko")]
mod build_gecko;

#[cfg(not(feature = "gecko"))]
mod build_gecko {
    pub fn generate() {}
}

#[cfg(windows)]
fn find_python() -> String {
    if Command::new("python2.7.exe").arg("--version").output().is_ok() {
        return "python2.7.exe".to_owned();
    }

    if Command::new("python27.exe").arg("--version").output().is_ok() {
        return "python27.exe".to_owned();
    }

    if Command::new("python.exe").arg("--version").output().is_ok() {
        return "python.exe".to_owned();
    }

    panic!(concat!("Can't find python (tried python2.7.exe, python27.exe, and python.exe)! ",
                   "Try fixing PATH or setting the PYTHON env var"));
}

#[cfg(not(windows))]
fn find_python() -> String {
    if Command::new("python2.7").arg("--version").output().unwrap().status.success() {
        "python2.7"
    } else {
        "python"
    }.to_owned()
}

fn generate_properties() {
    for entry in WalkDir::new("properties") {
        let entry = entry.unwrap();
        match entry.path().extension().and_then(|e| e.to_str()) {
            Some("mako") | Some("rs") | Some("py") | Some("zip") => {
                println!("cargo:rerun-if-changed={}", entry.path().display());
            }
            _ => {}
        }
    }

    let python = env::var("PYTHON").ok().unwrap_or_else(find_python);
    let script = Path::new(file!()).parent().unwrap().join("properties").join("build.py");
    let product = if cfg!(feature = "gecko") { "gecko" } else { "servo" };
    let status = Command::new(python)
        .arg(&script)
        .arg(product)
        .arg("style-crate")
        .arg(if cfg!(feature = "testing") { "testing" } else { "regular" })
        .status()
        .unwrap();
    if !status.success() {
        exit(1)
    }

    let path = Path::new(&env::var("OUT_DIR").unwrap()).join("static_ids.rs");
    let static_ids = Path::new(&env::var("OUT_DIR").unwrap()).join("static_ids.txt");
    let mut file = BufWriter::new(File::create(&path).unwrap());
    let static_ids = BufReader::new(File::open(&static_ids).unwrap());

    write!(&mut file, "static STATIC_IDS: ::phf::Map<&'static str, StaticId> = ").unwrap();
    let mut map = phf_codegen::Map::new();
    for result in static_ids.lines() {
        let line = result.unwrap();
        let mut split = line.split('\t');
        let key = split.next().unwrap().to_owned();
        let value = split.next().unwrap();
        map.entry(key, value);
    }
    map.build(&mut file).unwrap();
    write!(&mut file, ";\n").unwrap();
}

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    generate_properties();
    build_gecko::generate();
}
