mod core;
mod file_reader;
mod runner;

use clap::Parser;
use std::fs;
use std::path::PathBuf;

use file_reader::read_file;

fn main() {
    let args = VMOptions::parse();
    let file_bytes = fs::read(&args.file_path)
        .expect(format!("Failed to read file {}", &args.file_path.display()).as_str());
    let mut vm = read_file(file_bytes);
    vm.run();
}

const ABOUT: &str = "The Hyacinth Virtual Machine

This is the bytecode interpreter for Hyacinth, a language created by Robin Gieseking. For more
information on Hyacinth, check out the documentation at github.com/avian-lovelace/hyacinth#readme";

#[derive(Parser)]
#[command(about=ABOUT, long_about = None)]
struct VMOptions {
    file_path: PathBuf,
}
