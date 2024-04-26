mod core;
mod file_reader;
mod runner;

use std::env;
use std::fs;

use file_reader::read_file;

fn main() {
    let args: Vec<String> = env::args().collect();
    let bytecode_file_path = args.get(1).expect("No file path provided");
    let file_bytes = fs::read(bytecode_file_path)
        .expect(format!("Failed to read file {}", bytecode_file_path).as_str());
    let mut vm = read_file(file_bytes);
    vm.run();
}
