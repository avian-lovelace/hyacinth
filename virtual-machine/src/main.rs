use std::env;
use std::fs;
use std::process;

fn main() {
    let args: Vec<String> = env::args().collect();
    let bytecode_file_path = args.get(1).expect("No file path provided");
    let file_bytes = fs::read(bytecode_file_path)
        .expect(format!("Failed to read file {}", bytecode_file_path).as_str());
    let chunk = read_chunk(file_bytes);
    interpret(chunk).unwrap_or_else(|err| {
        eprintln!("{}", err);
        process::exit(1);
    });
}

fn read_chunk(bytes: Vec<u8>) -> Chunk {
    let mut f = FileState { bytes, index: 0 };
    let constants_length = f.read_int() as usize;
    let mut constants: Vec<Value> = Vec::new();
    for _ in (0..constants_length) {
        constants.push(f.read_int());
    }
    let code = f.read_rest();
    return Chunk { constants, code };
}

struct FileState {
    bytes: Vec<u8>,
    index: usize,
}

impl FileState {
    fn read_byte(&mut self) -> u8 {
        let byte = self.bytes[self.index];
        self.index = self.index + 1;
        return byte;
    }

    fn read_int(&mut self) -> i32 {
        let int = ((self.bytes[self.index] as i32) << 24)
            + ((self.bytes[self.index + 1] as i32) << 16)
            + ((self.bytes[self.index + 2] as i32) << 8)
            + (self.bytes[self.index + 3] as i32);
        self.index = self.index + 4;
        return int;
    }

    fn read_rest(&mut self) -> Vec<u8> {
        return self.bytes[self.index..].to_vec();
    }
}

fn interpret(chunk: Chunk) -> Result<(), String> {
    let mut vm = VM {
        chunk,
        ip: 0,
        stack: Vec::new(),
    };
    return run(&mut vm);
}

fn run(vm: &mut VM) -> Result<(), String> {
    loop {
        let instruction = vm.read_instruction();
        match instruction {
            Some(Instruction::Return) => break Ok(()),
            Some(Instruction::Print) => println!("{}", vm.pop()?),
            Some(Instruction::Constant(const_index)) => vm.push(vm.read_constant(const_index)?),
            Some(Instruction::Negate) => {
                let value = vm.pop()?;
                vm.push(-value);
            }
            Some(Instruction::Add) => {
                let value_2 = vm.pop()?;
                let value_1 = vm.pop()?;
                vm.push(value_1 + value_2);
            }
            Some(Instruction::Subtract) => {
                let value_2 = vm.pop()?;
                let value_1 = vm.pop()?;
                vm.push(value_1 - value_2);
            }
            Some(Instruction::Multiply) => {
                let value_2 = vm.pop()?;
                let value_1 = vm.pop()?;
                vm.push(value_1 * value_2);
            }
            Some(Instruction::Divide) => {
                let value_2 = vm.pop()?;
                let value_1 = vm.pop()?;
                vm.push(value_1 / value_2);
            }
            Some(Instruction::Modulo) => {
                let value_2 = vm.pop()?;
                let value_1 = vm.pop()?;
                vm.push(value_1 % value_2);
            }
            None => break Err("Reached end of instructions".to_owned()),
        };
    }
}

// TODO: ConstIndex being u8 limits the size of the constant pool to 256 values. Ideally, this should be changed to
// usize after implementing byte-to-int decoding.
type ConstIndex = u8;
type InstructionIndex = usize;

type Value = i32;
struct Chunk {
    code: Vec<u8>,
    constants: Vec<Value>,
}

struct VM {
    chunk: Chunk,
    ip: InstructionIndex,
    stack: Vec<Value>,
}

impl VM {
    fn read_byte(&mut self) -> u8 {
        let byte = self.chunk.code[self.ip];
        self.ip = self.ip + 1;
        return byte;
    }

    fn read_instruction(&mut self) -> Option<Instruction> {
        let op_code = self.read_byte();
        match op_code {
            1 => Some(Instruction::Return),
            2 => Some(Instruction::Print),
            3 => Some(Instruction::Constant(self.read_byte())),
            4 => Some(Instruction::Negate),
            5 => Some(Instruction::Add),
            6 => Some(Instruction::Subtract),
            7 => Some(Instruction::Multiply),
            8 => Some(Instruction::Divide),
            9 => Some(Instruction::Modulo),
            _ => None,
        }
    }

    fn read_constant(&self, index: ConstIndex) -> Result<Value, String> {
        self.chunk
            .constants
            .get(index as usize)
            .copied()
            .ok_or(format!("Failed to read constant at index {}", index).to_owned())
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value)
    }

    fn pop(&mut self) -> Result<Value, String> {
        self.stack
            .pop()
            .ok_or("Attempted to pop while the stack was empty".to_owned())
    }
}

enum Instruction {
    Return,
    Print,
    Constant(ConstIndex),
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
}
