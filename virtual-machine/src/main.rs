use std::fs;

fn main() {
    let read_result = fs::read("../code.byte");
    match read_result {
        Ok(bytes) => {
            // for byte in bytes {
            //     println!("{}, ", byte);
            // }
            let chunk = readChunk(bytes);
            interpret(chunk);
        }
        Err(_) => println!("Failed to read file"),
    }

    // let chunk = Chunk {
    //     code: vec![2, 0, 2, 1, 6, 2, 2, 5, 1, 0],
    //     constants: vec![3, 5, 7],
    // };
}

fn readChunk(bytes: Vec<u8>) -> Chunk {
    let mut f = FileState { bytes, index: 0 };
    let constants_length = f.read_int() as usize;
    println!("constants_length {}", constants_length);
    let mut constants: Vec<Value> = Vec::new();
    for _ in (0..constants_length) {
        constants.push(f.read_int());
    }
    print!("constants:");
    for constant in &constants {
        print!("{},", constant);
    }
    println!("");
    let code = f.read_rest();
    print!("code:");
    for byte in &code {
        print!("{},", byte);
    }
    println!("");
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

fn interpret(chunk: Chunk) -> InterpretResult {
    let mut vm = VM {
        chunk,
        ip: 0,
        stack: Vec::new(),
    };
    return run(&mut vm);
}

fn run(vm: &mut VM) -> InterpretResult {
    loop {
        let instruction = vm.read_instruction();
        match instruction {
            Some(Instruction::Return) => break InterpretResult::Ok,
            Some(Instruction::Print) => match vm.pop() {
                Some(value) => println!("{value}"),
                None => break InterpretResult::RuntimeError,
            },
            Some(Instruction::Constant(const_index)) => match vm.read_constant(const_index) {
                Some(value) => vm.push(value),
                None => break InterpretResult::RuntimeError,
            },
            Some(Instruction::Negate) => match vm.pop() {
                Some(value) => vm.push(-value),
                None => break InterpretResult::RuntimeError,
            },
            Some(Instruction::Add) => match (vm.pop(), vm.pop()) {
                (Some(value_2), Some(value_1)) => vm.push(value_1 + value_2),
                _ => break InterpretResult::RuntimeError,
            },
            Some(Instruction::Subtract) => match (vm.pop(), vm.pop()) {
                (Some(value_2), Some(value_1)) => vm.push(value_1 - value_2),
                _ => break InterpretResult::RuntimeError,
            },
            Some(Instruction::Multiply) => match (vm.pop(), vm.pop()) {
                (Some(value_2), Some(value_1)) => vm.push(value_1 * value_2),
                _ => break InterpretResult::RuntimeError,
            },
            Some(Instruction::Divide) => match (vm.pop(), vm.pop()) {
                (Some(value_2), Some(value_1)) => vm.push(value_1 / value_2),
                _ => break InterpretResult::RuntimeError,
            },
            Some(Instruction::Modulo) => match (vm.pop(), vm.pop()) {
                (Some(value_2), Some(value_1)) => vm.push(value_1 % value_2),
                _ => break InterpretResult::RuntimeError,
            },
            None => break InterpretResult::RuntimeError,
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

    fn read_constant(&self, index: ConstIndex) -> Option<Value> {
        self.chunk.constants.get(index as usize).copied()
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value)
    }

    fn pop(&mut self) -> Option<Value> {
        self.stack.pop()
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

enum InterpretResult {
    Ok,
    CompileError,
    RuntimeError,
}
