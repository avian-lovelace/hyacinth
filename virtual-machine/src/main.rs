use std::env;
use std::fmt;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    let bytecode_file_path = args.get(1).expect("No file path provided");
    let file_bytes = fs::read(bytecode_file_path)
        .expect(format!("Failed to read file {}", bytecode_file_path).as_str());
    let chunk = read_chunk(file_bytes);
    interpret(chunk);
}

fn read_chunk(bytes: Vec<u8>) -> Chunk {
    let mut f = FileState { bytes, index: 0 };
    let constants_length = f.read_int() as usize;
    let mut constants: Vec<Value> = Vec::new();
    for _ in 0..constants_length {
        match f.read_byte() {
            1 => constants.push(Value::Int(f.read_int())),
            2 => constants.push(Value::Double(f.read_double())),
            _ => panic!("Encountered unexpected code when reading constants"),
        }
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
        let byte_array: [u8; 4] = self.bytes[self.index..self.index + 4]
            .try_into()
            .expect("Failed to read bytes when parsing double constant");
        let int = i32::from_be_bytes(byte_array);
        self.index = self.index + 4;
        return int;
    }

    fn read_double(&mut self) -> f64 {
        let byte_array: [u8; 8] = self.bytes[self.index..self.index + 8]
            .try_into()
            .expect("Failed to read bytes when parsing double constant");
        let double = f64::from_be_bytes(byte_array);
        self.index = self.index + 8;
        return double;
    }

    fn read_rest(&mut self) -> Vec<u8> {
        return self.bytes[self.index..].to_vec();
    }
}

fn interpret(chunk: Chunk) {
    let mut vm = VM {
        chunk,
        ip: 0,
        stack: Vec::new(),
    };
    return run(&mut vm);
}

fn run(vm: &mut VM) {
    loop {
        match vm.read_instruction() {
            Some(Instruction::Return) => {
                break;
            }
            Some(Instruction::Print) => println!("{}", vm.pop()),
            Some(Instruction::Constant(const_index)) => vm.push(vm.read_constant(const_index)),
            Some(Instruction::Negate) => {
                let value = vm.pop();
                match value {
                    Value::Int(i) => vm.push(Value::Int(-i)),
                    Value::Double(d) => vm.push(Value::Double(-d)),
                    _ => panic!("Attemped to negate value {}", value),
                }
            }
            Some(Instruction::Add) => {
                let value_2 = vm.pop();
                let value_1 = vm.pop();
                match (value_1, value_2) {
                    (Value::Int(i1), Value::Int(i2)) => vm.push(Value::Int(i1 + i2)),
                    (Value::Double(d1), Value::Double(d2)) => vm.push(Value::Double(d1 + d2)),
                    _ => panic!("Attempted to add values {} and {}", value_1, value_2),
                }
            }
            Some(Instruction::Subtract) => {
                let value_2 = vm.pop();
                let value_1 = vm.pop();
                match (value_1, value_2) {
                    (Value::Int(i1), Value::Int(i2)) => vm.push(Value::Int(i1 - i2)),
                    (Value::Double(d1), Value::Double(d2)) => vm.push(Value::Double(d1 - d2)),
                    _ => panic!("Attempted to subtract values {} and {}", value_1, value_2),
                }
            }
            Some(Instruction::Multiply) => {
                let value_2 = vm.pop();
                let value_1 = vm.pop();
                match (value_1, value_2) {
                    (Value::Int(i1), Value::Int(i2)) => vm.push(Value::Int(i1 * i2)),
                    (Value::Double(d1), Value::Double(d2)) => vm.push(Value::Double(d1 * d2)),
                    _ => panic!("Attempted to multiply values {} and {}", value_1, value_2),
                }
            }
            Some(Instruction::Divide) => {
                let value_2 = vm.pop();
                let value_1 = vm.pop();
                match (value_1, value_2) {
                    (Value::Int(i1), Value::Int(i2)) => vm.push(Value::Int(i1 / i2)),
                    (Value::Double(d1), Value::Double(d2)) => vm.push(Value::Double(d1 / d2)),
                    _ => panic!("Attempted to divide values {} and {}", value_1, value_2),
                }
            }
            Some(Instruction::Modulo) => {
                let value_2 = vm.pop();
                let value_1 = vm.pop();
                match (value_1, value_2) {
                    (Value::Int(i1), Value::Int(i2)) => vm.push(Value::Int(i1 % i2)),
                    _ => panic!("Attempted to modulo values {} and {}", value_1, value_2),
                }
            }
            Some(Instruction::Not) => {
                let value = vm.pop();
                match value {
                    Value::Bool(b) => vm.push(Value::Bool(!b)),
                    _ => panic!("Attemped to not value {}", value),
                }
            }
            Some(Instruction::And) => {
                let value_2 = vm.pop();
                let value_1 = vm.pop();
                match (value_1, value_2) {
                    (Value::Bool(b1), Value::Bool(b2)) => vm.push(Value::Bool(b1 && b2)),
                    _ => panic!("Attempted to and values {} and {}", value_1, value_2),
                }
            }
            Some(Instruction::Or) => {
                let value_2 = vm.pop();
                let value_1 = vm.pop();
                match (value_1, value_2) {
                    (Value::Bool(b1), Value::Bool(b2)) => vm.push(Value::Bool(b1 || b2)),
                    _ => panic!("Attempted to or values {} and {}", value_1, value_2),
                }
            }
            Some(Instruction::Equal) => {
                let value_2 = vm.pop();
                let value_1 = vm.pop();
                match (value_1, value_2) {
                    (Value::Int(i1), Value::Int(i2)) => vm.push(Value::Bool(i1 == i2)),
                    (Value::Double(d1), Value::Double(d2)) => vm.push(Value::Bool(d1 == d2)),
                    (Value::Bool(b1), Value::Bool(b2)) => vm.push(Value::Bool(b1 == b2)),
                    _ => panic!("Attempted to equal values {} and {}", value_1, value_2),
                }
            }
            Some(Instruction::NotEqual) => {
                let value_2 = vm.pop();
                let value_1 = vm.pop();
                match (value_1, value_2) {
                    (Value::Int(i1), Value::Int(i2)) => vm.push(Value::Bool(i1 != i2)),
                    (Value::Double(d1), Value::Double(d2)) => vm.push(Value::Bool(d1 != d2)),
                    (Value::Bool(b1), Value::Bool(b2)) => vm.push(Value::Bool(b1 != b2)),
                    _ => panic!("Attempted to not equal values {} and {}", value_1, value_2),
                }
            }
            Some(Instruction::Greater) => {
                let value_2 = vm.pop();
                let value_1 = vm.pop();
                match (value_1, value_2) {
                    (Value::Int(i1), Value::Int(i2)) => vm.push(Value::Bool(i1 > i2)),
                    (Value::Double(d1), Value::Double(d2)) => vm.push(Value::Bool(d1 > d2)),
                    _ => panic!("Attempted to greater values {} and {}", value_1, value_2),
                }
            }
            Some(Instruction::Less) => {
                let value_2 = vm.pop();
                let value_1 = vm.pop();
                match (value_1, value_2) {
                    (Value::Int(i1), Value::Int(i2)) => vm.push(Value::Bool(i1 < i2)),
                    (Value::Double(d1), Value::Double(d2)) => vm.push(Value::Bool(d1 < d2)),
                    _ => panic!("Attempted to less values {} and {}", value_1, value_2),
                }
            }
            Some(Instruction::GreaterEqual) => {
                let value_2 = vm.pop();
                let value_1 = vm.pop();
                match (value_1, value_2) {
                    (Value::Int(i1), Value::Int(i2)) => vm.push(Value::Bool(i1 >= i2)),
                    (Value::Double(d1), Value::Double(d2)) => vm.push(Value::Bool(d1 >= d2)),
                    _ => panic!(
                        "Attempted to greater equal values {} and {}",
                        value_1, value_2
                    ),
                }
            }
            Some(Instruction::LessEqual) => {
                let value_2 = vm.pop();
                let value_1 = vm.pop();
                match (value_1, value_2) {
                    (Value::Int(i1), Value::Int(i2)) => vm.push(Value::Bool(i1 <= i2)),
                    (Value::Double(d1), Value::Double(d2)) => vm.push(Value::Bool(d1 <= d2)),
                    _ => panic!("Attempted to less equal values {} and {}", value_1, value_2),
                }
            }
            Some(Instruction::True) => vm.push(Value::Bool(true)),
            Some(Instruction::False) => vm.push(Value::Bool(false)),
            None => {
                panic!("Reached end of instructions")
            }
        };
    }
}

// TODO: ConstIndex being u8 limits the size of the constant pool to 256 values. Ideally, this should be changed to
// usize after implementing byte-to-int decoding.
type ConstIndex = u8;
type InstructionIndex = usize;

#[derive(Clone, Copy)]
enum Value {
    Int(i32),
    Double(f64),
    Bool(bool),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Int(i) => write!(f, "{}", i),
            Value::Double(d) => write!(f, "{}", d),
            Value::Bool(b) => write!(f, "{}", b),
        }
    }
}

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
            10 => Some(Instruction::Not),
            11 => Some(Instruction::And),
            12 => Some(Instruction::Or),
            13 => Some(Instruction::Equal),
            14 => Some(Instruction::NotEqual),
            15 => Some(Instruction::Greater),
            16 => Some(Instruction::Less),
            17 => Some(Instruction::GreaterEqual),
            18 => Some(Instruction::LessEqual),
            19 => Some(Instruction::True),
            20 => Some(Instruction::False),
            _ => None,
        }
    }

    fn read_constant(&self, index: ConstIndex) -> Value {
        self.chunk
            .constants
            .get(index as usize)
            .copied()
            .expect(format!("Failed to read constant at index {}", index).as_str())
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value)
    }

    fn pop(&mut self) -> Value {
        self.stack
            .pop()
            .expect("Attempted to pop while the stack was empty")
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
    Not,
    And,
    Or,
    Equal,
    NotEqual,
    Greater,
    Less,
    GreaterEqual,
    LessEqual,
    True,
    False,
}
