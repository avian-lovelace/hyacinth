use crate::core::{Chunk, Value};

pub fn interpret(chunk: Chunk) {
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

// TODO: ConstIndex being u8 limits the size of the constant pool to 256 values. Ideally, this should be changed to
// usize after implementing byte-to-int decoding.
type ConstIndex = u8;
type InstructionIndex = usize;
