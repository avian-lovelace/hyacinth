use core::fmt;
use std::collections::HashSet;
use std::str;

use crate::core::{Chunk, FloatValue, Heap, IntValue, Object, ObjectKey, Value};

pub fn interpret(chunk: Chunk, heap: Heap) {
    let mut vm = VM {
        chunk,
        ip: 0,
        stack: Vec::new(),
        heap,
    };
    run(&mut vm);
    vm.garbage_collect();
}

fn run(vm: &mut VM) {
    while vm.ip < vm.chunk.code.len() {
        match vm.read_instruction() {
            Instruction::Return => {
                break;
            }
            Instruction::Print => {
                let value = vm.pop();
                match value {
                    Value::Nil => println!("()"),
                    Value::Int(i) => println!("{}", i),
                    Value::Double(d) => println!("{}", d),
                    Value::Bool(b) => println!("{}", b),
                    Value::Char(c) => println!("{}", c),
                    Value::Object(object_index) => {
                        let object = vm.heap.get(object_index);
                        match object {
                            Object::StringObj(s) => println!("{}", s),
                            _ => println!("Object {}", object_index),
                        }
                    }
                }
            }
            Instruction::Constant(const_index) => vm.push(vm.read_constant(const_index)),
            Instruction::Negate => {
                let value = vm.pop();
                match value {
                    Value::Int(i) => vm.push(Value::Int(-i)),
                    Value::Double(d) => vm.push(Value::Double(-d)),
                    _ => panic!("Attemped to negate value {}", value),
                }
            }
            Instruction::Add => {
                let value_2 = vm.pop();
                let value_1 = vm.pop();
                match (value_1, value_2) {
                    (Value::Int(i1), Value::Int(i2)) => vm.push(Value::Int(i1 + i2)),
                    (Value::Double(d1), Value::Double(d2)) => vm.push(Value::Double(d1 + d2)),
                    (Value::Object(o1), Value::Object(o2)) => {
                        match (vm.heap.get(o1), vm.heap.get(o2)) {
                            (Object::StringObj(s1), Object::StringObj(s2)) => {
                                let concat_string = [s1.as_str(), s2.as_str()].join("");
                                let concat_string_ref =
                                    vm.heap.add(Object::StringObj(concat_string));
                                vm.push(concat_string_ref);
                            }
                            _ => panic!("Attemped to add objects {} and {}", o1, o2),
                        }
                    }
                    (Value::Object(o), Value::Char(c)) => match vm.heap.get(o) {
                        Object::StringObj(s) => {
                            let mut concat_string = s.to_owned();
                            concat_string.push(c);
                            let concat_string_ref = vm.heap.add(Object::StringObj(concat_string));
                            vm.push(concat_string_ref);
                        }
                        _ => panic!("Attemped to add character {} and object {}", c, o),
                    },
                    (Value::Char(c), Value::Object(o)) => match vm.heap.get(o) {
                        Object::StringObj(s) => {
                            let mut concat_string = s.to_owned();
                            concat_string.insert(0, c);
                            let concat_string_ref = vm.heap.add(Object::StringObj(concat_string));
                            vm.push(concat_string_ref);
                        }
                        _ => panic!("Attemped to add character {} and object {}", c, o),
                    },
                    _ => panic!("Attempted to add values {} and {}", value_1, value_2),
                }
            }
            Instruction::Subtract => {
                let value_2 = vm.pop();
                let value_1 = vm.pop();
                match (value_1, value_2) {
                    (Value::Int(i1), Value::Int(i2)) => vm.push(Value::Int(i1 - i2)),
                    (Value::Double(d1), Value::Double(d2)) => vm.push(Value::Double(d1 - d2)),
                    _ => panic!("Attempted to subtract values {} and {}", value_1, value_2),
                }
            }
            Instruction::Multiply => {
                let value_2 = vm.pop();
                let value_1 = vm.pop();
                match (value_1, value_2) {
                    (Value::Int(i1), Value::Int(i2)) => vm.push(Value::Int(i1 * i2)),
                    (Value::Double(d1), Value::Double(d2)) => vm.push(Value::Double(d1 * d2)),
                    _ => panic!("Attempted to multiply values {} and {}", value_1, value_2),
                }
            }
            Instruction::Divide => {
                let value_2 = vm.pop();
                let value_1 = vm.pop();
                match (value_1, value_2) {
                    (Value::Int(i1), Value::Int(i2)) => vm.push(Value::Int(i1 / i2)),
                    (Value::Double(d1), Value::Double(d2)) => vm.push(Value::Double(d1 / d2)),
                    _ => panic!("Attempted to divide values {} and {}", value_1, value_2),
                }
            }
            Instruction::Modulo => {
                let value_2 = vm.pop();
                let value_1 = vm.pop();
                match (value_1, value_2) {
                    (Value::Int(i1), Value::Int(i2)) => vm.push(Value::Int(i1 % i2)),
                    _ => panic!("Attempted to modulo values {} and {}", value_1, value_2),
                }
            }
            Instruction::Not => {
                let value = vm.pop();
                match value {
                    Value::Bool(b) => vm.push(Value::Bool(!b)),
                    _ => panic!("Attemped to not value {}", value),
                }
            }
            Instruction::And => {
                let value_2 = vm.pop();
                let value_1 = vm.pop();
                match (value_1, value_2) {
                    (Value::Bool(b1), Value::Bool(b2)) => vm.push(Value::Bool(b1 && b2)),
                    _ => panic!("Attempted to and values {} and {}", value_1, value_2),
                }
            }
            Instruction::Or => {
                let value_2 = vm.pop();
                let value_1 = vm.pop();
                match (value_1, value_2) {
                    (Value::Bool(b1), Value::Bool(b2)) => vm.push(Value::Bool(b1 || b2)),
                    _ => panic!("Attempted to or values {} and {}", value_1, value_2),
                }
            }
            Instruction::Equal => {
                let value_2 = vm.pop();
                let value_1 = vm.pop();
                match (value_1, value_2) {
                    (Value::Int(i1), Value::Int(i2)) => vm.push(Value::Bool(i1 == i2)),
                    (Value::Double(d1), Value::Double(d2)) => vm.push(Value::Bool(d1 == d2)),
                    (Value::Bool(b1), Value::Bool(b2)) => vm.push(Value::Bool(b1 == b2)),
                    _ => panic!("Attempted to equal values {} and {}", value_1, value_2),
                }
            }
            Instruction::NotEqual => {
                let value_2 = vm.pop();
                let value_1 = vm.pop();
                match (value_1, value_2) {
                    (Value::Int(i1), Value::Int(i2)) => vm.push(Value::Bool(i1 != i2)),
                    (Value::Double(d1), Value::Double(d2)) => vm.push(Value::Bool(d1 != d2)),
                    (Value::Bool(b1), Value::Bool(b2)) => vm.push(Value::Bool(b1 != b2)),
                    _ => panic!("Attempted to not equal values {} and {}", value_1, value_2),
                }
            }
            Instruction::Greater => {
                let value_2 = vm.pop();
                let value_1 = vm.pop();
                match (value_1, value_2) {
                    (Value::Int(i1), Value::Int(i2)) => vm.push(Value::Bool(i1 > i2)),
                    (Value::Double(d1), Value::Double(d2)) => vm.push(Value::Bool(d1 > d2)),
                    _ => panic!("Attempted to greater values {} and {}", value_1, value_2),
                }
            }
            Instruction::Less => {
                let value_2 = vm.pop();
                let value_1 = vm.pop();
                match (value_1, value_2) {
                    (Value::Int(i1), Value::Int(i2)) => vm.push(Value::Bool(i1 < i2)),
                    (Value::Double(d1), Value::Double(d2)) => vm.push(Value::Bool(d1 < d2)),
                    _ => panic!("Attempted to less values {} and {}", value_1, value_2),
                }
            }
            Instruction::GreaterEqual => {
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
            Instruction::LessEqual => {
                let value_2 = vm.pop();
                let value_1 = vm.pop();
                match (value_1, value_2) {
                    (Value::Int(i1), Value::Int(i2)) => vm.push(Value::Bool(i1 <= i2)),
                    (Value::Double(d1), Value::Double(d2)) => vm.push(Value::Bool(d1 <= d2)),
                    _ => panic!("Attempted to less equal values {} and {}", value_1, value_2),
                }
            }
            Instruction::True => vm.push(Value::Bool(true)),
            Instruction::False => vm.push(Value::Bool(false)),
            Instruction::ReadVariable(stack_index) => vm.push(vm.peek(stack_index)),
            Instruction::MutateVariable(stack_index) => {
                let value = vm.pop();
                vm.set(stack_index, value)
            }
            Instruction::Nil => vm.push(Value::Nil),
            Instruction::Pop => {
                vm.pop();
            }
            Instruction::PopMultiple(num) => vm.pop_multiple(num),
            Instruction::Jump(offset) => vm.jump(offset),
            Instruction::JumpIfFalse(offset) => {
                let value = vm.pop();
                match value {
                    Value::Bool(true) => {}
                    Value::Bool(false) => vm.jump(offset),
                    _ => panic!(
                        "Got non-boolean value {} as the condition when evaluating JumpIfFalse",
                        value
                    ),
                }
            }
            Instruction::Int(i) => vm.push(Value::Int(i)),
            Instruction::Float(f) => vm.push(Value::Double(f)),
            Instruction::Char(c) => vm.push(Value::Char(c)),
        };
    }
}

struct VM {
    chunk: Chunk,
    ip: InstructionIndex,
    stack: Vec<Value>,
    heap: Heap,
}

impl VM {
    fn read_byte(&mut self) -> u8 {
        let byte = self.chunk.code[self.ip];
        self.ip = self.ip + 1;
        return byte;
    }

    fn read_u16(&mut self) -> u16 {
        let byte_array: [u8; 2] = self.chunk.code[self.ip..self.ip + 2]
            .try_into()
            .expect("Failed to read u16 when decoding instruction");
        let int = u16::from_be_bytes(byte_array);
        self.ip = self.ip + 2;
        return int;
    }

    fn read_i16(&mut self) -> i16 {
        let byte_array: [u8; 2] = self.chunk.code[self.ip..self.ip + 2]
            .try_into()
            .expect("Failed to read i16 when decoding instruction");
        let int = i16::from_be_bytes(byte_array);
        self.ip = self.ip + 2;
        return int;
    }

    fn read_i32(&mut self) -> i32 {
        let byte_array: [u8; 4] = self.chunk.code[self.ip..self.ip + 4]
            .try_into()
            .expect("Failed to read i32 when decoding instruction");
        let int = i32::from_be_bytes(byte_array);
        self.ip = self.ip + 4;
        return int;
    }

    fn read_f64(&mut self) -> f64 {
        let byte_array: [u8; 8] = self.chunk.code[self.ip..self.ip + 8]
            .try_into()
            .expect("Failed to read float when decoding instruction");
        let int = f64::from_be_bytes(byte_array);
        self.ip = self.ip + 8;
        return int;
    }

    fn read_char(&mut self) -> char {
        // Trim padding from the end of the encoded char
        let mut end_index = self.ip + 3;
        while end_index > self.ip && self.chunk.code[end_index] == 0 {
            end_index -= 1;
        }
        let value_slice = &self.chunk.code[self.ip..end_index + 1];
        let value_string = str::from_utf8(value_slice)
            .expect("Failed to read char value as utf8 text when reading file");
        let value_char = value_string
            .chars()
            .next()
            .expect("Char value was empty when reading file");
        self.ip += 4;
        return value_char;
    }

    fn read_instruction(&mut self) -> Instruction {
        let op_code = self.read_byte();
        match op_code {
            1 => Instruction::Return,
            2 => Instruction::Print,
            3 => Instruction::Constant(self.read_u16()),
            4 => Instruction::Negate,
            5 => Instruction::Add,
            6 => Instruction::Subtract,
            7 => Instruction::Multiply,
            8 => Instruction::Divide,
            9 => Instruction::Modulo,
            10 => Instruction::Not,
            11 => Instruction::And,
            12 => Instruction::Or,
            13 => Instruction::Equal,
            14 => Instruction::NotEqual,
            15 => Instruction::Greater,
            16 => Instruction::Less,
            17 => Instruction::GreaterEqual,
            18 => Instruction::LessEqual,
            19 => Instruction::True,
            20 => Instruction::False,
            21 => Instruction::ReadVariable(self.read_u16()),
            22 => Instruction::MutateVariable(self.read_u16()),
            23 => Instruction::Nil,
            24 => Instruction::Pop,
            25 => Instruction::PopMultiple(self.read_u16()),
            26 => Instruction::Jump(self.read_i16()),
            27 => Instruction::JumpIfFalse(self.read_i16()),
            28 => Instruction::Int(self.read_i32()),
            29 => Instruction::Float(self.read_f64()),
            30 => Instruction::Char(self.read_char()),
            op => panic!("Got invalid op code {}", op),
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

    fn pop_multiple(&mut self, num: u16) {
        self.stack.truncate(self.stack.len() - num as usize);
    }

    fn peek(&self, index: StackIndex) -> Value {
        self.stack
            .get(index as usize)
            .copied()
            .expect(format!("Failed to read stack at index {}", index).as_str())
    }

    fn set(&mut self, index: StackIndex, value: Value) {
        self.stack[index as usize] = value
    }

    fn jump(&mut self, offset: i16) {
        self.ip = if offset >= 0 {
            self.ip + (offset as usize)
        } else {
            self.ip - (-offset as usize)
        }
    }

    fn garbage_collect(&mut self) {
        let mut reachable_objects = HashSet::<ObjectKey>::new();
        for value in &self.stack {
            match value {
                Value::Object(key) => {
                    reachable_objects.insert(*key);
                }
                _ => {}
            }
        }
        for value in &self.chunk.constants {
            match value {
                Value::Object(key) => {
                    reachable_objects.insert(*key);
                }
                _ => {}
            }
        }
        self.heap.garbage_collect(reachable_objects);
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
    ReadVariable(StackIndex),
    MutateVariable(StackIndex),
    Nil,
    Pop,
    PopMultiple(StackIndex),
    Jump(InstructionOffset),
    JumpIfFalse(InstructionOffset),
    Int(IntValue),
    Float(FloatValue),
    Char(char),
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::Return => write!(f, "Return"),
            Instruction::Print => write!(f, "Print"),
            Instruction::Constant(const_index) => write!(f, "Constant {}", const_index),
            Instruction::Negate => write!(f, "Negate"),
            Instruction::Add => write!(f, "Add"),
            Instruction::Subtract => write!(f, "Subtract"),
            Instruction::Multiply => write!(f, "Multiply"),
            Instruction::Divide => write!(f, "Divide"),
            Instruction::Modulo => write!(f, "Modulo"),
            Instruction::Not => write!(f, "Not"),
            Instruction::And => write!(f, "And"),
            Instruction::Or => write!(f, "Or"),
            Instruction::Equal => write!(f, "Equal"),
            Instruction::NotEqual => write!(f, "NotEqual"),
            Instruction::Greater => write!(f, "Greater"),
            Instruction::Less => write!(f, "Less"),
            Instruction::GreaterEqual => write!(f, "GreaterEqual"),
            Instruction::LessEqual => write!(f, "LessEqual"),
            Instruction::True => write!(f, "True"),
            Instruction::False => write!(f, "False"),
            Instruction::ReadVariable(stack_index) => {
                write!(f, "ReadVariable {}", stack_index)
            }
            Instruction::MutateVariable(stack_index) => {
                write!(f, "MutateVariable {}", stack_index)
            }
            Instruction::Nil => write!(f, "()"),
            Instruction::Pop => write!(f, "Pop"),
            Instruction::PopMultiple(u) => write!(f, "PopMultiple {}", u),
            Instruction::Jump(offset) => write!(f, "Jump {}", offset),
            Instruction::JumpIfFalse(offset) => write!(f, "JumpIfFalse {}", offset),
            Instruction::Int(i) => write!(f, "Int {}", i),
            Instruction::Float(d) => write!(f, "Float {}", d),
            Instruction::Char(c) => write!(f, "Char {}", c),
        }
    }
}

// TODO: ConstIndex being u8 limits the size of the constant pool to 256 values. Ideally, this should be changed to
// usize after implementing byte-to-int decoding.
type ConstIndex = u16;
type StackIndex = u16;
type InstructionOffset = i16;
type InstructionIndex = usize;
