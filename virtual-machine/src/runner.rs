use core::fmt;
use std::str;
use std::{collections::HashSet, io::stdin};

use crate::core::{
    BuiltInFunction, ConstIndex, FieldIndex, FloatValue, Frame, FunctionIndex, InstructionOffset,
    IntValue, Object, ObjectKey, RecordId, StackIndex, Value, VM,
};

impl VM {
    pub fn run(&mut self) {
        loop {
            let instruction = self.read_instruction();
            match instruction {
                Instruction::Return => match self.frames.pop() {
                    None => break,
                    Some(frame) => {
                        let return_value = self.pop();
                        // We truncate at one lower than the stack index to remove the function object that we just finished calling
                        self.stack.truncate(self.stack_index as usize - 1);
                        self.function_index = frame.function_index;
                        self.instruction_index = frame.instruction_index;
                        self.stack_index = frame.stack_index;
                        self.push(return_value);
                    }
                },
                Instruction::BuiltInFunction(function_id) => {
                    self.push(Value::BuiltInFunction(BuiltInFunction::from(function_id)))
                }
                Instruction::Constant(const_index) => {
                    self.push(self.constants[const_index as usize])
                }
                Instruction::True => self.push(Value::Bool(true)),
                Instruction::False => self.push(Value::Bool(false)),
                Instruction::ReadVariable(stack_index) => {
                    self.push(self.peek_from_frame_bottom(stack_index))
                }
                Instruction::MutateVariable(stack_index) => {
                    let value = self.pop();
                    self.set_from_frame_bottom(stack_index, value)
                }
                Instruction::Nil => self.push(Value::Nil),
                Instruction::Pop => {
                    self.pop();
                }
                Instruction::PopMultiple(num) => self.pop_multiple(num as usize),
                Instruction::Jump(offset) => self.jump(offset),
                Instruction::JumpIfFalse(offset) => {
                    let value = self.pop();
                    match value {
                        Value::Bool(true) => {}
                        Value::Bool(false) => self.jump(offset),
                        _ => panic!(
                            "Got non-boolean value {} as the condition when evaluating JumpIfFalse",
                            value
                        ),
                    }
                }
                Instruction::Int(i) => self.push(Value::Int(i)),
                Instruction::Float(f) => self.push(Value::Float(f)),
                Instruction::Char(c) => self.push(Value::Char(c)),
                Instruction::Function(function_index, num_closed) => {
                    let closed_variables = self.pop_slice(num_closed as usize);
                    let function_object = Object::FunctionObj {
                        function_index,
                        closed_variables,
                    };
                    let function_object_value = self.heap.add(function_object);
                    self.push(function_object_value)
                }
                Instruction::Call(num_arguments) => {
                    match self.peek_from_top(num_arguments as StackIndex) {
                        Value::BuiltInFunction(built_in) => match built_in {
                            BuiltInFunction::Negate => {
                                let value = self.pop();
                                self.pop();
                                match value {
                                    Value::Int(i) => self.push(Value::Int(-i)),
                                    Value::Float(d) => self.push(Value::Float(-d)),
                                    _ => panic!("Attemped to negate value {}", value),
                                }
                            }
                            BuiltInFunction::Add => {
                                let value_2 = self.pop();
                                let value_1 = self.pop();
                                self.pop();
                                match (value_1, value_2) {
                                    (Value::Int(i1), Value::Int(i2)) => {
                                        self.push(Value::Int(i1 + i2))
                                    }
                                    (Value::Float(d1), Value::Float(d2)) => {
                                        self.push(Value::Float(d1 + d2))
                                    }
                                    (Value::Object(o1), Value::Object(o2)) => {
                                        match (self.heap.get(o1), self.heap.get(o2)) {
                                            (Object::StringObj(s1), Object::StringObj(s2)) => {
                                                let concat_string =
                                                    [s1.as_str(), s2.as_str()].join("");
                                                let concat_string_ref =
                                                    self.heap.add(Object::StringObj(concat_string));
                                                self.push(concat_string_ref);
                                            }
                                            _ => {
                                                panic!("Attemped to add objects {} and {}", o1, o2)
                                            }
                                        }
                                    }
                                    (Value::Object(o), Value::Char(c)) => match self.heap.get(o) {
                                        Object::StringObj(s) => {
                                            let mut concat_string = s.to_owned();
                                            concat_string.push(c);
                                            let concat_string_ref =
                                                self.heap.add(Object::StringObj(concat_string));
                                            self.push(concat_string_ref);
                                        }
                                        _ => panic!(
                                            "Attemped to add character {} and object {}",
                                            c, o
                                        ),
                                    },
                                    (Value::Char(c), Value::Object(o)) => match self.heap.get(o) {
                                        Object::StringObj(s) => {
                                            let mut concat_string = s.to_owned();
                                            concat_string.insert(0, c);
                                            let concat_string_ref =
                                                self.heap.add(Object::StringObj(concat_string));
                                            self.push(concat_string_ref);
                                        }
                                        _ => panic!(
                                            "Attemped to add character {} and object {}",
                                            c, o
                                        ),
                                    },
                                    _ => panic!(
                                        "Attempted to add values {} and {}",
                                        value_1, value_2
                                    ),
                                }
                            }
                            BuiltInFunction::Subtract => {
                                let value_2 = self.pop();
                                let value_1 = self.pop();
                                self.pop();
                                match (value_1, value_2) {
                                    (Value::Int(i1), Value::Int(i2)) => {
                                        self.push(Value::Int(i1 - i2))
                                    }
                                    (Value::Float(d1), Value::Float(d2)) => {
                                        self.push(Value::Float(d1 - d2))
                                    }
                                    _ => panic!(
                                        "Attempted to subtract values {} and {}",
                                        value_1, value_2
                                    ),
                                }
                            }
                            BuiltInFunction::Multiply => {
                                let value_2 = self.pop();
                                let value_1 = self.pop();
                                self.pop();
                                match (value_1, value_2) {
                                    (Value::Int(i1), Value::Int(i2)) => {
                                        self.push(Value::Int(i1 * i2))
                                    }
                                    (Value::Float(d1), Value::Float(d2)) => {
                                        self.push(Value::Float(d1 * d2))
                                    }
                                    _ => panic!(
                                        "Attempted to multiply values {} and {}",
                                        value_1, value_2
                                    ),
                                }
                            }
                            BuiltInFunction::Divide => {
                                let value_2 = self.pop();
                                let value_1 = self.pop();
                                self.pop();
                                match (value_1, value_2) {
                                    (Value::Int(i1), Value::Int(i2)) => {
                                        self.push(Value::Int(i1 / i2))
                                    }
                                    (Value::Float(d1), Value::Float(d2)) => {
                                        self.push(Value::Float(d1 / d2))
                                    }
                                    _ => {
                                        panic!(
                                            "Attempted to divide values {} and {}",
                                            value_1, value_2
                                        )
                                    }
                                }
                            }
                            BuiltInFunction::Modulo => {
                                let value_2 = self.pop();
                                let value_1 = self.pop();
                                self.pop();
                                match (value_1, value_2) {
                                    (Value::Int(i1), Value::Int(i2)) => {
                                        self.push(Value::Int(i1 % i2))
                                    }
                                    _ => {
                                        panic!(
                                            "Attempted to modulo values {} and {}",
                                            value_1, value_2
                                        )
                                    }
                                }
                            }
                            BuiltInFunction::Not => {
                                let value = self.pop();
                                self.pop();
                                match value {
                                    Value::Bool(b) => self.push(Value::Bool(!b)),
                                    _ => panic!("Attemped to not value {}", value),
                                }
                            }
                            BuiltInFunction::Equal => {
                                let value_2 = self.pop();
                                let value_1 = self.pop();
                                self.pop();
                                match (value_1, value_2) {
                                    (Value::Nil, Value::Nil) => self.push(Value::Bool(true)),
                                    (Value::Int(i1), Value::Int(i2)) => {
                                        self.push(Value::Bool(i1 == i2))
                                    }
                                    (Value::Float(d1), Value::Float(d2)) => {
                                        self.push(Value::Bool(d1 == d2))
                                    }
                                    (Value::Bool(b1), Value::Bool(b2)) => {
                                        self.push(Value::Bool(b1 == b2))
                                    }
                                    (Value::Char(c1), Value::Char(c2)) => {
                                        self.push(Value::Bool(c1 == c2))
                                    }
                                    (Value::Object(o1), Value::Object(o2)) => {
                                        let are_equal = (o1 == o2)
                                            || {
                                                let obj_1 = self.heap.get(o1);
                                                let obj_2 = self.heap.get(o2);
                                                match (self.heap.get(o1), self.heap.get(o2)) {
                                                    (Object::StringObj(s1), Object::StringObj(s2)) => {
                                                        s1 == s2
                                                    }
                                                    _ => panic!(
                                                        "Attempted to check equality of objects {} and {}",
                                                        obj_1, obj_2
                                                    ),
                                                }
                                            };
                                        self.push(Value::Bool(are_equal))
                                    }
                                    _ => panic!(
                                        "Attempted to check equality of values {} and {}",
                                        value_1, value_2
                                    ),
                                }
                            }
                            BuiltInFunction::NotEqual => {
                                let value_2 = self.pop();
                                let value_1 = self.pop();
                                self.pop();
                                match (value_1, value_2) {
                                    (Value::Nil, Value::Nil) => self.push(Value::Bool(false)),
                                    (Value::Int(i1), Value::Int(i2)) => {
                                        self.push(Value::Bool(i1 != i2))
                                    }
                                    (Value::Float(d1), Value::Float(d2)) => {
                                        self.push(Value::Bool(d1 != d2))
                                    }
                                    (Value::Bool(b1), Value::Bool(b2)) => {
                                        self.push(Value::Bool(b1 != b2))
                                    }
                                    (Value::Char(c1), Value::Char(c2)) => {
                                        self.push(Value::Bool(c1 != c2))
                                    }
                                    (Value::Object(o1), Value::Object(o2)) => {
                                        let are_equal = (o1 == o2)
                                            || {
                                                let obj_1 = self.heap.get(o1);
                                                let obj_2 = self.heap.get(o2);
                                                match (self.heap.get(o1), self.heap.get(o2)) {
                                                    (Object::StringObj(s1), Object::StringObj(s2)) => {
                                                        s1 == s2
                                                    }
                                                    _ => panic!(
                                                        "Attempted to check equality of objects {} and {}",
                                                        obj_1, obj_2
                                                    ),
                                                }
                                            };
                                        self.push(Value::Bool(!are_equal))
                                    }
                                    _ => panic!(
                                        "Attempted to check equality of values {} and {}",
                                        value_1, value_2
                                    ),
                                }
                            }
                            BuiltInFunction::Greater => {
                                let value_2 = self.pop();
                                let value_1 = self.pop();
                                self.pop();
                                match (value_1, value_2) {
                                    (Value::Int(i1), Value::Int(i2)) => {
                                        self.push(Value::Bool(i1 > i2))
                                    }
                                    (Value::Float(d1), Value::Float(d2)) => {
                                        self.push(Value::Bool(d1 > d2))
                                    }
                                    _ => panic!(
                                        "Attempted to greater values {} and {}",
                                        value_1, value_2
                                    ),
                                }
                            }
                            BuiltInFunction::Less => {
                                let value_2 = self.pop();
                                let value_1 = self.pop();
                                self.pop();
                                match (value_1, value_2) {
                                    (Value::Int(i1), Value::Int(i2)) => {
                                        self.push(Value::Bool(i1 < i2))
                                    }
                                    (Value::Float(d1), Value::Float(d2)) => {
                                        self.push(Value::Bool(d1 < d2))
                                    }
                                    _ => panic!(
                                        "Attempted to less values {} and {}",
                                        value_1, value_2
                                    ),
                                }
                            }
                            BuiltInFunction::GreaterEqual => {
                                let value_2 = self.pop();
                                let value_1 = self.pop();
                                self.pop();
                                match (value_1, value_2) {
                                    (Value::Int(i1), Value::Int(i2)) => {
                                        self.push(Value::Bool(i1 >= i2))
                                    }
                                    (Value::Float(d1), Value::Float(d2)) => {
                                        self.push(Value::Bool(d1 >= d2))
                                    }
                                    _ => panic!(
                                        "Attempted to greater equal values {} and {}",
                                        value_1, value_2
                                    ),
                                }
                            }
                            BuiltInFunction::LessEqual => {
                                let value_2 = self.pop();
                                let value_1 = self.pop();
                                self.pop();
                                match (value_1, value_2) {
                                    (Value::Int(i1), Value::Int(i2)) => {
                                        self.push(Value::Bool(i1 <= i2))
                                    }
                                    (Value::Float(d1), Value::Float(d2)) => {
                                        self.push(Value::Bool(d1 <= d2))
                                    }
                                    _ => panic!(
                                        "Attempted to less equal values {} and {}",
                                        value_1, value_2
                                    ),
                                }
                            }
                            BuiltInFunction::Print => {
                                let value = self.pop();
                                self.pop();
                                match value {
                                    Value::Int(i) => print!("{}", i),
                                    Value::Float(f) => print!("{}", f),
                                    Value::Char(c) => print!("{}", c),
                                    Value::Bool(b) => print!("{}", b),
                                    Value::Nil => print!("nil"),
                                    Value::Object(object_index) => {
                                        let object = self.heap.get(object_index);
                                        match object {
                                            Object::StringObj(s) => print!("{}", s),
                                            _ => panic!("Ran print on {}", value),
                                        }
                                    }
                                    _ => panic!("Ran print on {}", value),
                                }
                                self.push(Value::Nil);
                            }
                            BuiltInFunction::PrintLine => {
                                let value = self.pop();
                                self.pop();
                                match value {
                                    Value::Int(i) => println!("{}", i),
                                    Value::Float(f) => println!("{}", f),
                                    Value::Char(c) => println!("{}", c),
                                    Value::Bool(b) => println!("{}", b),
                                    Value::Nil => println!("nil"),
                                    Value::Object(object_index) => {
                                        let object = self.heap.get(object_index);
                                        match object {
                                            Object::StringObj(s) => println!("{}", s),
                                            _ => panic!("Ran printLine on {}", value),
                                        }
                                    }
                                    _ => panic!("Ran printLine on {}", value),
                                }
                                self.push(Value::Nil);
                            }
                            BuiltInFunction::ReadLine => {
                                self.pop();
                                let mut input = String::new();
                                stdin()
                                    .read_line(&mut input)
                                    .expect("Failed to read from stdin");
                                let input_string_ref =
                                    self.heap.add(Object::StringObj(input.trim().to_owned()));
                                self.push(input_string_ref);
                            }
                            BuiltInFunction::Push => {
                                let push_value = self.pop();
                                let list_value = self.pop();
                                self.pop();
                                match list_value {
                                    Value::Object(list_object_index) => {
                                        let object = self.heap.get_mut(list_object_index);
                                        match object {
                                            Object::ListObj { values } => values.push(push_value),
                                            obj => panic!("Ran push on object {}", obj),
                                        }
                                    }
                                    _ => panic!("Ran push on {}", list_value),
                                }
                                self.push(list_value);
                            }
                            BuiltInFunction::Pop => {
                                let list_value = self.pop();
                                self.pop();
                                let pop_value = match list_value {
                                    Value::Object(list_object_index) => {
                                        let object = self.heap.get_mut(list_object_index);
                                        match object {
                                            Object::ListObj { values } => {
                                                values.pop().expect("Ran pop while list was empty")
                                            }
                                            obj => panic!("Ran pop on object {}", obj),
                                        }
                                    }
                                    _ => panic!("Ran pop on {}", list_value),
                                };
                                self.push(pop_value);
                            }
                        },
                        Value::Object(object_index) => match self.heap.get(object_index) {
                            Object::FunctionObj {
                                function_index,
                                closed_variables,
                            } => {
                                let current_frame = Frame {
                                    function_index: self.function_index,
                                    instruction_index: self.instruction_index,
                                    stack_index: self.stack_index,
                                };
                                self.frames.push(current_frame);
                                self.function_index = *function_index;
                                self.instruction_index = 0;
                                self.stack_index = self.stack.len() as u16 - num_arguments as u16;
                                self.stack.extend(closed_variables);
                            }
                            obj => panic!("Attempted to call non-function object {}", obj),
                        },
                        value => panic!("Attempted to call non-function value {}", value),
                    }
                }
                Instruction::Record(record_id, num_fields) => {
                    let fields = self.pop_slice(num_fields as usize);
                    let record_object = Object::RecordObj { record_id, fields };
                    let record_object_value = self.heap.add(record_object);
                    self.push(record_object_value)
                }
                Instruction::Field(field_index) => match self.pop() {
                    Value::Object(object_index) => match self.heap.get(object_index) {
                        Object::RecordObj {
                            record_id: _,
                            fields,
                        } => self.push(fields[field_index as usize]),
                        obj => panic!("Attempted to access a field of non-record object {}", obj),
                    },
                    value => panic!("Attempted to access a field of non-record value {}", value),
                },
                Instruction::JumpIfDoesntMatchRecordId(record_id, offset) => {
                    let switch_value = self.peek_from_top(0);
                    match switch_value {
                        Value::Object(object_index) => match self.heap.get(object_index) {
                            Object::RecordObj {
                                record_id: switch_record_id,
                                fields: _,
                            } => {
                                if *switch_record_id != record_id {
                                    self.jump(offset);
                                }
                            }
                            obj => panic!("Attempted to switch on a non-record object {}", obj),
                        },
                        value => panic!("Attempted to switch on a non-record value {}", value),
                    }
                }
                Instruction::RemoveFromStack(stack_index) => {
                    self.remove_at_index_from_top(stack_index)
                }
                Instruction::MutateField(field_index) => {
                    let value = self.pop();
                    match self.pop() {
                        Value::Object(object_index) => match self.heap.get_mut(object_index) {
                            Object::RecordObj {
                                record_id: _,
                                fields,
                            } => fields[field_index as usize] = value,
                            obj => {
                                panic!("Attempted to mutate a field of non-record object {}", obj)
                            }
                        },
                        value => {
                            panic!("Attempted to mutate a field of non-record value {}", value)
                        }
                    }
                }
                Instruction::List(list_length) => {
                    let values = self.pop_slice(list_length as usize);
                    let list_object = Object::ListObj { values };
                    let list_object_value = self.heap.add(list_object);
                    self.push(list_object_value);
                }
                Instruction::Index => {
                    let index_value = self.pop();
                    let list_object_value = self.pop();
                    match (list_object_value, index_value) {
                        (Value::Object(list_object_key), Value::Int(index)) => {
                            match self.heap.get(list_object_key) {
                                Object::ListObj { values } => match values.get(index as usize) {
                                    Some(value) => self.push(value.clone()),
                                    None => panic!(
                                        "Attempted to get index {} of list object {} which has length {}",
                                        index,
                                        list_object_key,
                                        values.len()
                                    ),
                                },
                                list_object => {
                                    panic!(
                                        "Attempted to index into a list with list object {}",
                                        list_object
                                    );
                                }
                            }
                        }
                        _ => {
                            panic!("Attempted to index into a list with list value {} and index value {}", list_object_value, index_value);
                        }
                    }
                }
                Instruction::MutateIndex => {
                    let value = self.pop();
                    let index_value = self.pop();
                    let list_object_value = self.pop();
                    match (list_object_value, index_value) {
                        (Value::Object(list_object_key), Value::Int(index)) => {
                            match self.heap.get_mut(list_object_key) {
                                Object::ListObj { values } => {
                                    if index < 0 || (index as usize) >= values.len() {
                                        panic!(
                                            "Attempted to mutate index {} of list object {} which has length {}",
                                            index,
                                            list_object_key,
                                            values.len()
                                        )
                                    }
                                    values[index as usize] = value;
                                }
                                list_object => {
                                    panic!(
                                        "Attempted to mutate list with list object {}",
                                        list_object
                                    );
                                }
                            }
                        }
                        _ => {
                            panic!(
                                "Attempted to mutate list with list value {} and index value {}",
                                list_object_value, index_value
                            );
                        }
                    }
                }
            };
            if self.heap.should_garbage_collect {
                self.garbage_collect()
            }
        }
    }

    fn read_byte(&mut self) -> u8 {
        let byte = self.functions[self.function_index as usize][self.instruction_index];
        self.instruction_index = self.instruction_index + 1;
        return byte;
    }

    fn read_slice(&mut self, num_bytes: usize) -> &[u8] {
        let slice = &self.functions[self.function_index as usize]
            [self.instruction_index..self.instruction_index + num_bytes];
        self.instruction_index += num_bytes;
        return slice;
    }

    fn read_u16(&mut self) -> u16 {
        let byte_array: [u8; 2] = self
            .read_slice(2)
            .try_into()
            .expect("Failed to read u16 in VM");
        return u16::from_be_bytes(byte_array);
    }

    fn read_i16(&mut self) -> i16 {
        let byte_array: [u8; 2] = self
            .read_slice(2)
            .try_into()
            .expect("Failed to read i16 in VM");
        return i16::from_be_bytes(byte_array);
    }

    fn read_i32(&mut self) -> i32 {
        let byte_array: [u8; 4] = self
            .read_slice(4)
            .try_into()
            .expect("Failed to read i32 in VM");
        return i32::from_be_bytes(byte_array);
    }

    fn read_f64(&mut self) -> f64 {
        let byte_array: [u8; 8] = self
            .read_slice(8)
            .try_into()
            .expect("Failed to read float in VM");
        return f64::from_be_bytes(byte_array);
    }

    fn read_char(&mut self) -> char {
        let char_slice = self.read_slice(4);
        // Trim padding from the end of the encoded char
        let mut end_index = 3;
        while end_index > 0 && char_slice[end_index] == 0 {
            end_index -= 1;
        }
        let trimmed_slice = &char_slice[..end_index + 1];
        let value_string = str::from_utf8(trimmed_slice)
            .expect("Failed to read char value as utf8 text when reading file");
        let value_char = value_string
            .chars()
            .next()
            .expect("Char value was empty when reading file");
        return value_char;
    }

    fn read_instruction(&mut self) -> Instruction {
        let op_code = self.read_byte();
        match op_code {
            1 => Instruction::Return,
            2 => Instruction::BuiltInFunction(self.read_byte()),
            3 => Instruction::Constant(self.read_u16()),
            4 => Instruction::Nil,
            5 => Instruction::True,
            6 => Instruction::False,
            7 => Instruction::Int(self.read_i32()),
            8 => Instruction::Float(self.read_f64()),
            9 => Instruction::Char(self.read_char()),
            10 => Instruction::Pop,
            11 => Instruction::PopMultiple(self.read_u16()),
            12 => Instruction::RemoveFromStack(self.read_u16()),
            13 => Instruction::Jump(self.read_i16()),
            14 => Instruction::JumpIfFalse(self.read_i16()),
            15 => Instruction::ReadVariable(self.read_u16()),
            16 => Instruction::MutateVariable(self.read_u16()),
            17 => Instruction::Function(self.read_u16(), self.read_byte()),
            18 => Instruction::Call(self.read_byte()),
            19 => Instruction::Record(self.read_u16(), self.read_byte()),
            20 => Instruction::Field(self.read_byte()),
            21 => Instruction::MutateField(self.read_byte()),
            22 => Instruction::JumpIfDoesntMatchRecordId(self.read_u16(), self.read_i16()),
            23 => Instruction::List(self.read_byte()),
            24 => Instruction::Index,
            25 => Instruction::MutateIndex,
            op => panic!("Got invalid op code {}", op),
        }
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value)
    }

    fn pop(&mut self) -> Value {
        self.stack
            .pop()
            .expect("Attempted to pop while the stack was empty")
    }

    fn pop_multiple(&mut self, num: usize) {
        self.stack.truncate(self.stack.len() - num);
    }

    fn pop_slice(&mut self, slice_len: usize) -> Vec<Value> {
        let stack_size = self.stack.len();
        let slice = self.stack[stack_size - slice_len..].to_vec();
        self.stack.truncate(stack_size - slice_len);
        return slice;
    }

    fn peek_from_top(&self, index: StackIndex) -> Value {
        self.stack
            .get(self.stack.len() - index as usize - 1)
            .copied()
            .expect(format!("Failed to read stack at index {}", index).as_str())
    }

    fn peek_from_frame_bottom(&self, index: StackIndex) -> Value {
        self.stack
            .get((self.stack_index + index) as usize)
            .copied()
            .expect(format!("Failed to read stack at index {}", index).as_str())
    }

    fn set_from_frame_bottom(&mut self, index: StackIndex, value: Value) {
        self.stack[(self.stack_index + index) as usize] = value
    }

    fn remove_at_index_from_top(&mut self, index: StackIndex) {
        self.stack.remove(self.stack.len() - index as usize - 1);
    }

    fn jump(&mut self, offset: i16) {
        self.instruction_index = if offset >= 0 {
            self.instruction_index + (offset as usize)
        } else {
            self.instruction_index - (-offset as usize)
        }
    }

    fn garbage_collect(&mut self) {
        let mut garbage_collector = GarbageCollector {
            reachable_objects: HashSet::<ObjectKey>::new(),
            objects_to_expand: Vec::<ObjectKey>::new(),
        };
        let reachable_objects = garbage_collector.get_reachable_objects(&self);
        self.heap.garbage_collect(reachable_objects);
    }
}

struct GarbageCollector {
    reachable_objects: HashSet<ObjectKey>,
    objects_to_expand: Vec<ObjectKey>,
}

impl GarbageCollector {
    fn get_reachable_objects(&mut self, vm: &VM) -> &HashSet<ObjectKey> {
        for value in &vm.constants {
            self.process_value(&value)
        }
        for value in &vm.stack {
            self.process_value(&value)
        }
        loop {
            match self.objects_to_expand.pop() {
                None => break,
                Some(object_key) => match vm.heap.get(object_key) {
                    Object::RecordObj {
                        record_id: _,
                        fields,
                    } => {
                        for field in fields {
                            self.process_value(field)
                        }
                    }
                    _ => {}
                },
            }
        }
        return &self.reachable_objects;
    }
    fn process_value(&mut self, value: &Value) {
        match value {
            Value::Object(key) => {
                let first_time_reached = self.reachable_objects.insert(*key);
                if first_time_reached {
                    self.objects_to_expand.push(*key)
                }
            }
            _ => {}
        }
    }
}

enum Instruction {
    Return,
    BuiltInFunction(u8),
    Constant(ConstIndex),
    Nil,
    True,
    False,
    Int(IntValue),
    Float(FloatValue),
    Char(char),
    Pop,
    PopMultiple(StackIndex),
    RemoveFromStack(StackIndex),
    Jump(InstructionOffset),
    JumpIfFalse(InstructionOffset),
    ReadVariable(StackIndex),
    MutateVariable(StackIndex),
    Function(FunctionIndex, u8),
    Call(u8),
    Record(RecordId, u8),
    Field(FieldIndex),
    MutateField(FieldIndex),
    JumpIfDoesntMatchRecordId(RecordId, InstructionOffset),
    List(u8),
    Index,
    MutateIndex,
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::Return => write!(f, "Return"),
            Instruction::BuiltInFunction(function_id) => write!(f, "Built-in {}", function_id),
            Instruction::Constant(const_index) => write!(f, "Constant {}", const_index),
            Instruction::Nil => write!(f, "Nil"),
            Instruction::True => write!(f, "True"),
            Instruction::False => write!(f, "False"),
            Instruction::Int(i) => write!(f, "Int {}", i),
            Instruction::Float(d) => write!(f, "Float {}", d),
            Instruction::Char(c) => write!(f, "Char {}", c),
            Instruction::Pop => write!(f, "Pop"),
            Instruction::PopMultiple(u) => write!(f, "PopMultiple {}", u),
            Instruction::RemoveFromStack(stack_index) => {
                write!(f, "RemoveFromStack {}", stack_index)
            }
            Instruction::Jump(offset) => write!(f, "Jump {}", offset),
            Instruction::JumpIfFalse(offset) => write!(f, "JumpIfFalse {}", offset),
            Instruction::ReadVariable(stack_index) => {
                write!(f, "ReadVariable {}", stack_index)
            }
            Instruction::MutateVariable(stack_index) => {
                write!(f, "MutateVariable {}", stack_index)
            }
            Instruction::Function(function_index, num_closed) => {
                write!(f, "Function {} {}", function_index, num_closed)
            }
            Instruction::Call(num_arguments) => write!(f, "Call {}", num_arguments),
            Instruction::Record(record_id, num_fields) => {
                write!(f, "Record {} {}", record_id, num_fields)
            }
            Instruction::Field(field_index) => write!(f, "Field {}", field_index),
            Instruction::MutateField(field_index) => write!(f, "MutateField {}", field_index),
            Instruction::JumpIfDoesntMatchRecordId(record_id, offset) => {
                write!(f, "JumpIfDoesntMatchRecordId {} {}", record_id, offset)
            }
            Instruction::List(list_length) => write!(f, "List {}", list_length),
            Instruction::Index => write!(f, "Index"),
            Instruction::MutateIndex => write!(f, "MutateIndex"),
        }
    }
}
