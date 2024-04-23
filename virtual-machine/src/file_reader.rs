use crate::core::{Chunk, Heap, Value};
use std::str;

pub fn read_chunk(bytes: Vec<u8>, heap: &mut Heap) -> Chunk {
    let mut f = FileState { bytes, index: 0 };

    let constants_length = f.read_int() as usize;
    let mut constants: Vec<Value> = Vec::new();
    for _ in 0..constants_length {
        match f.read_byte() {
            1 => constants.push(Value::Int(f.read_int())),
            2 => constants.push(Value::Double(f.read_double())),
            3 => {
                let num_bytes = f.read_byte();
                let value_slice = f.read_byte_slice(num_bytes as usize);
                let value_string = str::from_utf8(value_slice)
                    .expect("Failed to read char value as string when reading constants");
                let value_char = value_string
                    .chars()
                    .next()
                    .expect("Char value was empty when reading constants");
                constants.push(Value::Char(value_char));
            }
            4 => {
                let num_bytes = f.read_int();
                let value_slice = f.read_byte_slice(num_bytes as usize);
                let value_string = str::from_utf8(value_slice)
                    .expect("Failed to read string value when reading constants")
                    .to_owned();
                let object_value = heap.add(crate::core::Object::StringObj(value_string));
                constants.push(object_value);
            }
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

    fn read_byte_slice(&mut self, num_bytes: usize) -> &[u8] {
        let bytes = &self.bytes[self.index..self.index + num_bytes];
        self.index = self.index + num_bytes;
        return bytes;
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
