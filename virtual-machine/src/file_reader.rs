use crate::core::{Chunk, Heap, Value};
use std::str;

pub fn read_chunk(bytes: Vec<u8>, heap: &mut Heap) -> Chunk {
    let mut f = FileState { bytes, index: 0 };

    let constants_length = f.read_u16() as usize;
    let mut constants: Vec<Value> = Vec::new();
    for _ in 0..constants_length {
        match f.read_byte() {
            1 => {
                let num_bytes = f.read_u16();
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

    fn read_u16(&mut self) -> u16 {
        let byte_array: [u8; 2] = self.bytes[self.index..self.index + 2]
            .try_into()
            .expect("Failed to read u16 when parsing file");
        let int = u16::from_be_bytes(byte_array);
        self.index = self.index + 2;
        return int;
    }

    fn read_rest(&mut self) -> Vec<u8> {
        return self.bytes[self.index..].to_vec();
    }
}
