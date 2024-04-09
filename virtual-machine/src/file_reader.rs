use crate::core::{Chunk, Value};

pub fn read_chunk(bytes: Vec<u8>) -> Chunk {
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
