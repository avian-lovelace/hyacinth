use std::fmt;

pub struct Chunk {
    pub code: Vec<u8>,
    pub constants: Vec<Value>,
}

#[derive(Clone, Copy)]
pub enum Value {
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
