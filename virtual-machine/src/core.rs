use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt;

pub struct VM {
    pub functions: Vec<Vec<u8>>,
    pub constants: Vec<Value>,
    pub function_index: FunctionIndex,
    pub instruction_index: InstructionIndex,
    pub stack_index: StackIndex,
    pub frames: Vec<Frame>,
    pub stack: Vec<Value>,
    pub heap: Heap,
}

#[derive(Clone, Copy)]
pub enum Value {
    Nil,
    Int(IntValue),
    Float(FloatValue),
    Bool(bool),
    Char(char),
    Object(ObjectKey),
    BuiltInFunction(BuiltInFunction),
}

pub type IntValue = i32;
pub type FloatValue = f64;

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Int(i) => write!(f, "{}", i),
            Value::Float(d) => write!(f, "{}", d),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Char(c) => write!(f, "{}", c),
            Value::Object(k) => write!(f, "Object {}", k),
            Value::BuiltInFunction(id) => write!(f, "{:?}", id),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum BuiltInFunction {
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Not,
    Equal,
    NotEqual,
    Greater,
    Less,
    GreaterEqual,
    LessEqual,
    Print,
    PrintLine,
    ReadLine,
}

impl From<u8> for BuiltInFunction {
    fn from(value: u8) -> Self {
        return match value {
            1 => BuiltInFunction::Negate,
            2 => BuiltInFunction::Add,
            3 => BuiltInFunction::Subtract,
            4 => BuiltInFunction::Multiply,
            5 => BuiltInFunction::Divide,
            6 => BuiltInFunction::Modulo,
            7 => BuiltInFunction::Not,
            8 => BuiltInFunction::Equal,
            9 => BuiltInFunction::NotEqual,
            10 => BuiltInFunction::Greater,
            11 => BuiltInFunction::Less,
            12 => BuiltInFunction::GreaterEqual,
            13 => BuiltInFunction::LessEqual,
            14 => BuiltInFunction::Print,
            15 => BuiltInFunction::PrintLine,
            16 => BuiltInFunction::ReadLine,
            id => panic!("Got invalid built-in function id {}", id),
        };
    }
}

pub enum Object {
    StringObj(String),
    FunctionObj {
        function_index: FunctionIndex,
        closed_variables: Vec<Value>,
    },
    RecordObj {
        record_id: RecordId,
        fields: Vec<Value>,
    },
    ListObj {
        values: Vec<Value>,
    },
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::StringObj(s) => write!(f, "StringObj {}", s),
            Object::FunctionObj {
                function_index,
                closed_variables: _,
            } => write!(f, "FunctionObj {}", function_index),
            Object::RecordObj { record_id, fields } => {
                write!(f, "RecordObj {} [", record_id)?;
                for field in fields {
                    write!(f, "{} ", field)?
                }
                write!(f, "]")
            }
            Object::ListObj { values } => {
                write!(f, "ListObj [")?;
                for value in values {
                    write!(f, "{} ", value)?
                }
                write!(f, "]")
            }
        }
    }
}

pub type ObjectKey = u32;

const INITIAL_GC_THRESHOLD: usize = 1024;
const GC_THRESHOLD_MULTIPLIER: usize = 2;

pub struct Heap {
    objects: HashMap<ObjectKey, Object>,
    next_key: ObjectKey,
    pub should_garbage_collect: bool,
    garbage_collection_threshold: usize,
}

impl Heap {
    pub fn new() -> Self {
        Heap {
            objects: HashMap::new(),
            next_key: 0,
            should_garbage_collect: false,
            garbage_collection_threshold: INITIAL_GC_THRESHOLD,
        }
    }

    /*  Accessing objects through the heap map is likely slow, and ideally we would give each Object value a direct
    reference to the associated Object on the heap. However, it's difficult to make the lifetimes work out with garbage
    collection. Eventually, we probably want to use some combination of raw pointers, Box, and Pin to let this work.
    */
    pub fn add(&mut self, obj: Object) -> Value {
        let key = self.next_key;
        self.objects.insert(key, obj);
        self.next_key += 1;
        self.should_garbage_collect = self.objects.len() > self.garbage_collection_threshold;
        return Value::Object(key);
    }

    pub fn get(&self, key: ObjectKey) -> &Object {
        self.objects
            .get(&key)
            .expect("Attempted to get invalid object")
    }

    pub fn get_mut(&mut self, key: ObjectKey) -> &mut Object {
        self.objects
            .get_mut(&key)
            .expect("Attempted to get invalid object")
    }

    pub fn garbage_collect(&mut self, reachable_keys: &HashSet<ObjectKey>) {
        self.objects.retain(|k, _| reachable_keys.contains(k));
        self.garbage_collection_threshold *= GC_THRESHOLD_MULTIPLIER;
        self.should_garbage_collect = false;
    }
}

pub struct Frame {
    pub function_index: FunctionIndex,
    pub instruction_index: InstructionIndex,
    pub stack_index: StackIndex,
}

pub type ConstIndex = u16;
pub type StackIndex = u16;
pub type InstructionOffset = i16;
pub type InstructionIndex = usize;
pub type FunctionIndex = u16;
pub type RecordId = u16;
pub type FieldIndex = u8;
