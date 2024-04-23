use std::collections::HashMap;
use std::collections::HashSet;
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
    Char(char),
    Object(ObjectKey),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Int(i) => write!(f, "{}", i),
            Value::Double(d) => write!(f, "{}", d),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Char(c) => write!(f, "{}", c),
            Value::Object(k) => write!(f, "Object {}", k),
        }
    }
}

pub enum Object {
    StringObj(String),
}

pub type ObjectKey = u32;

pub struct Heap {
    objects: HashMap<ObjectKey, Object>,
    next_key: ObjectKey,
}

impl Heap {
    pub fn new() -> Self {
        Heap {
            objects: HashMap::new(),
            next_key: 0,
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
        return Value::Object(key);
    }

    pub fn get(&self, key: ObjectKey) -> &Object {
        self.objects
            .get(&key)
            .expect("Attempted to get invalid object")
    }

    pub fn garbage_collect(&mut self, reachable_keys: HashSet<ObjectKey>) {
        self.objects.retain(|k, _| reachable_keys.contains(k))
    }
}
