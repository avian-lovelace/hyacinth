# Records

In Hyacinth, record types can be defined with a `rec` statement.

```
rec Dog = [
    name: String,
    age: Int
];

let myPet = Dog[
    name = "Fido",
    age = 4,
];
myPet.name>>printLine[];
myPet.age>>printLine[];

// Outputs:
// Fido
// 4
```

If a record has no fields, it is instantiated by with just the record name, no brackets.
```
rec Empty = [];
let myEmpty = Empty;
// not let myEmpty = Empty[];
```

## Record Unions & Case Expressions

Record union types let an identifier hold values of multiple possible record types. If all record types in the union share a field with the same name and [compatible types](misc.md#union-compatible-types), that field value can be used.
```
rec Foo = [
    shared: Int,
    justFoo: String,
];
rec Bar = [
    shared: Int,
    justBar: Bool,
];

let mut x: Foo | Bar = Foo[shared = 1, justFoo = "foo"];
x.shared>>printLine[];
mut x = Bar[shared = 2, justBar = true];
x.shared>>printLine[];

// Outputs:
// 1
// 2
```

Case expressions allow for branching based on the record type of a value.
```
rec Foo = [value: String];
rec Bar = [];

func printFooBar = [fb: Foo | Bar]: Nil -> case fb of [
    Foo: f -> {
        f.value>>printLine[];
    },
    Bar: b -> {
        "Bar">>printLine[];
    },
];

printFooBar[Foo[value = "Foo"]];
printFooBar[Bar];

// Outputs:
// Foo
// Bar
```

## Type parameters

Records may take type parameters
```
rec Pair = ⟨T⟩ => [
    first: T,
    second: T,
];

let point = Pair⟨Float⟩[first = 1.23, second = 4.5];
point.first>>printLine[];
point.second>>printLine[];

// Outputs:
// 1.23
// 4.5
```

## Mutability

Records are immutable by default, but a record instance can be made mutable with the `mut` keyword. The fields of a mutable record can be modified using `mut` statements.
```
rec Box = [value: Int];

let myBox = mut Box[value = 1];
myBox.value>>printLine[];
mut myBox.value = 2;
myBox.value>>printLine[];

// Output
// 1
// 2
```

The `mut` keyword can also be used in type annotations to require a mutable record.
```
rec Box = [value: Int];

func doubleValue = [box: mut Box]: Nil -> {
    mut box.value = box.value * 2;
};

let myBox = mut Box[value = 4];
myBox.value>>printLine[];
doubleValue[myBox];
myBox.value>>printLine[];

// Outputs:
// 4
// 8
```

## Mutability parameters

By default, mutability of a record type is propagated to its field types. This is intended to keep simple the common case where a nested record should be deeply mutable or deeply immutable.
```
rec Foo = [a: Bar];
rec Bar = [b: Int];

let mutableFoo = mut Foo[
    a = mut Bar[
        b = 1
    ]
];
mutableFoo.a.b>>printLine[];
mut mutableFoo.a.b = 2;
mutableFoo.a.b>>printLine[];

let immutableFooCopy: Foo = mutableFoo;
// The following would result in a type error, as immutableFooCopy is deeply immutable
// mut immutableFooCopy.a.b = 3; 

// Outputs:
// 1
// 2
```

However, record field mutability can instead be controlled manually by adding a mutability parameter to a record declaration. If used, the mutability parameter must be the first element of the type parameter list and be prefixed by `mut`.
```
rec Foo = ⟨mut M⟩ => [
    // field a is always immutable
    a: Bar,
    // field b is always immutable
    b: mut Bar,
    // field c has the same mutability as Foo
    c: M Bar,
];
rec Bar = [value: Int];
```