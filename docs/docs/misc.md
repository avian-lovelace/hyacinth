# Other topics

## Type Inference

As seen in previous examples, in some cases Hyacinth can infer variable types, function parameter types, and type arguments, and type annotations can be omitted. The Hyacinth compiler uses bidirectional type-checking, so information about type expectations flows from the outside in. Unlike some other systems using Hidley-Milner style type inference, Hyacinth does not make any type inferences about identifiers from how they are used.

## Union-compatible Types

If expressions, case expressions, and field acces on a record union all create an expression could be one of multiple types. If all branches have the same type, then there's no problem, and the expression has the type of the branches. However, the branches are allowed to have different types only if they meet the following conditions

 - The type of each branch is a record type or record union type
 - If a record type appears in multiple braches, it must have the same type parameters in each branch

For example, here's an example of valid code with if expressions.
```
// combo1 has type Foo⟨Int⟩ | Bar
let combo1 = if _condition_
    then Foo⟨Int⟩[value = 5]
    else Bar
// combo2 has type Foo⟨Int⟩ | Baz
let combo2 = if _condition_
    then Foo⟨Int⟩[value = 10]
    else Baz
// combo3 has type Foo⟨Int⟩ | Bar | Baz
let combo3 = if _condition_
    then combo1
    else combo2

rec Foo = ⟨T⟩ => [value: T];
rec Bar = [];
rec Baz = [];
```

And here's an example of valid code with field access on a record union.
```
rec A = [value: X];
rec B = [value: Y];
rec X = [];
rec Y = [];

let foo: A | B = _value_;
// bar has type X | Y
let bar = foo.value;
``` 

The following example is invalid, as one branch is a non-record type.
```
rec Foo = [];

let bar = if _condition_
    then Foo
    else 5;
```

And this final example is invalid because braches have different type parameters for the same record.
```
rec Box = ⟨T⟩ => [value: T];

let foo = if _condition_
    then Box⟨Int⟩[value = 3]
    else Box⟨String⟩[value = "foo"];
```