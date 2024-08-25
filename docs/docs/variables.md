# Variables

Values can be stored and retrieved later using variables.
```
let foo = 5;
foo>>printLine[];

// Ouptuts:
// 5
```

Variable declarations may include type annotations. In many cases, the compiler can infer the type of a variable from its initial value, and the type annotation can be omitted.
```
let bar: Bool = false;
bar>>printLine[];

// Ouptuts:
// false
```

## Mutability
Variables are immutable by default but can be declared mutable with a `let mut` statement. If declared mutable, variables can be mutated later with `mut` statements.
```
let mut baz = 1;
baz>>printLine[];
mut baz = 2;
baz>>printLine[];
mut baz = baz * 2;
baz>>printLine[];

// Outputs;
// 1
// 2
// 4
```