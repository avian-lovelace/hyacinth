# Variables

Values can be stored and retrieved later using variables.
```
let foo = 5;
print⟨Int⟩[foo];

// Ouptuts:
// 5
```

Variable declarations may include type annotations. In many cases, the compiler can infer the type of a variable from its initial value, and the type annotation can be omitted.
```
let bar: Bool = false;
print⟨Bool⟩[bar];

// Ouptuts:
// false
```

## Mutability
Variables are immutable by default but can be declared mutable with a `let mut` statement. If declared mutable, variables can be mutated later with `mut` statements.
```
let mut baz = 1;
printLine⟨Int⟩[baz];
mut baz = 2;
printLine⟨Int⟩[baz];
mut baz = baz * 2;
printLine⟨Int⟩[baz];

// Outputs;
// 1
// 2
// 4
```