# Core Concepts

## Statements

A Hyacinth program consists of a sequence of statements, separated by semicolons. We've already seen and example of an expression statement in [Running the compiler](getting_started.md#running-the-compiler) that evaluates an expression for its side effects. Other types of statement will be covered later in the docs.

## Values
Hyacinth has six core value types:

 - `Int` - A 32-bit signed integer
 - `Float` - A 64-bit floating point number
 - `String` - UTF-8 encoded text
 - `Char` - A single UTF-8 encoded Unicode code point
 - `Bool` - A boolean value, `True` or `False`
 - `Nil` - A type with a single possible value, `nil`

Each of the core value types can be created via literal expressions and output via print statements.
```
printLine⟨Int⟩[123];
printLine⟨Float⟩[3.14];
printLine⟨Char⟩['a'];
printLine⟨String⟩["Hyacinth"];
printLine⟨Bool⟩[true];
printLine⟨Nil⟩[nil];

// Outputs:
// 123
// 3.14
// a
// Hyacinth
// true
// nil
```

## Operators
Hyacinth includes various operators that act on these core value types:

* Arithmetic: `+`, `-`, `*`, `/`, `%`
* Boolean: `&&`, `||`, `!`
* Equality: `==`, `!=`
* Comparison: `>`, `<`, `>=`, `>=`
* String concatenation: `+`
```
printLine⟨Int⟩[1 + 2 * 3];
printLine⟨Bool⟩[15 % 4 == 3 && 6 > -13];
printLine⟨String⟩["concatenating" + ' ' + "strings"];

// Outputs:
// 7
// true
// concatenating strings
```