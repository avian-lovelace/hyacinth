# Core Concepts

## Statements

A Hyacinth program consists of a sequence of statements, separated by semicolons. We've already seen print statements in [Running the compiler](getting_started.md#running-the-compiler), and other statement types will be covered later in the docs.

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
print 123;
print 3.14;
print 'a';
print "Hyacinth";
print true;
print nil;

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
print 1 + 2 * 3;
print 15 % 4 == 3 && 6 > -13;
print "concatenating" + ' ' + "strings";

// Outputs:
// 7
// true
// concatenating strings
```