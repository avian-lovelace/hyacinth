# TODO

## Features
 - Record union types/switch expressions
 - Type synonyms
 - Lists
 - Garbage collection at runtime
 - Add more debugging support to compiler/vm
 - Compilation warnings
 - Short-circuiting for && and || operators
 - Effects
 - Add compile-time optimizations

## Technical debt
 - Improve error reporting in parts of the parser that use parser combinators
 - Consider whether the function lifting phase should be a full intermediate representation generation phase to prepare for potential optimization

## Bugs
 - [ ] [BUG-1] Parsing errors in record expression fields get eaten
 - [ ] [BUG-2] Garbage collector doesn't look at object fields
