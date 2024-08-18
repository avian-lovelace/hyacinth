# TODO

## Features
 - Add more debugging support to compiler/vm
 - Compilation warnings
 - Effects
 - Add compile-time optimizations
 - Importing and exporting from modules
 - Type assertion expressions
 - Type parameter inference for functions

## Technical debt
 - Improve error reporting in parts of the parser that use parser combinators
 - Update identifier binding to separate out type identifiers and value identifiers
 - Consolidate error messages for identifiers usage not matching their definition in identifier binding
 - Add cases to typeCheckExpression for built-in expressions like the one for NegateExpression
 - Allow circular references in type synonym definitions when a reasonable final type can be determined (e.g. type ConsList = Pair⟨Int, Maybe⟨ConsList⟩⟩)
 - Throw an error if a mutability annotation is provided to a type synonym without a mutability parameter
 - Variance calculation currently traverses each type expression every time its variance is recalculated. For performance, we could instead traverse the type expression once to create a function from the in-calculation variances to the expression's variance, then just call the returned function each iteration.
 - For getting the type info of built-in functions, we write out IBTypeExpression values to parse. However, this requires using dummy data in a few places. It would be better to instead create a special type instance of TypeExpression specifically for built-ins that has only the needed data.

## Bugs
 - [ ] [BUG-1] Parsing errors in record expression fields get eaten
 - [X] [BUG-2] Garbage collector doesn't look at object fields
 - [X] [BUG-3] If a scope is created when there are non-variable values on the stack, variables in that scope are not found correctly
 - [X] [BUG-4] The type checking phase assumes that immutable records are covariant in all their type parameters, and that mutable records are invariant in all their type parameters. This is unsound, but I think is correct more often than not. To make the type system sound, we should add variance annotations to type parameters and/or implement automatic variance determination in the type checking phase. Note that syntax for variance annotations may be inelegant, as even in simple cases, a type parameter may want a different variance depending on the record mutability.
 - [ ] [BUG-5] If compilation fails, the compiler process should exit with an error code