# Functions

In Hyacinth, function declarations statements can be used to define functions.
```
func sumSquare = [x: Int, y: Int]: Int -> x * x + y * y;
print sumSquare[3, 4];

// Outputs:
// 25
```

Functions defined via function declaration statements may be referenced earlier in a scope than their definition.
```
printFoo[];
func printFoo = []: Nil -> {
    print "Foo";
}

// Outputs:
// Foo
```

They also may be self-recursive or mutually recursive.
```
func triangle = [n: Int]: Int ->
    if n <= 0
        then 0
        else n + triangle[n-1];
print triangle[5];

// Outputs:
// 15
```

## Return Statements

Return statements can be used to exit out of a function immediately and return a value.
```
func myFunc = [x: Float]: Float -> {
    if x <= 0.0 then {
        return -x * 0.5;
    };
    print "input was positive";
    return x * 2.0;
};

print myFunc[-1.2];
print myFunc[1.2];

// Outputs:
// 0.6
// input was positive
// 2.4
```

Note that in the previous example, the body of `myFunc` is a block, which has type `Nil`, but `myFunc` has return type `Float`. In Hyacinth, if the body of a function is a block, and the compiler can determine that running the function will always execute a return statement, then the return type of the function is based only on the types of the values in the return statements.

Return statements can be used without a return value to return `nil`.
```
func countdown = [x: Int]: Nil -> {
    if x <= 0 then {
        print "Happy New Year!";
        return;
    };
    print x;
    countdown[x - 1];
};
countdown[3];

// Outputs:
// 3
// 2
// 1
// Happy New Year!
```

## Function Expressions

Functions can also be created inline with function expressions.
```
print applyTwice[
    // Function expression
    [x] -> x * 3,
    1
];

func applyTwice = [f: [Int] -> Int, x: Int]: Int -> f[f[x]];
// Outputs:
// 9
```

Function expressions can be combined with variables to get something like a function declaration statement.
```
let sumSquare = [x: Int, y: Int]: Int -> x * x + y * y;
print sumSquare[3, 4];

// Outputs:
// 25
```

However, since variables can only be referenced after their declaration, this does not (straightforwarly) permit self-recursive and mutually recursive functions.

## Variable Capturing

Functions can capture variables. That is, the body of a function can reference an identifier defined in an enclosing scope.
```
let message = "Have a nice day :)";
let printMessage = []: Nil -> {
    print message;
};
printMessage[];

// Outputs:
// Have a nice day :)
```

In Hyacinth, variables are captured statically, and if a captured variable is mutated after capture, the captured value will not change. For function expressions, variables are captured when the function is defined.
```
let mut x = 1;
let printCapturedValue = []: Nil -> {
    print x;
};
mut x = 2;
print x;
printCapturedValue[];

// Outputs:
// 2
// 1
```

For functions defined by a function declaration statement, variables are instead captured at the point where the function is referenced.
```
let mut x = 1;
let firstReference = printCapturedValue;
mut x = 2;
printCapturedValue[];
mut x = 3;
print x;
firstReference[];

func printCapturedValue = []: Nil -> {
    print x;
};

// Outputs:
// 2
// 3
// 1
```

## Type Parameters

Functions defined by a function declaration statement may take type parameters. If a function has type parameters, type arguments must be explicitly provided when the function is referenced.
```
func apply = ⟨T, V⟩ => [f: [T] -> V, x: T]: V -> f[x];
print apply⟨Int, Bool⟩[[x] -> x > 0, 5];

// Outputs true
```