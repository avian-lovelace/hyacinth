# Functions

In Hyacinth, function declarations statements can be used to define functions.
```
func sumSquare = [x: Int, y: Int]: Int -> x * x + y * y;
sumSquare[3, 4]>>printLine[];

// Outputs:
// 25
```

Functions defined via function declaration statements may be referenced earlier in a scope than their definition.
```
printFoo[];
func printFoo = []: Nil -> {
    "Foo">>printLine[];
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
triangle[5]>>printLine[];

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
    "input was positive">>printLine[];
    return x * 2.0;
};

myFunc[-1.2]>>printLine[];
myFunc[1.2]>>printLine[];

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
        "Happy New Year!">>printLine[];
        return;
    };
    x>>printLine[];
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
applyTwice[
    // Function expression
    [x] -> x * 3,
    1
]>>printLine[];

func applyTwice = [f: [Int] -> Int, x: Int]: Int -> f[f[x]];
// Outputs:
// 9
```

Function expressions can be combined with variables to get something like a function declaration statement.
```
let sumSquare = [x: Int, y: Int]: Int -> x * x + y * y;
sumSquare[3, 4]>>printLine[];

// Outputs:
// 25
```

However, since variables can only be referenced after their declaration, this does not (straightforwardly) permit self-recursive and mutually recursive functions.

## Variable Capturing

Functions can capture variables. That is, the body of a function can reference an identifier defined in an enclosing scope.
```
let message = "Have a nice day :)";
let printMessage = []: Nil -> {
    message>>printLine[];
};
printMessage[];

// Outputs:
// Have a nice day :)
```

In Hyacinth, variables are captured statically, and if a captured variable is mutated after capture, the captured value will not change. For function expressions, variables are captured when the function is defined.
```
let mut x = 1;
let printCapturedValue = []: Nil -> {
    x>>printLine[];
};
mut x = 2;
x>>printLine[];
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
printLine⟨Int⟩[x];
firstReference[];

func printCapturedValue = []: Nil -> {
    x>>printLine[];
};

// Outputs:
// 2
// 3
// 1
```

## Type Parameters

Functions defined by a function declaration statement may take type parameters. If a function has type parameters, type arguments can be provided when the function is referenced.
```
func apply = ⟨T, V⟩ => [f: [T] -> V, x: T]: V -> f[x];
apply⟨Int, Bool⟩[[x] -> x > 0, 5]>>printLine[];

// Outputs:
// true
```

In some cases, a function's type arguments can be inferred from known type information by the compiler, and the type arguments can be omitted.
```
func printAndReturn = ⟨T⟩ => [x: T]: T -> {
    x>>printLine[];
    return x;
};
let foo: Int = printAndReturn[3];

// Outputs:
// 3
```

## Built-in Functions

Hyacinth includes a number of built-in functions that give access to otherwise unavailable functionality. Below are some of the built-in functions provided by Hyacinth. Some more built-in functions are covered in the [list documentation](lists.md).

 - `print: ⟨T⟩ => [T] -> Nil` - Outputs a value to the standard output
 - `printLine: ⟨T⟩ => [T] -> Nil` - Outputs a value to the standard output with a line break after
 - `readLine: [] -> String` - Reads a line from the standard input and returns it as a string


## Method Call Operator

The method call operator (`>>`) is an alternate syntax for calling functions.
```
func myFunc = [x: String, y: String]: Nil -> {
    ("The first parameter is " + x)>>printLine[];
    ("The second parameter is " + y)>>printLine[];
};

let foo = "foo";
let bar = "bar";
foo>>myFunc[bar];

// Outputs:
// The first parameter is foo
// The second parameter is bar
```

Unlike some other languages, the method call operator does not enable dynamic dispatch; it is equivalent at run-time to a normal function call. However, using method call syntax can help to write cleaner code in a couple ways.

Method calls can be chained, passing the output of one function as the first parameter of the next.
```
func double = [x: Int]: Int -> 2 * x;
func addOne = [x: Int]: Int -> x + 1;

3>>double[]>>addOne[]>>double[]>>printLine[];

// Outputs:
// 14
```

With method call syntax, the compiler uses the type of the first parameter when inferring a function's type arguments. This in some cases allows omitting a function's type arguments when a normal function call would require passing them explicitly.
```
func printIfPositive = ⟨T⟩ => [printValue: T, switchValue: Int]: Nil ->
    if switchValue > 0 then {
        printValue>>printLine[];
    };

// With normal function call syntax, explicit type arguments are required
printIfPositive⟨String⟩["foo", 5];

// With method call syntax, type arguments can be inferred
"foo">>printIfPositive[5];


// Outputs:
// foo
// foo
```