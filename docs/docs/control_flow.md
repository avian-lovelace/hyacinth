# Control Flow

## Block Expressions
Block expressions group together a sequence of statements. When a block expression is evaluated, the statements in the block are evaluated in order, then the block expression evaluates to `nil`.

```
print {
    print "This happens first";
};

// Outputs:
// This happens first
// nil
```

## If expressions
If expressions allow for code branching depending on a condition. They can be used as part of expressions for their value.

```
let a = 5;
let b = 3;
let max = if a > b then a else b;
print max;

// Outputs:
// 5
```

If expressions can also be used for the side-effects of their branches, often in combination with block expressions.

```
if 1 + 1 == 2 then {
    print "1 + 1 is 2";
} else {
    print "1 + 1 is not 2";
};

// Outputs:
// 1 + 1 is 2
```

When the `true` branch of an if expressions has type `Nil`, the `false` branch may be omitted.

```
if !(true && false) then {
    print "evaluated to true";
};

// Outputs:
// evaluated to true
```

## While loop statements
While loop statements evaluate their body expression repeatedly until their condition is false.

```
let mut a = 1;
while a < 10 loop {
    print a;
    mut a = a * 3;
};

// Outputs: 
// 1
// 3
// 9
```