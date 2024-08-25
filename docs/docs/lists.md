# Lists

Hyacinth provides a built-in list type that can contain multiple values of the same type. Values can be retrieved from a list with `#`, the list indexing operator.
```
let myVals = List⟨String⟩["foo", "bar", "baz"];
myVals#0>>printLine[];
myVals#2>>printLine[];

// Outputs:
// foo
// baz
```

The number of elements in a list can be determined using the built-in `length` function.
```
let myVals = List⟨String⟩["foo", "bar", "baz"];
myVals>>length[]>>printLine[]

// Outputs:
// 3
```

## Mutability

Lists are immutable by default, but a list can be declared as mutable with the `mut` keyword. The values of a mutable list can be modified using `mut` statements.
```
let myVals = mut List⟨Int⟩[1, 2, 4, 8, 16, 32];
myVals#5>>printLine[];
mut myVals#5 = 31;
myVals#5>>printLine[];

// Outputs:
// 32
// 31
```

Elements can be added or removed from the end of a mutable list using the built-in `push` and `pop` functions.
```
func sumLast = [nums: mut List⟨Int⟩]: mut List⟨Int⟩ -> {
    let x = nums>>pop[];
    let y = nums>>pop[];
    return nums>>push[x + y];
};

let myVals = mut List⟨Int⟩[1, 3, 5, 7];
myVals>>sumLast[];
myVals#0>>printLine[];
myVals#1>>printLine[];
myVals#2>>printLine[];

// Outputs:
// 1
// 3
// 12
```