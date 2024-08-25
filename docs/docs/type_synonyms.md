## Type Synonyms

Type synonyms allow a custom name to be assigned to a type.
```
type FileName = String;
type Extension = String;

func printFile = [fileName: FileName, extension: Extension]: Nil -> {
    (fileName + '.' + extension)>>printLine[];
};

printFile["myDoc", "txt"];

// Outputs:
// myDoc.txt
```

A type synonym is identical to its value in type checking. Two different synonyms for the same type can be substituted.
```
type Latitude = Float;
type Longitude = Float;

func printCoordinates = [lat: Latitude, lon: Longitude]: Nil -> {
    lat>>printLine[];
    lon>>printLine[];
};

let currentLat: Latitude = 32.1;
let currentLon: Longitude = -0.98;

// Oops, reversed the coordinates
printCoordinates[currentLon, currentLat];

// Outputs:
// -0.98
// 32.1
```

Type synonyms may have type parameters.
```
type Maybe = ⟨T⟩ => Just⟨T⟩ | Nothing;
rec Just = ⟨T⟩ => [value: T];
rec Nothing = [];

func getValueOrDefault = ⟨T⟩ => [maybe: Maybe⟨T⟩, default: T]: T ->
    case maybe of [
        Just: j -> j.value,
        Nothing: n -> default,
    ];

Just⟨Bool⟩[value = true]>>getValueOrDefault⟨Bool⟩[false]>>printLine[];

// Outputs:
// true
```

Type synonyms may also have mutability parameters.
```
type Tree = ⟨mut M, T⟩ => M Leaf | Node⟨T⟩;
rec Leaf = [];
rec Node = ⟨T⟩ => [value: T, left: Tree⟨T⟩, right: Tree⟨T⟩];

let mutableTree: mut Tree⟨Int⟩ = mut Node[
    value = 1,
    left = mut Node[
        value = 2,
        left = Leaf,
        right = Leaf,
    ],
    right = Leaf,
];
```