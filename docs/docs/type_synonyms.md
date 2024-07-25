## Type Synonyms

Type synonyms allow a custom name to be assigned to a type.
```
type FileName = String;
type Extension = String;

func printFile = [fileName: FileName, extension: Extension]: Nil -> {
    printLine⟨String⟩[fileName + '.' + extension];
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
    printLine⟨Latitude⟩[lat];
    printLine⟨Longitude⟩[lon];
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

print⟨Bool⟩[getValueOrDefault⟨Bool⟩[Just[value = true], false]];

// Outputs:
// true
```

Type synonyms may also have mutability parameters.
```
type List = ⟨mut M, T⟩ => M Empty | Cons⟨T⟩;
rec Empty = [];
rec Cons = ⟨T⟩ => [value: T, next: List⟨T⟩];

let mutableList: mut List⟨Int⟩ = mut Cons[
    value = 1,
    next = mut Cons[
        value = 2,
        next = Empty,
    ]
];
```