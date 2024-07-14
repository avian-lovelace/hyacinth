# Getting Started

## Installation

There are currently no pre-built binaries available for Hyacinth. If you'd like to try out Hyacinth, you can build it from source.

### Prerequisites

To build Hyacinth, you will need the following tools:

 - [Stack - The Haskell tool stack](https://docs.haskellstack.org)
 - [The Rust toolchain](https://www.rust-lang.org/tools/install)

### Building

To begin, clone this repository to your computer.
```
git clone https://github.com/avian-lovelace/hyacinth.git
cd hyacinth
```

The Hyacinth compiler can be built with Stack.
```
cd compiler
stack install
```

The Hyacinth bytecode VM can be build with Cargo.
```
cd ../virtual-machine
cargo install --path .
```

## Running the compiler

Let's write our first Hyacinth program! Create a file named `Hello.hyc`, and write the following Hyacinth code to it.
```
print "Hello, World!";
```

Navigate to the file location in a terminal and run the following command to compile your Hyacinth code file.
```
hyacinth Hello.hyc
```

This should have created a file `Hello.hyb` in the current directory. This is the compiled Hyacinth bytecode file. To run the program, run the following command.
```
hyacinth-vm Hello.hyb
```

This should print `Hello, World!` to the command line.

You've now installed and run the Hyacinth compiler! For more information on the design of the Hyacinth language, keep reading these docs.