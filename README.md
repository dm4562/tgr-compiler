# CS 4240

A Rust compiler for the Tiger language.

## Requirements

In order to compile this program, the latest **Stable** version of [Rust](https://www.rust-lang.org/) is required.


### Installing Rust

Because the installation process to Rust requires interactivity, this must be done **before** the grading script can be run. Official instructions on how to install Rust can be found [here](https://www.rust-lang.org/en-US/install.html). A quick summary of how to install Rust toolchains:

```
$ curl https://sh.rustup.rs -sSf | sh
```

Be sure to have `~/.cargo/bin` directory as part of the `$PATH` variable.


## Compiling

To compile the compiler, navigate to the root of the repository and run:

```
$ cargo build
```

To compile the program with optimizations, run:

```
$ cargo build --release
```


## Running Project 1

The compiled binaries are located in the *target* folder, and `cargo` offers a convenient, single command way to compile and run the code.

For example, to run the compiler on a file named `everything.tgr`, execute:

```
$ cargo run --release -- < everything.tgr
```

For the grade script, `compiler` handles this command, but because the compiler reads from stdin, the proper command in the script should be:

```
./compiler < $f > ${f%.tgr}.tokens
```