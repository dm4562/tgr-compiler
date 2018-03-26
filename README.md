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

Alternatively:

```
$ make debug
```

To compile the program with optimizations, run:

```
$ cargo build --release
```

Alternatively,

```
$ make
```

## Running Project Submissions

The compiled binaries are located in the *target* folder, and `cargo` offers a convenient, single command way to compile and run the code. Due to how `cargo` handles the build process the directory `p1` could not be created.

For example, to run the compiler on a file named `everything.tgr` and print the tokens to stdout, execute:

```
$ cargo run --release -- everything.tgr --tokens
```

For the grade script, `compiler` handles this command, as an example to print out tokens of `everything.tgr` execute the following:

```
$ ./compiler program.tgr --tokens > program.tokens
```

**Before running the scripts make sur to create a `tests` directory with the appropriate tests required to run the grading script.**

### Running Project 1 Tests

```
$ ./p1_grade.sh
```

### Running Project 2 Tests

```
$ ./p2_grade.sh
```