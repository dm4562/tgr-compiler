extern crate clap;
#[macro_use]
extern crate lazy_static;
extern crate regex;
extern crate serde_json;

mod parser;
mod scanner;

use clap::{Arg, App};
use regex::Regex;
use std::io::Read;
use std::fs::File;
use std::process;

fn read_file(file_name: String) -> String {
    let comment = Regex::new(r"/\*(([^\*/])*)\*/").unwrap();
    let white_space = Regex::new(r"\s+").unwrap();
    let mut buffer = String::new();
    let mut file = File::open(file_name).expect("Unable to open the file");
    file.read_to_string(&mut buffer).expect("Unable to read the file");
    buffer = comment.replace_all(&buffer, "").into_owned();
    buffer = white_space.replace_all(&buffer, " ").into_owned();
    buffer = buffer.trim().to_string();
    buffer
}

fn main() {
    // Argument parsing
    let matches = App::new("TigerCompiler")
        .version("0.1")
        .author("Christopher Tam and Dhruv Mehra")
        .about("A simple Scanner for Tiger programming language!")
        .arg(Arg::with_name("file")
            .help("Tiger file to scan for tokens")
            .required(true))
        .arg(Arg::with_name("tokens")
            .short("t")
            .long("tokens")
            .help("Print tokens to stdout"))
        .get_matches();

    let should_print_tokens = matches.is_present("tokens");
    let file_name = matches.value_of("file").unwrap().to_string();

    let buffer = read_file(file_name);

    // Step 1: run the scanner to parse the tokens
    let tokens = match scanner::parse_tokens(&buffer) {
        Ok(v) => v,
        Err(e) => {
            eprintln!("{}", e);
            process::exit(1);
        },
    };

    if should_print_tokens {
        scanner::print_tokens(&tokens);
    }
}
