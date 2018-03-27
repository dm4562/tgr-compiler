extern crate clap;
extern crate env_logger;
#[macro_use] extern crate lazy_static;
#[macro_use] extern crate log;
extern crate regex;

#[macro_use]
extern crate serde_derive;

mod scanner;
mod parser;

use clap::{Arg, App};
use regex::Regex;
use std::io::Read;
use std::fmt::Write;
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
        .arg(Arg::with_name("ast")
            .short("a")
            .long("ast")
            .help("Print out the Abstract Syntax Tree"))
        .get_matches();

    env_logger::init();

    let should_print_tokens = matches.is_present("tokens");
    let file_name = matches.value_of("file").unwrap().to_string();

    let buffer = read_file(file_name);

    // Step 1: run the scanner to parse the tokens
    let mut tokens = match scanner::parse_tokens(&buffer) {
        Ok(v) => v,
        Err(e) => {
            eprintln!("Parse error");
            e
        },
    };

    if should_print_tokens {
        scanner::print_tokens(&tokens);
    }

    // Step 2: run the parser to generate a proper syntax tree
    let table = match parser::load_parse_table() {
        Ok(t) => t,
        Err(e) => { eprintln!("{}", e); process::exit(1); }
    };

    let grammar = match parser::load_grammar() {
        Ok(t) => t,
        Err(e) => { eprintln!("{}", e); process::exit(1); }
    };
    debug!("{}", table);
    debug!("{}", grammar);

    match parser::parse_input(&grammar, &table, &mut tokens) {
        Ok(ast) => {
            info!("Successfully parsed the program");
            if matches.is_present("ast") {
                println!("{}", format_ast(&ast));
            }
        },
        Err(msg) => { eprintln!("Parse error: {}", msg); process::exit(1); }
    };
}

fn format_ast(ast: &Vec<String>) -> String {
    let mut output = String::new();
    let mut print_space = false;

    for symbol in ast {
        if print_space && !symbol.eq("@)") {
            write!(output, " ").unwrap();
        }

        print_space = !symbol.eq("@(");

        if symbol.starts_with("@") {
            write!(output, "{}", &symbol[1..]).unwrap();
        } else {
            write!(output, "{}", symbol).unwrap();
        }
    }

    return output;
}