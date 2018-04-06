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
use std::fs::File;
use std::process;

fn read_file(file_name: &str) -> String {
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

    // Initialize logger
    env_logger::init();

    // Read source file
    let buffer = read_file(matches.value_of("file").unwrap());

    // Step 1: run the scanner to parse the tokens
    let mut tokens = match scanner::parse_tokens(&buffer) {
        Ok(v) => {
            if matches.is_present("tokens") {
                println!("{}", scanner::format_tokens(&v));
            }
            v
        },
        Err(e) => {
            {
                let last_token = match e.back() {
                    Some(s) => s.to_string(),
                    None    => "N/A".to_owned(),
                };
                eprintln!("An error during lexical analysis occurred! Last valid token: {}", last_token);
            }
            if matches.is_present("tokens") {
                println!("{}", scanner::format_tokens(&e));
            }
            process::exit(1);
        },
    };


    // Step 2: run the parser to generate a proper syntax tree
    // Load parse table into memory
    let table = match parser::load_parse_table() {
        Ok(t) => t,
        Err(e) => { eprintln!("{}", e); process::exit(1); }
    };
    debug!("{}", table);

    // Load grammar into memory
    let grammar = match parser::load_grammar() {
        Ok(t) => t,
        Err(e) => { eprintln!("{}", e); process::exit(1); }
    };
    debug!("{}", grammar);

    // Run the parser
    match parser::parse_input(&grammar, &table, &mut tokens) {
        Ok(ast) => {
            info!("Successfully parsed the program");
            if matches.is_present("ast") {
                println!("{}", parser::format_ast(&ast));
            }
        },
        Err(msg) => { eprintln!("Parse error: {}", msg); process::exit(1); }
    };
}
