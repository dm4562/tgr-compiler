extern crate regex;
extern crate clap;

use std::io::{Read};
use std::collections::VecDeque;
use regex::Regex;
use std::fmt::Write;
use clap::{Arg, App};
use std::fs::File;


#[derive(Debug)]
struct Token<'a> {
    val: &'a str,
    token_name: &'a str,
    index: usize,
    length: usize,
    token_type: usize
}

fn print_tokens(queue: &VecDeque<Token>, print_tokens: &bool) {
    if !print_tokens {
        return;
    }

    let mut output: String = String::new();
    for token in queue {
        if token.token_name == "keyword" {
            write!(&mut output, "{} ", &token.val);
        } else {
            write!(&mut output, "{}:{} ", &token.val, &token.token_name);
        }
    }
    output = output.trim().to_string();
    print!("{}", &output);
}

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

    let token = matches.is_present("tokens");
    let file_name = matches.value_of("file").unwrap().to_string();

    let buffer = read_file(file_name);

    let keyword1 = Regex::new(r"^((array)|(begin)|(boolean)|(break)|(do)|(else)|(end)|(enddo)|(endif)|(false)|(float)|(for)|(func)|(if)|(in)|(int)|(let)|(of)|(return)|(then)|(to)|(true)|(type)|(unit)|(var)|(while))$").unwrap();
    let keyword2 = Regex::new(r"^(,|:|;|\(|\)|\[|\]|\{|\}|\.|\+|-|\*|/|=|<|>|<>|<=|>=|&|\||:=)$").unwrap();
    let id = Regex::new(r"^((_[A-Za-z_0-9]*([0-9A-Za-z])[A-Za-z_0-9]*)|(([A-Za-z])[0-9A-Za-z_]*))$").unwrap();
    let intlit = Regex::new(r"^(\d*)$").unwrap();
    let floatlit = Regex::new(r"^(\d+\.\d*)$").unwrap();

    let regex_vec = vec![&floatlit, &intlit, &id, &keyword1, &keyword2];
    let output_vec = vec!["floatlit", "intlit", "id", "keyword", "keyword"];

    let (mut start, mut end) = (0, 1);
    let mut match_found: i32 = -1;
    let mut pre_match_found: i32 = -1;
    let mut need_keyword2 = false;

    let mut q: VecDeque<Token> = VecDeque::new();

    while end <= buffer.len() {
        if buffer[end - 1..end].eq(" ") {

            if pre_match_found < 0 {
                eprintln!("Syntax error 1");
                print_tokens(&q, &token);
                return;
            }


            if need_keyword2 && pre_match_found != 4 {
                q.pop_back();
                eprintln!("Syntax error 2");
                print_tokens(&q, &token);
                return;
            }

            if need_keyword2 && pre_match_found == 4 {
                need_keyword2 = false;
            }

            let token = Token {
                val: &buffer[start..end - 1],
                token_name: output_vec[pre_match_found as usize],
                index: start, length: end - 1 - start,
                token_type: pre_match_found as usize
            };
            q.push_back(token);

            start = end;
            end += 1;
            pre_match_found = -1;

        }

        for (i, reg) in regex_vec.iter().enumerate() {
            if reg.is_match(&buffer[start..end]) {
                match_found = i as i32;
            }
        }

        if match_found == -1 && pre_match_found != -1 {
            if need_keyword2 && pre_match_found != 4 {
                q.pop_back();
                eprintln!("Syntax error 3");
                print_tokens(&q, &token);
            }

            if need_keyword2 && pre_match_found == 4 {
                need_keyword2 = false;
            }

            let token = Token {
                val: &buffer[start..end - 1],
                token_name: output_vec[pre_match_found as usize],
                index: start, length: end - 1 - start,
                token_type: pre_match_found as usize
            };
            q.push_back(token);

            if pre_match_found <= 2 && pre_match_found >= 0 {
                need_keyword2 = true;
            }

            start = end - 1;
        } else {
            end += 1;
        }

        pre_match_found = match_found;
        match_found = -1;
    }

    if pre_match_found != -1 {
        if need_keyword2 && pre_match_found != 4 {
            q.pop_back();
            eprintln!("Syntax error 5");
            print_tokens(&q, &token);
            return;
        }

        let token = Token {
            val: &buffer[start..end - 1],
            token_name: output_vec[pre_match_found as usize],
            index: start, length: end - 1 - start,
            token_type: pre_match_found as usize
        };
        q.push_back(token);
    }

    print_tokens(&q, &token);
}
