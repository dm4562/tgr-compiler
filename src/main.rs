extern crate regex;

use std::io::{self, Read};
use std::collections::VecDeque;
use regex::Regex;
use std::fmt::Write;

#[derive(Debug)]
struct Token<'a> {
    val: &'a str,
    token_name: &'a str,
    index: usize,
    length: usize,
    token_type: usize
}

fn print_queue(queue: &VecDeque<Token>, out: &mut String) {
    for token in queue {
        if token.token_name == "keyword" {
            write!(out, "{} ", &token.val);
        } else {
            write!(out, "{}:{} ", &token.val, &token.token_name);
        }
    }
    *out = out.trim().to_string();
}

// fn _push_queue<'a>(queue: &'a VecDeque<Token>, buffer: &'a String, start: &usize, end: &usize, token_type: &i32) {
//     let output_vec = vec!["floatlit", "intlit", "id", "keyword", "keyword"];
//     let token = Token {
//         val: &buffer[*start..*end - 1],
//         token_name: output_vec[*token_type as usize],
//         index: *start,
//         length: *end - 1 - *start,
//         token_type: *token_type as usize
//     };
//     queue.push_back(token);
// }

fn main() {
    let keyword1 = Regex::new(r"^((array)|(begin)|(boolean)|(break)|(do)|(else)|(end)|(enddo)|(endif)|(false)|(float)|(for)|(func)|(if)|(in)|(int)|(let)|(of)|(return)|(then)|(to)|(true)|(type)|(unit)|(var)|(while))$").unwrap();
    let keyword2 = Regex::new(r"^(,|:|;|\(|\)|\[|\]|\{|\}|\.|\+|-|\*|/|=|<|>|<>|<=|>=|&|\||:=)$").unwrap();
    let id = Regex::new(r"^((_[A-Za-z_0-9]*([0-9A-Za-z])[A-Za-z_0-9]*)|(([A-Za-z])[0-9A-Za-z_]*))$").unwrap();
    let intlit = Regex::new(r"^(\d*)$").unwrap();
    let floatlit = Regex::new(r"^(\d*\.\d*)$").unwrap();
    let comment = Regex::new(r"/\*(([^\*/])*)\*/").unwrap();
    let white_space = Regex::new(r"\s+").unwrap();
    let vec = vec![&floatlit, &intlit, &id, &keyword1, &keyword2];
    let output_vec = vec!["floatlit", "intlit", "id", "keyword", "keyword"];

    let mut buffer = String::new();
    let stdin = io::stdin();
    let mut handle = stdin.lock();

    handle.read_to_string(&mut buffer);

    buffer = comment.replace_all(&buffer, "").into_owned();

    buffer = white_space.replace_all(&buffer, " ").into_owned();
    buffer = buffer.trim().to_string();

    let (mut start, mut end) = (0, 1);
    let mut match_found: i32 = -1;
    let mut pre_match_found: i32 = -1;
    let mut need_keyword2 = false;
    let mut error = false;

    let mut q: VecDeque<Token> = VecDeque::new();

    while end <= buffer.len() {
        // println!("U{}- {}- {}- {}- {}", &buffer[start..end], start, end, pre_match_found, match_found);
        if buffer[end - 1..end].eq(" ") {

            if pre_match_found < 0 {
                eprintln!("Syntax error 1");
                error = true;
                break;
            }


            if need_keyword2 && pre_match_found != 4 {
                q.pop_back();
                eprintln!("Syntax error 2");
                error = true;
                break;
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
            // push_queue(&q, &buffer, &start, &end, &pre_match_found);

            start = end;
            end += 1;
            pre_match_found = -1;
            // println!("M{}- {}- {}- {}- {}", &buffer[start..end], start, end, pre_match_found, match_found);
        }

        for (i, reg) in vec.iter().enumerate() {
            if reg.is_match(&buffer[start..end]) {
                match_found = i as i32;
                // println!("{} - {}", &buffer[start..end], i);
            }
        }

        if match_found == -1 && pre_match_found != -1 {
            if need_keyword2 && pre_match_found != 4 {
                q.pop_back();
                eprintln!("Syntax error 3");
                error = true;
                break;
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
            // push_queue(&q, &buffer, &start, &end, &pre_match_found);

            if pre_match_found <= 2 && pre_match_found >= 0 {
                need_keyword2 = true;
            }
            // else {
            //     q.pop_back();
            //     eprintln!("Syntax error 4");
            //     error = true;
            //     break;
            // }
            start = end - 1;
        } else {
            end += 1;
        }

        pre_match_found = match_found;
        match_found = -1;
    }

    if pre_match_found != -1 && !error {
        if need_keyword2 && pre_match_found != 4 {
            q.pop_back();
            eprintln!("Syntax error 5");
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
        // push_queue(&q, &buffer, &start, &end, &pre_match_found);
    }

    let mut output: String = String::new();
    print_queue(&q, &mut output);
    print!("{}", &output);
}
