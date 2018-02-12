extern crate regex;

use std::io::{self, Read};
use regex::Regex;


fn main() {
    let keywords = Regex::new(r"^((array)|(begin)|(boolean)|(break)|(do)|(else)|(end)|(enddo)|(endif)|(false)|(float)|(for)|(func)|(if)|(in)|(int)|(let)|(of)|(return)|(then)|(to)|(true)|(type)|(unit)|(var)|(while)|,|:|;|\(|\)|\[|\]|\{|\}|\.|\+|-|\*|/|=|<|>|<>|<=|>=|&|\||:=)$").unwrap();
    let id = Regex::new(r"^((_.*(\d|[A-Za-z])[^\s]*)|(([A-Za-z])[^\s]*))$").unwrap();
    let intlit = Regex::new(r"^(\d*)$").unwrap();
    let floatlit = Regex::new(r"^(\d*\.\d*)$").unwrap();
    let comment = Regex::new(r"/\*(([^\*/])*)\*/").unwrap();
    let white_space = Regex::new(r"\s+").unwrap();
    let vec = vec![&floatlit, &intlit, &id, &keywords];
    let output_vec = vec!["floatlit", "intlit", "id"];

    // println!("Hello, world!");

    let mut buffer = String::new();
    let stdin = io::stdin();
    let mut handle = stdin.lock();

    handle.read_to_string(&mut buffer);
    // print!("{}", &mut buffer);

    buffer = comment.replace_all(&buffer, "").into_owned();
    // print!("REMOVING COMMENTS:\n-{}-\n", &buffer);

    buffer = white_space.replace_all(&buffer, " ").into_owned();
    buffer = buffer.trim().to_string();
    // print!("REMOVING NEW LINES AND MAKING SPACES UNIFORM:\n-{}-\n", &buffer);

    let (mut start, mut end) = (0, 1);
    let mut _sub = &buffer[start..end];
    let mut match_found: i32 = -1;
    let mut matches_multiple = false;
    let mut pre_match_found: i32 = -1;
    let mut pre_matches_multiple = false;

    while end <= buffer.len() {
        println!("U{}- {}- {}- {}- {}", &buffer[start..end], start, end, pre_match_found, match_found);
        if buffer[end - 1..end].eq(" ") {
            // if pre_matches_multiple {
            //     println!("Ambiguous keyword");
            //     break;
            // }

            if pre_match_found < 0 {
                eprintln!("Syntax error");
                break;
            }

            if pre_match_found == 3 {
                println!("{} ", &buffer[start..end - 1]);
            } else {
                println!("{}:{} ", &buffer[start..end - 1], &output_vec[pre_match_found as usize]);
            }

            start = end;
            end += 1;
            pre_match_found = -1;
            println!("M{}- {}- {}- {}- {}", &buffer[start..end], start, end, pre_match_found, match_found);
        }

        for (i, reg) in vec.iter().enumerate() {
            // println!("{}", &reg);
            if reg.is_match(&buffer[start..end]) {
                if match_found >= 0 {
                    matches_multiple = true;
                }
                match_found = i as i32;
                // println!("{} - {}", &buffer[start..end], i);
            }
        }

        if match_found == -1 && pre_match_found != -1 {
            // eprintln!("{} - {} - {}", match_found, pre_match_found, pre_matches_multiple);
            // eprintln!("Syntax error or ambiguous error");
            if pre_match_found == 3 {
                println!("{} ", &buffer[start..end - 1]);
            } else {
                println!("{}:{} ", &buffer[start..end - 1], &output_vec[pre_match_found as usize]);
            }

            start = end - 1;
        } else {
            end += 1;
        }

        pre_match_found = match_found;
        pre_matches_multiple = matches_multiple;
        match_found = -1;
        matches_multiple = false;
    }

    if pre_match_found != -1 {
        if pre_match_found == 3 {
            println!("{} ", &buffer[start..end - 1]);
        } else {
            println!("{}:{} ", &buffer[start..end - 1], &output_vec[pre_match_found as usize]);
        }
    }

    // let temp = "let";
    // println!("{} ----", keywords.is_match(&temp));
}
