use regex::Regex;
use std::collections::VecDeque;
use std::fmt::Write;

#[derive(Debug)]
pub struct Token<'a> {
    pub val: &'a str,
    pub token_name: &'a str,
    pub index: usize,
    pub length: usize,
    pub token_type: usize
}

pub fn parse_tokens(buffer: &String) -> Result<VecDeque<Token>, &'static str> {
    lazy_static! {
        static ref KEYWORD1_RE: Regex = Regex::new(r"^((array)|(begin)|(boolean)|(break)|(do)|(else)|(end)|(enddo)|(endif)|(false)|(float)|(for)|(func)|(if)|(in)|(int)|(let)|(of)|(return)|(then)|(to)|(true)|(type)|(unit)|(var)|(while))$").unwrap();
        static ref KEYWORD2_RE: Regex = Regex::new(r"^(,|:|;|\(|\)|\[|\]|\{|\}|\.|\+|-|\*|/|=|<|>|<>|<=|>=|&|\||:=)$").unwrap();
        static ref ID_RE: Regex = Regex::new(r"^((_[A-Za-z_0-9]*([0-9A-Za-z])[A-Za-z_0-9]*)|(([A-Za-z])[0-9A-Za-z_]*))$").unwrap();
        static ref INTLIT_RE: Regex = Regex::new(r"^(\d*)$").unwrap();
        static ref FLOATLIT_RE: Regex = Regex::new(r"^(\d+\.\d*)$").unwrap();

        static ref TOKEN_REGEXES: Vec<&'static Regex> = vec![&FLOATLIT_RE, &INTLIT_RE, &ID_RE, &KEYWORD1_RE, &KEYWORD2_RE];
        static ref TOKEN_NAMES: Vec<&'static str> = vec!["floatlit", "intlit", "id", "keyword", "keyword"];
    }

    let (mut start, mut end) = (0, 1);
    let mut match_found: i32 = -1;
    let mut pre_match_found: i32 = -1;
    let mut need_keyword2 = false;

    let mut result: VecDeque<Token> = VecDeque::new();

    while end <= buffer.len() {
        if buffer[end - 1..end].eq(" ") {

            if pre_match_found < 0 {
                // print_tokens(&q, &token);
                return Err("Syntax error 1");
            }


            if need_keyword2 && pre_match_found != 4 {
                result.pop_back();
                // print_tokens(&q, &token);
                return Err("Syntax error 2");
            }

            if need_keyword2 && pre_match_found == 4 {
                need_keyword2 = false;
            }

            let token = Token {
                val: &buffer[start..end - 1],
                token_name: TOKEN_NAMES[pre_match_found as usize],
                index: start, length: end - 1 - start,
                token_type: pre_match_found as usize
            };
            result.push_back(token);

            start = end;
            end += 1;
            pre_match_found = -1;

        }

        for (i, reg) in TOKEN_REGEXES.iter().enumerate() {
            if reg.is_match(&buffer[start..end]) {
                match_found = i as i32;
            }
        }

        if match_found == -1 && pre_match_found != -1 {
            if need_keyword2 && pre_match_found != 4 {
                result.pop_back();
                // print_tokens(&q, &token);
                return Err("Syntax error 3");
            }

            if need_keyword2 && pre_match_found == 4 {
                need_keyword2 = false;
            }

            let token = Token {
                val: &buffer[start..end - 1],
                token_name: TOKEN_NAMES[pre_match_found as usize],
                index: start, length: end - 1 - start,
                token_type: pre_match_found as usize
            };
            result.push_back(token);

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
            result.pop_back();
            // print_tokens(&q, &token);
            return Err("Syntax error 5");
        }

        let token = Token {
            val: &buffer[start..end - 1],
            token_name: TOKEN_NAMES[pre_match_found as usize],
            index: start, length: end - 1 - start,
            token_type: pre_match_found as usize
        };
        result.push_back(token);
    }

    Ok(result)
}

pub fn print_tokens(queue: &VecDeque<Token>) {
    let mut output: String = String::new();
    for token in queue {
        if token.token_name == "keyword" {
            write!(&mut output, "{} ", &token.val).unwrap();
        } else {
            write!(&mut output, "{}:{} ", &token.val, &token.token_name).unwrap();
        }
    }
    output = output.trim().to_string();
    print!("{}", &output);
}