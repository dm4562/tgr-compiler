extern crate regex;

use std::io::{self, BufRead, Read};
use regex::Regex;


fn main() {
    let keywords = Regex::new(r"(array)|(begin)|(boolean)|(break)|(do)|(else)|(end)|(enddo)|(endif)|(false)|(float)|(for)|(func)|(if)|(in)|(int)|(let)|(of)|(return)|(then)|(to)|(true)|(type)|(unit)|(var)|(while)|,|:|;|\(|\)|\[|\]|\{|\}|\.|\+|-|\*|/|=|<|>|<>|<=|>=|&|\||:=").unwrap();
    let lit = Regex::new(r"(_|\d|[A-Za-z])(.*(\d|[A-Za-z]).*)").unwrap();
    let intlit = Regex::new(r"\d*").unwrap();
    let floatlit = Regex::new(r"\d*\.\d*").unwrap();
    let comment = Regex::new(r"/\*(([^\*/])*)\*/").unwrap();

    println!("Hello, world!");

    let mut buffer = String::new();
    let stdin = io::stdin();
    let mut handle = stdin.lock();

    handle.read_to_string(&mut buffer);
    print!("{}", &mut buffer);

    buffer = comment.replace_all(&buffer, "").into_owned();
    print!("REMOVING COMMENTS:\n{}", &buffer);
}
