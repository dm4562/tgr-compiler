/*
 * Build a map from int <-> NT (based on the indexing provided in the parse table)
 * Build a map for all the productions int -> [list] where each NT is a number based on the
 * above map.
 * Each element in the list is a tuple where first index is the type (NT, keyword, id, intlit...)
 * and second element is the value
 * Then use this setup to build the parser
 */

extern crate serde;
extern crate serde_json;

use std::fs::File;
use std::collections::{VecDeque, HashMap};
use std::fmt;
use std::fmt::Write;

use scanner::Token;

#[derive(Debug, Serialize, Deserialize)]
pub struct ParseTable {
    pub terminals: Vec<String>,
    pub table: Vec<Vec<usize>>
}

impl fmt::Display for ParseTable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // Write strictly the first element into the supplied output
        // stream: `f`. Returns `fmt::Result` which indicates whether the
        // operation succeeded or failed. Note that `write!` uses syntax which
        // is very similar to `println!`.
        write!(f, "Parse Table\nTerminals: {} - Table: {} x {}", self.terminals.len(), self.table.len(), self.table[0].len())
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Grammar {
    pub nonterminals: Vec<String>,
    pub productions: Vec<Vec<String>>
}

impl fmt::Display for Grammar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // Write strictly the first element into the supplied output
        // stream: `f`. Returns `fmt::Result` which indicates whether the
        // operation succeeded or failed. Note that `write!` uses syntax which
        // is very similar to `println!`.
        write!(f, "Grammar\nNon-Terminals: {} - Productions: {}", self.nonterminals.len(), self.productions.len())
    }
}


pub fn load_parse_table() -> Result<ParseTable, &'static str> {
    let table_file = File::open("data/parsing_table.json").expect("Unable to open json file");
    let table: ParseTable = serde_json::from_reader(table_file).expect("Could not read json file");
    Ok(table)
}

pub fn load_grammar() -> Result<Grammar, &'static str> {
    let file = File::open("data/grammar.json").expect("Unable to open json file");
    let grammar: Grammar = serde_json::from_reader(file).expect("Could not read json file");
    Ok(grammar)
}

pub fn parse_input(_grammar: &Grammar, table: &ParseTable, tokens: &mut VecDeque<Token>) -> Result<(), &'static str> {
    // Build the reverse map for terminals
    let mut terminal_map: HashMap<&str, usize>= HashMap::new();
    for (i, terminal) in table.terminals.iter().enumerate() {
        terminal_map.insert(terminal, i);
    }

    let mut token = tokens.pop_front();
    let mut stack: VecDeque<String> = VecDeque::new();
    stack.push_front("1".to_string());
    // let mut top = stack.front().unwrap();

    loop {
        if stack.is_empty() && token.is_none() {
            break;
        }

        let non_terminal: usize = match stack.front().unwrap().parse::<usize>() {
            Ok(num) => num,
            Err(_e) => 0
        };

        let token_val = token.expect("Couldn't unwrap token");

        if non_terminal == 0 {
            if (token_val.token_name.eq("keyword") && token_val.val.eq(stack.front().unwrap())) || token_val.token_name.eq(stack.front().unwrap()) {
                // success
                stack.pop_front();
                token = tokens.pop_front();
            } else {
                // error
                let mut err = String::new();
                write!(&mut err, "Looking for {}", &stack.front().unwrap()).unwrap();
                return Err("Looking for stack.front().unwrap()");
            }
        } else {
            let production_no: usize = *table.table.get(non_terminal).unwrap().get(*terminal_map.get(token_val.val).unwrap()).unwrap();
            println!("{} - {}", &non_terminal, &production_no);

        }
    }

    Ok(())

}