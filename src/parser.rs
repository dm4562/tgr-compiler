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

use log::Level;
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

pub fn parse_input(grammar: &Grammar, table: &ParseTable, tokens: &mut VecDeque<Token>) -> Result<(), String> {
    // Build the reverse map for terminals
    let mut terminal_map: HashMap<&str, usize>= HashMap::new();
    for (i, terminal) in table.terminals.iter().enumerate() {
        terminal_map.insert(terminal, i);
        debug!("{}:{}", &terminal, &i);
    }

    let mut token = tokens.pop_front();
    let mut stack: VecDeque<String> = VecDeque::new();
    stack.push_front("1".to_string());
    // let mut top = stack.front().unwrap();

    loop {
        if stack.is_empty() && token.is_none() {
            break;
        }

        let token_val = token.expect("Couldn't unwrap token");

        print_debug_stack(&stack, &grammar.nonterminals);
        debug!("{}\n", token_val);

        let non_terminal: usize = match stack.front().unwrap().parse::<usize>() {
            Ok(num) => num,
            Err(_e) => 0
        };

        if non_terminal == 0 {
            if (token_val.token_name.eq("keyword") && token_val.val.eq(stack.front().unwrap())) || token_val.token_name.eq(stack.front().unwrap()) {
                // success
                stack.pop_front();
                token = tokens.pop_front();
            } else {
                // error
                let mut err = String::new();
                write!(&mut err, "Looking for {}", &stack.front().unwrap()).unwrap();
                return Err(err);
            }
        } else {
            let terminal_ndx: usize = match terminal_map.get(token_val.val) {
                Some(expr) => *expr,
                None => *terminal_map.get(token_val.token_name).unwrap(),
            };
            let row: &Vec<usize> = table.table.get(non_terminal).expect("Could not get table row");
            debug!("------------------");
            debug!("row: {:?}", row);
            debug!("Token: {}:{}", token_val.val, terminal_ndx);
            let production_no: usize = *row.get(terminal_ndx).expect("Could not get production number");
            let production = match grammar.productions.get(production_no) {
                Some(v) => v,
                None    => {
                    let mut err = String::new();
                    write!(&mut err, "unexpected token '{}' found!", token_val.val).unwrap();
                    return Err(err);
                }
            };            debug!("{}:{} - {}:{} - {}:{}",
                &grammar.nonterminals.get(non_terminal).unwrap(),
                &non_terminal,
                &terminal_ndx,
                table.terminals.get(terminal_ndx).unwrap(),
                &production_no,
                &debug_production(&production, &grammar.nonterminals));
            debug!("------------------");
            stack.pop_front();
            for rule in production.iter().rev() {
                stack.push_front(rule.to_string());
            }
        }
    }

    Ok(())
}

// pub fn debug_table(table: &ParseTable, grammar: &Grammar) {
//     for row in table.table
// }
pub fn debug_production(production: &Vec<String>, nonterminals: &Vec<String>) -> String {
    let mut output = String::new();
    for r in production {
        let non_terminal: usize = match r.parse::<usize>() {
            Ok(num) => num,
            Err(_e) => 0
        };
        if non_terminal != 0 {
            write!(output, "{} ", nonterminals.get(non_terminal).unwrap()).unwrap();
        } else {
            write!(output, "{} ", r).unwrap();
        }
    }
    output
}

pub fn print_debug_stack(stack: &VecDeque<String>, nonterminals: &Vec<String>) {
    if !log_enabled!(Level::Debug) {
        return;
    }

    let mut output = String::new();
    for e in stack {
        let non_terminal: usize = match e.parse::<usize>() {
            Ok(num) => num,
            Err(_e) => 0
        };
        if non_terminal != 0 {
            write!(output, "{}-", nonterminals.get(non_terminal).unwrap()).unwrap();
        } else {
            write!(output, "{}-", e).unwrap();
        }
    }
    write!(output, "$").unwrap();

    debug!("{}", output);
}