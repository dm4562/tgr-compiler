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
use std::collections::VecDeque;
use std::fmt;

use scanner::Token;

#[derive(Debug, Serialize, Deserialize)]
pub struct ParseTable {
    pub terminals: Vec<String>,
    pub table: Vec<Vec<u8>>
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


pub fn load_parse_table() -> Result<ParseTable, &'static str> {
    let table_file = File::open("src/parsing_table.json").expect("Unable to open json file");
    let table: ParseTable = serde_json::from_reader(table_file).expect("Could not read json file");
    Ok(table)
}

pub fn parse_input(table: &ParseTable, tokens: &VecDeque<Token>) {

}