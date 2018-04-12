extern crate serde;
extern crate serde_json;

use log::Level;
use std::rc::Rc;
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
        write!(f, "Grammar\nNon-Terminals: {} - Productions: {}", self.nonterminals.len(), self.productions.len())
    }
}


pub fn load_parse_table() -> Result<ParseTable, serde_json::Error> {
    serde_json::from_str(include_str!("../data/parsing_table.json"))
}

pub fn load_grammar() -> Result<Grammar, serde_json::Error> {
    serde_json::from_str(include_str!("../data/grammar.json"))
}

pub fn parse_input<'a>(grammar: &Grammar, table: &ParseTable, tokens: &mut VecDeque<Token>) -> Result<(Vec<String>, Vec<Rc<Token>>), String> {
    // Build the reverse map for terminals
    let mut terminal_map: HashMap<&str, usize>= HashMap::new();
    for (i, terminal) in table.terminals.iter().enumerate() {
        terminal_map.insert(terminal, i);
        debug!("{}:{}", &terminal, &i);
    }

    let mut next_token_opt = tokens.pop_front();
    let mut cur_token: Rc<Token>;
    let mut stack: VecDeque<String> = VecDeque::new();
    let mut ast: Vec<String> = Vec::new();
    let mut new_ast: Vec<Rc<Token>> = Vec::new();
    let mut recurse_idx_stack: Vec<usize> = Vec::new();
    stack.push_front("1".to_owned());

    loop {
        if stack.is_empty() && next_token_opt.is_none() {
            break;
        } else if stack.is_empty() {
            return Err(format!("unexpected token '{}' after end of program.", next_token_opt.unwrap()).to_owned());
        } else if stack.front().unwrap().eq("@#") {
            // Found special token denoting the upwards traversal in the AST
            // Mark the end of an expanded non-terminal
            ast.push("@)".to_owned());
            new_ast.push(Rc::new(Token::new(Rc::new("@)".to_owned()), "helper", 0, 0, 0)));
            stack.pop_front();
            continue;
        }

        cur_token = match next_token_opt.clone() {
            Some(v) => Rc::new(v),
            None    => return Err("unexpected end of program!".to_owned()),
        };

        let non_terminal: usize = match stack.front().unwrap().parse::<usize>() {
            Ok(num) => num,
            Err(_e) => 0
        };

        print_debug_stack(&stack, &grammar.nonterminals);
        debug!("{}\n", cur_token);

        if non_terminal == 0 {
            if (cur_token.token_name.eq("keyword") && (*cur_token.val).eq(stack.front().unwrap())) || cur_token.token_name.eq(stack.front().unwrap()) {
                // success
                ast.push(get_token_ast_value(&cur_token));
                new_ast.push(cur_token.clone());
                stack.pop_front();
                next_token_opt = tokens.pop_front();
            } else {
                return Err(format!("unexpected token '{}' found!", cur_token.val));
            }
        } else {
            let terminal_ndx: usize = match terminal_map.get(&(*cur_token.val)[..]) {
                Some(expr) => *expr,
                None => *terminal_map.get(cur_token.token_name).unwrap(),
            };
            let row: &Vec<usize> = table.table.get(non_terminal).expect("Could not get table row");

            debug!("------------------");
            debug!("row: {:?}", row);
            debug!("Token: {}:{}", cur_token.val, terminal_ndx);

            let production_no: usize = *row.get(terminal_ndx).expect("Could not get production number");
            let production = match grammar.productions.get(production_no) {
                Some(v) => v,
                None    => return Err(format!("unexpected token '{}' found!", *cur_token.val)),
            };

            debug!("{}:{} - {}:{} - {}:{}",
                &grammar.nonterminals.get(non_terminal).unwrap(),
                &non_terminal,
                &terminal_ndx,
                table.terminals.get(terminal_ndx).unwrap(),
                &production_no,
                &debug_production(&production, &grammar.nonterminals));
            debug!("------------------");


            stack.pop_front();

            // Push original productions onto the AST
            {
                let prod_name = grammar.nonterminals.get(non_terminal).unwrap();
                if prod_name.ends_with("^") {
                    // Push the index of the latest left-recursive production
                    recurse_idx_stack.push(ast.len());
                } else if prod_name.ends_with("^'") {
                    if production.is_empty() {
                        recurse_idx_stack.pop();
                    } else {
                        let recurse_idx = recurse_idx_stack.last_mut().unwrap();

                        // Insert at the recurse index to reintroduce left recursion
                        ast.insert(*recurse_idx, "@(".to_owned());
                        new_ast.insert(*recurse_idx, Rc::new(Token::new(Rc::new("@(".to_owned()), "helper", 0, 0, 0)));
                        ast.insert(*recurse_idx + 1, get_readable_production_name(prod_name));
                        new_ast.insert(*recurse_idx + 1, Rc::new(Token::new(
                            Rc::new(get_readable_production_name(prod_name)),
                            "nonterminal",
                            0,
                            0,
                            0
                        )));
                        // This may need to change
                        *recurse_idx += 2;

                        // Close the left recursive call inserted above
                        ast.push("@)".to_owned());
                        new_ast.push(Rc::new(Token::new(Rc::new("@)".to_owned()), "helper", 0, 0, 0)));
                    }
                }

                if !prod_name.ends_with("'") {
                    if !production.is_empty() {
                        // Push a special token to mark where the newly expanded production will end
                        stack.push_front("@#".to_owned());

                        // Mark the end of an expanded non-terminal
                        ast.push("@(".to_owned());
                        new_ast.push(Rc::new(Token::new(Rc::new("@(".to_owned()), "helper", 0, 0, 0)))
                    }

                    ast.push(get_readable_production_name(prod_name));
                    new_ast.push(Rc::new(Token::new(
                        Rc::new(get_readable_production_name(prod_name)),
                        "nonterminal",
                        0,
                        0,
                        0
                    )));
                }
            }

            // Push the expanded productions onto the stack (in reverse order)
            for rule in production.iter().rev() {
                stack.push_front(rule.to_owned());
            }

        }
    }

    Ok((ast, new_ast))
}

pub fn get_token_ast_value(token: &Token) -> String {
    match token.token_name {
        "keyword"   => (*token.val).to_owned(),
        "id"        => (*token.val).to_owned(),
        "intlit"    => (*token.val).to_owned(),
        "floatlit"  => (*token.val).to_owned(),
        _           => token.token_name.to_owned(),
    }
}

fn get_readable_production_name(name: &str) -> String {
    name.to_lowercase().replace("^", "").replace("'", "").to_owned()
}

fn debug_production(production: &Vec<String>, nonterminals: &Vec<String>) -> String {
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

fn print_debug_stack(stack: &VecDeque<String>, nonterminals: &Vec<String>) {
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

/// Formats the internally represented abstract syntax tree into a human-friendly `String`.
pub fn format_ast(ast: &Vec<String>) -> String {
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
