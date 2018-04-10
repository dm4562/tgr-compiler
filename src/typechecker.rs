use std::collections::HashMap;
use std::collections::VecDeque;
use std::fmt;

use scanner::Token;

#[derive(Debug)]
struct SymbolTable {
    map: HashMap<String, VecDeque<Type>>
}

impl SymbolTable {
    fn push(&mut self, id: &String, sym_type: Type) -> bool {
        self.map.entry(id.to_owned()).or_insert(VecDeque::new());
        let q: &mut VecDeque<Type> = match self.map.get_mut(id) {
            Some(v) => v,
            None => {
                return false;
            }
        };

        q.push_back(sym_type);
        true
    }

    fn pop(&mut self, id: &String) -> Option<Type> {
        let v: &mut VecDeque<Type> = self.map.get_mut(id).unwrap();
        v.pop_back()
    }

    fn find(&self, id: &String) -> Option<&Type> {
        let v: &VecDeque<Type> = match self.map.get(id) {
            Some(vec) => vec,
            None => {
                return None;
            }
        };

        v.back()
    }
}

impl fmt::Display for SymbolTable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // let mut out = String::new();
        write!(f, "Symbol Table\n");
        for key in self.map.keys() {
            write!(f, "{} - {:?}\n", key, self.map.get(key).unwrap());
        }
        write!(f, "\n")
    }
}

#[derive(Debug)]
enum Type {
    Integer,
    Float,
    Boolean
}


