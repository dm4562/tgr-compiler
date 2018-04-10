use std::collections::HashMap;
use std::collections::VecDeque;
use std::fmt;
use indextree::{Arena, NodeId};

use scanner::Token;

#[derive(Debug)]
pub struct SymbolTable {
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

pub fn build_type_maps(ast: &Vec<String>) -> (SymbolTable, SymbolTable) {
    let atable = SymbolTable {
        map: HashMap::new()
    };

    let ttable = SymbolTable {
        map: HashMap::new()
    };

    let (arena, root) = build_ast(&ast);
    debug_print_ast(&arena, root);

    let mut queue = VecDeque::new();
    queue.push_back(root);
    while let Some(node) = queue.pop_front() {
        if arena[node].data.eq("declseg") {
            break;
        }

        for child in node.children(&arena) {
            queue.push_back(child);
        }
    }

    (atable, ttable)
}

pub fn build_ast(ast: &Vec<String>) -> (Arena<String>, NodeId) {
    let mut arena: Arena<String> = Arena::new();
    let mut ast_iter = ast.into_iter();
    // ast_iter.next();

    // let root = arena.new_node(ast_iter.next().unwrap().to_string());
    let mut root = arena.new_node(String::from("-"));
    let mut curr = root;

    while let Some(node) = ast_iter.next() {
        if node.eq("@(") {
            let val = ast_iter.next().unwrap().to_string();
            let nid: NodeId = arena.new_node(val);
            curr.append(nid, &mut arena);
            curr = nid;
        } else if node.eq("@)") {
            curr = curr.ancestors(&arena).next().unwrap();
        } else {
            let nid = arena.new_node(node.to_string());
            curr.append(nid, &mut arena);
        }
    }
    root = root.children(&arena).next().unwrap();
    (arena, root)
}

pub fn debug_print_ast(arena: &Arena<String>, root: NodeId) {
    let mut v = VecDeque::new();
    v.push_back(root);
    print!("\n");
    let mut curr;
    while !v.is_empty() {
        curr = v.pop_front().unwrap();
        print!("{}-", arena[curr].data);
        for mut child in curr.children(&arena) {
            // print!("{} ", arena[child].data);
            v.push_back(child);
        };
        // print!("\n");
    }
}
