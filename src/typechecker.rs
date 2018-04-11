use std::collections::HashMap;
use std::collections::VecDeque;
use std::fmt;
use std::rc::Rc;
use indextree::{Arena, NodeId};

use scanner::Token;

#[derive(Debug)]
pub struct SymbolTable {
    map: HashMap<String, VecDeque<DynamicType>>
}

impl SymbolTable {
    fn push(&mut self, id: &String, sym_type: DynamicType) -> bool {
        self.map.entry(id.to_owned()).or_insert(VecDeque::new());
        let q: &mut VecDeque<DynamicType> = match self.map.get_mut(id) {
            Some(v) => v,
            None => {
                return false;
            }
        };

        q.push_back(sym_type);
        true
    }

    fn pop(&mut self, id: &String) -> Option<DynamicType> {
        let v: &mut VecDeque<DynamicType> = self.map.get_mut(id).unwrap();
        v.pop_back()
    }

    fn find(&self, id: &String) -> Option<&DynamicType> {
        let v: &VecDeque<DynamicType> = match self.map.get(id) {
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

#[derive(Debug, PartialEq, Clone, Copy)]
enum BaseType {
    Integer,
    Float,
    Boolean,
    Array
}

impl BaseType {
    fn from_str(s: &str) -> BaseType {
        match s {
            "int"       => BaseType::Integer,
            "float"     => BaseType::Float,
            "boolean"   => BaseType::Boolean,
            "array"     => BaseType::Array,
            _           => panic!("invalid type reached the typechecker!"),
        }
    }

    fn is_recursive_type(&self) -> bool {
        *self == BaseType::Array
    }
}

#[derive(Debug, Clone)]
struct DynamicType {
    cur_type: BaseType,
    sub_type: Option<Rc<Box<DynamicType>>>
}

impl PartialEq for DynamicType {
    fn eq(&self, other: &DynamicType) -> bool {
        if self.cur_type != other.cur_type {
            return false;
        }


        // Need to clone `DynamicType` since comparator needs references to the boxed sub_type
        let mine = self.clone();
        let other = other.clone();
        // Recursively checks if sub_type is equal
        mine.sub_type.map_or(other.sub_type.is_none(), |lhs| other.sub_type.map_or(false, |rhs| *lhs == *rhs))
    }
}

impl DynamicType {
    /// Converts a TYPE subtree into a recursive `DynamicType` representation.
    ///
    /// `node` should be a `NodeId` pointing to a TYPE node in an `Arena`.
    fn from_tree_node(node: NodeId, arena: &Arena<Rc<Token>>) -> DynamicType {
        // The base type can be detected from the first token in a type declaration
        let cur_type = BaseType::from_str(&*(arena[node.children(arena).nth(0).unwrap()].data.val));
        DynamicType {
            cur_type: cur_type,
            sub_type: match cur_type.is_recursive_type() {
                true    => Some(Rc::new(Box::new(DynamicType::from_tree_node(node.children(arena).last().unwrap(), arena)))),
                false   => None,
            }
        }
    }
}

pub fn build_type_maps(ast: &Vec<Rc<Token>>) -> (SymbolTable, SymbolTable) {
    let (arena, root) = build_ast(&ast);
    // debug_print_ast(&arena, root);

    let mut queue = VecDeque::new();
    queue.push_back(root);
    let mut curr: Option<NodeId> = None;
    while let Some(node) = queue.pop_front() {
        if (arena[node].data.token_name).eq("nonterminal") && (*arena[node].data.val).eq("declseg") {
            curr = Some(node);
            break;
        }

        for child in node.children(&arena) {
            queue.push_back(child);
        }
    }

    let mut typedecls_node: Option<NodeId> = None;
    let mut vardecls_node: Option<NodeId> = None;
    let mut funcdecls_node: Option<NodeId> = None;

    for child in curr.expect("Couldn't find declseg").children(&arena) {
        if (*arena[child].data).token_name.eq("nonterminal") {
            match (arena[child].data).val.as_str() {
                "typedecls"     => typedecls_node   = Some(child),
                "vardecls"      => vardecls_node    = Some(child),
                "funcdecls"     => funcdecls_node   = Some(child),
                _               => { }
            }
        }
    }

    let atable = build_alias_map(typedecls_node.unwrap(), &arena);
    let ttable = build_context_map(vardecls_node.unwrap(), funcdecls_node.unwrap(), &arena);
    (atable, ttable)
}

fn build_alias_map(typedecls_node: NodeId, arena: &Arena<Rc<Token>>) -> SymbolTable {
    let atable = SymbolTable {
        map: HashMap::new()
    };

    let mut iter = typedecls_node.children(&arena);
    while let Some(child) = iter.next() {
        // call chris_func
        // (*arena[node].data)
        print!("-{}-", arena[child].data);
        for n in child.children(&arena) {
            print!("{} ", arena[n].data);
        }
        iter = iter.next().expect("Expected TYPEDECLS").children(&arena);
    }

    atable
}

fn build_context_map(vardecls_node: NodeId, funcdecls_node: NodeId, arena: &Arena<Rc<Token>>) -> SymbolTable {
    let ttable = SymbolTable {
        map: HashMap::new()
    };

    let mut iter = vardecls_node.children(&arena);


    ttable
}

pub fn build_ast(ast: &Vec<Rc<Token>>) -> (Arena<Rc<Token>>, NodeId) {
    let mut arena: Arena<Rc<Token>> = Arena::new();
    let mut ast_iter = ast.into_iter();

    let mut root = arena.new_node(Rc::new(Token::get_blank()));
    let mut curr = root;

    while let Some(node) = ast_iter.next() {
        if (*node.val).eq("@(") {
            let rc_token = ast_iter.next().unwrap();
            print!("\n-{}-\n ", rc_token);
            let nid: NodeId = arena.new_node(rc_token.clone());
            curr.append(nid, &mut arena);
            curr = nid;
        } else if (*node.val).eq("@)") {
            curr = curr.ancestors(&arena).next().unwrap();
            print!("\n^{}^\n", arena[curr].data);
        } else {
            print!("{}* ", node);
            let nid = arena.new_node(node.clone());
            curr.append(nid, &mut arena);
        }
    }
    root = root.children(&arena).next().unwrap();
    (arena, root)
}

pub fn debug_print_ast(arena: &Arena<Rc<Token>>, root: NodeId) {
    let mut v = VecDeque::new();
    v.push_back(root);
    print!("\n");
    let mut curr;
    while !v.is_empty() {
        curr = v.pop_front().unwrap();
        // print!("{}-", arena[curr].data);
        for mut child in curr.children(&arena) {
            print!("{} ", arena[child].data);
            v.push_back(child);
        };
        print!("\n");
    }
}
