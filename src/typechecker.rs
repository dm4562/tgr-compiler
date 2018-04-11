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
    fn from_str(s: &str) -> Option<BaseType> {
        match s {
            "int"       => Some(BaseType::Integer),
            "float"     => Some(BaseType::Float),
            "boolean"   => Some(BaseType::Boolean),
            "array"     => Some(BaseType::Array),
            _           => None,
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
    fn from_tree_node(node: NodeId, arena: &Arena<Rc<Token>>, symbol_table: &SymbolTable) -> Option<DynamicType> {
        // The base type can be detected from the first token in a type declaration
        let type_name = &*(arena[node.children(arena).nth(0).unwrap()].data.val);
        let cur_type = match BaseType::from_str(type_name) {
            Some(base)  => base,
            None        => match symbol_table.find(type_name) {
                Some(dyn_type)  => return Some(dyn_type.clone()),
                None            => return None,
            }
        };

        Some(DynamicType {
            cur_type: cur_type,
            sub_type: match cur_type.is_recursive_type() {
                true    => match DynamicType::from_tree_node(node.children(arena).last().unwrap(), arena, symbol_table) {
                    Some(dyn_type)  => Some(Rc::new(Box::new(dyn_type))),
                    None            => return None,
                },
                false   => None,
            }
        })
    }
}

pub fn build_type_maps(ast: &Vec<Rc<Token>>) -> (SymbolTable, SymbolTable) {
    let (arena, root) = build_ast(&ast);

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
    let ctable = build_context_map(vardecls_node.unwrap(), funcdecls_node.unwrap(), &arena, &atable);
    (atable, ctable)
}

fn build_alias_map(typedecls_node: NodeId, arena: &Arena<Rc<Token>>) -> SymbolTable {
    let mut atable = SymbolTable {
        map: HashMap::new()
    };

    let mut iter = typedecls_node.children(arena);
    while let Some(typedecl_node) = iter.next() {
        let mut n = typedecl_node.children(arena).nth(1).expect("Could not extract identifier");
        let id = arena[n].data.clone();
        n = typedecl_node.children(arena).nth(3).expect("Could not extract TYPE");
        let ret_type = DynamicType::from_tree_node(n, arena, &atable);
        // TODO: throw type-checking error if ret_type is None
        // Should never encounter ^^ case coz would be parse error
        atable.push(&(*id).val, ret_type.unwrap());
        iter = iter.next().expect("Expected TYPEDECLS").children(arena);
    }

    // print!("{}", atable);
    atable
}

fn build_context_map(vardecls_node: NodeId, funcdecls_node: NodeId, arena: &Arena<Rc<Token>>, atable: &SymbolTable) -> SymbolTable {
    let mut ctable = SymbolTable {
        map: HashMap::new()
    };

    let mut iter = vardecls_node.children(arena);
    while let Some(child) = iter.next() {
        // Implement logic to parse out vardecl

        let mut ndx = child.children(arena).nth(3).expect("Could not extract TYPE");
        let var_type = DynamicType::from_tree_node(ndx, arena, atable);

        let mut ids_iter = child.children(arena).nth(1).unwrap().children(arena);
        while let Some(nt_ids) = ids_iter.next() {
            let id = arena[nt_ids].data.clone();
            ctable.push(&(*id).val, var_type.clone().unwrap());

            ids_iter = match ids_iter.nth(1) {
                Some(i) => i.children(&arena),
                None    => break
            }
        }

        iter = iter.next().expect("Expected VARDECLS").children(arena);
    }

    iter = funcdecls_node.children(&arena);
    while let Some(func_node) = iter.next() {
        // Extract function return type
        let type_node = func_node.children(arena).nth(6).unwrap();
        let ret_type = DynamicType::from_tree_node(type_node, arena, atable);

        // Extract function identifier
        let id_ndx = func_node.children(arena).nth(1).unwrap();
        let id = arena[id_ndx].data.clone();

        let mut params_iter = func_node.children(arena).nth(3).unwrap().children(arena);

        // Insert param types
        if let Some(neparams_node) = params_iter.next() {
            let mut neparams_iter = neparams_node.children(arena);
            while let Some(param_node) = neparams_iter.next() {
                let param_type_node = param_node.children(arena).nth(2).unwrap();
                let param_type = DynamicType::from_tree_node(param_type_node, arena, atable);
                ctable.push(&(*id).val, param_type.clone().unwrap());

                neparams_iter = match neparams_iter.next() {
                    Some(node)  => node.children(arena),
                    None        => break
                };
            }
        }

        // Insert return type
        ctable.push(&(*id).val, ret_type.clone().unwrap());
        iter = iter.next().unwrap().children(arena);
    }

    // print!("{}", ctable);
    ctable
}

pub fn build_ast(ast: &Vec<Rc<Token>>) -> (Arena<Rc<Token>>, NodeId) {
    let mut arena: Arena<Rc<Token>> = Arena::new();
    let mut ast_iter = ast.into_iter();

    let mut root = arena.new_node(Rc::new(Token::get_blank()));
    let mut curr = root;

    while let Some(node) = ast_iter.next() {
        if (*node.val).eq("@(") {
            let rc_token = ast_iter.next().unwrap();
            let nid: NodeId = arena.new_node(rc_token.clone());
            curr.append(nid, &mut arena);
            curr = nid;
        } else if (*node.val).eq("@)") {
            curr = curr.ancestors(&arena).nth(1).unwrap();
        } else {
            let nid = arena.new_node(node.clone());
            curr.append(nid, &mut arena);
        }
    }
    root = root.children(&arena).next().unwrap();
    (arena, root)
}

pub fn debug_print_ast(arena: &Arena<Rc<Token>>, root: NodeId) {
    let mut v = VecDeque::new();
    let mut i = 1;
    v.push_back((root, 1));
    print!("\n");
    let mut curr: Rc<Token>;
    while !v.is_empty() {
        let (curr, i) = v.pop_front().unwrap();
        // print!("{}-", arena[curr].data);
        for mut child in curr.children(&arena) {
            print!("{}:{} ", arena[child].data, i);
            v.push_back((child, i + 1));
        };
        print!("\n");
    }
}
