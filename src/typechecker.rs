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
        write!(f, "Symbol Table\n")?;
        for key in self.map.keys() {
            write!(f, "{} - {:?}\n", key, self.map.get(key).unwrap())?;
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

    fn from_const_token(token: &Token) -> Option<BaseType> {
        match token.token_name {
            "intlit"    => Some(BaseType::Integer),
            "floatlit"  => Some(BaseType::Float),
            "boolean"   => Some(BaseType::Boolean),
            _           => match &(*token.val.as_str()) {
                "true"  => Some(BaseType::Boolean),
                "false" => Some(BaseType::Boolean),
                _       => None
            }
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
    /// Attempts to convert a TYPE subtree into a recursive `DynamicType` representation.
    ///
    /// `node` should be a `NodeId` pointing to a TYPE node in an `Arena`.
    fn from_tree_node(node: NodeId, arena: &Arena<Rc<Token>>, symbol_table: &SymbolTable) -> Result<DynamicType, String> {
        // The base type can be detected from the first token in a type declaration
        let type_name = &*(arena[node.children(arena).nth(0).unwrap()].data.val);
        let cur_type = match BaseType::from_str(type_name) {
            Some(base)  => base,
            None        => match symbol_table.find(type_name) {
                Some(dyn_type)  => return Ok(dyn_type.clone()),
                None            => return Err(format!("'{}' is not a valid type!", type_name)),
            }
        };

        Ok(DynamicType {
            cur_type: cur_type,
            sub_type: match cur_type.is_recursive_type() {
                true    => match DynamicType::from_tree_node(node.children(arena).last().unwrap(), arena, symbol_table) {
                    Ok(t)  => Some(Rc::new(Box::new(t))),
                    Err(e) => return Err(e),
                },
                false   => None,
            }
        })
    }

    fn from_const_token(token: &Token) -> Option<DynamicType> {
        let cur_type = match BaseType::from_const_token(token) {
            Some(base)  => base,
            None        => return None
        };

        Some(DynamicType {
            cur_type: cur_type,
            sub_type: None
        })
    }
}

pub fn build_type_maps(arena: &Arena<Rc<Token>>, root: NodeId) -> Result<(SymbolTable, SymbolTable), String> {
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

    for child in curr.expect("Couldn't find declseg").children(arena) {
        if (*arena[child].data).token_name.eq("nonterminal") {
            match (arena[child].data).val.as_str() {
                "typedecls"     => typedecls_node   = Some(child),
                "vardecls"      => vardecls_node    = Some(child),
                "funcdecls"     => funcdecls_node   = Some(child),
                _               => { }
            }
        }
    }

    let atable = match build_alias_map(typedecls_node.unwrap(), arena) {
        Ok(table)   => table,
        Err(msg)    => return Err(msg)
    };

    let ctable = match build_context_map(vardecls_node.unwrap(), funcdecls_node.unwrap(), arena, &atable) {
        Ok(table)   => table,
        Err(msg)    => return Err(msg)
    };

    Ok((atable, ctable))
}

fn build_alias_map(typedecls_node: NodeId, arena: &Arena<Rc<Token>>) -> Result<SymbolTable, String> {
    let mut atable = SymbolTable {
        map: HashMap::new()
    };

    let mut iter = typedecls_node.children(arena);
    while let Some(typedecl_node) = iter.next() {
        let mut n = typedecl_node.children(arena).nth(1).expect("Could not extract identifier");
        let id = arena[n].data.clone();
        n = typedecl_node.children(arena).nth(3).expect("Could not extract TYPE");
        let ret_type = DynamicType::from_tree_node(n, arena, &atable)?;

        atable.push(&(*id).val, ret_type);
        iter = iter.next().expect("Expected TYPEDECLS").children(arena);
    }

    // print!("{}", atable);
    Ok(atable)
}

fn build_context_map(vardecls_node: NodeId, funcdecls_node: NodeId, arena: &Arena<Rc<Token>>, atable: &SymbolTable) -> Result<SymbolTable, String> {
    let mut ctable = SymbolTable {
        map: HashMap::new()
    };

    let mut iter = vardecls_node.children(arena);
    while let Some(vardecl_node) = iter.next() {

        let mut ndx = vardecl_node.children(arena).nth(3).expect("Could not extract TYPE");
        let var_type = DynamicType::from_tree_node(ndx, arena, atable)?;

        let mut ids_iter = vardecl_node.children(arena).nth(1).unwrap().children(arena);
        while let Some(nt_ids) = ids_iter.next() {
            let id = arena[nt_ids].data.clone();
            ctable.push(&(*id).val, var_type.clone());

            ids_iter = match ids_iter.nth(1) {
                Some(i) => i.children(&arena),
                None    => break
            }
        }

        if let Some(const_node) = vardecl_node.children(arena).nth(4).unwrap().children(arena).nth(1) {
            let const_ndx = const_node.children(arena).next().unwrap();
            let const_token_rc = arena[const_ndx].data.clone();
            let const_type: DynamicType = match DynamicType::from_const_token(&*const_token_rc) {
                Some(t) => t,
                None    => return Err(format!("unable to resolve constant '{}' type!", const_token_rc.val))
            };

            if !const_type.eq(&var_type) {
                return Err(format!("type mismatch error!"));
            }
        }

        iter = iter.next().expect("Expected VARDECLS").children(arena);
    }

    iter = funcdecls_node.children(&arena);
    while let Some(func_node) = iter.next() {
        // Extract function return type
        let type_node = func_node.children(arena).nth(6).unwrap();
        let ret_type = DynamicType::from_tree_node(type_node, arena, atable)?;

        // Extract function identifier
        let id_ndx = func_node.children(arena).nth(1).unwrap();
        let id = arena[id_ndx].data.clone();

        let mut params_iter = func_node.children(arena).nth(3).unwrap().children(arena);

        // Insert param types
        if let Some(neparams_node) = params_iter.next() {
            let mut neparams_iter = neparams_node.children(arena);
            while let Some(param_node) = neparams_iter.next() {
                let param_type_node = param_node.children(arena).nth(2).unwrap();
                let param_type = DynamicType::from_tree_node(param_type_node, arena, atable)?;
                ctable.push(&(*id).val, param_type);

                neparams_iter = match neparams_iter.next() {
                    Some(node)  => node.children(arena),
                    None        => break
                };
            }
        }

        // Insert return type
        ctable.push(&(*id).val, ret_type);
        iter = iter.next().unwrap().children(arena);
    }

    // print!("{}", ctable);
    Ok(ctable)
}

fn evaluate_expr(expr_node: NodeId, arena: &Arena<Rc<Token>>) -> Result<DynamicType, &'static str> {
    let mut expr_child_iterator = expr_node.children(arena);
    while let Some(clause_node) = expr_child_iterator.nth(2) {
        // Evaluate clause node


        // Iteratively expand expr
        // Unwrap should never fail here
        expr_child_iterator = expr_node.children(arena).next().unwrap().children(arena);
    }

    // Evaluate the last clause node

    Err("unimplemented")
}

fn evaluate_clause(clause_node: NodeId, arena: &Arena<Rc<Token>>) -> Result<DynamicType, &'static str> {
    let mut clause_child_iter = clause_node.children(arena);
    while let Some(pred_node) = clause_child_iter.nth(2) {

        // Iteratively expand clause
        // Unwrap should never fail here
        clause_child_iter = clause_node.children(arena).next().unwrap().children(arena);
    }

    Err("unimplemented")
}

fn evaluate_term(term_node: NodeId, arena: &Arena<Rc<Token>>) -> Result<DynamicType, &'static str> {
    let mut term_child_iter = term_node.children(arena);

    while let Some(factor_node) = term_child_iter.nth(2) {
        let factor_child = factor_node.children(arena).next().unwrap();

        // Check if factor is a constant literal
        let mut factor_type = DynamicType::from_const_token(&*(arena[factor_child].data));
        match factor_type {
            Some(t) => return Ok(t),
            None    => {}
        };

        // Iteratively expand term
        // Unwrap should never fail here
        term_child_iter = term_node.children(arena).next().unwrap().children(arena);
    }

    Err("unimplemented")
}

fn evaluate_factor(factor_node: NodeId, arena: &Arena<Rc<Token>>) -> Result<DynamicType, &'static str> {
    let factor_child = factor_node.children(arena).next().unwrap();

    // Check if factor is a constant literal
    let mut factor_type = DynamicType::from_const_token(&*(arena[factor_child].data));
    match factor_type {
        Some(t) => return Ok(t),
        None    => {}
    };

     Err("unimplemented")
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
