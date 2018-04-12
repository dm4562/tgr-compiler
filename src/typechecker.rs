use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::collections::VecDeque;
use std::fmt;
use std::rc::Rc;
use indextree::{Arena, NodeId};
use indexmap::IndexMap;

use scanner::Token;

#[derive(Debug)]
pub struct SymbolTable {
    map: HashMap<String, DynamicType>
}

impl SymbolTable {
    fn push(&mut self, id: &String, sym_type: DynamicType) -> Result<(), String> {
        // self.map.entry(id.to_owned()).or_insert(VecDeque::new());
        match self.map.entry(id.to_owned()) {
            Entry::Vacant(v)      => v.insert(sym_type),
            Entry::Occupied(_)    => return Err(format!("'{}' is defined multiple times in the same lexical scope!", id)),
        };

        Ok(())
    }

    fn pop(&mut self, id: &String) -> Option<DynamicType> {
        self.map.remove(id)
    }

    fn find(&self, id: &String) -> Option<&DynamicType> {
        self.map.get(id)
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

#[derive(Debug)]
pub struct FunctionTable {
    map: HashMap<Rc<String>, IndexMap<Rc<String>, DynamicType>>
}

impl FunctionTable {
    fn match_args(&self, id: &String, args_list: &Vec<DynamicType>) -> bool {
        let argmap = match self.map.get(id) {
            Some(map)   => map,
            None        => return false
        };

        let mut vi = args_list.iter();
        for (param, param_type) in argmap.iter() {
            let a_type  = match vi.next() {
                Some(t) => t,
                None    => return false
            };

            if param_type != a_type {
                return false;
            }
        }

        true
    }
}

impl fmt::Display for FunctionTable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Symbol Table\n")?;
        for key in self.map.keys() {
            write!(f, "func: {}\n", key)?;
            let argmap = self.map.get(key).unwrap();
            for arg_key in argmap.keys() {
                write!(f, "{} - {:?}\n", arg_key, argmap.get(arg_key).unwrap())?;
            }
            write!(f, "\n")?;
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

pub fn check_program(arena: &Arena<Rc<Token>>, program_node: NodeId) -> Result<(SymbolTable, SymbolTable, FunctionTable), String> {
    let mut queue = VecDeque::new();
    queue.push_back(program_node);
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

    let atable = build_alias_map(typedecls_node.unwrap(), arena)?;
    let mut ctable = build_context_map(vardecls_node.unwrap(), funcdecls_node.unwrap(), arena, &atable)?;
    let ftable = build_func_context_map(funcdecls_node.unwrap(), arena, &atable, &mut ctable)?;

    check_funcdecls(funcdecls_node.unwrap(), arena, &ctable, &ftable)?;
    check_stmts(program_node.children(arena).nth(3).unwrap(), arena, &ctable, &ftable)?;

    Ok((atable, ctable, ftable))
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

        atable.push(&(*id).val, ret_type)?;
        iter = iter.next().expect("Expected TYPEDECLS").children(arena);
    }

    Ok(atable)
}

fn build_func_context_map(funcdecls_node: NodeId, arena: &Arena<Rc<Token>>, atable: &SymbolTable, ctable: &mut SymbolTable) -> Result<FunctionTable, String> {
    let mut ftable = FunctionTable {
        map: HashMap::new()
    };

    let mut iter = funcdecls_node.children(&arena);
    while let Some(func_node) = iter.next() {
        // Extract function return type
        let type_node = func_node.children(arena).nth(6).unwrap();
        let ret_type = DynamicType::from_tree_node(type_node, arena, atable)?;

        // Extract function identifier
        let id_ndx = func_node.children(arena).nth(1).unwrap();
        let id = arena[id_ndx].data.clone();

        let mut params_iter = func_node.children(arena).nth(3).unwrap().children(arena);

        // Create entry in FunctionTable for id
        ftable.map.insert(id.val.clone(), IndexMap::new());
        {
            let args_map = ftable.map.get_mut(&(*id.val)).unwrap();

            // Insert param types
            if let Some(neparams_node) = params_iter.next() {
                let mut neparams_iter = neparams_node.children(arena);
                while let Some(param_node) = neparams_iter.next() {
                    let param_id_ndx = param_node.children(arena).next().unwrap();
                    let param_id = arena[param_id_ndx].data.clone();

                    let param_type_node = param_node.children(arena).nth(2).unwrap();
                    let param_type = DynamicType::from_tree_node(param_type_node, arena, atable)?;

                    args_map.insert(param_id.val.clone(), param_type);

                    neparams_iter = match neparams_iter.next() {
                        Some(node)  => node.children(arena),
                        None        => break
                    };
                }
            }
        }


        // Insert return type
        ctable.push(&(*id).val, ret_type)?;
        iter = iter.next().unwrap().children(arena);
    }

    Ok(ftable)
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
            ctable.push(&(*id).val, var_type.clone())?;

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

    Ok(ctable)
}

fn check_funcdecls(funcdecls_node: NodeId, arena: &Arena<Rc<Token>>, ctable: &SymbolTable, ftable: &FunctionTable) -> Result<(), String> {
    let mut node = funcdecls_node;

    while let Some(funcdecl_node) = node.children(arena).nth(0) {
        let id_ndx = funcdecl_node.children(arena).nth(1).unwrap();
        let func_name = &*arena[id_ndx].data.val;

        let func_ret_type = match ctable.find(func_name) {
            Some(t) => t,
            None    => return Err("Return type not defined".to_owned())
        };

        check_funcdecl(funcdecl_node, arena, ctable, ftable, func_ret_type, func_name)?;

        node = node.children(arena).nth(1).unwrap();
    }

    Ok(())
}

fn check_funcdecl(funcdecl_node: NodeId, arena: &Arena<Rc<Token>>, ctable: &SymbolTable, ftable: &FunctionTable, ret_type: &DynamicType, func_name: &String) -> Result<(), String> {
    check_stmts(funcdecl_node.children(arena).nth(8).unwrap(), arena, ctable, ftable)?;
    check_return_paths(funcdecl_node.children(arena).nth(8).unwrap(), arena, ctable, ftable, ret_type, func_name)?;

    Ok(())
}

/// Checks that all possible executable paths in a function return and return the correct type.
fn check_return_paths(stmts_node: NodeId, arena: &Arena<Rc<Token>>, ctable: &SymbolTable, ftable: &FunctionTable, ret_type: &DynamicType, name: &String) -> Result<Option<DynamicType>, String> {
    let mut cur_return: Option<DynamicType> = None;

    for child_node in stmts_node.children(arena) {
        match arena[child_node].data.val.as_str() {
            "if"    => {
                check_return_paths(child_node.children(arena).nth(3).unwrap(), arena, ctable, ftable, ret_type, name)?;
                // If both 'if' and 'else' parts have a return type, then the whole block has a return value as well
                match child_node.children(arena).nth(5) {
                    Some(v) => {
                        if let Some(ret_type) = check_return_paths(child_node.children(arena).nth(3).unwrap(), arena, ctable, ftable, ret_type, name)? {
                            cur_return = Some(ret_type);
                        }
                    },
                    None    => {},
                    }
            }
            "return"    => {
                let new_ret_type = evaluate_expr(child_node.children(arena).nth(3).unwrap(), arena, ctable, ftable)?;
                cur_return = match new_ret_type == *ret_type {
                    true    => Some(new_ret_type),
                    false   => return Err(format!("the type of a return path of '{}' does not match its definition!", name)),
                };
            },
            _       => {},
        }
    };

    cur_return.map_or(Err(format!("does not return a value on all code paths!")), |ret| Ok(Some(ret)))
}

fn evaluate_id(id_node: NodeId, arena: &Arena<Rc<Token>>, ctable: &SymbolTable) -> Result<DynamicType, String> {
    // match ftable {
    //     Some(f) => match f.map.get(key)(id) {
    //         Some(t)  => return Ok(t),
    //         None        => {},
    //     },
    //     None    => {},
    // };

    let id = &*arena[id_node].data.val;
    match ctable.find(id) {
        Some(t) => Ok(t.clone()),
        None    => Err(format!("symbol '{}' is not defined!", id)),
    }
}

fn check_stmts(stmts_node: NodeId, arena: &Arena<Rc<Token>>, ctable: &SymbolTable, ftable: &FunctionTable) -> Result<(), String> {
    let mut node = stmts_node;

    while let Some(fullstmt_node) = node.children(arena).next() {
        let stmt_node = match fullstmt_node.children(arena).next() {
            Some(n) => n,
            None    => return Err("STMT not found".to_owned())
        };

        check_stmt(stmt_node, arena, ctable, ftable)?;
        node = node.children(arena).nth(1).unwrap();
    }

    Ok(())
}

fn check_stmt(stmt_node: NodeId, arena: &Arena<Rc<Token>>, ctable: &SymbolTable, ftable: &FunctionTable) -> Result<(), String> {
    fn evaluate_lvalue(lvalue_node: NodeId, arena: &Arena<Rc<Token>>, ctable: &SymbolTable, ftable: &FunctionTable) -> Result<DynamicType, String> {
        let id_node = lvalue_node.children(arena).nth(0).unwrap();
        let id_type = evaluate_id(id_node, arena, ctable)?;
        // Check if OPTOFFSET is there (indexing into a value)
        match lvalue_node.children(arena).nth(1).unwrap().children(arena).nth(1) {
            Some(optoffset_node) => {
                // Make sure the index is an integer
                if evaluate_expr(optoffset_node, arena, ctable, ftable)?.cur_type != BaseType::Integer {
                    return Err(format!("index expressions must be of type int!"));
                }
                match id_type.sub_type {
                    Some(sub_type)  => Ok((**sub_type).clone()),
                    None            => return Err(format!("'{}' is not of an indexable type!", &*arena[id_node].data.val)),
                }
            },
            None    => Ok(id_type),
        }
    }

    match (*arena[stmt_node.children(arena).nth(0).unwrap()].data.val).as_str() {
        "if"        => {
            /*
             * STMT -> if EXPR then STMTS endif
             * STMT -> if EXPR then STMTS else STMTS endif
             */
            if evaluate_expr(stmt_node.children(arena).nth(1).unwrap(), arena, ctable, ftable)?.cur_type != BaseType::Boolean {
                return Err(format!("an if condition must be of type boolean!"));
            }
            check_stmts(stmt_node.children(arena).nth(3).unwrap(), arena, ctable, ftable)?;
            match stmt_node.children(arena).nth(5) {
                Some(else_stmts_node) => { check_stmts(else_stmts_node, arena, ctable, ftable)?; },
                None  => {},
            }
        },
        "while"     => {
            // STMT -> while EXPR do STMTS enddo
            if evaluate_expr(stmt_node.children(arena).nth(1).unwrap(), arena, ctable, ftable)?.cur_type != BaseType::Boolean {
                return Err(format!("a while condition must be of type boolean!"));
            }
            check_stmts(stmt_node.children(arena).nth(3).unwrap(), arena, ctable, ftable)?;
        },
        "for"       => {
            // STMT -> for id := EXPR to EXPR do STMTS enddo
            if evaluate_id(stmt_node.children(arena).nth(0).unwrap(), arena, ctable)?.cur_type != BaseType::Integer {
                return Err(format!("a while condition must be of type boolean!"));
            }
            if evaluate_expr(stmt_node.children(arena).nth(3).unwrap(), arena, ctable, ftable)?.cur_type != BaseType::Integer {
                return Err(format!("a while condition must be of type boolean!"));
            }
            if evaluate_expr(stmt_node.children(arena).nth(5).unwrap(), arena, ctable, ftable)?.cur_type != BaseType::Integer {
                return Err(format!("a while condition must be of type boolean!"));
            }
            check_stmts(stmt_node.children(arena).nth(3).unwrap(), arena, ctable, ftable)?;
        },
        "break"     => {},
        "return"    => {},
        _           => {
            //STMT -> LVALUE := EXPR
            let lhs_node = stmt_node.children(arena).nth(0).unwrap();
            let rhs_node = stmt_node.children(arena).nth(2).unwrap();
            if evaluate_lvalue(lhs_node, arena, ctable, ftable)? != evaluate_expr(rhs_node, arena, ctable, ftable)? {
                return Err(format!("type mismatch in assignment statement!"));
            }
        },
    }

    Ok(())
}

fn evaluate_expr(expr_node: NodeId, arena: &Arena<Rc<Token>>, ctable: &SymbolTable, ftable: &FunctionTable) -> Result<DynamicType, String> {
    match expr_node.children(arena).nth(2) {
        Some(clause_node)  => {
            let clause_type = evaluate_clause(clause_node, arena, ctable, ftable)?;
            let expr_type = evaluate_expr(expr_node.children(arena).nth(0).unwrap(), arena, ctable, ftable)?;
            match clause_type == expr_type {
                true    => Ok(expr_type),
                false   => Err(format!("expression is not well-typed!")),
            }
        }
        None => evaluate_clause(expr_node.children(arena).nth(0).unwrap(), arena, ctable, ftable)
    }
}

fn evaluate_exprs(exprs_node: NodeId, arena: &Arena<Rc<Token>>, ctable: &SymbolTable, ftable: &FunctionTable) -> Result<Vec<DynamicType>, String> {
    let mut result: Vec<DynamicType> = Vec::new();
    let mut neexprs_node = match exprs_node.children(arena).next() {
        Some(node)  => node,
        None => return Ok(result)
    };

    while let Some(expr_node) = neexprs_node.children(arena).next() {
        let expr_type = evaluate_expr(expr_node, arena, ctable, ftable)?;
        result.push(expr_type);
        // Iterate over neexprs
        neexprs_node = neexprs_node.children(arena).nth(2).unwrap();
    }

    Ok(result)
}

fn evaluate_clause(clause_node: NodeId, arena: &Arena<Rc<Token>>, ctable: &SymbolTable, ftable: &FunctionTable) -> Result<DynamicType, String> {
    let mut node = clause_node;
    let mut flag = false;
    while let Some(pred_node) = node.children(arena).nth(2) {
        flag = true;
        evaluate_pred(pred_node, arena, ctable, ftable)?;
        // Iteratively expand clause
        // Unwrap should never fail here
        node = node.children(arena).next().unwrap();
    }

    let final_node = match node.children(arena).next() {
        Some(node)  => node,
        None        => return Err("Invalid AEXPR".to_owned())
    };

    if !flag {
        evaluate_pred(final_node, arena, ctable, ftable)
    } else {
        Ok(DynamicType {
            cur_type: BaseType::Boolean,
            sub_type: None
        })
    }
}

fn evaluate_pred(pred_node: NodeId, arena: &Arena<Rc<Token>>, ctable: &SymbolTable, ftable: &FunctionTable) -> Result<DynamicType, String> {
    let mut node = pred_node;

    let mut flag = false;
    while let Some(aexpr_node) = node.children(arena).nth(2) {
        // evaluate term_node
        flag = true;
        evaluate_aexpr(aexpr_node, arena, ctable, ftable)?;

        node = node.children(arena).next().expect("AEXPR not found in PRED");
    }

    let final_aexpr = match node.children(arena).next() {
        Some(node)  => node,
        None        => return Err("Invalid AEXPR".to_owned())
    };

    if !flag {
        evaluate_aexpr(final_aexpr, arena, ctable, ftable)
    } else {
        Ok(DynamicType {
            cur_type: BaseType::Boolean,
            sub_type: None
        })
    }
}

fn evaluate_aexpr(aexpr_node: NodeId, arena: &Arena<Rc<Token>>, ctable: &SymbolTable, ftable: &FunctionTable) -> Result<DynamicType, String> {
    let mut node = aexpr_node;

    let mut pre_term_type: Option<DynamicType> = None;
    while let Some(term_node) = node.children(arena).nth(2) {
        // evaluate term_node
        let term_type = evaluate_term(term_node, arena, ctable, ftable)?;
        pre_term_type = match pre_term_type {
            Some(pre_type)  => {
                if pre_type.cur_type == BaseType::Float {
                    Some(pre_type)
                } else {
                    Some(term_type)
                }
            },
            None            => Some(term_type),
        };

        // iteratively expand aexpr
        node = node.children(arena).next().expect("TERM not found in AEXPR");
    }

    let final_term_node = match node.children(arena).next() {
        Some(node)  => node,
        None        => return Err("Invalid TERM".to_owned())
    };

    let final_term_type = evaluate_term(final_term_node, arena, ctable, ftable)?;
    pre_term_type = match pre_term_type {
        Some(pre_type)  => {
            if pre_type.cur_type == BaseType::Float {
                Some(pre_type)
            } else {
                Some(final_term_type)
            }
        },
        None            => Some(final_term_type),
    };

    Ok(pre_term_type.unwrap())
}

fn evaluate_term(term_node: NodeId, arena: &Arena<Rc<Token>>, ctable: &SymbolTable, ftable: &FunctionTable) -> Result<DynamicType, String> {
    let mut curr_term_node = term_node;
    let mut term_child_iter = curr_term_node.children(arena);

    let mut pre_factor_type: Option<DynamicType> = None;
    while let Some(factor_node) = term_child_iter.nth(2) {
        // get the type of factor node
        let factor_type: DynamicType = evaluate_factor(factor_node, arena, ctable, ftable)?;
        pre_factor_type = match pre_factor_type {
            Some(pre_type)  => {
                if pre_type.cur_type == BaseType::Float {
                    Some(pre_type)
                } else {
                    Some(factor_type)
                }
            },
            None            => Some(factor_type),
        };

        // Iteratively expand term
        // Unwrap should never fail here
        curr_term_node = curr_term_node.children(arena).next().unwrap();
        term_child_iter = curr_term_node.children(arena);
    }

    let final_factor_node = term_node.children(arena).next().unwrap();
    let final_factor_type = evaluate_factor(final_factor_node, arena, ctable, ftable)?;
    pre_factor_type = match pre_factor_type {
        Some(pre_type)  => {
            if pre_type.cur_type == BaseType::Float {
                Some(pre_type)
            } else {
                Some(final_factor_type)
            }
        },
        None            => Some(final_factor_type),
    };

    Ok(pre_factor_type.unwrap())
}

fn evaluate_factor(factor_node: NodeId, arena: &Arena<Rc<Token>>, ctable: &SymbolTable, ftable: &FunctionTable) -> Result<DynamicType, String> {
    let factor_child: NodeId = factor_node.children(arena).next().unwrap();

    // Check if factor is a constant literal
    let factor_type = DynamicType::from_const_token(&*(arena[factor_child].data));
    match factor_type {
        Some(t) => return Ok(t),
        None    => {}
    };

    let ret_type: Result<DynamicType, String>;
    if (*arena[factor_child].data).token_name.eq("identifier") {
        // If factor starts with an identifier
        let id = (*arena[factor_child].data).val.clone();
        let id_type: DynamicType = match ctable.find(&id) {
            Some(t) => t.clone(),
            None    => return Err("Type mismatch error: Type not found".to_owned())
        };

        let bracket_node = match factor_child.following_siblings(arena).next() {
            Some(bracket)   => bracket,
            None            => return Ok(id_type)
        };

        let e_node = match bracket_node.following_siblings(arena).next() {
            Some(node)  => node,
            None        => return Err("expression not found".to_owned())
        };

        ret_type = match &*arena[bracket_node].data.val.as_str() {
            "[" => {
                // TODO: return the array type
                let e_type = evaluate_expr(e_node, arena, ctable, ftable)?;
                if e_type.cur_type != BaseType::Integer {
                    Err("Type mismatch error: Can only index with Integer".to_owned())
                } else {
                    match e_type.sub_type {
                        Some(rc_type)   => Ok((**rc_type).clone()),
                        None            => Err("Type mismatch error".to_owned())
                    }
                }
            },
            "(" => {
                let arglist = evaluate_exprs(e_node, arena, ctable, ftable)?;
                if !ftable.match_args(&id, &arglist) {
                    Err("Type mismatch error!".to_owned())
                } else {
                    Ok(id_type)
                }
            },
            _   => Err("Unexpected bracket".to_owned())
        };
    } else {
        let e_node = match factor_child.following_siblings(arena).next() {
            Some(node)  => node,
            None        => return Err("Could not find EXPR node".to_owned())
        };

        ret_type = evaluate_expr(e_node, arena, ctable, ftable);
    }

    ret_type
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
