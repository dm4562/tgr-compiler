use indextree::{Arena, NodeId};
use petgraph as pg;
use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};
use std::rc::Rc;

use scanner::Token;

type ControlFlowGraph = pg::graph::Graph<Rc<BasicBlock>, ()>;
type Expression = Vec<OptimizerToken>;

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
enum OptimizerTokenType {
    Variable,
    Operator
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
struct OptimizerToken {
    name: Rc<String>,
    id: u64,
    token_type: OptimizerTokenType
}

struct Statement {
    lhs: Option<OptimizerToken>, // the LVALUE (if there is one)
    rhs: Option<Expression>, // the variables used in the RHS
    node: NodeId
}

impl Hash for Statement {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.node.hash(state);
    }
}

impl PartialEq for Statement {
    fn eq(&self, other: &Statement) -> bool {
        self.node == other.node
    }
}
impl Eq for Statement {}

struct BasicBlock {
    statements: Vec<Rc<Statement>>,
    id: u64
}

impl BasicBlock {
    pub fn new(id: u64) -> BasicBlock {
        BasicBlock {
            statements: Vec::new(),
            id: id
        }
    }
}

impl Hash for BasicBlock {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl PartialEq for BasicBlock {
    fn eq(&self, other: &BasicBlock) -> bool {
        self.id == other.id
    }
}
impl Eq for BasicBlock {}

pub fn optimize(arena: &Arena<Rc<Token>>, program_node: NodeId) -> Result<(), String> {
    // Get declseg node
    let declseg_node = program_node.children(arena).nth(1).unwrap();

    // Get program body
    let program_body = program_node.children(arena).nth(3).unwrap(); 

    Ok(())
}


fn build_cfg(arena: &Arena<Rc<Token>>, root_node: NodeId) {
    let mut cfg = ControlFlowGraph::new();    
}


fn analyze_cfg(cfg: &ControlFlowGraph, root_node: pg::graph::NodeIndex) {
    // The expressions generated in a BasicBlock
    let mut gen_map = HashMap::<Rc<BasicBlock>, HashSet<Rc<Expression>>>::new();
    // Maps a variable name to its current OptimizerToken
    let mut token_map = HashMap::<Rc<String>, OptimizerToken>::new();
    // Maps a variable name to the next available id
    let mut id_map = HashMap::<Rc<String>, u64>::new();
    // Maps whether a certain OptimizerToken has been used
    let mut used_map = HashMap::<OptimizerToken, bool>::new();
    // Maps an OptimizerToken to where it was defined
    let mut creation_map = HashMap::<OptimizerToken, Rc<Statement>>::new();

    // Add global variables to maps

    // Add/overwrite function variables to maps


    traverse_cfg(cfg, root_node, &mut gen_map, token_map, &mut id_map, &mut used_map, &mut creation_map, HashSet::new());

    let dead_code = find_dead_code(&used_map, &creation_map);
}

fn traverse_cfg(cfg: &ControlFlowGraph, node: pg::graph::NodeIndex, gen_map: &mut HashMap<Rc<BasicBlock>, HashSet<Rc<Expression>>>, mut token_map: HashMap<Rc<String>, OptimizerToken>, id_map: &mut HashMap<Rc<String>, u64>, used_map: &mut HashMap<OptimizerToken, bool>, creation_map: &mut HashMap<OptimizerToken, Rc<Statement>>, mut visited: HashSet<pg::graph::NodeIndex>) {
    let mut expressions = HashSet::<Rc<Expression>>::new();
    let block = cfg.node_weight(node).unwrap();

    // Go through the BasicBlock and annotate everything
    for stmt in &block.statements {
        if let Some(ref rhs) = stmt.rhs {
            let mut expr = Expression::new();

            for opt_token in rhs {
                if opt_token.token_type == OptimizerTokenType::Variable {
                    // Get the actual token for the current variable
                    let var_token = token_map.get(&opt_token.name).unwrap();

                    // Mark the variable as used
                    {
                        let used_bool = used_map.get_mut(var_token).unwrap();
                        *used_bool |= true;
                    }

                    // Push the variable token into the current expression
                    expr.push(var_token.clone());
                } else {
                    // Push the non-variable token into the current expression
                    expr.push(opt_token.clone());
                }
            }

            expressions.insert(Rc::new(expr));
        }

        if let Some(ref lhs_token) = stmt.lhs {
            assert!(lhs_token.token_type == OptimizerTokenType::Variable);

            // Get the next available id
            let id: u64;
            {
                let id_entry = id_map.get_mut(&lhs_token.name).unwrap();
                id = *id_entry;
                *id_entry += 1;
            }

            // Create a new OptimizerToken
            let mut new_token = lhs_token.clone();
            new_token.id = id;

            // Update the token map to point to the new token
            {
                let token_entry = token_map.get_mut(&new_token.name).unwrap();
                *token_entry = new_token.clone();
            }

            // Insert into the used map
            used_map.insert(new_token.clone(), false);

            // Record the assign statement where new_token was initialized
            creation_map.insert(new_token, stmt.clone());
        }
    }
    
    // Add/merge the expressions generated by the current block to its gen set
    {
        let gen_set = gen_map.entry(block.clone()).or_insert(HashSet::new());
        *gen_set = gen_set.union(&expressions).cloned().collect();
    }

    // Mark the current node as visited
    visited.insert(node);

    // Visit all the unvisited neighbors
    for neighbor in cfg.neighbors(node) {
        if !visited.contains(&neighbor) {
            traverse_cfg(cfg, neighbor, gen_map, token_map.clone(), id_map, used_map, creation_map, visited.clone());
        }
    }
}

fn find_dead_code(used_map: &HashMap<OptimizerToken, bool>, creation_map: &HashMap<OptimizerToken, Rc<Statement>>) -> HashSet<Rc<Statement>> {
    let mut result = HashSet::<Rc<Statement>>::new();

    for (token, is_used) in used_map {
        if !is_used {
            let statement = creation_map.get(token).unwrap().clone();

            // Sanity check
            assert!(statement.lhs.is_some());

            result.insert(statement);
        }
    }

    result
}