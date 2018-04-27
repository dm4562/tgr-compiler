use indexmap::{IndexMap, IndexSet};
use indextree::{Arena, NodeId};
use petgraph as pg;
use std::collections::{HashMap, HashSet};
use petgraph::graph::{Graph, NodeIndex};
use std::hash::{Hash, Hasher};
use std::rc::Rc;
use std::cell::RefCell;
use std::fmt;

use scanner::Token;
use cfg;

pub type ControlFlowGraph = pg::graph::Graph<RefCell<BasicBlock>, ()>;
pub type Expression = Vec<OptimizerToken>;

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum OptimizerTokenType {
    Variable,
    Operator,
    Constant,
    Keyword
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct OptimizerToken {
    pub name: Rc<String>,
    pub id: u64,
    pub token_type: OptimizerTokenType
}

pub struct Statement {
    pub lhs: Option<OptimizerToken>, // the LVALUE (if there is one)
    pub rhs: Option<Expression>, // the variables used in the RHS
    pub node: NodeId
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

impl fmt::Display for OptimizerToken {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", *self.name, self.id)
    }
}

impl fmt::Debug for OptimizerToken {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(ref lhs) = self.lhs {
            write!(f, "{}", lhs);
        } else {
            write!(f, "None");
        }
        write!(f, " := ");
        match self.rhs {
            Some(ref vec)   => {
                write!(f, "[ ");
                for rhs in vec {
                    write!(f, "{} ", rhs);
                }
                write!(f, "]")
            },
            None        => write!(f, "None")
        }
    }
}

impl fmt::Debug for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

// #[derive(Debug)]
pub struct BasicBlock {
    pub statements: Vec<Rc<Statement>>,
    pub id: u64
}

impl BasicBlock {
    pub fn new(id: &mut u64) -> BasicBlock {
        let b = BasicBlock {
            statements: Vec::new(),
            id: *id
        };
        *id += 1;
        b
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

impl fmt::Display for BasicBlock {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[ id: {}\t", self.id);
        write!(f, "statements: ");
        for stmt in &self.statements {
            write!(f, "{}, ", stmt);
        }
        write!(f, "]")
    }
}

impl fmt::Debug for BasicBlock {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

pub fn optimize(arena: &Arena<Rc<Token>>, program_node: NodeId) -> Result<(), String> {
    let (cfg_root, control) = cfg::build_cfg(arena, program_node);

    Ok(())
}

fn analyze_cfg(graph: &ControlFlowGraph, cfg_root: pg::graph::NodeIndex) {
    // The expressions generated in a BasicBlock
    let mut gen_map = HashMap::<Rc<BasicBlock>, HashSet<Rc<Expression>>>::new();
    // Maps a variable name to its current OptimizerToken
    let mut token_map = HashMap::<Rc<String>, OptimizerToken>::new();
    // Maps a variable name to the next available id
    let mut id_map = HashMap::<Rc<String>, u64>::new();
    // Maps whether a certain OptimizerToken has been used
    let mut used_map = IndexMap::<OptimizerToken, bool>::new();
    // Maps an OptimizerToken to where it was defined
    let mut creation_map = HashMap::<OptimizerToken, Rc<Statement>>::new();

    // Add global variables to maps

    // Add/overwrite function variables to maps


    traverse_cfg(graph, cfg_root, &mut gen_map, token_map, &mut id_map, &mut used_map, &mut creation_map, HashSet::new());

    let dead_code = find_dead_code(&used_map, &creation_map);

    println!("{:?}", dead_code);
}

fn traverse_cfg(graph: &ControlFlowGraph, node: pg::graph::NodeIndex, gen_map: &mut HashMap<Rc<BasicBlock>, HashSet<Rc<Expression>>>, mut token_map: HashMap<Rc<String>, OptimizerToken>, id_map: &mut HashMap<Rc<String>, u64>, used_map: &mut IndexMap<OptimizerToken, bool>, creation_map: &mut HashMap<OptimizerToken, Rc<Statement>>, mut visited: HashSet<pg::graph::NodeIndex>) {
    let mut expressions = HashSet::<Rc<Expression>>::new();
    let block = graph.node_weight(node).unwrap();

    // Go through the BasicBlock and annotate everything
    for stmt in &block.borrow().statements {
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
    // {
    //     let gen_set = gen_map.entry(block.clone()).or_insert(HashSet::new());
    //     *gen_set = gen_set.union(&expressions).cloned().collect();
    // }

    // Mark the current node as visited
    visited.insert(node);

    // Visit all the unvisited neighbors
    for neighbor in graph.neighbors(node) {
        if !visited.contains(&neighbor) {
            traverse_cfg(graph, neighbor, gen_map, token_map.clone(), id_map, used_map, creation_map, visited.clone());
        }
    }
}

fn find_dead_code(used_map: &IndexMap<OptimizerToken, bool>, creation_map: &HashMap<OptimizerToken, Rc<Statement>>) -> IndexSet<Rc<Statement>> {
    let mut result = IndexSet::<Rc<Statement>>::new();

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