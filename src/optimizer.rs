use indextree::{Arena, NodeId};
use petgraph::graph::{Graph};
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

use scanner::Token;

type ControlFlowGraph = Graph<BasicBlock, ()>;
type Expression = Vec<OptimizerToken>;

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
enum OptimizerTokenType {
    Variable,
    Operator
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
struct OptimizerToken {
    name: Rc<String>,
    num: u64,
    token_type: OptimizerTokenType
}

struct Statement {
    lhs: Option<OptimizerToken>, // the LVALUE (if there is one)
    rhs: Expression, // the variables used in the RHS
    node: NodeId
}

impl Hash for Statement {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.node.hash(state);
    }
}

struct BasicBlock {
    statements: Vec<Statement>,
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


fn analyze_cfg(cfg: &ControlFlowGraph) {
    // The expressions generated in a BasicBlock
    let mut gen_map = HashMap::<BasicBlock, Vec<Expression>>::new();
    // Maps a variable name to its current OptimizerToken
    let mut intermediate_map = HashMap::<Rc<String>, OptimizerToken>::new();
    // Maps whether a certain OptimizerToken has been used
    let mut used_map = HashMap::<OptimizerToken, bool>::new();
    // Maps an OptimizerToken to where it was defined
    let mut creation_map = HashMap::<OptimizerToken, Expression>::new();
}