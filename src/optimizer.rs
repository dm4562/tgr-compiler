use indextree::{Arena, NodeId};
use petgraph::graph::{Graph};
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

use scanner::Token;

type ControlFlowGraph = Graph<BasicBlock, ()>;

#[derive(Debug, Hash)]
struct SubVariable {
    name: Rc<String>,
    num: u64,
}

struct Statement {
    assigned: Option<Rc<String>>, // the LVALUE (if there is one)
    used: Vec<Rc<String>>, // the variables used in the RHS
    code: Rc<String>, // original code
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
    
}