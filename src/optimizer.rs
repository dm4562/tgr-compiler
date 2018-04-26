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
    num: usize,
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
    statements: Vec<Statement>
}

impl BasicBlock {
    pub fn new() -> BasicBlock {
        BasicBlock {
            statements: Vec::new()
        }
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
    let mut cfg = Graph::<BasicBlock, ()>::new();
    let mut cur_block = cfg.add_node(BasicBlock::new());
    

}


fn find_dead_code(cfg: &ControlFlowGraph) {

}