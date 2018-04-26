use indextree::{Arena, NodeId};
use petgraph::graph::{Graph, NodeIndex};
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

use scanner::Token;

type ControlFlowGraph = Graph<BasicBlock, ()>;

#[derive(Debug, Hash)]
enum OptimizerTokenType {
    Variable,
    Operator,
    Constant
}

#[derive(Debug, Hash)]
struct OptimizerToken {
    name: Rc<String>,
    num: u64,
    token_type: OptimizerTokenType
}

#[derive(Debug)]
struct Statement {
    lhs: Option<OptimizerToken>, // the LVALUE (if there is one)
    rhs: Option<Vec<OptimizerToken>>, // the variables used in the RHS
    node: NodeId
}

impl Hash for Statement {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.node.hash(state);
    }
}

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
    let mut block_count = 1;
    let declseg_node = root_node.children(arena).nth(1).unwrap();
    let typedecls_node = declseg_node.children(arena).nth(0).unwrap();
    let vardecls_node = declseg_node.children(arena).nth(1).unwrap();

    let typedecls_block = build_typedecls(typedecls_node, arena, &mut block_count);
    let typedecls_ndx = cfg.add_node(typedecls_block);
    let last_vardecls_ndx = add_vardecls_blocks(vardecls_node, arena, &mut block_count, &mut cfg, typedecls_ndx);
}


fn analyze_cfg(cfg: &ControlFlowGraph) {
    let mut avail_map = HashMap::<BasicBlock, Vec<OptimizerToken>>::new();
    let mut intermediate_map = HashMap::<Rc<String>, OptimizerToken>::new();



}

fn build_typedecls(typedecls_node: NodeId, arena: &Arena<Rc<Token>>, counter: &mut u64) -> BasicBlock {
    let mut node = typedecls_node;
    *counter += 1;
    let mut block = BasicBlock::new(*counter);

    while let Some(typedecl_node) = node.children(arena).nth(0) {
        let statement = build_typedecl_statement(typedecl_node, arena);
        block.statements.push(Rc::new(statement));
        node = node.children(arena).nth(1).unwrap();
    }

    block
}

fn build_typedecl_statement(typedecl_node: NodeId, arena: &Arena<Rc<Token>>) -> Statement {
    let id_node = typedecl_node.children(arena).nth(1).unwrap();
    let id = (arena[id_node].data).val.clone();
    let token = OptimizerToken {
        name: id,
        token_type: OptimizerTokenType::Variable,
        num: 0
    };

    Statement {
        lhs: Some(token),
        rhs: None,
        node: typedecl_node
    }
}

fn add_vardecls_blocks(vardecls: NodeId, arena: &Arena<Rc<Token>>, counter: &mut u64, graph: &mut ControlFlowGraph, parent: NodeIndex) -> NodeIndex {
    let mut node = vardecls;
    let mut node_ndx = parent;
    while let Some(vardecl_node) = node.children(arena).nth(0) {
        let block = build_vardecl_block(vardecl_node, arena, counter);
        let bi = graph.add_node(block);
        graph.add_edge(node_ndx, bi, ());
        node_ndx = bi;
        node = vardecl_node.children(arena).nth(2).unwrap();
    }

    node_ndx
}

fn build_vardecl_block(vardecl: NodeId, arena: &Arena<Rc<Token>>, counter: &mut u64) -> BasicBlock {
    let optinit_node = vardecl.children(arena).nth(4).unwrap();
    let optinit_val = optinit_node.children(arena).nth(2);
    let init_val = match optinit_val {
        Some(val_node) => Some((arena[val_node].data).val.clone()),
        None => None
    };

    *counter += 1;
    let mut block = BasicBlock::new(*counter);
    let mut ids_node = vardecl.children(arena).nth(1).unwrap();
    while let Some(id_node) = ids_node.children(arena).nth(0) {
        let id = (arena[id_node].data).val.clone();
        let token = OptimizerToken {
            name: id,
            token_type: OptimizerTokenType::Variable,
            num: 0
        };

        let stmt = Statement {
            lhs: Some(token),
            rhs: match init_val {
                Some(ref token) => Some(vec![OptimizerToken {
                    name: token.clone(),
                    token_type: OptimizerTokenType::Constant,
                    num: 0
                }]),
                None        => None
            },
            node: id_node
        };

        block.statements.push(Rc::new(stmt));
        ids_node = ids_node.children(arena).nth(2).unwrap();
    }

    block
}