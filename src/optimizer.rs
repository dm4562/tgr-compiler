use indextree::{Arena, NodeId};
use petgraph::graph::{Graph, NodeIndex};
use petgraph::dot::{Dot, Config};
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::rc::Rc;
use std::cell::RefCell;
use std::fmt;

use scanner::Token;

type ControlFlowGraph = Graph<RefCell<BasicBlock>, ()>;

#[derive(Debug, Hash)]
enum OptimizerTokenType {
    Variable,
    Operator,
    Constant,
    Keyword
}

#[derive(Debug, Copy, Clone, PartialEq)]
enum StmtType {
    Lvalue,
    If,
    IfElse,
    While,
    For,
    Break,
    Return
}

#[derive(Debug, Copy, Clone, PartialEq)]
enum StmtsScope {
    Program,
    If,
    Else,
    While,
    For,
    Function
}

#[derive(Hash)]
struct OptimizerToken {
    name: Rc<String>,
    num: u64,
    token_type: OptimizerTokenType
}

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

impl fmt::Display for OptimizerToken {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", *self.name, self.num)
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
struct BasicBlock {
    statements: Vec<Rc<Statement>>,
    id: u64
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
    // Get declseg node
    let declseg_node = program_node.children(arena).nth(1).unwrap();

    // Get program body
    let program_body = program_node.children(arena).nth(3).unwrap();

    Ok(())
}


pub fn build_cfg(arena: &Arena<Rc<Token>>, root_node: NodeId) {
    let mut cfg = ControlFlowGraph::new();
    let mut block_count = 0;
    let declseg_node = root_node.children(arena).nth(1).unwrap();
    let typedecls_node = declseg_node.children(arena).nth(0).unwrap();
    let vardecls_node = declseg_node.children(arena).nth(1).unwrap();
    let stmts_node = root_node.children(arena).nth(3).unwrap();

    let typedecls_block = build_typedecls(typedecls_node, arena, &mut block_count);
    let typedecls_ndx = cfg.add_node(RefCell::new(typedecls_block));
    let last_vardecls_ndx = add_vardecls_blocks(vardecls_node, arena, &mut block_count, &mut cfg, typedecls_ndx);

    // let stmt_block = BasicBlock::new(&mut block_count);
    // let stmt_block_ndx = cfg.add_node(RefCell::new(stmt_block));
    // cfg.add_edge(last_vardecls_ndx, stmt_block_ndx, ());
    // let outgoing_nodes = build_stmts(stmts_node, arena, &mut cfg, stmt_block_ndx, &mut block_count);

    let incoming = vec![last_vardecls_ndx];
    let outgoing_nodes = build_stmts(stmts_node, arena, &mut cfg, &incoming, &mut block_count, StmtsScope::Program);
    if outgoing_nodes.len() > 0 {
        // Create a new empty last block
        println!("MANY OUTGOING");
        let last = BasicBlock::new(&mut block_count);
        let last_ndx = cfg.add_node(RefCell::new(last));
        for n in outgoing_nodes.iter() {
            cfg.add_edge(*n, last_ndx, ());
        }
    }

    println!("{:?}", Dot::with_config(&cfg, &[Config::EdgeNoLabel]));
}


fn analyze_cfg(cfg: &ControlFlowGraph) {
    let mut avail_map = HashMap::<BasicBlock, Vec<OptimizerToken>>::new();
    let mut intermediate_map = HashMap::<Rc<String>, OptimizerToken>::new();

}

fn build_typedecls(typedecls_node: NodeId, arena: &Arena<Rc<Token>>, counter: &mut u64) -> BasicBlock {
    let mut node = typedecls_node;
    let mut block = BasicBlock::new(counter);

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
        let bi = graph.add_node(RefCell::new(block));
        graph.add_edge(node_ndx, bi, ());
        node_ndx = bi;
        node = node.children(arena).nth(1).unwrap();
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

    let mut block = BasicBlock::new(counter);
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
                None            => None
            },
            node: id_node
        };

        block.statements.push(Rc::new(stmt));
        ids_node = match ids_node.children(arena).nth(2) {
            Some(ids)   => ids,
            None        => break
        };
    }

    block
}


fn build_stmts(stmts: NodeId, arena: &Arena<Rc<Token>>, graph: &mut ControlFlowGraph, incoming: &Vec<NodeIndex>, counter: &mut u64, scope: StmtsScope) -> Vec<NodeIndex> {
    let node = stmts;
    let fullstmt_node = node.children(arena).nth(0).unwrap();
    let stmt_node = fullstmt_node.children(arena).nth(0).unwrap();

    let (mut outgoing, stmt_type) = build_stmt(stmt_node, arena, graph, incoming, counter);
    if let Some(stmts_node) = stmts.children(arena).nth(1) {
        println!("outgoing: {:?} - for {:?}", outgoing, *arena[stmt_node.children(arena).next().unwrap()].data.val);

        if stmt_type == StmtType::Break &&
            (scope == StmtsScope::For || scope == StmtsScope::While) {

        } else {
            outgoing = build_stmts(stmts_node, arena, graph, &outgoing, counter, scope);
        }
    }

    outgoing
}

fn build_stmt(stmt_node: NodeId, arena: &Arena<Rc<Token>>, graph: &mut ControlFlowGraph, incoming: &Vec<NodeIndex>, counter: &mut u64) -> (Vec<NodeIndex>, StmtType) {
    let first_node = stmt_node.children(arena).nth(0).unwrap();
    if first_node.children(arena).nth(0).is_some() {
        // LVALUE := EXPR (add to block and parse expr)
        return build_lvalue_stmt(stmt_node, arena, graph, incoming, counter);
    } else {
        let first_word = (arena[first_node].data).val.clone();
        if *first_word == "if" {
            return build_if_stmt(stmt_node, arena, graph, incoming, counter);
        } else if *first_word == "while" {
            return build_while_stmt(stmt_node, arena, graph, incoming, counter);
        } else if *first_word == "for" {

        } else if *first_word == "break" {
            let mut block = BasicBlock::new(counter);
            let stmt = Statement {
                lhs: None,
                rhs: Some(parse_expr(stmt_node, arena)),
                node: stmt_node
            };
            block.statements.push(Rc::new(stmt));
            let break_ndx = graph.add_node(RefCell::new(block));
            for n in incoming {
                graph.add_edge(*n, break_ndx, ());
            }
            return (vec![break_ndx], StmtType::Break);
        } else {
            // return
        }
    }
    unimplemented!();
}



fn build_while_stmt(stmt_node: NodeId, arena: &Arena<Rc<Token>>, graph: &mut ControlFlowGraph, incoming: &Vec<NodeIndex>, counter: &mut u64) -> (Vec<NodeIndex>, StmtType) {

    let expr_node = stmt_node.children(arena).nth(1).expect("while EXPR not found");
    let rhs = parse_expr(expr_node, arena);
    let expr_stmt = Statement { lhs: None, rhs: Some(rhs), node: expr_node };

    let mut expr_block = BasicBlock::new(counter);
    expr_block.statements.push(Rc::new(expr_stmt));
    let expr_block_ndx = graph.add_node(RefCell::new(expr_block));
    for n in incoming {
        graph.add_edge(*n, expr_block_ndx, ());
    }

    let stmts_incoming = vec![expr_block_ndx];
    let stmts_node = stmt_node.children(arena).nth(3).expect("while STMTS not found");

    // Check if contains break
    let while_outgoing = build_stmts(stmts_node, arena, graph, &stmts_incoming, counter, StmtsScope::While);

    for n in &while_outgoing {
        graph.add_edge(*n, expr_block_ndx, ());
    }

    (vec![expr_block_ndx], StmtType::While)
}

fn build_if_stmt(stmt_node: NodeId, arena: &Arena<Rc<Token>>, graph: &mut ControlFlowGraph, incoming: &Vec<NodeIndex>, counter: &mut u64) -> (Vec<NodeIndex>, StmtType) {
    // let expr
    let mut outgoing_nodes: Vec<NodeIndex> = Vec::new();

    // Deal with EXPR
    let expr_node = stmt_node.children(arena).nth(1).expect("EXPR not found");
    let rhs = parse_expr(expr_node, arena);
    let expr_stmt = Statement { lhs: None, rhs: Some(rhs), node: expr_node };

    let mut expr_block = BasicBlock::new(counter);
    expr_block.statements.push(Rc::new(expr_stmt));
    let expr_block_ndx = graph.add_node(RefCell::new(expr_block));
    for n in incoming {
        graph.add_edge(*n, expr_block_ndx, ());
    }

    // Deal with if STMTS
    let if_incoming = vec![expr_block_ndx];
    let if_stmts_node = stmt_node.children(arena).nth(3).expect("if STMTS not found");
    let mut if_outgoing = build_stmts(if_stmts_node, arena, graph, &if_incoming, counter, StmtsScope::If);
    println!("if outgoing: {:?}", if_outgoing);
    outgoing_nodes.append(&mut if_outgoing);
    let ret_type;
    // Check if else exists
    if let Some(else_stmts_node) = stmt_node.children(arena).nth(5) {
        // Deal with else
        let else_incoming = vec![expr_block_ndx];
        let mut else_outgoing = build_stmts(else_stmts_node, arena, graph, &else_incoming, counter, StmtsScope::Else);
        println!("else outgoing: {:?}", else_outgoing);
        outgoing_nodes.append(&mut else_outgoing);
        ret_type = StmtType::IfElse;
    } else {
        outgoing_nodes.push(expr_block_ndx);
        ret_type = StmtType::If;
    }

    // println!("final outgoing: {:?}", outgoing_nodes);
    (outgoing_nodes, ret_type)
}

fn build_lvalue_stmt(stmt_node: NodeId, arena: &Arena<Rc<Token>>, graph: &mut ControlFlowGraph, incoming: &Vec<NodeIndex>, counter: &mut u64) -> (Vec<NodeIndex>, StmtType) {
    let first_node = stmt_node.children(arena).nth(0).unwrap();
    let id_node = first_node.children(arena).nth(0).unwrap();
    let token = OptimizerToken {
        name: (arena[id_node].data).val.clone(),
        token_type: OptimizerTokenType::Variable,
        num: 0
    };

    let expr_node = stmt_node.children(arena).nth(2).unwrap();
    let rhs = parse_expr(expr_node, arena);

    let stmt = Statement {
        lhs: Some(token),
        rhs: Some(rhs),
        node: stmt_node
    };

    let mut block = BasicBlock::new(counter);
    block.statements.push(Rc::new(stmt));

    let bi = graph.add_node(RefCell::new(block));
    for n in incoming {
        graph.add_edge(*n, bi, ());
    }

    (vec![bi], StmtType::Lvalue)
}
// fn build_stmts(stmts: NodeId, arena: &Arena<Rc<Token>>, graph: &mut ControlFlowGraph, block_ndx: NodeIndex, counter: &mut u64) -> Vec<NodeIndex> {
//     let mut node = stmts;
//     let fullstmt_node = node.children(arena).nth(0).unwrap();
//     let stmt_node = fullstmt_node.children(arena).nth(0).unwrap();

//     let mut outgoing_nodes = build_stmt(stmt_node, arena, graph, block_ndx, counter);
//     if let Some(stmts_node) = stmts.children(arena).nth(1) {
//         println!("outgoing: {:?} - for {:?}", outgoing_nodes, *arena[stmt_node.children(arena).next().unwrap()].data.val);
//         if outgoing_nodes.len() > 0 {
//             let block = BasicBlock::new(counter);
//             let bi = graph.add_node(RefCell::new(block));
//             for outgoing_node in outgoing_nodes {
//                 // println!("IN outgoing - {:?}", outgoing_node);
//                 graph.add_edge(outgoing_node, bi, ());
//             }
//             outgoing_nodes = build_stmts(stmts_node, arena, graph, bi, counter);
//         } else {
//             outgoing_nodes = build_stmts(stmts_node, arena, graph, block_ndx, counter);
//         }
//     }

//     outgoing_nodes
// }

// fn build_stmt(stmt_node: NodeId, arena: &Arena<Rc<Token>>, graph: &mut ControlFlowGraph, block_ndx: NodeIndex, counter: &mut u64) -> Vec<NodeIndex> {
//     let first_node = stmt_node.children(arena).nth(0).unwrap();


//     if first_node.children(arena).nth(0).is_some() {
//         // LVALUE := EXPR (add to block and parse expr)
//         let id_node = first_node.children(arena).nth(0).unwrap();
//         let token = OptimizerToken {
//             name: (arena[id_node].data).val.clone(),
//             token_type: OptimizerTokenType::Variable,
//             num: 0
//         };

//         let expr_node = stmt_node.children(arena).nth(2).unwrap();
//         let rhs = parse_expr(expr_node, arena);

//         let stmt = Statement {
//             lhs: Some(token),
//             rhs: Some(rhs),
//             node: stmt_node
//         };

//         if let Some(block) = graph.node_weight_mut(block_ndx) {
//             // print!("{:?}", *block);
//             block.borrow_mut().statements.push(Rc::new(stmt));
//         }
//         // block.statements.push(Rc::new(stmt));

//         return vec![];
//     } else {
//         let first_word = (arena[first_node].data).val.clone();
//         if *first_word == "if" {
//             // let expr
//             let mut outgoing_nodes: Vec<NodeIndex> = Vec::new();

//             // Deal with EXPR
//             let expr_node = stmt_node.children(arena).nth(1).expect("EXPR not found");
//             let rhs = parse_expr(expr_node, arena);
//             let expr_stmt = Statement { lhs: None, rhs: Some(rhs), node: expr_node };
//             let expr_block_ndx: NodeIndex;

//             if graph.node_weight_mut(block_ndx).expect("Expect block").borrow_mut().statements.len() == 0 {
//                 graph.node_weight_mut(block_ndx).expect("Expect block").borrow_mut().statements.push(Rc::new(expr_stmt));
//                 expr_block_ndx = block_ndx;
//                 println!("IN HERE - {}", graph.node_weight_mut(block_ndx).expect("Expect block").borrow_mut());
//             } else {
//                 let mut expr_block = BasicBlock::new(counter);
//                 expr_block.statements.push(Rc::new(expr_stmt));
//                 println!("IN HERE 2 - {}", expr_block);
//                 expr_block_ndx = graph.add_node(RefCell::new(expr_block));
//                 graph.add_edge(block_ndx, expr_block_ndx, ());
//             }

//             // let mut expr_block = BasicBlock::new(counter);
//             // expr_block.statements.push(Rc::new(expr_stmt));
//             // expr_block_ndx = graph.add_node(RefCell::new(expr_block));
//             // graph.add_edge(block_ndx, expr_block_ndx, ());

//             // Deal with if STMTS
//             let if_stmts_node = stmt_node.children(arena).nth(3).expect("if STMTS not found");
//             let if_stmts_block = BasicBlock::new(counter);
//             let if_stmts_block_ndx = graph.add_node(RefCell::new(if_stmts_block));
//             graph.add_edge(expr_block_ndx, if_stmts_block_ndx, ());
//             let mut if_outgoing = build_stmts(if_stmts_node, arena, graph, if_stmts_block_ndx, counter);
//             println!("if outgoing: {:?}", if_outgoing);
//             outgoing_nodes.append(&mut if_outgoing);
//             outgoing_nodes.push(if_stmts_block_ndx);

//             // Check if else exists
//             if let Some(else_stmts_node) = stmt_node.children(arena).nth(5) {
//                 // Deal with else
//                 let else_stmts_block = BasicBlock::new(counter);
//                 let else_stmts_block_ndx = graph.add_node(RefCell::new(else_stmts_block));
//                 graph.add_edge(expr_block_ndx, else_stmts_block_ndx, ());
//                 let mut else_outgoing = build_stmts(else_stmts_node, arena, graph, else_stmts_block_ndx, counter);
//                 println!("else outgoing: {:?}", else_outgoing);
//                 outgoing_nodes.append(&mut else_outgoing);
//                 outgoing_nodes.push(else_stmts_block_ndx);
//             } else {
//                 outgoing_nodes.push(expr_block_ndx);
//             }

//              println!("final outgoing: {:?}", outgoing_nodes);
//             return outgoing_nodes;
//         } else if *first_word == "while" {

//         } else if *first_word == "for" {

//         } else if *first_word == "break" {

//         } else {
//             // return
//         }
//     }
//     vec![]
// }

fn parse_expr(expr_node: NodeId, arena: &Arena<Rc<Token>>) -> Vec<OptimizerToken> {
    let mut rhs = Vec::new();
    let mut iter = expr_node.descendants(arena);
    iter.next();
    for term in iter {
        if arena[term].data.token_name == "nonterminal" {
            continue
        }

        // print!("{} - \n", arena[term].data.token_name);
        let term_val = OptimizerToken {
            name: (arena[term].data).val.clone(),
            token_type: match (arena[term].data).token_name {
                "id"        => OptimizerTokenType::Variable,
                "keyword"   => OptimizerTokenType::Operator,
                "intlit"    => OptimizerTokenType::Constant,
                "floatlit"  => OptimizerTokenType::Constant,
                _           => OptimizerTokenType::Constant
            },
            num: 0
        };
        rhs.push(term_val);
    }

    rhs
}
