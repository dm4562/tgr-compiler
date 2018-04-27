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
    let mut scopes = vec![StmtsScope::Program];
    let outgoing_nodes = build_stmts(stmts_node, arena, &mut cfg, &incoming, &mut block_count, &mut scopes);
    scopes.pop();
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

fn build_stmts(stmts: NodeId, arena: &Arena<Rc<Token>>, graph: &mut ControlFlowGraph, incoming: &Vec<NodeIndex>, counter: &mut u64, scopes: &mut Vec<StmtsScope>) -> Vec<NodeIndex> {
    let node = stmts;
    let fullstmt_node = node.children(arena).nth(0).unwrap();
    let stmt_node = fullstmt_node.children(arena).nth(0).unwrap();

    let (stmt_outgoing, stmt_type) = build_stmt(stmt_node, arena, graph, incoming, counter, scopes);
    let mut outgoing= Vec::new();
    println!("scope: {:?} | stmt: {:?}", scopes, &stmt_type);
    if let Some(stmts_node) = stmts.children(arena).nth(1) {
        println!("stmt_outgoing: {:?} - for {:?}", stmt_outgoing, *arena[stmt_node.children(arena).next().unwrap()].data.val);

        if stmt_type == StmtType::Break &&
            (scopes.contains(&StmtsScope::For) || scopes.contains(&StmtsScope::While)) {
            println!("FOR WHILE CONDITION");
        } else {

            let mut stmts_incoming: Vec<NodeIndex> = Vec::new();
            for n in &stmt_outgoing {
                let is_break = is_keyword(*n, graph, &"break".to_owned());
                if is_break && (scopes.contains(&StmtsScope::For) || scopes.contains(&StmtsScope::While)) {
                    outgoing.push(*n);
                } else {
                    stmts_incoming.push(*n);
                }
            }

            outgoing.append(&mut build_stmts(stmts_node, arena, graph, &stmts_incoming, counter, scopes));
        }

    }

    outgoing
}

fn build_stmt(stmt_node: NodeId, arena: &Arena<Rc<Token>>, graph: &mut ControlFlowGraph, incoming: &Vec<NodeIndex>, counter: &mut u64, scopes: &mut Vec<StmtsScope>) -> (Vec<NodeIndex>, StmtType) {
    let first_node = stmt_node.children(arena).nth(0).unwrap();
    if first_node.children(arena).nth(0).is_some() {
        // LVALUE := EXPR (add to block and parse expr)
        return build_lvalue_stmt(stmt_node, arena, graph, incoming, counter, scopes);
    } else {
        let first_word = (arena[first_node].data).val.clone();
        if *first_word == "if" {
            return build_if_stmt(stmt_node, arena, graph, incoming, counter, scopes);
        } else if *first_word == "while" {
            return build_while_stmt(stmt_node, arena, graph, incoming, counter, scopes);
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

fn build_while_stmt(stmt_node: NodeId, arena: &Arena<Rc<Token>>, graph: &mut ControlFlowGraph, incoming: &Vec<NodeIndex>, counter: &mut u64, scopes: &mut Vec<StmtsScope>) -> (Vec<NodeIndex>, StmtType) {

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
    scopes.push(StmtsScope::While);
    let stmts_outgoing = build_stmts(stmts_node, arena, graph, &stmts_incoming, counter, scopes);
    scopes.pop();
    let mut outgoing = vec![expr_block_ndx];
    for n in &stmts_outgoing {
        let is_break: bool = is_keyword(*n, graph, &"break".to_owned());
        if is_break {
            outgoing.push(*n);
        } else {
            graph.add_edge(*n, expr_block_ndx, ());
        }
    }

    (outgoing, StmtType::While)
}

fn build_if_stmt(stmt_node: NodeId, arena: &Arena<Rc<Token>>, graph: &mut ControlFlowGraph, incoming: &Vec<NodeIndex>, counter: &mut u64, scopes: &mut Vec<StmtsScope>) -> (Vec<NodeIndex>, StmtType) {
    // let expr
    let mut outgoing_nodes: Vec<NodeIndex> = Vec::new();
    debug_print_nodes(incoming, graph);

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
    scopes.push(StmtsScope::If);
    let mut if_outgoing = build_stmts(if_stmts_node, arena, graph, &if_incoming, counter, scopes);
    scopes.pop();
    println!("if outgoing: {:?}", if_outgoing);
    outgoing_nodes.append(&mut if_outgoing);
    let ret_type;
    // Check if else exists
    if let Some(else_stmts_node) = stmt_node.children(arena).nth(5) {
        // Deal with else
        let else_incoming = vec![expr_block_ndx];
        scopes.push(StmtsScope::Else);
        let mut else_outgoing = build_stmts(else_stmts_node, arena, graph, &else_incoming, counter, scopes);
        scopes.pop();
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

fn build_lvalue_stmt(stmt_node: NodeId, arena: &Arena<Rc<Token>>, graph: &mut ControlFlowGraph, incoming: &Vec<NodeIndex>, counter: &mut u64, scopes: &Vec<StmtsScope>) -> (Vec<NodeIndex>, StmtType) {
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

fn is_keyword(node: NodeIndex, graph: &ControlFlowGraph, k: &String) -> bool {
    let found = match graph[node].borrow().statements.iter().next() {
        Some(statement_rc)      => match statement_rc.clone().rhs {
            Some(ref token_vec) => match token_vec.iter().next() {
                Some(token)     => *token.name == k.to_owned(),
                None            => false
            },
            None                => false
        },
        None                    => false
    };
    found
}

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

fn debug_print_nodes(vec: &Vec<NodeIndex>, graph: &ControlFlowGraph) {
    for n in vec {
        println!("{:?}", graph[*n].borrow());
    }
}