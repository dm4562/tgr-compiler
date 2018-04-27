use indextree::{Arena, NodeId};
use petgraph::graph::{Graph, NodeIndex};
use std::hash::{Hash, Hasher};
use std::rc::Rc;
use std::cell::RefCell;
use std::fmt;

use scanner::Token;

pub type ControlFlowGraph = Graph<RefCell<BasicBlock>, ()>;

#[derive(Debug, Hash)]
pub enum OptimizerTokenType {
    Variable,
    Operator,
    Constant,
    Keyword
}

#[derive(Hash)]
pub struct OptimizerToken {
    pub name: Rc<String>,
    pub num: u64,
    pub token_type: OptimizerTokenType
}

pub struct Statement {
    pub lhs: Option<OptimizerToken>, // the LVALUE (if there is one)
    pub rhs: Option<Vec<OptimizerToken>>, // the variables used in the RHS
    pub node: NodeId
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
    // Get declseg node
    let declseg_node = program_node.children(arena).nth(1).unwrap();

    // Get program body
    let program_body = program_node.children(arena).nth(3).unwrap();

    Ok(())
}