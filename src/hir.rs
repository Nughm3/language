use std::ops::{Index, IndexMut};

use id_arena::{Arena, Id};
use internment::Intern;

use self::{lower::Builder, resolve::Resolvable};
use crate::ast::{InfixOp, PrefixOp};

mod lower;
mod resolve;

// TODO: HIR formatting

#[derive(Debug, Default)]
pub struct Hir {
    modules: Arena<Module>,
    functions: Arena<Function>,
    blocks: Arena<Block>,
    stmts: Arena<Stmt>,
    bindings: Arena<Binding>,
    exprs: Arena<Expr>,
}

impl Hir {
    pub fn builder<'a>() -> Builder<'a> {
        Builder::new()
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Ref {
    Function(FunctionId),
    Local(BindingId),
    Param(usize),

    Late(usize),
    Error,
}

// NOTE (invariant): only functions are late bound, so they don't need to be restricted
impl Resolvable for Ref {
    fn restricted(&self) -> bool {
        matches!(self, Ref::Local(_) | Ref::Param(_))
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Module {
    functions: Vec<FunctionId>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Type {
    Int,
    Bool,
    Void,
    Function {
        params: Vec<Type>,
        return_type: Box<Type>,
    },
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Function {
    pub name: Intern<str>,
    pub signature: Type,
    pub body: BlockId,
}

#[derive(Debug, Default, Clone, Hash, PartialEq, Eq)]
pub struct Block {
    pub functions: Vec<FunctionId>,
    pub stmts: Vec<StmtId>,
    pub tail: Option<ExprId>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Stmt {
    Expr(ExprId),
    Let(BindingId),
    Break,
    Continue,
    Return(Option<ExprId>),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Binding {
    pub name: Intern<str>,
    pub r#type: Option<Type>,
    pub value: ExprId,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Expr {
    Block(BlockId),
    Decision {
        conditions: Vec<ExprId>,
        branches: Vec<BlockId>,
        default: Option<BlockId>,
    },
    Loop(BlockId),

    Prefix {
        op: PrefixOp,
        expr: ExprId,
    },
    Infix {
        lhs: ExprId,
        op: InfixOp,
        rhs: ExprId,
    },
    Call {
        function: ExprId,
        args: Vec<ExprId>,
    },

    Int(u64),
    Bool(bool),
    Ref(Ref),
}

trait Node {
    type Id;

    fn alloc(self, hir: &mut Hir) -> Self::Id;
}

macro_rules! id {
    ($ty:ident, $id:ident, $arena:ident) => {
        pub type $id = Id<$ty>;

        impl Node for $ty {
            type Id = $id;

            fn alloc(self, hir: &mut Hir) -> Self::Id {
                hir.$arena.alloc(self)
            }
        }

        impl Index<$id> for Hir {
            type Output = $ty;

            fn index(&self, id: $id) -> &Self::Output {
                &self.$arena[id]
            }
        }

        impl IndexMut<$id> for Hir {
            fn index_mut(&mut self, id: $id) -> &mut Self::Output {
                &mut self.$arena[id]
            }
        }
    };
}

id!(Module, ModuleId, modules);
id!(Function, FunctionId, functions);
id!(Block, BlockId, blocks);
id!(Stmt, StmtId, stmts);
id!(Binding, BindingId, bindings);
id!(Expr, ExprId, exprs);
