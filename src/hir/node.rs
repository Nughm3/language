use std::ops::{Index, IndexMut};

use internment::Intern;

use super::Hir;
use crate::ast::{CharLiteral, Float, InfixOp, PrefixOp, StringLiteral};

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Module {
    pub module_metadata: Metadata,
    pub item_metadata: Vec<Metadata>,
    pub items: Vec<Item>,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Item {
    TypeDef(TypeDefId),
    Function(FunctionId),
    Constant(BindingId),
}

/// TODO
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Metadata {}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Ref {
    Item(Item),
    Local(BindingId),
    Param { function: FunctionId, index: usize },
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum TypeDef {
    Alias {
        lhs: TypeExprId,
        rhs: TypeExprId,
    },
    Struct {
        ty: TypeExprId,
        body: Variant<TypeExprId>,
    },
    Enum {
        ty: TypeExprId,
        variants: Vec<Variant<TypeExprId>>,
    },
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Variant<T> {
    Unit,
    Tuple(Vec<T>),
    Record(Vec<(Intern<str>, T)>),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum TypeExpr {
    Int,
    Float,
    Bool,
    String,
    Char,
    Tuple(Vec<TypeExprId>),
    Array {
        ty: TypeExprId,
        len: Option<usize>,
    },
    Function {
        params: Vec<TypeExprId>,
        return_ty: Option<TypeExprId>,
        generics: Option<Vec<TypeExprId>>,
    },
    Named {
        name: Intern<str>, // TODO: resolution
        generics: Option<Vec<TypeExprId>>,
    },
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Function {
    pub name: Intern<str>,
    pub signature: TypeExprId,
    pub body: BlockId,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Binding {
    pub name: Intern<str>,
    pub ty: Option<TypeExprId>,
    pub value: ExprId,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Block {
    stmts: Vec<StmtId>,
    tail: ExprId,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Stmt {
    Expr(ExprId),
    Let(BindingId),
    Break,
    Continue,
    Return(Option<ExprId>),
    Item(Item),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Expr {
    Block(BlockId),
    DecisionTree {
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
    Index {
        expr: ExprId,
        index: ExprId,
    },
    Call {
        function: ExprId,
        args: Vec<ExprId>,
    },

    Tuple(Vec<ExprId>),
    Array(Vec<ExprId>),
    UnitStruct(TypeExprId),
    RecordStruct {
        ty: TypeExprId,
        fields: Vec<(Intern<str>, ExprId)>,
    },
    TupleStruct {
        ty: TypeExprId,
        values: Vec<ExprId>,
    },

    Bool(bool),
    Int(u64),
    Float(Float),
    Char(CharLiteral),
    String(StringLiteral),
    Ref(Ref),
}

pub trait Node {
    type Id;

    fn alloc(self, hir: &mut Hir) -> Self::Id;
}

macro_rules! id {
    ($ty:ident, $id:ident, $arena:ident) => {
        pub type $id = id_arena::Id<$ty>;

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
id!(TypeDef, TypeDefId, type_defs);
id!(TypeExpr, TypeExprId, type_exprs);
id!(Function, FunctionId, functions);
id!(Binding, BindingId, bindings);
id!(Block, BlockId, blocks);
id!(Stmt, StmtId, stmts);
id!(Expr, ExprId, exprs);
