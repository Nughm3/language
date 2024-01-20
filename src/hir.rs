use id_arena::Arena;

pub use self::node::*;

mod node;
mod pretty;

pub struct Hir {
    modules: Arena<Module>,
    type_defs: Arena<TypeDef>,
    type_exprs: Arena<TypeExpr>,
    functions: Arena<Function>,
    bindings: Arena<Binding>,
    blocks: Arena<Block>,
    stmts: Arena<Stmt>,
    exprs: Arena<Expr>,
}
