use ahash::HashMap;

use self::context::{Context, Var};
pub use self::{check::TypeChecker, context::Key};
use crate::hir;

mod check;
mod context;

pub type TypeResult<T> = Result<T, TypeError>;

#[derive(Debug)]
pub enum TypeError {
    Mismatch(Key, Key),
    NotCallable,
    CannotInfer,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Type {
    Int,
    Bool,
    Void,
    Function(Vec<Type>, Box<Type>),
}

#[derive(Debug, Clone)]
pub struct Types {
    ctx: Context,
    functions: HashMap<hir::FunctionId, Var>,
    bindings: HashMap<hir::BindingId, Var>,
    exprs: HashMap<hir::ExprId, Var>,
}

impl Types {
    pub fn function(&self, id: hir::FunctionId) -> TypeResult<Type> {
        let var = self.functions.get(&id).expect("not type checked");
        self.ctx.reconstruct(*var)
    }

    pub fn binding(&self, id: hir::BindingId) -> TypeResult<Type> {
        let var = self.bindings.get(&id).expect("not type checked");
        self.ctx.reconstruct(*var)
    }

    pub fn expr(&self, id: hir::ExprId) -> TypeResult<Type> {
        let var = self.exprs.get(&id).expect("not type checked");
        self.ctx.reconstruct(*var)
    }
}
