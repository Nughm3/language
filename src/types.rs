pub use check::TypeTable;
use id_arena::Id;

mod check;
mod context;

#[derive(Debug)]
pub enum Type {
    Int,
    Bool,
    Void,
    Function(Vec<Type>, Box<Type>),
}

pub type TypeId = Id<TypeInfo>;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum TypeInfo {
    Unknown,
    Eq(TypeId),
    Int,
    Bool,
    Void,
    Function(Vec<TypeId>, TypeId),
}

pub type TypeResult<T> = Result<T, TypeError>;

#[derive(Debug)]
pub enum TypeError {
    Mismatch(TypeInfo, TypeInfo),
    NotCallable,
    CannotInfer,
}
