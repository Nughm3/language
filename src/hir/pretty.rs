//! TODO

#![allow(unused)]

use std::fmt::{Debug, Formatter, Result};

use super::{node::*, Hir};

pub trait Pretty {
    fn fmt(&self, hir: &Hir, f: &mut Formatter<'_>) -> Result;
}

impl Debug for Hir {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        todo!()
    }
}

impl Pretty for Module {
    fn fmt(&self, hir: &Hir, f: &mut Formatter<'_>) -> Result {
        todo!()
    }
}

impl Pretty for Item {
    fn fmt(&self, hir: &Hir, f: &mut Formatter<'_>) -> Result {
        todo!()
    }
}

impl Pretty for Ref {
    fn fmt(&self, _hir: &Hir, f: &mut Formatter<'_>) -> Result {
        Debug::fmt(&self, f)
    }
}

impl Pretty for TypeDef {
    fn fmt(&self, hir: &Hir, f: &mut Formatter<'_>) -> Result {
        todo!()
    }
}

impl<T: Debug> Pretty for Variant<T> {
    fn fmt(&self, hir: &Hir, f: &mut Formatter<'_>) -> Result {
        todo!()
    }
}

impl Pretty for TypeExpr {
    fn fmt(&self, hir: &Hir, f: &mut Formatter<'_>) -> Result {
        todo!()
    }
}

impl Pretty for Function {
    fn fmt(&self, hir: &Hir, f: &mut Formatter<'_>) -> Result {
        todo!()
    }
}

impl Pretty for Binding {
    fn fmt(&self, hir: &Hir, f: &mut Formatter<'_>) -> Result {
        todo!()
    }
}

impl Pretty for Block {
    fn fmt(&self, hir: &Hir, f: &mut Formatter<'_>) -> Result {
        todo!()
    }
}

impl Pretty for Stmt {
    fn fmt(&self, hir: &Hir, f: &mut Formatter<'_>) -> Result {
        todo!()
    }
}

impl Pretty for Expr {
    fn fmt(&self, hir: &Hir, f: &mut Formatter<'_>) -> Result {
        todo!()
    }
}
