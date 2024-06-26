use std::{fmt, ops::Deref};

use internment::Intern;

use crate::token::Token;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct File(pub Vec<Function>);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Type {
    Int,
    Bool,
    Void,
    Function {
        params: Vec<Type>,
        return_type: Option<Box<Type>>,
    },

    ParseError,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Function {
    pub name: Ident,
    pub params: Vec<(Ident, Type)>,
    pub return_type: Option<Type>,
    pub body: Block,
}

#[derive(Debug, Default, Clone, Hash, PartialEq, Eq)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub functions: Vec<Function>,
    pub tail: Option<Box<Expr>>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Stmt {
    Expr(Expr),
    Let {
        name: Ident,
        ty: Option<Type>,
        value: Expr,
    },
    Break,
    Continue,
    Return(Option<Expr>),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Expr {
    Block(Block),
    If(ExprIf),
    Loop(Block),
    While {
        condition: Box<Expr>,
        body: Block,
    },

    Prefix {
        op: PrefixOp,
        expr: Box<Expr>,
    },
    Infix {
        lhs: Box<Expr>,
        op: InfixOp,
        rhs: Box<Expr>,
    },
    Paren(Box<Expr>),
    Call {
        function: Box<Expr>,
        args: Vec<Expr>,
    },

    Int(u64),
    Bool(bool),
    Ident(Ident),

    ParseError,
}

impl Expr {
    pub fn is_inline(&self) -> bool {
        matches!(
            self,
            Expr::Prefix { .. }
                | Expr::Infix { .. }
                | Expr::Paren(_)
                | Expr::Call { .. }
                | Expr::Int(_)
                | Expr::Bool(_)
                | Expr::Ident(_)
        )
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ExprIf {
    pub condition: Box<Expr>,
    pub then: Block,
    pub r#else: Option<ExprElse>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum ExprElse {
    If(Box<ExprIf>),
    Block(Block),
}

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct Ident(Intern<str>);

impl<T: Into<Intern<str>>> From<T> for Ident {
    fn from(s: T) -> Self {
        Ident(s.into())
    }
}

impl fmt::Debug for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.0.as_ref())
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.0, f)
    }
}

impl Deref for Ident {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum PrefixOp {
    Not,
    Negate,
}

impl From<Token> for PrefixOp {
    fn from(token: Token) -> Self {
        match token {
            Token::Not => PrefixOp::Not,
            Token::Minus => PrefixOp::Negate,
            _ => panic!("invalid prefix operator: {token:?}"),
        }
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum InfixOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    LogicAnd,
    LogicOr,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    Assign,
}

impl From<Token> for InfixOp {
    fn from(token: Token) -> Self {
        match token {
            Token::Plus => InfixOp::Add,
            Token::Minus => InfixOp::Sub,
            Token::Star => InfixOp::Mul,
            Token::Slash => InfixOp::Div,
            Token::Percent => InfixOp::Rem,
            Token::LogicAnd => InfixOp::LogicAnd,
            Token::LogicOr => InfixOp::LogicOr,
            Token::Eq => InfixOp::Eq,
            Token::Ne => InfixOp::Ne,
            Token::Lt => InfixOp::Lt,
            Token::Le => InfixOp::Le,
            Token::Gt => InfixOp::Gt,
            Token::Ge => InfixOp::Ge,
            Token::Equals => InfixOp::Assign,
            _ => panic!("invalid infix operator: {token:?}"),
        }
    }
}
