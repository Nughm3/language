use internment::Intern;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct File {
    pub items: Vec<Item>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Item {
    Import(Import),
    TypeAlias(TypeAlias),
    Struct(Struct),
    Enum(Enum),
    Function(Function),
    Constant(Binding),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Import {
    Tree(ImportTree),
    Group(Vec<Import>),
    Wildcard,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ImportTree {
    pub name: Intern<str>,
    pub options: Option<ImportOptions>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum ImportOptions {
    Child(Box<Import>),
    Rename(Intern<str>),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct TypeAlias {
    pub lhs: TypeExpr,
    pub rhs: TypeExpr,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Struct {
    pub ty: TypeExpr,
    pub body: Variant<TypeExpr>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Enum {
    pub ty: TypeExpr,
    pub variants: Vec<(Intern<str>, Variant<TypeExpr>)>,
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
    Named {
        name: Intern<str>,
        generics: Option<Vec<TypeExpr>>,
    },
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Function {
    pub name: Intern<str>,
    pub generics: Option<Vec<TypeExpr>>,
    pub params: Vec<(Intern<str>, TypeExpr)>,
    pub return_ty: Option<TypeExpr>,
    pub body: Block,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Binding {
    pub name: Intern<str>,
    pub ty: Option<TypeExpr>,
    pub value: Expr,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub tail: Option<Box<Expr>>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Stmt {
    Expr(Expr),
    Let(Binding),
    Break,
    Continue,
    Return(Option<Expr>),
    Item(Item),
}

pub type Float = ordered_float::NotNan<f64>;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum CharLiteral {
    Character(char),
    Byte(u8),
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum StringLiteral {
    Text(Intern<str>),
    Byte(Intern<[u8]>),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Expr {
    Block(Block),
    If {
        condition: Box<Expr>,
        then_branch: Block,
        else_branch: Option<Block>,
    },
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
    Index {
        expr: Box<Expr>,
        index: Box<Expr>,
    },
    Call {
        function: Box<Expr>,
        args: Vec<Expr>,
    },

    Tuple(Vec<Expr>),
    Array(Vec<Expr>),
    StructLiteral {
        name: Path,
        fields: Vec<(Intern<str>, Expr)>,
    },

    Bool(bool),
    Int(u64),
    Float(Float),
    Char(CharLiteral),
    String(StringLiteral),
    Path(Path),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Path {
    pub prefix: Vec<Intern<str>>,
    pub name: Intern<str>,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum PrefixOp {
    Not,
    Negate,
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
