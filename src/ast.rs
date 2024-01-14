use internment::Intern;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct File<S> {
    pub items: Vec<Item<S>>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Item<S> {
    Import(Import),
    TypeAlias(TypeAlias<S>),
    Struct(Struct<S>),
    Enum(Enum<S>),
    Function(Function<S>),
    Constant(Binding<S>),
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
pub struct TypeAlias<S> {
    pub lhs: TypeExpr<S>,
    pub rhs: TypeExpr<S>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Struct<S> {
    pub ty: TypeExpr<S>,
    pub body: Variant<TypeExpr<S>>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Enum<S> {
    pub ty: TypeExpr<S>,
    pub variants: Vec<(Intern<str>, Variant<TypeExpr<S>>)>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Variant<T> {
    Unit,
    Tuple(Vec<T>),
    Record(Vec<(Intern<str>, T)>),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum TypeExpr<S> {
    Int,
    Float,
    Bool,
    String,
    Char,
    Named {
        path: S,
        generics: Option<Vec<TypeExpr<S>>>,
    },
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Function<S> {
    pub name: Intern<str>,
    pub generics: Option<Vec<TypeExpr<S>>>,
    pub params: Vec<(Intern<str>, TypeExpr<S>)>,
    pub return_ty: Option<TypeExpr<S>>,
    pub body: Block<S>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Binding<S> {
    pub name: Intern<str>,
    pub ty: Option<TypeExpr<S>>,
    pub value: Expr<S>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Block<S> {
    pub stmts: Vec<Stmt<S>>,
    pub tail: Option<Box<Expr<S>>>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Stmt<S> {
    Expr(Expr<S>),
    Let(Binding<S>),
    Break,
    Continue,
    Return(Option<Expr<S>>),
    Item(Item<S>),
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
pub enum Expr<S> {
    Block(Block<S>),
    If {
        condition: Box<Expr<S>>,
        then_branch: Block<S>,
        else_branch: Option<Block<S>>,
    },
    Loop(Block<S>),
    While {
        condition: Box<Expr<S>>,
        body: Block<S>,
    },

    Prefix {
        op: PrefixOp,
        expr: Box<Expr<S>>,
    },
    Infix {
        lhs: Box<Expr<S>>,
        op: InfixOp,
        rhs: Box<Expr<S>>,
    },
    Paren(Box<Expr<S>>),
    Index {
        expr: Box<Expr<S>>,
        index: Box<Expr<S>>,
    },
    Call {
        function: Box<Expr<S>>,
        args: Vec<Expr<S>>,
    },

    Tuple(Vec<Expr<S>>),
    Array(Vec<Expr<S>>),
    StructLiteral {
        name: Path,
        fields: Vec<(Intern<str>, Expr<S>)>,
    },

    Bool(bool),
    Int(u64),
    Float(Float),
    Char(CharLiteral),
    String(StringLiteral),
    Path(S),
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

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Path {
    pub components: Vec<Intern<str>>,
}
