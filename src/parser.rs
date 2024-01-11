use chumsky::prelude::{Parser as _, *};
use internment::Intern;
use token::Token::{self, *};

use crate::{ast::*, codemap::FileId, span::Span};

mod token;

pub type ParseError = Simple<Token, Span>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParsedFile {
    pub ast: Option<File>,
    pub errors: Vec<ParseError>,
}

pub fn parse(file_id: FileId, input: &str) -> ParsedFile {
    let (ast, errors) = file().parse_recovery(Token::stream(file_id, input));
    ParsedFile { ast, errors }
}

trait Parser<T>: chumsky::Parser<Token, T, Error = ParseError> + Clone {}
impl<T, P: chumsky::Parser<Token, T, Error = ParseError> + Clone> Parser<T> for P {}

fn file() -> impl Parser<File> {
    item()
        .repeated()
        .then_ignore(end())
        .map(|items| File { items })
}

fn item() -> impl Parser<Item> {
    todo()
}

fn import() -> impl Parser<Import> {
    recursive(|import| {
        let options = just(AsKw)
            .ignore_then(ident())
            .map(ImportOptions::Rename)
            .or(import
                .clone()
                .map(|import| ImportOptions::Child(Box::new(import))));

        let tree = ident()
            .then(just(Dot).ignore_then(options).or_not())
            .map(|(name, options)| Import::Tree(ImportTree { name, options }));

        let group = import
            .separated_by(just(Comma))
            .allow_trailing()
            .delimited_by(just(LeftBrace), just(RightBrace))
            .map(Import::Group);

        let wildcard = just(Star).to(Import::Wildcard);

        just(ImportKw)
            .ignore_then(choice((tree, group, wildcard)))
            .then_ignore(just(Semicolon))
    })
}

fn type_alias() -> impl Parser<TypeAlias> {
    just(TypeKw)
        .ignore_then(type_expr())
        .then_ignore(just(Equals))
        .then(type_expr())
        .then_ignore(just(Semicolon))
        .map(|(lhs, rhs)| TypeAlias { lhs, rhs })
}

fn struct_() -> impl Parser<Struct> {
    just(StructKw)
        .ignore_then(type_expr())
        .then(variant(type_expr()))
        .then(just(Semicolon).or_not())
        .try_map(|((ty, body), semicolon), span| {
            if matches!(body, Variant::Unit | Variant::Tuple(_)) && semicolon.is_none() {
                Err(ParseError::custom(
                    span,
                    "struct variant requires a semicolon: ';'",
                ))
            } else {
                Ok(Struct { ty, body })
            }
        })
}

fn enum_() -> impl Parser<Enum> {
    just(EnumKw)
        .ignore_then(type_expr())
        .then(
            ident()
                .then(variant(type_expr()))
                .separated_by(just(Comma))
                .allow_trailing()
                .delimited_by(just(LeftBrace), just(LeftBrace)),
        )
        .map(|(ty, variants)| Enum { ty, variants })
}

fn variant<T>(p: impl Parser<T>) -> impl Parser<Variant<T>> {
    let tuple = p
        .clone()
        .separated_by(just(Comma))
        .allow_trailing()
        .delimited_by(just(LeftParen), just(RightParen))
        .map(Variant::Tuple);

    let record = ident()
        .then_ignore(just(Colon))
        .then(p)
        .separated_by(just(Comma))
        .allow_trailing()
        .delimited_by(just(LeftBrace), just(RightBrace))
        .map(Variant::Record);

    tuple
        .or(record)
        .or_not()
        .map(|variant| variant.unwrap_or(Variant::Unit))
}

fn type_expr() -> impl Parser<TypeExpr> {
    recursive(|type_expr| {
        let primitive = select! {
            IntTy => TypeExpr::Int,
            FloatTy => TypeExpr::Float,
            BoolTy => TypeExpr::Bool,
            StringTy => TypeExpr::String,
            CharTy => TypeExpr::Char
        };

        let named = ident()
            .then(
                type_expr
                    .separated_by(just(Comma))
                    .allow_trailing()
                    .delimited_by(just(LeftBracket), just(RightBracket))
                    .or_not(),
            )
            .map(|(name, generics)| TypeExpr::Named { name, generics });

        primitive.or(named)
    })
}

fn function() -> impl Parser<Function> {
    let generics = type_expr()
        .separated_by(just(Comma))
        .allow_trailing()
        .delimited_by(just(LeftBracket), just(RightBracket));

    let params = ident()
        .then_ignore(just(Colon))
        .then(type_expr())
        .separated_by(just(Comma))
        .allow_trailing()
        .delimited_by(just(LeftParen), just(RightParen));

    let return_ty = just(Arrow).ignore_then(type_expr());

    just(FnKw)
        .ignore_then(ident())
        .then(generics.or_not())
        .then(params)
        .then(return_ty.or_not())
        .then(block())
        .map(|((((name, generics), params), return_ty), body)| Function {
            name,
            generics,
            params,
            return_ty,
            body,
        })
}

fn binding(start_token: Token) -> impl Parser<Binding> {
    just(start_token)
        .ignore_then(ident())
        .then(just(Colon).ignore_then(type_expr()).or_not())
        .then_ignore(just(Equals))
        .then(expr())
        .then_ignore(just(Semicolon))
        .map(|((name, ty), value)| Binding { name, ty, value })
}

fn block() -> impl Parser<Block> {
    stmt()
        .repeated()
        .then(expr().map(Box::new).or_not())
        .delimited_by(just(LeftBrace), just(RightBrace))
        .map(|(stmts, tail)| Block { stmts, tail })
}

fn stmt() -> impl Parser<Stmt> {
    let expr_stmt = expr().then_ignore(just(Semicolon)).map(Stmt::Expr);
    let let_stmt = binding(LetKw).map(Stmt::Let);
    let break_stmt = just(BreakKw).to(Stmt::Break);
    let continue_stmt = just(ContinueKw).to(Stmt::Continue);
    let return_stmt = just(ReturnKw)
        .ignore_then(expr().or_not())
        .map(Stmt::Return);
    let item_stmt = item().map(Stmt::Item);

    choice((
        expr_stmt,
        let_stmt,
        break_stmt,
        continue_stmt,
        return_stmt,
        item_stmt,
    ))
}

fn expr() -> impl Parser<Expr> {
    expr_rec(true)
}

// FIXME: we need an alpha version of chumsky to keep state
fn expr_rec(parse_structs: bool) -> impl Parser<Expr> {
    recursive(|expr| {
        let expr_boxed = expr.clone().map(Box::new);

        let block_expr = block().map(Expr::Block);

        let if_expr = recursive(|if_expr| {
            let else_branch = if_expr
                .map(|if_expr| Block {
                    stmts: Vec::new(),
                    tail: Some(Box::new(if_expr)),
                })
                .or(block());

            just(IfKw)
                .ignore_then(expr_boxed.clone())
                .then(block())
                .then(just(ElseKw).ignore_then(else_branch).or_not())
                .map(|((condition, then_branch), else_branch)| Expr::If {
                    condition,
                    then_branch,
                    else_branch,
                })
        });

        let loop_expr = just(LoopKw).ignore_then(block()).map(Expr::Loop);

        let while_expr = just(WhileKw)
            .ignore_then(expr_boxed.clone())
            .then(block())
            .map(|(condition, body)| Expr::While { condition, body });

        let paren_expr = expr
            .clone()
            .delimited_by(just(LeftParen), just(RightParen))
            .map(|expr| Expr::Paren(Box::new(expr)));

        let index_expr = expr_boxed
            .clone()
            .then(
                expr_boxed
                    .clone()
                    .delimited_by(just(LeftBracket), just(RightBracket)),
            )
            .map(|(expr, index)| Expr::Index { expr, index });

        let call_expr = expr_boxed
            .clone()
            .then(
                expr.clone()
                    .separated_by(just(Comma))
                    .allow_trailing()
                    .delimited_by(just(LeftParen), just(RightParen)),
            )
            .map(|(function, args)| Expr::Call { function, args });

        let tuple_expr = expr
            .clone()
            .separated_by(just(Comma))
            .allow_trailing()
            .delimited_by(just(LeftParen), just(RightParen))
            .map(Expr::Tuple);

        let array_expr = expr
            .clone()
            .separated_by(just(Comma))
            .allow_trailing()
            .delimited_by(just(LeftBracket), just(RightBracket))
            .map(Expr::Array);

        let primitive_literal = select! {
            True => Expr::Bool(true),
            False => Expr::Bool(false),
            Int(i) => Expr::Int(i),
            Float(f) => Expr::Float(f),
            Char(c) => Expr::Char(c),
            String(s) => Expr::String(s),
        }
        .or(path().map(Expr::Path));

        let mut atom = choice((
            block_expr,
            if_expr,
            loop_expr,
            while_expr,
            paren_expr,
            index_expr,
            call_expr,
            tuple_expr,
            array_expr,
            primitive_literal,
        ))
        .boxed();

        if parse_structs {
            let struct_literal = {
                let field = ident().then_ignore(just(Colon)).then(expr.clone());

                path()
                    .then(
                        field
                            .separated_by(just(Comma))
                            .allow_trailing()
                            .delimited_by(just(LeftBrace), just(RightBrace)),
                    )
                    .map(|(name, fields)| Expr::StructLiteral { name, fields })
            };

            atom = atom.or(struct_literal).boxed();
        }

        todo()
    })
}

fn path() -> impl Parser<Path> {
    ident()
        .separated_by(just(Dot))
        .at_least(1)
        .map(|components| {
            let (name, prefix) = components.split_last().unwrap();
            Path {
                prefix: prefix.to_vec(),
                name: *name,
            }
        })
}

fn ident() -> impl Parser<Intern<str>> {
    select! { Ident(i) => i }
}
