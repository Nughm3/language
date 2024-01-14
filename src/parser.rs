use chumsky::{
    input::{Input as _, Stream, ValueInput},
    prelude::*,
    Parser as _,
};
use internment::Intern;

use crate::{
    ast::*,
    codemap::FileId,
    span::Span,
    token::Token::{self, *},
};

pub fn parse(file_id: FileId, input: &str) -> ParseResult<File<Path>, Rich<'_, Token, Span>> {
    let eoi = {
        let len = input.len() as u32;
        Span::new(file_id, len, len + 1)
    };

    let tokens = Token::lexer(file_id, input);
    let stream = Stream::from_iter(tokens).spanned(eoi);

    file().parse(stream)
}

type Extra<'a> = extra::Err<Rich<'a, Token, Span>>;
trait Input<'a>: ValueInput<'a, Token = Token, Span = Span> {}
impl<'a, I: ValueInput<'a, Token = Token, Span = Span>> Input<'a> for I {}
trait Parser<'a, I: Input<'a>, O>: chumsky::Parser<'a, I, O, Extra<'a>> + Clone {}
impl<'a, I: Input<'a>, O, P: chumsky::Parser<'a, I, O, Extra<'a>> + Clone> Parser<'a, I, O> for P {}

fn file<'a, I: Input<'a>>() -> impl Parser<'a, I, File<Path>> {
    let item = recursive(|item| {
        let expr_ = std::cell::OnceCell::new(); // FIXME horrible hack

        let block = recursive(|block| {
            let expr = recursive(|expr| {
                let paren = expr
                    .clone()
                    .delimited_by(just(LeftParen), just(RightParen))
                    .map(|expr| Expr::Paren(Box::new(expr)));

                let literal = select! {
                        True => Expr::Bool(true),
                        False => Expr::Bool(false),
                        Int(i) => Expr::Int(i),
                        Float(f) => Expr::Float(f),
                        Char(c) => Expr::Char(c),
                        String(s) => Expr::String(s),
                };

                let expr_list = expr
                    .clone()
                    .separated_by(just(Comma))
                    .allow_trailing()
                    .collect();

                let tuple = expr_list
                    .clone()
                    .delimited_by(just(LeftParen), just(RightParen))
                    .map(Expr::Tuple);

                let array = expr_list
                    .clone()
                    .delimited_by(just(LeftBracket), just(RightBracket))
                    .map(Expr::Array);

                let path = path().map(Expr::Path);

                let value = choice((literal, tuple, array, path));

                let atom = choice((paren, value));

                let inline_expr = atom.pratt({
                    use chumsky::pratt::*;

                    let prefix = |bp, op: Token| {
                        prefix(bp, just(op), move |expr| Expr::Prefix {
                            op: op.into(),
                            expr: Box::new(expr),
                        })
                    };

                    let infix = |assoc, op: Token| {
                        infix(assoc, just(op), move |lhs, rhs| Expr::Infix {
                            lhs: Box::new(lhs),
                            op: op.into(),
                            rhs: Box::new(rhs),
                        })
                    };

                    (
                        postfix(
                            8,
                            expr_list.delimited_by(just(LeftParen), just(RightParen)),
                            |function, args| Expr::Call {
                                function: Box::new(function),
                                args,
                            },
                        ),
                        postfix(
                            8,
                            expr.clone()
                                .delimited_by(just(LeftBracket), just(RightBracket)),
                            |expr, index| Expr::Index {
                                expr: Box::new(expr),
                                index: Box::new(index),
                            },
                        ),
                        prefix(7, Minus),
                        prefix(7, Not),
                        infix(left(6), Star),
                        infix(left(6), Slash),
                        infix(left(6), Percent),
                        infix(left(5), Plus),
                        infix(left(5), Minus),
                        infix(left(4), Eq),
                        infix(left(4), Ne),
                        infix(left(4), Lt),
                        infix(left(4), Le),
                        infix(left(4), Gt),
                        infix(left(4), Ge),
                        infix(left(3), LogicAnd),
                        infix(left(2), LogicOr),
                        infix(right(1), Equals),
                    )
                });

                let block_expr = block.map(Expr::Block);

                inline_expr.or(block_expr)
            });

            expr_.set(expr.clone()).ok().unwrap();

            let stmt = {
                let expr_stmt = expr.clone().then(just(Semicolon).or_not()).try_map(
                    |(expr, semicolon), span| {
                        if !matches!(
                            expr,
                            Expr::Block(_) | Expr::If { .. } | Expr::Loop(_) | Expr::While { .. }
                        ) && semicolon.is_none()
                        {
                            Err(Rich::custom(span, "expected semicolon: ';'"))
                        } else {
                            Ok(Stmt::Expr(expr))
                        }
                    },
                );
                let let_stmt = just(LetKw)
                    .ignore_then(ident())
                    .then(just(Colon).ignore_then(type_expr()).or_not())
                    .then_ignore(just(Equals))
                    .then(expr.clone())
                    .then_ignore(just(Semicolon))
                    .map(|((name, ty), value)| Binding { name, ty, value })
                    .map(Stmt::Let);
                let break_stmt = just(BreakKw).then(just(Semicolon)).to(Stmt::Break);
                let continue_stmt = just(ContinueKw).then(just(Semicolon)).to(Stmt::Continue);
                let return_stmt = just(ReturnKw)
                    .ignore_then(expr.clone().or_not())
                    .then_ignore(just(Semicolon))
                    .map(Stmt::Return);
                let item_stmt = item.map(Stmt::Item);

                choice((
                    expr_stmt,
                    let_stmt,
                    break_stmt,
                    continue_stmt,
                    return_stmt,
                    item_stmt,
                ))
            };

            stmt.repeated()
                .collect()
                .then(expr.clone().map(Box::new).or_not())
                .delimited_by(just(LeftBrace), just(RightBrace))
                .map(|(stmts, tail)| Block { stmts, tail })
        });

        let function = {
            let generics = type_expr()
                .separated_by(just(Comma))
                .allow_trailing()
                .collect()
                .delimited_by(just(LeftBracket), just(RightBracket));

            let params = ident()
                .then_ignore(just(Colon))
                .then(type_expr())
                .separated_by(just(Comma))
                .allow_trailing()
                .collect()
                .delimited_by(just(LeftParen), just(RightParen));

            let return_ty = just(Arrow).ignore_then(type_expr());

            just(FnKw)
                .ignore_then(ident())
                .then(generics.or_not())
                .then(params)
                .then(return_ty.or_not())
                .then(block)
                .map(|((((name, generics), params), return_ty), body)| Function {
                    name,
                    generics,
                    params,
                    return_ty,
                    body,
                })
        };

        let binding = just(ConstKw)
            .ignore_then(ident())
            .then(just(Colon).ignore_then(type_expr()).or_not())
            .then_ignore(just(Equals))
            .then(expr_.get().unwrap().clone())
            .then_ignore(just(Semicolon))
            .map(|((name, ty), value)| Binding { name, ty, value });

        choice((
            import().map(Item::Import),
            type_alias().map(Item::TypeAlias),
            r#struct().map(Item::Struct),
            r#enum().map(Item::Enum),
            function.map(Item::Function),
            binding.map(Item::Constant),
        ))
    });

    item.repeated()
        .collect()
        .then_ignore(end())
        .map(|items| File { items })
}

fn import<'a, I: Input<'a>>() -> impl Parser<'a, I, Import> {
    let import = recursive(|import| {
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
            .collect()
            .delimited_by(just(LeftBrace), just(RightBrace))
            .map(Import::Group);

        let wildcard = just(Star).to(Import::Wildcard);

        choice((tree, group, wildcard))
    });

    just(ImportKw)
        .ignore_then(import)
        .then_ignore(just(Semicolon))
}

fn type_alias<'a, I: Input<'a>>() -> impl Parser<'a, I, TypeAlias<Path>> {
    just(TypeKw)
        .ignore_then(type_expr())
        .then_ignore(just(Equals))
        .then(type_expr())
        .then_ignore(just(Semicolon))
        .map(|(lhs, rhs)| TypeAlias { lhs, rhs })
}

fn r#struct<'a, I: Input<'a>>() -> impl Parser<'a, I, Struct<Path>> {
    just(StructKw)
        .ignore_then(type_expr())
        .then(variant(type_expr()))
        .then(just(Semicolon).or_not())
        .try_map(|((ty, body), semicolon), span| {
            if matches!(body, Variant::Unit | Variant::Tuple(_)) && semicolon.is_none() {
                Err(Rich::custom(
                    span,
                    "struct variant requires a semicolon: ';'",
                ))
            } else {
                Ok(Struct { ty, body })
            }
        })
}

fn r#enum<'a, I: Input<'a>>() -> impl Parser<'a, I, Enum<Path>> {
    just(EnumKw)
        .ignore_then(type_expr())
        .then(
            ident()
                .then(variant(type_expr()))
                .separated_by(just(Comma))
                .allow_trailing()
                .collect()
                .delimited_by(just(LeftBrace), just(LeftBrace)),
        )
        .map(|(ty, variants)| Enum { ty, variants })
}

fn variant<'a, I: Input<'a>, T>(p: impl Parser<'a, I, T>) -> impl Parser<'a, I, Variant<T>> {
    let tuple = p
        .clone()
        .separated_by(just(Comma))
        .allow_trailing()
        .collect()
        .delimited_by(just(LeftParen), just(RightParen))
        .map(Variant::Tuple);

    let record = ident()
        .then_ignore(just(Colon))
        .then(p)
        .separated_by(just(Comma))
        .allow_trailing()
        .collect()
        .delimited_by(just(LeftBrace), just(RightBrace))
        .map(Variant::Record);

    tuple
        .or(record)
        .or_not()
        .map(|variant| variant.unwrap_or(Variant::Unit))
}

fn type_expr<'a, I: Input<'a>>() -> impl Parser<'a, I, TypeExpr<Path>> {
    recursive(|type_expr| {
        let primitive = select! {
            IntTy => TypeExpr::Int,
            FloatTy => TypeExpr::Float,
            BoolTy => TypeExpr::Bool,
            StringTy => TypeExpr::String,
            CharTy => TypeExpr::Char
        };

        let generics = type_expr
            .separated_by(just(Comma))
            .allow_trailing()
            .collect()
            .delimited_by(just(LeftBracket), just(RightBracket));

        let named = path()
            .then(generics.or_not())
            .map(|(path, generics)| TypeExpr::Named { path, generics });

        primitive.or(named)
    })
}

fn path<'a, I: Input<'a>>() -> impl Parser<'a, I, Path> {
    ident()
        .separated_by(just(Dot))
        .at_least(1)
        .collect()
        .map(|components| Path { components })
}

fn ident<'a, I: Input<'a>>() -> impl Parser<'a, I, Intern<str>> {
    select! { Ident(i) => i }
}
