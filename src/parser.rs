use chumsky::{
    input::{Input as _, Stream, ValueInput},
    prelude::{Parser as _, *},
};
use internment::Intern;

use crate::{
    ast::*,
    codemap::FileId,
    span::Span,
    token::Token::{self, *},
};

pub fn parse(file_id: FileId, input: &str) -> ParseResult<File, Rich<'_, Token, Span>> {
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
trait Parser<'a, I: Input<'a>, T>: chumsky::Parser<'a, I, T, Extra<'a>> + Clone {}
impl<'a, I: Input<'a>, T, P: chumsky::Parser<'a, I, T, Extra<'a>> + Clone> Parser<'a, I, T> for P {}

fn file<'a, I: Input<'a>>() -> impl Parser<'a, I, File> {
    item()
        .repeated()
        .collect()
        .then_ignore(end())
        .map(|items| File { items })
}

fn item<'a, I: Input<'a>>() -> impl Parser<'a, I, Item> {
    choice((
        import().map(Item::Import),
        type_alias().map(Item::TypeAlias),
        struct_().map(Item::Struct),
        enum_().map(Item::Enum),
        function().map(Item::Function),
        binding(ConstKw).map(Item::Constant),
    ))
    .boxed()
}

fn import<'a, I: Input<'a>>() -> impl Parser<'a, I, Import> {
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
            .collect()
            .delimited_by(just(LeftBrace), just(RightBrace))
            .map(Import::Group);

        let wildcard = just(Star).to(Import::Wildcard);

        just(ImportKw)
            .ignore_then(choice((tree, group, wildcard)))
            .then_ignore(just(Semicolon))
    })
}

fn type_alias<'a, I: Input<'a>>() -> impl Parser<'a, I, TypeAlias> {
    just(TypeKw)
        .ignore_then(type_expr())
        .then_ignore(just(Equals))
        .then(type_expr())
        .then_ignore(just(Semicolon))
        .map(|(lhs, rhs)| TypeAlias { lhs, rhs })
}

fn struct_<'a, I: Input<'a>>() -> impl Parser<'a, I, Struct> {
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

fn enum_<'a, I: Input<'a>>() -> impl Parser<'a, I, Enum> {
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

fn type_expr<'a, I: Input<'a>>() -> impl Parser<'a, I, TypeExpr> {
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
                    .collect()
                    .delimited_by(just(LeftBracket), just(RightBracket))
                    .or_not(),
            )
            .map(|(name, generics)| TypeExpr::Named { name, generics });

        primitive.or(named)
    })
}

fn function<'a, I: Input<'a>>() -> impl Parser<'a, I, Function> {
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
        .then(block())
        .map(|((((name, generics), params), return_ty), body)| Function {
            name,
            generics,
            params,
            return_ty,
            body,
        })
}

fn binding<'a, I: Input<'a>>(start_token: Token) -> impl Parser<'a, I, Binding> {
    just(start_token)
        .ignore_then(ident())
        .then(just(Colon).ignore_then(type_expr()).or_not())
        .then_ignore(just(Equals))
        .then(expr())
        .then_ignore(just(Semicolon))
        .map(|((name, ty), value)| Binding { name, ty, value })
}

fn block<'a, I: Input<'a>>() -> impl Parser<'a, I, Block> {
    stmt()
        .repeated()
        .collect()
        .then(expr().map(Box::new).or_not())
        .delimited_by(just(LeftBrace), just(RightBrace))
        .map(|(stmts, tail)| Block { stmts, tail })
}

fn stmt<'a, I: Input<'a>>() -> impl Parser<'a, I, Stmt> {
    let expr_stmt = expr()
        .then(just(Semicolon).or_not())
        .try_map(|(expr, semicolon), span| {
            if !matches!(
                expr,
                Expr::Block(_) | Expr::If { .. } | Expr::Loop(_) | Expr::While { .. }
            ) && semicolon.is_none()
            {
                Err(Rich::custom(span, "expected semicolon: ';'"))
            } else {
                Ok(Stmt::Expr(expr))
            }
        });
    let let_stmt = binding(LetKw).map(Stmt::Let);
    let break_stmt = just(BreakKw).then(just(Semicolon)).to(Stmt::Break);
    let continue_stmt = just(ContinueKw).then(just(Semicolon)).to(Stmt::Continue);
    let return_stmt = just(ReturnKw)
        .ignore_then(expr().or_not())
        .then_ignore(just(Semicolon))
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

fn expr<'a, I: Input<'a>>() -> impl Parser<'a, I, Expr> {
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

        let paren = expr
            .clone()
            .delimited_by(just(LeftParen), just(RightParen))
            .map(|expr| Expr::Paren(Box::new(expr)));

        let tuple = expr
            .clone()
            .separated_by(just(Comma))
            .allow_trailing()
            .collect()
            .delimited_by(just(LeftParen), just(RightParen))
            .map(Expr::Tuple);

        let array = expr
            .clone()
            .separated_by(just(Comma))
            .allow_trailing()
            .collect()
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

        let struct_literal = {
            let field = ident().then_ignore(just(Colon)).then(expr.clone());

            path()
                .then(
                    field
                        .separated_by(just(Comma))
                        .allow_trailing()
                        .collect()
                        .delimited_by(just(LeftBrace), just(RightBrace)),
                )
                .map(|(name, fields)| Expr::StructLiteral { name, fields })
        };

        let call = expr_boxed
            .clone()
            .then(
                expr.clone()
                    .separated_by(just(Comma))
                    .allow_trailing()
                    .collect()
                    .delimited_by(just(LeftParen), just(RightParen)),
            )
            .map(|(function, args)| Expr::Call { function, args });

        let index = expr_boxed
            .clone()
            .then(
                expr_boxed
                    .clone()
                    .delimited_by(just(LeftBracket), just(RightBracket)),
            )
            .map(|(expr, index)| Expr::Index { expr, index });

        let atom = choice((
            block_expr,
            if_expr,
            loop_expr,
            while_expr,
            paren,
            tuple,
            array,
            primitive_literal,
            struct_literal,
            call,
            index,
        ));

        atom.pratt({
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
        })
    })
}

fn path<'a, I: Input<'a>>() -> impl Parser<'a, I, Path> {
    ident()
        .separated_by(just(Dot))
        .at_least(1)
        .collect()
        .map(|components: Vec<_>| {
            let (name, prefix) = components.split_last().unwrap();
            Path {
                prefix: prefix.to_vec(),
                name: *name,
            }
        })
}

fn ident<'a, I: Input<'a>>() -> impl Parser<'a, I, Intern<str>> {
    select! { Ident(i) => i }
}
