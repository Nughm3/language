use ariadne::{Color, Label, Report, ReportKind};

use crate::{
    ast::*,
    span::*,
    token::Token::{self, *},
};

pub fn parse(input: &str) -> Result<File, Vec<Report<Span>>> {
    let mut parser = {
        let (tokens, spans) = Token::lexer(input)
            .map(|spanned| (*spanned, spanned.span()))
            .unzip();

        Parser {
            input,
            tokens,
            spans,
            pos: 0,
            errors: Vec::new(),
        }
    };

    let file = file(&mut parser);
    if parser.errors.is_empty() {
        Ok(file)
    } else {
        Err(parser.errors)
    }
}

struct Parser<'a> {
    input: &'a str,
    tokens: Vec<Token>,
    spans: Vec<Span>,
    pos: usize,
    errors: Vec<Report<'a, Span>>,
}

impl<'a> Parser<'a> {
    fn next(&mut self) -> Token {
        let token = self.peek();
        self.pos += 1;
        token
    }

    fn peek(&self) -> Token {
        self.tokens.get(self.pos).copied().unwrap_or(Token::Eof)
    }

    fn at(&self, token: Token) -> bool {
        self.peek() == token
    }

    fn consume(&mut self, token: Token) -> bool {
        let consumed = self.at(token);
        if consumed {
            self.pos += 1;
        }
        consumed
    }

    fn expect(&mut self, token: Token) -> bool {
        let ok = self.consume(token);
        if !ok {
            self.error(&[token]);
        }
        ok
    }

    fn error(&mut self, expected: &[Token]) {
        let actual = self.next();
        let span = self.span();

        let message = if let Error = actual {
            format!("unrecognized token: '{}'", self.text())
        } else {
            let expected = match expected.len() {
                0 => panic!("empty expected list"),
                1 => format!("{:?}", expected[0]),
                _ => {
                    let expected: Vec<String> =
                        expected.iter().map(|token| format!("{token:?}")).collect();
                    expected.join("', '")
                }
            };

            format!("expected '{expected}', found '{actual:?}'")
        };

        self.errors.push(
            Report::build(ReportKind::Error, (), span.start as usize)
                .with_message(&message)
                .with_label(
                    Label::new(span)
                        .with_message(&message)
                        .with_color(Color::Red),
                )
                .finish(),
        );
    }

    fn recover_to(&mut self, tokens: &[Token]) {
        let Some(mut current) = self.tokens.get(self.pos - 1).copied() else {
            return;
        };

        while !tokens.contains(&current) && current != Token::Eof {
            current = self.next();
        }
    }

    fn expect_or_recover(&mut self, token: Token, tokens: &[Token]) -> bool {
        let ok = self.expect(token);
        if !ok {
            self.recover_to(tokens);
        }
        ok
    }

    fn span(&self) -> Span {
        self.spans.get(self.pos - 1).copied().unwrap_or_default()
    }

    fn text(&self) -> &str {
        &self.input[self.span()]
    }
}

fn file(p: &mut Parser<'_>) -> File {
    let mut functions = Vec::new();

    while !p.at(Eof) {
        if p.at(Fn) {
            functions.push(function(p));
        } else {
            p.expect_or_recover(Fn, &[Fn]);
        }
    }

    File(functions)
}

fn r#type(p: &mut Parser<'_>) -> Type {
    match p.peek() {
        IntTy => {
            p.next();
            Type::Int
        }
        BoolTy => {
            p.next();
            Type::Bool
        }
        Fn => {
            p.next();
            let params = delimited_list(p, LeftParen, RightParen, Comma, r#type);
            let return_ty = p.consume(Arrow).then(|| Box::new(r#type(p)));
            Type::Function {
                params,
                return_type: return_ty,
            }
        }
        VoidTy => {
            p.next();
            Type::Void
        }

        _ => {
            p.error(&[IntTy, BoolTy, Fn, VoidTy]);
            Type::ParseError
        }
    }
}

fn function(p: &mut Parser<'_>) -> Function {
    p.expect(Fn);
    let name = ident(p);

    let params = delimited_list(p, LeftParen, RightParen, Comma, |p| {
        let name = ident(p);
        p.expect_or_recover(Colon, &[Comma, RightParen, Arrow, LeftBrace]);
        let ty = r#type(p);
        (name, ty)
    });

    let return_ty = p.consume(Arrow).then(|| r#type(p));

    let body = block(p);

    Function {
        name,
        params,
        return_type: return_ty,
        body,
    }
}

fn block(p: &mut Parser<'_>) -> Block {
    p.expect(LeftBrace);

    let mut stmts = Vec::new();
    let mut functions = Vec::new();
    let mut tail = None;
    while !p.at(RightBrace) && !p.at(Eof) {
        if let Let | Break | Continue | Return = p.peek() {
            stmts.push(stmt(p));
        } else if let Fn = p.peek() {
            functions.push(function(p));
        } else {
            let expr = expr(p);
            if p.consume(Semicolon) {
                stmts.push(Stmt::Expr(expr));
            } else {
                tail = Some(Box::new(expr));
                break;
            }
        }
    }

    p.expect(RightBrace);
    Block {
        stmts,
        functions,
        tail,
    }
}

fn stmt(p: &mut Parser<'_>) -> Stmt {
    match p.peek() {
        Let => {
            p.next();

            let name = ident(p);
            let ty = p.consume(Colon).then(|| r#type(p));
            p.expect_or_recover(Equals, &[Semicolon]);
            let value = expr(p);

            p.expect(Semicolon);
            Stmt::Let {
                name,
                r#type: ty,
                value,
            }
        }
        Break => {
            p.next();
            p.expect(Semicolon);
            Stmt::Break
        }
        Continue => {
            p.next();
            p.expect(Semicolon);
            Stmt::Continue
        }
        Return => {
            p.next();
            let value = (!p.at(Semicolon)).then(|| expr(p));
            p.expect(Semicolon);
            Stmt::Return(value)
        }
        _ => {
            let expr = expr(p);
            if expr.is_inline() {
                p.expect(Semicolon);
            }
            Stmt::Expr(expr)
        }
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct BindingPower(u8);

impl BindingPower {
    const MIN: Self = BindingPower(u8::MIN);
    const MAX: Self = BindingPower(u8::MAX);

    fn of(op: Token) -> Option<(Self, Self)> {
        const ASSIGNMENT: &[Token] = &[Equals];

        let bp = [
            ASSIGNMENT,
            &[LogicOr],
            &[LogicAnd],
            &[Eq, Ne, Lt, Le, Gt, Ge],
            &[Plus, Minus],
            &[Star, Slash, Percent],
        ]
        .iter()
        .position(|level| level.contains(&op))
        .map(|bp| bp as u8 * 2)?;

        Some(if ASSIGNMENT.contains(&op) {
            (BindingPower(bp + 2), BindingPower(bp + 1))
        } else {
            (BindingPower(bp + 1), BindingPower(bp + 2))
        })
    }
}

fn expr(p: &mut Parser<'_>) -> Expr {
    expr_rec(p, BindingPower::MIN)
}

fn expr_rec(p: &mut Parser<'_>, min: BindingPower) -> Expr {
    let mut lhs = match p.peek() {
        LeftBrace => Expr::Block(block(p)),
        If => {
            fn r#if(p: &mut Parser<'_>) -> ExprIf {
                p.expect(If);

                let condition = Box::new(expr(p));
                let then = block(p);

                let r#else = p.consume(Else).then(|| {
                    if p.at(If) {
                        ExprElse::If(Box::new(r#if(p)))
                    } else {
                        ExprElse::Block(block(p))
                    }
                });

                ExprIf {
                    condition,
                    then,
                    r#else,
                }
            }

            Expr::If(r#if(p))
        }
        Loop => {
            p.next();
            Expr::Loop(block(p))
        }
        While => {
            p.next();
            let condition = Box::new(expr(p));
            let body = block(p);
            Expr::While { condition, body }
        }
        op @ (Not | Minus) => {
            p.next();
            Expr::Prefix {
                op: op.into(),
                expr: Box::new(expr_rec(p, BindingPower::MAX)),
            }
        }
        LeftParen => Expr::Paren(Box::new(delimited_by(p, LeftParen, RightParen, expr))),
        Int => {
            p.next();
            Expr::Int(p.text().parse().unwrap())
        }
        True => {
            p.next();
            Expr::Bool(true)
        }
        False => {
            p.next();
            Expr::Bool(false)
        }
        Ident => Expr::Ident(ident(p)),
        _ => {
            p.error(&[Int]);
            return Expr::ParseError;
        }
    };

    loop {
        if p.at(LeftParen) {
            lhs = Expr::Call {
                function: Box::new(lhs),
                args: delimited_list(p, LeftParen, RightParen, Comma, expr),
            };
        } else if let Some((l, r)) = BindingPower::of(p.peek()) {
            if l < min {
                break;
            }

            let op = p.next().into();
            let rhs = expr_rec(p, r);
            lhs = Expr::Infix {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
            };
        } else {
            break;
        }
    }

    lhs
}

fn ident(p: &mut Parser<'_>) -> crate::ast::Ident {
    p.expect(Token::Ident);
    p.text().into()
}

fn delimited_list<T>(
    p: &mut Parser<'_>,
    l: Token,
    r: Token,
    separator: Token,
    f: impl FnMut(&mut Parser<'_>) -> T,
) -> Vec<T> {
    delimited_by(p, l, r, |p| separated_by(p, separator, r, f))
}

fn separated_by<T>(
    p: &mut Parser<'_>,
    separator: Token,
    end: Token,
    mut f: impl FnMut(&mut Parser<'_>) -> T,
) -> Vec<T> {
    if p.at(end) || p.at(Eof) {
        return Vec::new();
    }

    let mut values = vec![f(p)];

    while p.consume(separator) && !p.at(end) && !p.at(Eof) {
        values.push(f(p));
    }

    values
}

fn delimited_by<T>(
    p: &mut Parser<'_>,
    l: Token,
    r: Token,
    f: impl FnOnce(&mut Parser<'_>) -> T,
) -> T {
    p.expect(l);
    let value = f(p);
    p.expect(r);
    value
}
