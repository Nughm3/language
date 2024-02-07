use logos::Logos;

use crate::span::*;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, Logos)]
#[logos(skip r"([ \n\r\t]|//.*)+")]
pub enum Token {
    #[token("(")]
    LeftParen,
    #[token(")")]
    RightParen,
    // #[token("[")]
    // LeftBracket,
    // #[token("]")]
    // RightBracket,
    #[token("{")]
    LeftBrace,
    #[token("}")]
    RightBrace,

    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token("!")]
    Not,
    #[token("&&")]
    LogicAnd,
    #[token("||")]
    LogicOr,
    #[token("==")]
    Eq,
    #[token("!=")]
    Ne,
    #[token("<")]
    Lt,
    #[token("<=")]
    Le,
    #[token(">")]
    Gt,
    #[token(">=")]
    Ge,

    #[token(".")]
    Dot,
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token(";")]
    Semicolon,
    #[token("->")]
    Arrow,
    #[token("=")]
    Equals,

    #[token("let")]
    Let,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("loop")]
    Loop,
    #[token("while")]
    While,
    #[token("break")]
    Break,
    #[token("continue")]
    Continue,
    #[token("fn")]
    Fn,
    #[token("return")]
    Return,

    #[token("int")]
    IntTy,
    #[token("bool")]
    BoolTy,
    #[token("void")]
    VoidTy,

    #[token("true")]
    True,
    #[token("false")]
    False,
    #[regex(r"\d+")]
    Int,
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*")]
    Ident,

    Eof,
    Error,
}

impl Token {
    pub fn lexer(input: &str) -> impl Iterator<Item = Spanned<Token>> + '_ {
        logos::Logos::lexer(input).spanned().map(|(token, range)| {
            Spanned::new(
                token.unwrap_or(Token::Error),
                Span {
                    start: range.start as u32,
                    end: range.end as u32,
                },
            )
        })
    }
}
