use std::char::ParseCharError;

use internment::Intern;
use logos::Logos;
use thiserror::Error;

use crate::{
    ast::{CharLiteral, Float, InfixOp, PrefixOp, StringLiteral},
    codemap::FileId,
    span::Span,
};

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, Logos)]
#[logos(skip r"([ \n\r\t]|//.*)+", error = LexicalError)]
pub enum Token {
    #[token("(")]
    LeftParen,
    #[token(")")]
    RightParen,
    #[token("[")]
    LeftBracket,
    #[token("]")]
    RightBracket,
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
    LetKw,
    #[token("const")]
    ConstKw,
    #[token("if")]
    IfKw,
    #[token("else")]
    ElseKw,
    #[token("loop")]
    LoopKw,
    #[token("while")]
    WhileKw,
    #[token("break")]
    BreakKw,
    #[token("continue")]
    ContinueKw,
    #[token("fn")]
    FnKw,
    #[token("return")]
    ReturnKw,
    #[token("type")]
    TypeKw,
    #[token("struct")]
    StructKw,
    #[token("enum")]
    EnumKw,
    #[token("pub")]
    PubKw,
    #[token("import")]
    ImportKw,
    #[token("as")]
    AsKw,

    #[token("int")]
    IntTy,
    #[token("float")]
    FloatTy,
    #[token("bool")]
    BoolTy,
    #[token("string")]
    StringTy,
    #[token("char")]
    CharTy,

    #[token("true")]
    True,
    #[token("false")]
    False,
    #[regex(r"\d+", |s| s.slice().parse::<u64>().unwrap())]
    Int(u64),
    #[regex(r"\d+\.\d+", |s| Float::try_from(s.slice().parse::<f64>().unwrap()).unwrap())]
    Float(Float),
    #[regex(r"b?'([^']|\\[\\nrt0'])'", |s| lex_char(s.slice()))]
    Char(CharLiteral),
    #[regex(r#"b?r?"([^"]|\\[\\nrt0"])*""#, |s| lex_string(s.slice()))]
    String(StringLiteral),
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |s| Intern::from(s.slice()))]
    Ident(Intern<str>),

    Error,
}

impl Token {
    pub fn lexer(file_id: FileId, input: &str) -> impl Iterator<Item = (Token, Span)> + '_ {
        logos::Logos::lexer(input)
            .spanned()
            .map(move |(result, range)| {
                (
                    // TODO: report the lexical error
                    result.unwrap_or(Token::Error),
                    Span::new(file_id, range.start as u32, range.end as u32),
                )
            })
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq, Error)]
pub enum LexicalError {
    #[error("invalid character literal: {0}")]
    Char(#[from] ParseCharError),
    #[error("missing escape code after '\\'")]
    NoEscape,
    #[error("invalid escape code: '{0}'")]
    InvalidEscape(char),
    #[default]
    #[error("placeholder: no error occurred")]
    None,
}

fn lex_char(mut s: &str) -> Result<CharLiteral, ParseCharError> {
    let mut byte = false;
    if &s[0..1] == "b" {
        byte = true;
        s = &s[1..];
    }

    let ch = s[1..s.len() - 1].parse()?;

    Ok(if byte {
        CharLiteral::Byte(ch as u8)
    } else {
        CharLiteral::Character(ch)
    })
}

fn lex_string(mut s: &str) -> Result<StringLiteral, LexicalError> {
    let (mut byte, mut raw) = (false, false);

    if &s[0..1] == "b" {
        byte = true;
        s = &s[1..];
    }

    if &s[0..1] == "r" {
        raw = true;
        s = &s[1..];
    }

    let mut string = String::with_capacity(s.len());
    let mut chars = s[1..s.len() - 1].chars();
    while let Some(ch) = chars.next() {
        string.push(if ch == '\\' && !raw {
            let Some(escape_code) = chars.next() else {
                return Err(LexicalError::NoEscape);
            };

            match escape_code {
                '\\' => '\\',
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                '0' => '\0',
                '\'' => '\'',
                _ => return Err(LexicalError::InvalidEscape(escape_code)),
            }
        } else {
            ch
        })
    }

    Ok(if byte {
        StringLiteral::Byte(Intern::from(string.as_bytes()))
    } else {
        StringLiteral::Text(Intern::from(string.as_str()))
    })
}

impl From<Token> for PrefixOp {
    fn from(token: Token) -> Self {
        match token {
            Token::Not => PrefixOp::Not,
            Token::Minus => PrefixOp::Negate,
            err => panic!("invalid unary operator: {err:?}"),
        }
    }
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
            err => panic!("invalid binary operator: {err:?}"),
        }
    }
}
