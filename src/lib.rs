use std::{error::Error, path::Path};

use internment::Intern;
use thiserror::Error;

use crate::{
    codemap::CodeMap,
    fs::{FileSystem, LocalFs},
};

pub mod ast;
pub mod codemap;
pub mod fs;
pub mod parser;
pub mod resolve;
pub mod span;
pub mod token;

#[derive(Debug)]
pub struct Compiler<F> {
    codemap: CodeMap<F>,
}

impl Compiler<LocalFs> {
    pub fn new() -> std::io::Result<Self> {
        Ok(Compiler::with_fs(LocalFs::new_in_current_dir()?))
    }
}

impl<F: FileSystem> Compiler<F>
where
    F::Error: 'static,
{
    pub fn with_fs(fs: F) -> Self {
        Compiler {
            codemap: CodeMap::new(fs),
        }
    }

    pub fn compile(&mut self, root: impl AsRef<Path>) -> Result<(), Box<dyn Error>> {
        let root_id = self.codemap.load(root)?;

        let parse_result = parser::parse(root_id, self.codemap.text(root_id).unwrap());

        for error in parse_result.errors() {
            eprintln!("parse error: {error:?}");
        }

        if let Some(ast) = parse_result.output() {
            dbg!(ast);
        }

        Ok(())
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ResolvedPath {
    pub components: Vec<Intern<str>>,
}

pub type ResolutionResult<T> = Result<T, ResolutionError>;

#[derive(Debug, Error)]
pub enum ResolutionError {}
