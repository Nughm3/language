use crate::{
    codemap::CodeMap,
    fs::{FileSystem, LocalFs},
};

pub mod ast;
pub mod codemap;
pub mod fs;
pub mod parser;
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

impl<F: FileSystem> Compiler<F> {
    pub fn with_fs(fs: F) -> Self {
        Compiler {
            codemap: CodeMap::new(fs),
        }
    }
}
