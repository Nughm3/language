use std::{
    fmt,
    ops::Index,
    path::{Path, PathBuf},
};

use crate::{fs::FileSystem, span::Span};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CodeMap<F> {
    fs: F,
    files: Vec<File>,
    data: String,
}

impl<F: FileSystem> CodeMap<F> {
    pub fn new(fs: F) -> Self {
        CodeMap {
            fs,
            files: Vec::new(),
            data: String::new(),
        }
    }

    pub fn insert(&mut self, path: impl AsRef<Path>) -> Result<FileId, F::Error> {
        let path = path.as_ref();

        let raw_content = self.fs.read(path)?;
        let content = std::str::from_utf8(&raw_content).expect("invalid UTF-8 in file");
        self.data.push_str(content);

        let file = File {
            path: path.to_path_buf(),
            offset: self.data.len(),
            len: content.len(),
            line_breaks: content
                .char_indices()
                .filter_map(|(idx, ch)| (ch == '\n').then_some(idx as u32))
                .collect(),
        };

        let id = FileId(self.files.len() as u32);
        self.files.push(file);
        Ok(id)
    }

    pub fn get(&self, FileId(idx): FileId) -> Option<&File> {
        self.files.get(idx as usize)
    }

    pub fn text(&self, id: FileId) -> Option<&str> {
        self.get(id)
            .map(|File { offset, len, .. }| &self.data[*offset..*offset + *len])
    }

    pub fn span(
        &self,
        Span {
            file_id,
            start,
            end,
        }: Span,
    ) -> Option<&str> {
        self.text(file_id)
            .map(|text| &text[start as usize..end as usize])
    }
}

impl<F: FileSystem> Index<FileId> for CodeMap<F> {
    type Output = str;

    fn index(&self, id: FileId) -> &Self::Output {
        self.text(id).expect("index out of bounds")
    }
}

impl<F: FileSystem> Index<Span> for CodeMap<F> {
    type Output = str;

    fn index(&self, span: Span) -> &Self::Output {
        self.span(span).expect("index out of bounds")
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct FileId(u32);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct File {
    path: PathBuf,
    offset: usize,
    len: usize,
    line_breaks: Vec<u32>,
}

impl File {
    pub fn path(&self) -> &Path {
        &self.path
    }

    pub fn span(
        &self,
        Span {
            start,
            end,
            file_id,
        }: Span,
    ) -> Span {
        Span::new(
            file_id,
            start + self.offset as u32,
            end + self.offset as u32,
        )
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn position_of(&self, offset: u32) -> Position {
        let partition_point = self.line_breaks.partition_point(|idx| *idx < offset);

        if partition_point == 0 {
            Position {
                line: 0,
                column: offset,
            }
        } else {
            Position {
                line: partition_point as u32,
                column: offset - self.line_breaks[partition_point - 1] - 1,
            }
        }
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Position {
    pub line: u32,
    pub column: u32,
}

impl Position {
    pub fn new(line: u32, column: u32) -> Self {
        Position { line, column }
    }
}

impl fmt::Debug for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self:?}")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fs::TestFs;

    #[test]
    fn conversions() {
        let mut fs = TestFs::new();

        let texts = [
            "This text contains no line breaks",
            "This\nis\nsome\ntext\ncontaining\nline\nbreaks",
            "Trailing line break\n",
            "\nLeading line break",
            "Double\n\nline break",
        ];

        for (idx, text) in texts.iter().enumerate() {
            fs.write(Path::new(&idx.to_string()), text.as_bytes())
                .unwrap();
        }

        let mut codemap = CodeMap::new(fs);

        for (idx, text) in texts.iter().enumerate() {
            let id = codemap.insert(Path::new(&idx.to_string())).unwrap();
            let file = codemap.get(id).unwrap();
            dbg!(&file.line_breaks);

            let (mut line, mut col) = (0, 0);
            for (idx, ch) in text.char_indices() {
                assert_eq!(
                    file.position_of(idx as u32),
                    Position::new(line, col),
                    "position_of({idx}) : {text:?}"
                );

                if ch == '\n' {
                    line += 1;
                    col = 0;
                } else {
                    col += 1;
                }
            }
        }
    }
}
