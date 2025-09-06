use crate::interner::SpanInterner;
pub use monotonic::MonotonicVec;
use parking_lot::{RwLock, RwLockReadGuard};
use std::{
    fmt::{Debug, Display},
    hash::Hash,
    io::{self, ErrorKind, Read},
    marker::PhantomData,
    mem::MaybeUninit,
    ops::{Add, AddAssign, Index, Range, Sub},
    path::Path,
    sync::{Arc, OnceLock},
};

/// A span that is either interned or 8 bytes.
/// Bit 1 of inner is used to check if it's interned or not, where 0 means it is.
/// Bit 2-16 are the length
/// Bit 17-48 are the byte pos
/// Bit 49-64 is the FileId.
///
/// The span will be intenred when the length does not fit into 15 bits, if the position does not
/// fit into 32 bits or if the file id does not fit into 16 bits.
#[derive(Clone, Copy, Eq)]
pub struct Span<'arena> {
    inner: u64,
    _data: PhantomData<&'arena SpanData>,
}

#[derive(Clone)]
pub struct SpanWithFile {
    pub pos: BytePos,
    pub file: Arc<SourceFile>,
}

impl SpanWithFile {
    pub fn lookup_pos(&self) -> (u32, u32) {
        self.file.lookup_file_pos(self.pos)
    }
}

impl Display for SpanWithFile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let file = match std::env::current_dir() {
            Err(_) => &self.file.path,
            Ok(p) => self.file.path.strip_prefix(&p).unwrap_or(&self.file.path),
        };
        let (line, pos) = self.lookup_pos();
        Display::fmt(&file.display(), f)?;
        f.write_str(":")?;
        Display::fmt(&line, f)?;
        f.write_str(":")?;
        Display::fmt(&pos, f)
    }
}

impl<'arena> Span<'arena> {
    /// span wioth file id 0, len of 0 at position 0
    pub const DUMMY: Span<'arena> = Span {
        inner: 1,
        _data: PhantomData,
    };

    pub fn is_dummy(&self) -> bool {
        self.inner == 1
    }

    pub fn new(data: SpanData, interner: &SpanInterner<'arena>) -> Self {
        // file is only 16 bit and length only 15 bit
        if data.file.0 <= u16::MAX as u32 && data.len <= 0x7fff {
            let inner = 1
                | ((data.len as u64) << 1)
                | ((data.pos.to_u32() as u64) << 16)
                | ((data.file.0 as u64) << 48);
            return Self {
                inner,
                _data: PhantomData,
            };
        }

        Self {
            inner: <*const _>::addr(*interner.intern(data)) as u64,
            _data: PhantomData,
        }
    }

    pub fn with_source_file(self, ctx: &SourceMap) -> SpanWithFile {
        let SpanData { pos, file, .. } = self.get_span_data();
        let file = ctx.get_file(file).unwrap();
        SpanWithFile { pos, file }
    }

    fn is_interned(self) -> bool {
        self.inner & 1 == 0
    }

    fn get_pointer(self) -> &'arena SpanData {
        assert_eq!(self.inner & 1, 0);
        // SAFETY: This pointer was previously allocated with 'arena
        // TODO: figure out if this should use std::ptr::with_exposed_provenance.
        unsafe { &*std::ptr::without_provenance(self.inner as usize) }
    }

    pub fn get_span_data(self) -> SpanData {
        if self.is_interned() {
            *self.get_pointer()
        } else {
            SpanData {
                pos: BytePos::from_u32(((self.inner >> 16) & u32::MAX as u64) as u32),
                len: ((self.inner >> 1) & 0x7fff) as u32,
                file: FileId((self.inner >> 48) as u32),
            }
        }
    }

    pub fn get_range(self) -> Range<BytePos> {
        let data = self.get_span_data();
        Range {
            start: data.pos,
            end: data.pos + data.len,
        }
    }

    /// returns a span containing only the last character of this span.
    pub fn last(self, interner: &SpanInterner<'arena>) -> Self {
        let mut data = self.get_span_data();
        data.pos += data.len - 1;
        data.len = 1;
        Span::new(data, interner)
    }

    /// returns a span immediately following this span of `len` characters.
    pub fn after(self, len: u32, interner: &SpanInterner<'arena>) -> Self {
        let mut data = self.get_span_data();
        data.pos += data.len;
        data.len = len;
        Span::new(data, interner)
    }

    pub fn combine_with(
        self,
        others: impl IntoIterator<Item = Span<'arena>>,
        interner: &SpanInterner<'arena>,
    ) -> Self {
        let mut range = self.get_range();
        for span in others.into_iter() {
            let other_range = span.get_range();
            range.start = range.start.min(other_range.start);
            range.end = range.end.max(other_range.end);
        }
        let new_data = SpanData::new(
            range.start,
            (range.end - range.start).to_u32(),
            self.get_span_data().file,
        );
        if self.get_span_data() == new_data {
            return self;
        }
        Span::new(new_data, interner)
    }
}

impl Debug for Span<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.get_span_data(), f)
    }
}
impl Hash for Span<'_> {
    fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
        Hash::hash(&self.get_span_data(), hasher)
    }
}
impl PartialEq<Span<'_>> for SpanData {
    fn eq(&self, other: &Span<'_>) -> bool {
        other == self
    }
}
impl PartialEq<SpanData> for Span<'_> {
    fn eq(&self, other: &SpanData) -> bool {
        self.get_span_data() == *other
    }
}
impl PartialEq for Span<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.get_span_data() == other.get_span_data()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct SpanData {
    pub pos: BytePos,
    pub len: u32,
    pub file: FileId,
}

impl SpanData {
    pub fn new(pos: BytePos, len: u32, file: FileId) -> Self {
        Self { pos, len, file }
    }
}

impl AsRef<SpanData> for SpanData {
    fn as_ref(&self) -> &SpanData {
        self
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub struct BytePos(u32);
impl BytePos {
    pub fn to_usize(self) -> usize {
        self.0 as usize
    }

    pub fn from_u32(val: u32) -> Self {
        Self(val)
    }

    pub fn to_u32(self) -> u32 {
        self.0
    }
}

impl AddAssign<u32> for BytePos {
    fn add_assign(&mut self, rhs: u32) {
        self.0 += rhs;
    }
}
impl Sub<u32> for BytePos {
    type Output = Self;

    #[inline(always)]
    fn sub(self, rhs: u32) -> Self::Output {
        Self(self.0 - rhs)
    }
}
impl Add<u32> for BytePos {
    type Output = Self;

    #[inline(always)]
    fn add(self, rhs: u32) -> Self::Output {
        Self(self.0 + rhs)
    }
}
impl Add for BytePos {
    type Output = Self;

    #[inline(always)]
    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0 + rhs.0)
    }
}
impl Sub for BytePos {
    type Output = Self;

    #[inline(always)]
    fn sub(self, rhs: Self) -> Self::Output {
        Self(self.0 - rhs.0)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct FileId(u32);

impl FileId {
    #[cfg(test)]
    pub const ZERO: FileId = FileId(0);

    pub fn to_inner(self) -> u32 {
        self.0
    }
}

#[derive(Debug)]
pub struct SourceFile {
    pub path: Arc<Path>,
    pub package_root: Arc<Path>,
    pub source: Arc<str>,
    lines: OnceLock<Box<[BytePos]>>,
    pub source_len: BytePos,
    pub id: FileId,
}

impl SourceFile {
    pub const MAX_SIZE: usize = u32::MAX as usize;

    pub fn new(id: FileId, path: Arc<Path>, package_root: Arc<Path>, source: Arc<str>) -> Self {
        assert!(source.len() <= u32::MAX as usize);
        Self {
            package_root,
            lines: OnceLock::new(),
            source_len: BytePos::from_u32(source.len() as u32),
            path,
            source,
            id,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.source_len.to_u32() == 0
    }

    pub fn len(&self) -> u32 {
        self.source_len.to_u32()
    }

    pub fn line_bounds(&self, line: usize) -> Range<BytePos> {
        let lines = self.lines();
        if line >= lines.len() {
            return self.end_pos()..self.end_pos();
        }
        assert!(line < self.lines().len());
        match lines.get(line + 1) {
            Some(end) => lines[line]..*end,
            None => lines[line]..self.end_pos(),
        }
    }

    pub fn end_pos(&self) -> BytePos {
        self.source_len
    }

    pub fn get_line(&self, line: usize) -> &str {
        let lines = self.lines();
        let start = match lines.get(line) {
            Some(&v) => v,
            None => return "",
        };
        match lines.get(line + 1) {
            Some(end) => &self[start..*end - BytePos::from_u32(1)],
            None => &self.source[start.min(self.source_len).to_usize()..],
        }
    }

    pub fn lines(&self) -> &[BytePos] {
        self.lines.get_or_init(|| {
            std::iter::once(BytePos::from_u32(0))
                .chain(
                    self.source
                        .char_indices()
                        .filter(|v| v.1 == '\n')
                        .map(|v| BytePos::from_u32(v.0 as u32 + 1)),
                )
                .collect::<Vec<_>>()
                .into_boxed_slice()
        })
    }

    pub fn lookup_line(&self, pos: BytePos) -> Option<usize> {
        self.lines().partition_point(|x| *x <= pos).checked_sub(1)
    }

    /// Looks up the file's 1-based line and 0-based character position based on the byte position.
    pub fn lookup_file_pos(&self, pos: BytePos) -> (u32, u32) {
        let line = match self.lookup_line(pos) {
            Some(n) => n + 1,
            None => 0,
        };

        let (start_pos, s) = self
            .lines()
            .get(line.saturating_sub(1))
            .filter(|v| **v <= self.source_len)
            .map(|v| (v.to_usize(), &self.source[v.to_usize()..]))
            .unwrap_or((self.source.len(), ""));
        let pos = pos.to_usize() - start_pos;
        let mut charpos = 0;
        for (idx, _) in s.char_indices() {
            if idx < pos {
                charpos += 1;
            } else {
                break;
            }
        }
        (line as u32, charpos)
    }
}

impl Index<Range<BytePos>> for SourceFile {
    type Output = str;

    fn index(&self, index: Range<BytePos>) -> &Self::Output {
        &self.source[index.start.min(self.source_len).to_usize()
            ..index.end.to_usize().min(self.source_len.to_usize())]
    }
}

pub trait FileLoader {
    /// checks that some path exists and is readable
    fn exists(&self, path: &Path) -> bool;
    /// checks that some path exists, is readable and a directory
    fn is_dir(&self, path: &Path) -> bool;
    fn read_file(&self, path: &Path) -> io::Result<String>;
    fn read_binfile(&self, path: &Path) -> io::Result<Arc<[u8]>>;
}

#[derive(Clone, Copy)]
pub struct RealFileLoader;

impl FileLoader for RealFileLoader {
    fn exists(&self, path: &Path) -> bool {
        path.exists()
    }

    fn is_dir(&self, path: &Path) -> bool {
        path.is_dir()
    }

    fn read_file(&self, path: &Path) -> io::Result<String> {
        if path
            .metadata()
            .is_ok_and(|metadata| metadata.len() > SourceFile::MAX_SIZE as u64)
        {
            return Err(io::Error::other(format!(
                "text files larger than {} bytes are unsupported",
                SourceFile::MAX_SIZE
            )));
        }
        std::fs::read_to_string(path)
    }

    fn read_binfile(&self, path: &Path) -> io::Result<Arc<[u8]>> {
        // Copyright (c) The Rust Project Contributors
        // source: https://github.com/rust-lang/rust/blob/8708f3cd1f96d226f6420a58ebdd61aa0bc08b0a/compiler/rustc_span/src/source_map.rs#L127
        // license: https://github.com/rust-lang/rust/blob/8708f3cd1f96d226f6420a58ebdd61aa0bc08b0a/LICENSE-MIT
        let mut file = std::fs::File::open(path)?;
        let len = file.metadata()?.len();
        let mut bytes = Arc::new_uninit_slice(len as usize);
        let res = read_into_uninit(Arc::get_mut(&mut bytes).unwrap(), &mut file);
        match res {
            Ok(()) => {}
            Err(e) if e.kind() == ErrorKind::UnexpectedEof => {
                drop(bytes);
                return std::fs::read(path).map(Vec::into);
            }
            Err(e) => return Err(e),
        }
        // SAFETY: If the read_buf_exact call returns Ok(()), then we have
        // read len bytes and initialized the buffer.
        let bytes = unsafe { bytes.assume_init() };

        // At this point, we've read all the bytes that filesystem metadata reported exist.
        // But we are not guaranteed to be at the end of the file, because we did not attempt to do
        // a read with a non-zero-sized buffer and get Ok(0).
        // So we do small read to a fixed-size buffer. If the read returns no bytes then we're
        // already done, and we just return the Arc we built above.
        // If the read returns bytes however, we just fall back to reading into a Vec then turning
        // that into an Arc, losing our nice peak memory behavior. This fallback code path should
        // be rarely exercised.

        let mut probe = [0u8; 32];
        let n = loop {
            match file.read(&mut probe) {
                Ok(0) => return Ok(bytes),
                Err(e) if e.kind() == ErrorKind::Interrupted => continue,
                Err(e) => return Err(e),
                Ok(n) => break n,
            }
        };
        let mut bytes: Vec<u8> = bytes
            .iter()
            .copied()
            .chain(probe[..n].iter().copied())
            .collect();
        file.read_to_end(&mut bytes)?;
        Ok(bytes.into())
    }
}

fn read_into_uninit(buf: &mut [MaybeUninit<u8>], reader: &mut impl Read) -> io::Result<()> {
    let mut idx = 0;
    let mut data = [0u8; 1024];
    while idx < buf.len() {
        reader.read_exact(&mut data[..1024.min(buf.len() - idx)])?;
        idx += 1024;
    }
    Ok(())
}

pub struct SourceMap {
    files: RwLock<MonotonicVec<Arc<SourceFile>>>,
    file_loader: Box<dyn FileLoader + Send + Sync>,
}

impl SourceMap {
    pub fn new() -> Self {
        Self {
            files: Default::default(),
            file_loader: Box::new(RealFileLoader),
        }
    }

    pub fn new_with(file_loader: Box<dyn FileLoader + Send + Sync + 'static>) -> Self {
        Self {
            files: Default::default(),
            file_loader,
        }
    }

    pub fn get_file(&self, file: FileId) -> Option<Arc<SourceFile>> {
        self.files.read().get(file.0 as usize).cloned()
    }

    pub fn exists(&self, path: &Path) -> bool {
        self.file_loader.exists(path)
    }
    pub fn is_dir(&self, path: &Path) -> bool {
        self.file_loader.is_dir(path)
    }

    pub fn testing_new_file(&self, source: Arc<str>) -> Arc<SourceFile> {
        self.new_file(
            std::path::PathBuf::from(format!("root/file-{}.mr", self.files().len())).into(),
            Path::new("root").into(),
            source,
        )
    }

    pub fn new_file(&self, file: Arc<Path>, root: Arc<Path>, source: Arc<str>) -> Arc<SourceFile> {
        if let Some(f) = self.files.read().iter().find(|v| v.path == file) {
            if f.package_root != root {
                eprintln!(
                    "soft-err: new_file: loading {}, found in cache, but package root is mismatching (expected {}, found {})",
                    file.display(),
                    root.display(),
                    f.package_root.display()
                );
            }
            if f.source != source {
                eprintln!(
                    "soft-err: new_file: loading {}, found in cache, but the source is mismatching",
                    file.display(),
                );
            }
            return Arc::clone(f);
        }
        let mut files = self.files.write();
        let id = FileId(files.len() as u32);
        let file = Arc::new(SourceFile::new(id, file, root, source));
        files.push(file.clone());
        file
    }

    pub fn load_file(&self, path: Arc<Path>, root: Arc<Path>) -> io::Result<Arc<SourceFile>> {
        if let Some(f) = self.files.read().iter().find(|v| v.path == path) {
            return Ok(Arc::clone(f));
        }
        let source = self.file_loader.read_file(&path)?.into();
        Ok(self.new_file(path, root, source))
    }

    pub fn files<'a>(&'a self) -> RwLockReadGuard<'a, MonotonicVec<Arc<SourceFile>>> {
        self.files.read()
    }
}

impl Default for SourceMap {
    fn default() -> Self {
        Self::new()
    }
}

impl SourceMap {
    pub fn is_empty(&self, file: FileId) -> bool {
        self.get_file(file).unwrap().is_empty()
    }

    pub fn len(&self, file: FileId) -> u32 {
        self.get_file(file).unwrap().len()
    }

    pub fn line_bounds(&self, file: FileId, line: usize) -> Range<BytePos> {
        self.get_file(file).unwrap().line_bounds(line)
    }

    pub fn end_pos(&self, file: FileId) -> BytePos {
        self.get_file(file).unwrap().end_pos()
    }

    pub fn lookup_line(&self, file: FileId, pos: BytePos) -> Option<usize> {
        self.get_file(file).unwrap().lookup_line(pos)
    }

    /// Looks up the file's 1-based line and 0-based character position based on the byte position.
    pub fn lookup_file_pos(&self, file: FileId, pos: BytePos) -> (u32, u32) {
        self.get_file(file).unwrap().lookup_file_pos(pos)
    }
}

mod monotonic {
    use std::{fmt::Debug, ops::Deref};

    pub struct MonotonicVec<T>(Vec<T>);

    impl<T: Debug> Debug for MonotonicVec<T> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            Debug::fmt(&self.0, f)
        }
    }

    impl<T> MonotonicVec<T> {
        pub const fn new() -> Self {
            Self(Vec::new())
        }

        pub fn push(&mut self, item: T) {
            self.0.push(item);
        }
    }

    impl<T> Default for MonotonicVec<T> {
        fn default() -> Self {
            Self::new()
        }
    }

    impl<T> Deref for MonotonicVec<T> {
        type Target = Vec<T>;

        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }
}

#[cfg(test)]
mod test {
    use crate::arena::Arena;

    use super::*;

    macro_rules! fn_test {
        ($fn:ident: $(($($value:expr),* $(,)?) == $expected:expr;)*) => {
            #[test]
            fn $fn() {
                let src = "aaa\nbbbb\ncccc";
                let file = SourceFile::new(FileId::ZERO, Path::new("file").into(), Path::new("root").into(), src.into());

                $(assert_eq!(file.$fn($($value),*), $expected);)*
            }
        };
    }

    fn_test! {
        get_line:
        (0) == "aaa";
        (1) == "bbbb";
        (2) == "cccc";
        (3) == "";
    }

    fn_test! {
        lookup_file_pos:
        (BytePos::from_u32(0)) == (1, 0);
        (BytePos::from_u32(1)) == (1, 1);
        (BytePos::from_u32(2)) == (1, 2);
        (BytePos::from_u32(3)) == (1, 3);

        (BytePos::from_u32(4)) == (2, 0);
        (BytePos::from_u32(5)) == (2, 1);
        (BytePos::from_u32(6)) == (2, 2);
        (BytePos::from_u32(7)) == (2, 3);
        (BytePos::from_u32(8)) == (2, 4);

        (BytePos::from_u32(9)) == (3, 0);
        (BytePos::from_u32(10)) == (3, 1);
        (BytePos::from_u32(11)) == (3, 2);
        (BytePos::from_u32(12)) == (3, 3);

        // past document end
        (BytePos::from_u32(13))== (3, 4);
        (BytePos::from_u32(14))== (3, 4);
    }

    fn_test! {
        lookup_line:
        (BytePos::from_u32(0)) == Some(0);
        (BytePos::from_u32(1)) == Some(0);
        (BytePos::from_u32(2)) == Some(0);
        (BytePos::from_u32(3)) == Some(0);

        (BytePos::from_u32(4)) == Some(1);
        (BytePos::from_u32(5)) == Some(1);
        (BytePos::from_u32(6)) == Some(1);
        (BytePos::from_u32(7)) == Some(1);
        (BytePos::from_u32(8)) == Some(1);

        (BytePos::from_u32(9)) == Some(2);
        (BytePos::from_u32(10)) == Some(2);
        (BytePos::from_u32(11)) == Some(2);
        (BytePos::from_u32(12)) == Some(2);

        // past document end
        (BytePos::from_u32(13)) == Some(2);
        (BytePos::from_u32(14)) == Some(2);
    }

    #[test]
    fn span() {
        let arena = Arena::new();
        let mut interner = SpanInterner::new(&arena);

        let spans = [
            make_span(12, 120, 0, &mut interner),
            make_span(12, u32::MAX, 0, &mut interner),
            make_span(u32::MAX, 12, 0, &mut interner),
            make_span(60, 12, u16::MAX as u32, &mut interner),
            make_span(60, 12, u32::MAX, &mut interner),
        ];
        assert_eq!(spans[0].inner, 786673);
        assert_eq!(spans[2].inner, 281474976645145);
        assert_eq!(spans[3].inner, 18446462598736773145);
        assert!(!spans[0].is_interned());
        assert!(spans[1].is_interned());
        assert!(!spans[2].is_interned());
        assert!(!spans[3].is_interned());
        assert!(spans[4].is_interned());
        assert_eq!(spans[0].get_span_data(), make_span_data(12, 120, 0));
        assert_eq!(spans[1].get_span_data(), make_span_data(12, u32::MAX, 0));
        assert_eq!(spans[2].get_span_data(), make_span_data(u32::MAX, 12, 0));
        assert_eq!(
            spans[3].get_span_data(),
            make_span_data(60, 12, u16::MAX as u32)
        );
        assert_eq!(spans[4].get_span_data(), make_span_data(60, 12, u32::MAX));
    }

    fn make_span_data(pos: u32, len: u32, file: u32) -> SpanData {
        SpanData {
            pos: BytePos::from_u32(pos),
            len,
            file: FileId(file),
        }
    }
    fn make_span<'a>(pos: u32, len: u32, file: u32, interner: &mut SpanInterner<'a>) -> Span<'a> {
        Span::new(make_span_data(pos, len, file), interner)
    }
}
