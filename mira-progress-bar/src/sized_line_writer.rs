pub(crate) struct SizedLineWriter<'a> {
    pub max_width: usize,
    pub width_left: usize,
    pub underlying: &'a mut dyn std::io::Write,
    pub lines_written: usize,
}

impl SizedLineWriter<'_> {
    pub(crate) fn write_str(&mut self, s: &str) -> std::io::Result<()> {
        for c in s.chars() {
            self.write_char(c)?
        }
        Ok(())
    }

    pub(crate) fn write_char(&mut self, c: char) -> std::io::Result<()> {
        let mut c_bytes = [0u8; 4];
        let c_bytes = c.encode_utf8(&mut c_bytes).as_bytes();
        if c == '\r' || c == '\n' {
            self.width_left = self.max_width;
            if c == '\n' {
                self.lines_written += 1;
            }
            return self.underlying.write_all(c_bytes);
        }
        if self.width_left == 0 {
            return Ok(());
        }
        self.width_left -= 1;
        self.underlying.write_all(c_bytes)
    }
}

impl<'a> SizedLineWriter<'a> {
    pub(crate) fn new(max_width: usize, writer: &'a mut dyn std::io::Write) -> Self {
        Self {
            max_width,
            width_left: max_width,
            underlying: writer,
            lines_written: 0,
        }
    }
}
