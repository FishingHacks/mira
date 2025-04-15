pub struct SizedLineWriter<'a> {
    pub max_width: usize,
    pub width_left: usize,
    pub underlying: &'a mut dyn std::fmt::Write,
    pub lines_written: usize,
}

impl std::fmt::Write for SizedLineWriter<'_> {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        for c in s.chars() {
            self.write_char(c)?
        }
        Ok(())
    }

    fn write_char(&mut self, c: char) -> std::fmt::Result {
        if c == '\r' || c == '\n' {
            self.width_left = self.max_width;
            self.lines_written += 1;
            return self.underlying.write_char(c);
        }
        if self.width_left == 0 {
            return Ok(());
        }
        self.width_left -= 1;
        self.underlying.write_char(c)
    }
}

impl<'a> SizedLineWriter<'a> {
    pub fn new(max_width: usize, writer: &'a mut dyn std::fmt::Write) -> Self {
        Self {
            max_width,
            width_left: max_width,
            underlying: writer,
            lines_written: 1,
        }
    }
}
