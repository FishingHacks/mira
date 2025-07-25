use std::io::Write;

type Cmd<Data> = fn(&str, &mut Repl<Data>);
type CmdList<Data> = Vec<(&'static str, Cmd<Data>)>;

pub struct Repl<Data> {
    pub commands: CmdList<Data>,
    pub on_no_input: Cmd<Data>,
    pub buf: String,
    pub data: Data,
}

/// Returns the start index of the line or the number of lines
fn get_line_start(mut line: usize, buffer: &str) -> Result<usize, usize> {
    let mut lines = 0;
    for (idx, c) in buffer.char_indices() {
        if line == 0 {
            return Ok(idx);
        }
        if c == '\n' {
            line -= 1;
            lines += 1;
        }
    }
    Err(lines)
}

impl<Data> Repl<Data> {
    pub fn new(commands: CmdList<Data>, on_no_input: Cmd<Data>, data: Data, buf: String) -> Self {
        Self {
            commands,
            on_no_input,
            data,
            buf,
        }
    }

    pub fn run(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        loop {
            let mut output = std::io::stdout();
            write!(output, "> ")?;
            output.flush()?;
            let mut input = String::new();
            std::io::stdin().read_line(&mut input)?;
            let input = input.trim_end();
            if input.is_empty() {
                (self.on_no_input)("", self);
                continue;
            }

            if !input.starts_with('.') {
                if !self.buf.is_empty() && !self.buf.ends_with('\n') {
                    self.buf.push('\n');
                }
                self.buf.push_str(input);
            } else {
                let input = &input[1..];
                let (cmd, rest) = {
                    let mut chars = input.chars();
                    let first_char = chars.next();
                    let second_char = chars.next();
                    if matches!(first_char, Some('^' | 'v' | '=' | '%' | ']' | '['))
                        && matches!(second_char, Some('0'..='9'))
                    {
                        input.split_at(1)
                    } else {
                        input.split_once(' ').unwrap_or((input, ""))
                    }
                };
                match cmd {
                    "]" => {
                        let rest = rest.trim();
                        let (line, rest) = rest.split_once(' ').unwrap_or((rest, ""));
                        let line = line.trim();
                        let line: usize = match line.trim().parse() {
                            Ok(v) => v,
                            Err(e) => {
                                writeln!(output, "Could not parse {line:?} as number: {e:?}")?;
                                continue;
                            }
                        };

                        match get_line_start(line + 1, &self.buf) {
                            Ok(idx) => self.buf.insert_str(idx - 1, rest),
                            Err(lines) if lines == line + 1 => {
                                self.buf.insert_str(self.buf.len().saturating_sub(1), rest);
                            }
                            Err(lines) => writeln!(
                                output,
                                "line {line} doesn't exist (buffer has {lines} lines)"
                            )?,
                        }
                    }
                    "[" => {
                        let rest = rest.trim();
                        let (line, rest) = rest.split_once(' ').unwrap_or((rest, ""));
                        let line = line.trim();
                        let line: usize = match line.trim().parse() {
                            Ok(v) => v,
                            Err(e) => {
                                writeln!(output, "Could not parse {line:?} as number: {e:?}")?;
                                continue;
                            }
                        };

                        if line == 0 {
                            self.buf.insert_str(0, rest);
                        } else {
                            match get_line_start(line, &self.buf) {
                                Ok(idx) => self.buf.insert_str(idx, rest),
                                Err(lines) => writeln!(
                                    output,
                                    "line {line} doesn't exist (buffer has {lines} lines)"
                                )?,
                            }
                        }
                    }
                    "%" => {
                        let rest = rest.trim();
                        let (line, rest) = rest.split_once(' ').unwrap_or((rest, ""));
                        let line = line.trim();
                        let line: usize = match line.trim().parse() {
                            Ok(v) => v,
                            Err(e) => {
                                writeln!(output, "Could not parse {line:?} as number: {e:?}")?;
                                continue;
                            }
                        };
                        let (start, end) = {
                            let mut start = usize::MAX;
                            let mut end = usize::MAX;
                            let mut lines = 0;
                            let num = line;
                            for (idx, c) in self.buf.char_indices() {
                                if c == '\n' {
                                    lines += 1;
                                }
                                if num == 0 && start == usize::MAX {
                                    start = idx;
                                }
                                if c == '\n' && num == 0 {
                                    end = idx;
                                    break;
                                }
                            }
                            if start == usize::MAX {
                                writeln!(
                                    output,
                                    "line {line} doesn't exist (buffer has {lines} lines)"
                                )?;
                                continue;
                            } else {
                                if end == usize::MAX {
                                    end = self.buf.len() - 1;
                                }
                                (start, end)
                            }
                        };
                        self.buf.replace_range(start..end, rest);
                    }
                    "v" => {
                        let rest = rest.trim();
                        let (line, rest) = rest.split_once(' ').unwrap_or((rest, ""));
                        let line = line.trim();
                        let line: usize = match line.trim().parse() {
                            Ok(v) => v,
                            Err(e) => {
                                writeln!(output, "Could not parse {line:?} as number: {e:?}")?;
                                continue;
                            }
                        };

                        if line == 0 {
                            self.buf.insert_str(0, rest);
                            self.buf.insert(rest.len(), '\n');
                        } else {
                            match get_line_start(line, &self.buf) {
                                Ok(idx) => {
                                    self.buf.insert_str(idx, rest);
                                    self.buf.insert(idx + rest.len(), '\n');
                                }
                                Err(lines) => writeln!(
                                    output,
                                    "line {line} doesn't exist (buffer has {lines} lines)"
                                )?,
                            }
                        }
                    }
                    "^" => {
                        let rest = rest.trim();
                        let (line, rest) = rest.split_once(' ').unwrap_or((rest, ""));
                        let line = line.trim();
                        let line: usize = match line.trim().parse() {
                            Ok(v) => v,
                            Err(e) => {
                                writeln!(output, "Could not parse {line:?} as number: {e:?}")?;
                                continue;
                            }
                        };
                        match get_line_start(line + 1, &self.buf) {
                            Ok(idx) => {
                                self.buf.insert_str(idx, rest);
                                self.buf.insert(idx + rest.len(), '\n');
                            }
                            Err(lines) if lines == line + 1 => {
                                self.buf.push_str(rest);
                                self.buf.push('\n');
                            }
                            Err(lines) => writeln!(
                                output,
                                "line {line} doesn't exist (buffer has {lines} lines)"
                            )?,
                        }
                    }
                    "dd" | "del" | "delete" => {
                        let num: usize = match rest.trim().parse() {
                            Ok(v) => v,
                            Err(e) => {
                                writeln!(output, "Could not parse {rest:?} as number: {e:?}")?;
                                continue;
                            }
                        };
                        let (start, end) = {
                            let mut start = usize::MAX;
                            let mut end = usize::MAX;
                            let mut lines = 0;
                            let mut line = num;
                            for (idx, c) in self.buf.char_indices() {
                                if c == '\n' {
                                    lines += 1;
                                }
                                if line == 0 && start == usize::MAX {
                                    start = idx;
                                }
                                if c == '\n' && line == 0 {
                                    end = idx;
                                    break;
                                } else if c == '\n' {
                                    line -= 1;
                                }
                            }
                            if start == usize::MAX {
                                writeln!(
                                    output,
                                    "line {num} doesn't exist (buffer has {lines} lines)"
                                )?;
                                continue;
                            } else {
                                if end == usize::MAX {
                                    end = self.buf.len() - 1;
                                }
                                (start, end)
                            }
                        };
                        self.buf.replace_range(start..=end, "");
                    }
                    "gc" | "comment" => {
                        let num: usize = match rest.trim().parse() {
                            Ok(v) => v,
                            Err(e) => {
                                writeln!(output, "Could not parse {rest:?} as number: {e:?}")?;
                                continue;
                            }
                        };
                        let start = {
                            let mut start = usize::MAX;
                            let mut lines = 0;
                            let mut line = num;
                            for (idx, c) in self.buf.char_indices() {
                                if c == '\n' {
                                    lines += 1;
                                }
                                if line == 0 && start == usize::MAX {
                                    start = idx;
                                }
                                if c == '\n' && line == 0 {
                                    break;
                                } else if c == '\n' {
                                    line -= 1;
                                }
                            }
                            if start == usize::MAX {
                                writeln!(
                                    output,
                                    "line {num} doesn't exist (buffer has {lines} lines)"
                                )?;
                                continue;
                            } else {
                                start
                            }
                        };
                        if self.buf[start..].starts_with("//") {
                            self.buf.remove(start);
                            self.buf.remove(start);
                        } else {
                            self.buf.insert(start, '/');
                            self.buf.insert(start, '/');
                        }
                    }
                    "esc" => {
                        self.buf.push_str(rest);
                        self.buf.push('\n');
                    }
                    "clear" => {
                        self.buf.clear();
                        writeln!(output, "cleared buffer")?;
                    }
                    "list" => {
                        let rest = rest.trim();
                        let with_line_numbers = rest == "-l" || rest.starts_with("-l ");
                        for (idx, line) in self.buf.lines().enumerate() {
                            if with_line_numbers {
                                write!(output, "{idx: <4} ")?;
                            }
                            writeln!(output, "{line}")?;
                        }
                    }
                    "exit" => break Ok(()),
                    _ => {
                        if let Some(cmd) = self.commands.iter().find(|v| v.0 == cmd) {
                            cmd.1(rest, self);
                        } else {
                            writeln!(output, "Unknown command `{cmd}`.")?
                        }
                    }
                }
            }
        }
    }
}
