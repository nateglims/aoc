//

#[derive(Debug)]
struct Span(usize, usize);

#[derive(Debug)]
struct Number {
    line: usize,
    span: Span,
    val: i64,
}

#[derive(Debug)]
struct NumParser<'s> {
    // The current line number.
    line: usize,
    // Absolute index from the start of the input.
    idx: usize,
    // Index of the previous new line.
    last_line: usize,
    input: &'s str,
}

impl<'s> Iterator for NumParser<'s> {
    type Item = Number;

    fn next(&mut self) -> Option<Self::Item> {
        // Consume number.
        for ch in self.input[self.idx..].chars() {
            if ch.is_digit(10) {
                // eprintln!("found num start {} <{}>", ch, self.idx);

                // Track the start of this span.
                let start = self.idx;
                let mut chrs = self.input[start..].chars();

                while let Some(n) = chrs.next() {
                    // This might be easier with some kind of peekable iterator.
                    if !n.is_digit(10) {
                        let val: i64 = self.input[start..self.idx].parse().unwrap();
                        return Some(Number {
                            val,
                            line: self.line,
                            // Use the line offset for the span.
                            span: Span(start - self.last_line, self.idx - self.last_line),
                        });
                    }
                    self.idx += n.len_utf8();
                    // eprintln!("found digit {} <{}>", n, self.idx);
                }
            }

            self.idx += ch.len_utf8();

            if ch == '\n' {
                // eprintln!("new line");
                self.line += 1;
                self.last_line = self.idx;
            }
        }
        None
    }
}

fn num_parser(input: &str) -> NumParser {
    NumParser {
        line: 0,
        idx: 0,
        last_line: 0,
        input,
    }
}

#[derive(Debug)]
struct Symbol {
    line: usize,
    offset: usize,
    val: char,
}

impl Symbol {
    fn is_near(&self, n: &Number) -> bool {
        let is_near = self.line >= n.line.saturating_sub(1)
            && self.line <= n.line + 1
            && self.offset >= n.span.0.saturating_sub(1)
            && self.offset < n.span.1 + 1;
        if is_near {
            eprintln!("Symbol: {:?}", self);
            eprintln!("Number: {:?}", n);
            eprintln!("Near? {}", is_near);
        }
        is_near
    }
}

#[derive(Debug)]
struct SymParser<'s> {
    line: usize,
    idx: usize,
    last_line: usize,
    input: &'s str,
}

impl<'s> Iterator for SymParser<'s> {
    type Item = Symbol;

    fn next(&mut self) -> Option<Self::Item> {
        for ch in self.input[self.idx..].chars() {
            if ch == '\n' {
                self.line += 1;
                self.idx += ch.len_utf8();
                self.last_line = self.idx;
            } else if ch != '.' && !ch.is_digit(10) {
                let offset = self.idx - self.last_line;
                // eprintln!("{} ({}, {})", ch, self.line, offset);
                self.idx += ch.len_utf8();
                return Some(Symbol {
                    line: self.line,
                    offset,
                    val: ch,
                });
            } else {
                self.idx += ch.len_utf8();
            }
        }

        None
    }
}

fn sym_parser(input: &str) -> SymParser {
    SymParser {
        line: 0,
        idx: 0,
        last_line: 0,
        input,
    }
}

// Part 1
pub fn part1(input: &str) -> i64 {
    // First pass for numbers.
    let syms: Vec<Symbol> = sym_parser(input).collect();
    num_parser(input)
        .filter(|n| syms.iter().any(|s| s.is_near(n)))
        .map(|n| n.val)
        .sum()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_1() {
        let input = "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
";

        assert_eq!(part1(input), 4361);
    }
}
