use crate::syntax::span::{FileReader, Span};
use crate::util::enum_utils::{enum_meta, EnumMeta};
use crate::{ReadAtom, StreamReader};
use colored::{Color, Colorize};
use std::fmt::{Display, Formatter};

enum_meta! {
    #[derive(Debug)]
    pub enum(MessageTypeMeta) MessageType {
        Error = MessageTypeMeta {
            prefix: "error",
            color: Color::Red,
        },
        Warning = MessageTypeMeta {
            prefix: "warning",
            color: Color::Yellow,
        },
    }
}

#[derive(Debug)]
pub struct MessageTypeMeta {
    pub prefix: &'static str,
    pub color: Color,
}

#[derive(Debug, Clone)]
pub struct Message {
    pub span: Span,
    pub kind: MessageType,
    pub text: String,
}

impl Message {
    // FIXME: This code is complete garbage.
    // ^ 1. Diagnostics should be printed through user-selected printing strategies, not a default
    //      `display` method.
    //   2. We should support several spans in one code sample.
    //   3. We should minimize allocations and reduce the number of color changes.
    //   4. We should auto-wrap lines based off the terminal's width to make the span of long lines
    //      intelligible.
    //   5. What the heck is this control flow?!
    pub fn temp_display(&self) {
        // Display principal message
        let kind_meta = self.kind.meta();
        println!(
            "{}{} {}",
            kind_meta.prefix.color(kind_meta.color).bold(),
            ":".bright_white().bold(),
            self.text.bright_white().bold(),
        );

        // Get the full line span
        let sub_span = &self.span;
        let full_span = {
            let end = sub_span.end();
            let mut end_reader = end.reader();
            loop {
                match end_reader.consume() {
                    ReadAtom::Newline(_) => break,
                    ReadAtom::Eof => break,
                    _ => {}
                }
            }

            Span::new(sub_span.start().line_start(), end_reader.prev_loc())
        };

        // Display file heading
        let max_digits = full_span.end_pos().displayed_line().log10() + 1;
        let left_padding = max_digits + 1;
        let has_bar =
            full_span.start_pos().displayed_line() != full_span.end_pos().displayed_line();

        println!(
            "{}{} {}",
            Repeat {
                text: ' ',
                count: left_padding - 1,
            },
            "-->".bright_cyan().bold(),
            sub_span.start().to_string().blue().underline(),
        );

        // Display file reading
        let print_newline_prefix = move |reader: &mut FileReader, show_number: bool| {
            if show_number {
                let loc = reader.next_loc();
                print!(
                    "{}{}",
                    PaddedNumber {
                        number: loc.displayed_line(),
                        space: left_padding
                    }
                    .to_string()
                    .bright_cyan(),
                    "| ".bright_cyan().bold(),
                );
            } else {
                print!(
                    "{}{}",
                    Repeat {
                        text: ' ',
                        count: left_padding,
                    }
                    .to_string()
                    .bright_cyan(),
                    "| ".bright_cyan().bold(),
                );
            }
        };

        #[derive(Debug, Copy, Clone)]
        enum RightPadStyle {
            /// No space between the bar and the cursor.
            Collated,

            /// Same layout as with a bar but the bar is hidden.
            NoBar,

            /// Full bar layout.
            HasBar,
        }

        let print_right_pad = move |style: RightPadStyle| {
            if has_bar {
                match style {
                    RightPadStyle::Collated => print!(" "), // The space is a placeholder for the bar.
                    RightPadStyle::NoBar => print!("  "), // First space is bar, second is spacing.
                    RightPadStyle::HasBar => print!("{} ", "|".bright_yellow()),
                }
            }
        };

        let mut reader = full_span.reader();
        let mut in_span = false;

        // Print empty spacing line
        print_newline_prefix(&mut reader, false);
        println!();

        // Begin printing
        print_newline_prefix(&mut reader, true);
        print_right_pad(RightPadStyle::NoBar);

        loop {
            let current_loc = reader.next_loc();

            // Detect span start
            if current_loc == sub_span.start() {
                in_span = true;
            }

            // Handle character
            let print_char = |char: &str| {
                if in_span {
                    print!("{}", char);
                } else {
                    print!("{}", char.bright_black());
                }
            };

            let render_highlight_bars = |reader: &mut FileReader| {
                // Handle first line
                if current_loc.line_start() == full_span.start() {
                    // Print prefix
                    println!();
                    print_newline_prefix(reader, false);
                    print_right_pad(RightPadStyle::Collated);

                    // Print character underline
                    let prev_loc = reader.prev_loc();
                    let line_span = Span::new(prev_loc.line_start(), prev_loc);
                    let mut line_reader = line_span.reader();

                    if has_bar {
                        print!("{}", "_".bright_yellow());

                        while line_reader.consume() != ReadAtom::Eof {
                            if line_reader.prev_loc() == sub_span.start() {
                                print!("{}", "^".bright_yellow());
                                break;
                            }

                            print!("{}", "_".bright_yellow());
                        }
                    } else {
                        let mut char = " ".to_string();
                        while line_reader.consume() != ReadAtom::Eof {
                            if line_reader.prev_loc() == sub_span.start() {
                                char = "^".bright_yellow().to_string();
                            }

                            print!("{}", char);

                            if line_reader.prev_loc() == sub_span.end() {
                                char = " ".to_string();
                            }
                        }
                    }
                } else if current_loc.line_start() == full_span.end().line_start() {
                    // This code path is never reached in single-line scenarios.

                    // Print prefix
                    println!();
                    print_newline_prefix(reader, false);
                    print_right_pad(RightPadStyle::Collated);

                    // Print character underline
                    let prev_loc = reader.prev_loc();
                    let line_span = Span::new(prev_loc.line_start(), prev_loc);
                    let mut line_reader = line_span.reader();

                    print!("{}", "_".bright_yellow());

                    while line_reader.consume() != ReadAtom::Eof {
                        if line_reader.prev_loc() == sub_span.end() {
                            print!("{}", "^".bright_yellow());
                            break;
                        }

                        print!("{}", "_".bright_yellow());
                    }
                }
            };

            match reader.consume() {
                // Draw characters
                ReadAtom::Codepoint('\t') => print_char("    "),
                ReadAtom::Codepoint(char) => print_char(&char.to_string()),
                ReadAtom::Unknown(_) => print_char(&ReadAtom::UNRECOGNIZED_CHAR.to_string()),

                // Handle new line and decorations
                ReadAtom::Newline(_) => {
                    render_highlight_bars(&mut reader);

                    // Move to next line
                    println!();
                    print_newline_prefix(&mut reader, true);
                    print_right_pad(RightPadStyle::HasBar);
                }
                ReadAtom::Eof => {
                    render_highlight_bars(&mut reader);
                    break;
                }
            }

            // Detect span end
            if current_loc == sub_span.end() {
                in_span = false;
            }
        }

        println!();
    }
}

#[derive(Debug, Clone)]
struct Repeat<T> {
    pub count: u32,
    pub text: T,
}

impl<T: Display> Display for Repeat<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for _ in 0..self.count {
            self.text.fmt(f)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
struct PaddedNumber {
    pub number: usize,
    pub space: u32,
}

impl Display for PaddedNumber {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.number)?;
        Repeat {
            count: self.space - (self.number.log10() + 1),
            text: " ",
        }
        .fmt(f)?;
        Ok(())
    }
}
