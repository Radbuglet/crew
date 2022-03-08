use crate::syntax::span::{FileReader, ReadAtom, Span};
use crate::util::enum_utils::{enum_meta, EnumMeta};
use crate::util::fmt::{FmtPaddedNumber, FmtRepeat};
use crate::util::reader::{ErrorAccumulator, LookaheadReader, StreamReader};
use colored::{Color, Colorize};
use std::cmp::Ordering;
use std::fmt::{Display, Formatter};

// === Parse Results === //

pub type PResult<T> = Result<T, ParseError>;

pub trait PResultExt {
    type Result;

    fn push_diag(self, diagnostic: &mut Diagnostics) -> Result<Self::Result, ()>;
}

impl<T> PResultExt for PResult<T> {
    type Result = T;

    fn push_diag(self, diagnostic: &mut Diagnostics) -> Result<Self::Result, ()> {
        self.map_err(|err| {
            err.push_diag(diagnostic);
        })
    }
}

#[derive(Debug, Clone)]
pub struct ParseError {
    contents: Option<ParseErrorNonEmpty>,
}

#[derive(Debug, Clone)]
pub struct ParseErrorNonEmpty {
    span: Span,
    encountered: String,
    expected: Vec<String>,
}

impl ParseError {
    pub fn empty() -> Self {
        Self { contents: None }
    }

    pub fn new<U, E>(span: Span, encountered: U, expected: &[E]) -> Self
    where
        U: Display,
        E: Display,
    {
        Self {
            contents: Some(ParseErrorNonEmpty {
                span,
                encountered: encountered.to_string(),
                expected: expected.into_iter().map(|err| err.to_string()).collect(),
            }),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.contents.is_none()
    }

    pub fn push_diag(self, diag: &mut Diagnostics) {
        let contents = self
            .contents
            .as_ref()
            .expect("Cannot push diagnostics for an empty parse error.");

        diag.display_error(
            contents.span.clone(),
            format!(
                "Expected one of {}, found {}",
                ExpectListFmt(contents.expected.as_slice()),
                contents.encountered,
            ),
        );
    }

    fn logical_pos(&self) -> usize {
        self.contents
            .as_ref()
            .map_or(0, |inner| inner.span.start().char_index())
    }
}

impl ErrorAccumulator for ParseError {
    fn empty_error() -> Self {
        Self::empty()
    }

    fn extend_error(&mut self, mut error: Self) {
        match self.logical_pos().cmp(&error.logical_pos()) {
            Ordering::Less => *self = error,
            Ordering::Equal => match (&mut self.contents, &mut error.contents) {
                (Some(a), Some(b)) => a.expected.append(&mut b.expected),
                (Some(_), None) => { /* no errors to append */ }
                (None, None) => { /* no errors to append */ }
                (None, Some(_)) => unreachable!(),
            },
            Ordering::Greater => { /* no errors to append */ }
        }
    }
}

#[derive(Debug, Clone)]
struct ExpectListFmt<'a>(&'a [String]);

impl Display for ExpectListFmt<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let expected_list = &self.0;
        for (i, expected) in expected_list.iter().enumerate() {
            // Push prefix
            if i == expected_list.len() - 1 {
                f.write_str(", or ")?;
            } else if i > 0 {
                f.write_str(", ")?;
            } else {
                // No prefix for first element
            }

            // Push element
            f.write_str(expected.as_str())?;
        }

        Ok(())
    }
}

// === Diagnostics === //

#[derive(Debug, Default, Clone)]
pub struct Diagnostics {
    messages: Vec<Message>,
}

impl Diagnostics {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn display_error<F: Display>(&mut self, span: Span, text: F) {
        self.raw_push(Message {
            kind: MessageType::Error,
            span,
            text: text.to_string(),
        });
    }

    pub fn display_warn<F: Display>(&mut self, span: Span, text: F) {
        self.raw_push(Message {
            kind: MessageType::Warning,
            span,
            text: text.to_string(),
        });
    }

    pub fn raw_push(&mut self, message: Message) {
        self.messages.push(message);
    }

    pub fn temp_display(&self) {
        for message in &self.messages {
            message.temp_display();
        }
    }
}

impl LookaheadReader for Diagnostics {}

#[derive(Debug, Clone)]
pub struct Message {
    pub span: Span,
    pub kind: MessageType,
    pub text: String,
}

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

// === Diagnostic printing === //

impl Message {
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
            FmtRepeat {
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
                    FmtPaddedNumber {
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
                    FmtRepeat {
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
                    print!("{}", char.bold());
                } else {
                    print!("{}", char);
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
