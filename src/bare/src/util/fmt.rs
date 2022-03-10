use std::fmt::{Display, Formatter};

#[derive(Debug, Clone)]
pub struct FmtRepeat<T> {
    pub count: u32,
    pub text: T,
}

impl<T: Display> Display for FmtRepeat<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for _ in 0..self.count {
            self.text.fmt(f)?;
        }
        Ok(())
    }
}

pub fn tab(count: u32) -> FmtRepeat<&'static str> {
    FmtRepeat {
        count,
        text: "    ",
    }
}

#[derive(Debug, Clone)]
pub struct FmtPaddedNumber {
    pub number: usize,
    pub space: u32,
}

impl Display for FmtPaddedNumber {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.number)?;
        FmtRepeat {
            count: self.space - (self.number.log10() + 1),
            text: " ",
        }
        .fmt(f)?;
        Ok(())
    }
}
