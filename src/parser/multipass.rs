use std::{collections::HashSet, iter::Peekable, str::Chars};

use super::{BrailleChars, ParseError};

#[derive(Debug, PartialEq, Eq)]
pub struct Test {
    only_at_beginning: bool,
    only_at_end: bool,
    tests: Vec<TestInstruction>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
enum Attribute {
    Digit,
    Litdigit,
    Letter,
    Sign,
    Space,
    Math,
    Punctuation,
    Uppercase,
    Lowercase,
    Class1,
    Class2,
    Class3,
    Class4,
    Any,
}

#[derive(Debug, PartialEq, Eq)]
enum Operator {
    EQ,
    GT,
    LT,
    GTEQ,
    LTEQ,
}


#[derive(Debug, PartialEq, Eq)]
enum TestInstruction {
    Lookback { len: u8 },
    VariableTest { var: u8, val: u8, op: Operator },
    String { s: String },
    Dots { dots: BrailleChars },
    Attributes { attrs: HashSet<Attribute>, min_chars: u8, max_chars: u8 },
    SwapOrAttribute { name: String, min_chars: u8, max_chars: u8 },
    Negate { instr: Box<TestInstruction> },
    Replace { instr: Vec<TestInstruction> },
}

pub struct TestParser<'a> {
    chars: Peekable<Chars<'a>>
}

impl<'a> TestParser<'a> {
    pub fn new(source: &'a str) -> Self {
	Self {
	    chars: source.chars().peekable(),
	}
    }

    pub fn test (&mut self) -> Result<Test, ParseError> {
	match self.chars.peek() {
//	    Some('_') => self.lookback(),
	    _ => Err(ParseError::MultiTestExpected)
	}
    }

    fn ascii_number(&mut self) -> Result<u8, ParseError> {
	let mut s = String::new();
	while let Some(c) = self.chars.peek() {
	    if !c.is_ascii_digit() {break;}
	    s.push(self.chars.next().unwrap());
	}
	let number = s.parse::<u8>()?;
	Ok(number)
    }
    
    fn lookback(&mut self) -> Result<TestInstruction, ParseError> {
	if self.chars.next_if_eq(&'_').is_some() { // consume the '_'
	    let n = match self.chars.peek() {
		Some(c) if c.is_ascii_digit() => self.ascii_number()?,
		_ => 1
	    };
	    Ok(TestInstruction::Lookback { len: n })
	} else {
	    Err(ParseError::MultiTestExpected)
	}
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ascii_number_test() {
        assert_eq!(TestParser::new("123").ascii_number(),Ok(123));
        assert_eq!(TestParser::new("1").ascii_number(),Ok(1));
        assert_eq!(TestParser::new("1 ").ascii_number(),Ok(1));
        assert_eq!(TestParser::new("1abc").ascii_number(),Ok(1));
	assert_ne!(TestParser::new(" 1").ascii_number(),Ok(1));
	assert_ne!(TestParser::new("a1").ascii_number(),Ok(1));
    }

    #[test]
    fn look_back_test() {
        assert_eq!(TestParser::new("_123").lookback(),Ok(TestInstruction::Lookback { len: 123 }));
        assert_eq!(TestParser::new("_7").lookback(),Ok(TestInstruction::Lookback { len: 7 }));
        assert_eq!(TestParser::new("_ ").lookback(),Ok(TestInstruction::Lookback { len: 1 }));
        assert_eq!(TestParser::new("_abc").lookback(),Ok(TestInstruction::Lookback { len: 1 }));
        assert_eq!(TestParser::new("_12abc").lookback(),Ok(TestInstruction::Lookback { len: 12 }));
        assert_eq!(TestParser::new(" _ ").lookback(), Err(ParseError::MultiTestExpected));
    }

}
