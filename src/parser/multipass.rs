use std::{collections::HashSet, iter::Peekable, num::ParseIntError, str::Chars};

use super::braille::{self, braille_chars, is_braille_dot, BrailleChars, BrailleDot};

#[derive(thiserror::Error, Debug, PartialEq)]
pub enum ParseError {
    #[error("Expected {expected:?}, got {found:?}")]
    CharExpected { expected: char, found: Option<char> },
    #[error("Invalid number")]
    InvalidNumber(#[from] ParseIntError),
    #[error("Invalid braille")]
    InvalidBraille(#[from] braille::ParseError),
    #[error("Invalid test")]
    InvalidTest { found: Option<char> },
    #[error("Invalid class name")]
    InvalidClass,
    #[error("Invalid attribute")]
    InvalidAttribute { found: Option<char> },
    #[error("Invalid variable name")]
    InvalidVariableName,
    #[error("Invalid quantifier, expected a number, a range or a dot")]
    InvalidQuantifier,
    #[error("Invalid operator")]
    InvalidOperator { found: Option<char> },
}

#[derive(Debug, PartialEq, Eq)]
pub struct Test {
    at_beginning: bool,
    at_end: bool,
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
    Eq,
    Gt,
    Lt,
    GtEq,
    LtEq,
}

#[derive(Debug, PartialEq, Eq)]
enum Quantifier {
    Number(u8),
    Range(u8, u8),
    Any,
}

#[derive(Debug, PartialEq, Eq)]
enum TestInstruction {
    Lookback {
        len: u8,
    },
    Variable {
        var: u8,
        op: Operator,
        operand: u8,
    },
    String {
        s: String,
    },
    Dots {
        dots: BrailleChars,
    },
    Attributes {
        attrs: HashSet<Attribute>,
        quantifier: Option<Quantifier>,
    },
    Class {
        name: String,
        quantifier: Option<Quantifier>,
    },
    Negate {
        test: Box<TestInstruction>,
    },
    Replace {
        tests: Vec<TestInstruction>,
    },
}

fn is_attribute(c: &char) -> bool {
    matches!(
        c,
        'a' | 'd' | 'D' | 'l' | 'm' | 'p' | 'S' | 's' | 'U' | 'u' | 'w' | 'x' | 'y' | 'z'
    )
}

fn is_test(c: &char) -> bool {
    matches!(c, '_' | '%' | '@' | '"' | '$' | '[' | '#' | '!')
}

fn is_class_digit(c: &char) -> bool {
    matches!(c, '1'..='7')
}

pub struct TestParser<'a> {
    chars: Peekable<Chars<'a>>,
}

impl<'a> TestParser<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            chars: source.chars().peekable(),
        }
    }

    fn ascii_number(&mut self) -> Result<u8, ParseError> {
        let mut s = String::new();
        while self.chars.peek().filter(|c| c.is_ascii_digit()).is_some() {
            s.push(self.chars.next().unwrap());
        }
        let number = s.parse::<u8>()?;
        Ok(number)
    }

    fn consume(&mut self, expected: char) -> Result<(), ParseError> {
        match self.chars.next() {
            Some(e) if e == expected => Ok(()),
            Some(c) => Err(ParseError::CharExpected {
                expected: expected,
                found: Some(c),
            }),
            _ => Err(ParseError::CharExpected {
                expected: expected,
                found: None,
            }),
        }
    }

    fn maybe_quantifier(&mut self) -> Result<Option<Quantifier>, ParseError> {
        match self.chars.peek() {
            Some('.') => {
                self.chars.next();
                Ok(Some(Quantifier::Any))
            }
            Some(c) if c.is_ascii_digit() => {
                let min = self.ascii_number()?;
                if self.chars.next_if_eq(&'-').is_some() {
                    let max = match self.ascii_number() {
                        Ok(n) => n,
                        _ => {
                            return Err(ParseError::InvalidQuantifier);
                        }
                    };
                    Ok(Some(Quantifier::Range(min, max)))
                } else {
                    Ok(Some(Quantifier::Number(min)))
                }
            }
            _ => Ok(None),
        }
    }

    fn class(&mut self) -> Result<TestInstruction, ParseError> {
        self.consume('%')?;
        let mut name = String::new();
        while self.chars.peek().filter(|c| c.is_alphabetic()).is_some() {
            name.push(self.chars.next().unwrap());
        }
        if self.chars.peek().filter(|c| is_class_digit(c)).is_some() {
            name.push(self.chars.next().unwrap());
        }
        if name.is_empty() {
            Err(ParseError::InvalidClass)
        } else {
            Ok(TestInstruction::Class {
                name,
                quantifier: self.maybe_quantifier()?,
            })
        }
    }

    fn dots(&mut self) -> Result<TestInstruction, ParseError> {
        self.consume('@')?;
        let mut dots = String::new();
        while self
            .chars
            .peek()
            .filter(|c| is_braille_dot(**c) || **c == '-')
            .is_some()
        {
            dots.push(self.chars.next().unwrap());
        }
        if dots.is_empty() {
            Err(ParseError::InvalidBraille(
                braille::ParseError::InvalidBraille { character: None },
            ))
        } else {
            Ok(TestInstruction::Dots {
                dots: braille_chars(&dots)?,
            })
        }
    }

    fn string(&mut self) -> Result<TestInstruction, ParseError> {
        self.consume('"')?;
        let mut s = String::new();
        while self.chars.peek().filter(|&c| *c != '"').is_some() {
            s.push(self.chars.next().unwrap());
        }
        self.consume('"')?;
        Ok(TestInstruction::String { s })
    }

    fn lookback(&mut self) -> Result<TestInstruction, ParseError> {
        self.consume('_')?;
        let n = match self.chars.peek() {
            Some(c) if c.is_ascii_digit() => self.ascii_number()?,
            _ => 1,
        };
        Ok(TestInstruction::Lookback { len: n })
    }

    fn attribute(&mut self) -> Result<Attribute, ParseError> {
        match self.chars.next() {
            Some('a') => Ok(Attribute::Any),
            Some('d') => Ok(Attribute::Digit),
            Some('D') => Ok(Attribute::Litdigit),
            Some('l') => Ok(Attribute::Letter),
            Some('m') => Ok(Attribute::Math),
            Some('p') => Ok(Attribute::Punctuation),
            Some('S') => Ok(Attribute::Sign),
            Some('s') => Ok(Attribute::Space),
            Some('U') => Ok(Attribute::Uppercase),
            Some('u') => Ok(Attribute::Lowercase),
            Some('w') => Ok(Attribute::Class1),
            Some('x') => Ok(Attribute::Class2),
            Some('y') => Ok(Attribute::Class3),
            Some('z') => Ok(Attribute::Class4),
            Some(c) => Err(ParseError::InvalidAttribute { found: Some(c) }),
            _ => Err(ParseError::InvalidAttribute { found: None }),
        }
    }

    fn attributes(&mut self) -> Result<TestInstruction, ParseError> {
        self.consume('$')?;
        let mut attrs: HashSet<Attribute> = HashSet::new();
        while self.chars.peek().filter(|&c| is_attribute(c)).is_some() {
            attrs.insert(self.attribute()?);
        }
        if attrs.is_empty() {
            Err(ParseError::InvalidAttribute { found: None })
        } else {
            Ok(TestInstruction::Attributes {
                attrs,
                quantifier: self.maybe_quantifier()?,
            })
        }
    }

    fn replacement(&mut self) -> Result<TestInstruction, ParseError> {
        self.consume('[')?;
        let tests = self.many_tests()?;
        self.consume(']')?;
        Ok(TestInstruction::Replace { tests })
    }

    fn operator(&mut self) -> Result<Operator, ParseError> {
        match self.chars.next() {
            Some('=') => Ok(Operator::Eq),
            Some('<') => match self.chars.peek() {
                Some('=') => {
                    self.chars.next();
                    Ok(Operator::LtEq)
                }
                Some(_) => Ok(Operator::Lt),
                _ => Err(ParseError::InvalidOperator { found: None }),
            },
            Some('>') => match self.chars.peek() {
                Some('=') => {
                    self.chars.next();
                    Ok(Operator::GtEq)
                }
                Some(_) => Ok(Operator::Gt),
                _ => Err(ParseError::InvalidOperator { found: None }),
            },
            Some(c) => Err(ParseError::InvalidOperator { found: Some(c) }),
            _ => Err(ParseError::InvalidOperator { found: None }),
        }
    }

    fn variable(&mut self) -> Result<TestInstruction, ParseError> {
        self.consume('#')?;
        Ok(TestInstruction::Variable {
            var: self
                .ascii_number()
                .map_err(|_| ParseError::InvalidVariableName)?,
            op: self.operator()?,
            operand: self.ascii_number()?,
        })
    }

    fn negate(&mut self) -> Result<TestInstruction, ParseError> {
        self.consume('!')?;
        let test = self.test()?;
        Ok(TestInstruction::Negate {
            test: Box::new(test),
        })
    }

    fn test(&mut self) -> Result<TestInstruction, ParseError> {
        match self.chars.peek() {
            Some('_') => Ok(self.lookback()?),
            Some('%') => Ok(self.class()?),
            Some('@') => Ok(self.dots()?),
            Some('"') => Ok(self.string()?),
            Some('$') => Ok(self.attributes()?),
            Some('[') => Ok(self.replacement()?),
            Some('#') => Ok(self.variable()?),
            Some('!') => Ok(self.negate()?),
            Some(c) => Err(ParseError::InvalidTest { found: Some(*c) }),
            _ => Err(ParseError::InvalidTest { found: None }),
        }
    }

    fn many_tests(&mut self) -> Result<Vec<TestInstruction>, ParseError> {
        let mut tests: Vec<TestInstruction> = Vec::new();
        while self.chars.peek().filter(|&c| is_test(c)).is_some() {
            tests.push(self.test()?);
        }
        Ok(tests)
    }

    pub fn tests(&mut self) -> Result<Test, ParseError> {
        let at_beginning = self.chars.next_if_eq(&'`').is_some();
        let tests = self.many_tests()?;
        let at_end = self.chars.next_if_eq(&'~').is_some();
        Ok(Test {
            at_beginning,
            at_end,
            tests,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ascii_number_test() {
        assert_eq!(TestParser::new("123").ascii_number(), Ok(123));
        assert_eq!(TestParser::new("1").ascii_number(), Ok(1));
        assert_eq!(TestParser::new("1 ").ascii_number(), Ok(1));
        assert_eq!(TestParser::new("1abc").ascii_number(), Ok(1));
        assert_ne!(TestParser::new(" 1").ascii_number(), Ok(1));
        assert_ne!(TestParser::new("a1").ascii_number(), Ok(1));
    }

    #[test]
    fn dots_test() {
        assert_eq!(
            TestParser::new("@123").dots(),
            Ok(TestInstruction::Dots {
                dots: vec![HashSet::from([
                    BrailleDot::Dot1,
                    BrailleDot::Dot2,
                    BrailleDot::Dot3
                ])]
            })
        );
        assert_eq!(
            TestParser::new("@").dots(),
            Err(ParseError::InvalidBraille(
                braille::ParseError::InvalidBraille { character: None }
            ))
        );
        assert_eq!(
            TestParser::new("@-").dots(),
            Err(ParseError::InvalidBraille(
                braille::ParseError::InvalidBraille { character: None }
            ))
        );
        assert_eq!(
            TestParser::new("@1-2").dots(),
            Ok(TestInstruction::Dots {
                dots: vec![
                    HashSet::from([BrailleDot::Dot1]),
                    HashSet::from([BrailleDot::Dot2])
                ]
            })
        );
    }

    #[test]
    fn look_back_test() {
        assert_eq!(
            TestParser::new("_123").lookback(),
            Ok(TestInstruction::Lookback { len: 123 })
        );
        assert_eq!(
            TestParser::new("_7").lookback(),
            Ok(TestInstruction::Lookback { len: 7 })
        );
        assert_eq!(
            TestParser::new("_ ").lookback(),
            Ok(TestInstruction::Lookback { len: 1 })
        );
        assert_eq!(
            TestParser::new("_abc").lookback(),
            Ok(TestInstruction::Lookback { len: 1 })
        );
        assert_eq!(
            TestParser::new("_12abc").lookback(),
            Ok(TestInstruction::Lookback { len: 12 })
        );
        assert_eq!(
            TestParser::new(" _ ").lookback(),
            Err(ParseError::CharExpected {
                expected: '_',
                found: Some(' ')
            })
        );
    }

    #[test]
    fn attributes_test() {
        assert_eq!(
            TestParser::new("$a").attributes(),
            Ok(TestInstruction::Attributes {
                attrs: HashSet::from([Attribute::Any]),
                quantifier: None
            })
        );
        assert_eq!(
            TestParser::new("$ay").attributes(),
            Ok(TestInstruction::Attributes {
                attrs: HashSet::from([Attribute::Any, Attribute::Class3]),
                quantifier: None
            })
        );
        assert_eq!(
            TestParser::new("$").attributes(),
            Err(ParseError::InvalidAttribute { found: None })
        );
        assert_eq!(
            TestParser::new("$h").attributes(),
            Err(ParseError::InvalidAttribute { found: None })
        );
    }

    #[test]
    fn class_test() {
        assert_eq!(
            TestParser::new("%foo").class(),
            Ok(TestInstruction::Class {
                name: "foo".into(),
                quantifier: None
            })
        );
        assert_eq!(
            TestParser::new("%3").class(),
            Ok(TestInstruction::Class {
                name: "3".into(),
                quantifier: None
            })
        );
        assert_eq!(TestParser::new("%").class(), Err(ParseError::InvalidClass));
        assert_eq!(TestParser::new("% ").class(), Err(ParseError::InvalidClass));
        assert_eq!(
            TestParser::new("%🐂").class(),
            Err(ParseError::InvalidClass)
        );
        assert_eq!(TestParser::new("%8").class(), Err(ParseError::InvalidClass));
    }

    #[test]
    fn quantifier_test() {
        assert_eq!(
            TestParser::new("$a ").attributes(),
            Ok(TestInstruction::Attributes {
                attrs: HashSet::from([Attribute::Any]),
                quantifier: None
            })
        );
        assert_eq!(
            TestParser::new("$a.").attributes(),
            Ok(TestInstruction::Attributes {
                attrs: HashSet::from([Attribute::Any]),
                quantifier: Some(Quantifier::Any)
            })
        );
        assert_eq!(
            TestParser::new("$a3").attributes(),
            Ok(TestInstruction::Attributes {
                attrs: HashSet::from([Attribute::Any]),
                quantifier: Some(Quantifier::Number(3))
            })
        );
        assert_eq!(
            TestParser::new("$a3-5").attributes(),
            Ok(TestInstruction::Attributes {
                attrs: HashSet::from([Attribute::Any]),
                quantifier: Some(Quantifier::Range(3, 5))
            })
        );
        assert_eq!(
            TestParser::new("$a1-").attributes(),
            Err(ParseError::InvalidQuantifier)
        );
        assert_eq!(
            TestParser::new("$a1-a").attributes(),
            Err(ParseError::InvalidQuantifier)
        );
    }

    #[test]
    fn variable_test() {
        assert_eq!(
            TestParser::new("#1<2").variable(),
            Ok(TestInstruction::Variable {
                var: 1,
                op: Operator::Lt,
                operand: 2
            })
        );
        assert_eq!(
            TestParser::new("#1<=2").variable(),
            Ok(TestInstruction::Variable {
                var: 1,
                op: Operator::LtEq,
                operand: 2
            })
        );
        assert_eq!(
            TestParser::new("#1=3").variable(),
            Ok(TestInstruction::Variable {
                var: 1,
                op: Operator::Eq,
                operand: 3
            })
        );
        assert_eq!(
            TestParser::new("#a=3").variable(),
            Err(ParseError::InvalidVariableName)
        );
        assert_ne!(
            TestParser::new("#3=a").variable(),
            Ok(TestInstruction::Variable {
                var: 1,
                op: Operator::Eq,
                operand: 3
            })
        );
        assert_eq!(
            TestParser::new("#").variable(),
            Err(ParseError::InvalidVariableName)
        );
        assert_eq!(
            TestParser::new("#3").variable(),
            Err(ParseError::InvalidOperator { found: None }),
        );
        assert_ne!(
            TestParser::new("#3=").variable(),
            //Err(ParseError::InvalidNumber(ParseIntError { kind: Empty })),
            Ok(TestInstruction::Variable {
                var: 1,
                op: Operator::Eq,
                operand: 3
            })
        );
    }

    #[test]
    fn replacement_test() {
        assert_eq!(
            TestParser::new("[]").replacement(),
            Ok(TestInstruction::Replace { tests: vec![] })
        );
        assert_eq!(
            TestParser::new("[%foo]").replacement(),
            Ok(TestInstruction::Replace {
                tests: vec![TestInstruction::Class {
                    name: "foo".into(),
                    quantifier: None
                }]
            })
        );
    }

    #[test]
    fn test_test() {
        assert_eq!(
            TestParser::new("$d[]%digitletter").tests(),
            Ok(Test {
                at_beginning: false,
                at_end: false,
                tests: vec![
                    TestInstruction::Attributes {
                        attrs: HashSet::from([Attribute::Digit]),
                        quantifier: None
                    },
                    TestInstruction::Replace { tests: vec![] },
                    TestInstruction::Class {
                        name: "digitletter".into(),
                        quantifier: None
                    }
                ]
            })
        );
    }
}
