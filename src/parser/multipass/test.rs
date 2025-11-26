//! A Parser for the test operand of context and multipass opcodes
use std::{collections::HashSet, iter::Peekable, str::Chars};

use crate::parser::multipass::{ConversionError, IsLiteral, ParseError};

use super::braille::{self, BrailleChars, braille_chars, is_braille_dot};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Test {
    at_beginning: bool,
    at_end: bool,
    tests: Vec<Instruction>,
}

impl Test {
    pub fn new(at_beginning: bool, at_end: bool, tests: Vec<Instruction>) -> Self {
        Self {
            at_beginning,
            at_end,
            tests,
        }
    }
}

impl IsLiteral for Test {
    fn is_literal(&self) -> bool {
        if self.at_beginning || self.at_end {
            false
        } else {
            self.tests
                .iter()
                .all(|instruction| instruction.is_literal())
        }
    }
}

impl TryFrom<&Test> for String {
    type Error = ConversionError;

    fn try_from(test: &Test) -> Result<String, Self::Error> {
        if test.is_literal() {
            Ok(test
                .tests
                .iter()
                .map(|instruction| instruction.try_into().unwrap())
                .collect::<Vec<String>>()
                .join(""))
        } else {
            Err(ConversionError::TestNotLiteral)
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Attribute {
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Operator {
    Eq,
    Gt,
    Lt,
    GtEq,
    LtEq,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Quantifier {
    Number(u8),
    Range(u8, u8),
    Any,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Instruction {
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
        test: Box<Instruction>,
    },
    Replace {
        tests: Vec<Instruction>,
    },
}

impl IsLiteral for Instruction {
    fn is_literal(&self) -> bool {
        match self {
            Instruction::String { .. } => true,
            _ => false,
        }
    }
}

impl TryFrom<&Instruction> for String {
    type Error = ConversionError;

    fn try_from(instruction: &Instruction) -> Result<String, Self::Error> {
        match instruction {
            Instruction::String { s } => Ok(s.clone()),
            _ => Err(ConversionError::TestNotLiteral),
        }
    }
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

/// A Parser for the test operand of context and multipass opcodes
pub struct Parser<'a> {
    chars: Peekable<Chars<'a>>,
}

impl<'a> Parser<'a> {
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
                expected,
                found: Some(c),
            }),
            _ => Err(ParseError::CharExpected {
                expected,
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

    fn class(&mut self) -> Result<Instruction, ParseError> {
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
            Ok(Instruction::Class {
                name,
                quantifier: self.maybe_quantifier()?,
            })
        }
    }

    fn dots(&mut self) -> Result<Instruction, ParseError> {
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
            Ok(Instruction::Dots {
                dots: braille_chars(&dots)?,
            })
        }
    }

    fn handle_escape_sequence(&mut self, c: char) -> Result<char, ParseError> {
        if c != '\\' {
            Ok(c)
        } else {
            // handle \" and \s
            match self.chars.peek() {
                Some('s') => {
                    self.chars.next();
                    Ok(' ')
                }
                Some('"') => {
                    self.chars.next();
                    Ok('"')
                }
                // pass through any other escape chars
                Some(_) => Ok(c),
                _ => Err(ParseError::InvalidEscapeSequence { found: None }),
            }
        }
    }

    fn string(&mut self) -> Result<Instruction, ParseError> {
        self.consume('"')?;
        let mut s = String::new();
        while self.chars.peek().filter(|&c| *c != '"').is_some() {
            let raw = self.chars.next().unwrap();
            let c = self.handle_escape_sequence(raw)?;
            s.push(c);
        }
        self.consume('"')?;
        Ok(Instruction::String { s })
    }

    fn lookback(&mut self) -> Result<Instruction, ParseError> {
        self.consume('_')?;
        let n = match self.chars.peek() {
            Some(c) if c.is_ascii_digit() => self.ascii_number()?,
            _ => 1,
        };
        Ok(Instruction::Lookback { len: n })
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

    fn attributes(&mut self) -> Result<Instruction, ParseError> {
        self.consume('$')?;
        let mut attrs: HashSet<Attribute> = HashSet::new();
        while self.chars.peek().filter(|&c| is_attribute(c)).is_some() {
            attrs.insert(self.attribute()?);
        }
        if attrs.is_empty() {
            Err(ParseError::InvalidAttribute { found: None })
        } else {
            Ok(Instruction::Attributes {
                attrs,
                quantifier: self.maybe_quantifier()?,
            })
        }
    }

    fn replacement(&mut self) -> Result<Instruction, ParseError> {
        self.consume('[')?;
        let tests = self.many_tests()?;
        self.consume(']')?;
        Ok(Instruction::Replace { tests })
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

    fn variable(&mut self) -> Result<Instruction, ParseError> {
        self.consume('#')?;
        Ok(Instruction::Variable {
            var: self
                .ascii_number()
                .map_err(|_| ParseError::InvalidVariableName)?,
            op: self.operator()?,
            operand: self.ascii_number()?,
        })
    }

    fn negate(&mut self) -> Result<Instruction, ParseError> {
        self.consume('!')?;
        let test = self.test()?;
        Ok(Instruction::Negate {
            test: Box::new(test),
        })
    }

    fn test(&mut self) -> Result<Instruction, ParseError> {
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

    fn many_tests(&mut self) -> Result<Vec<Instruction>, ParseError> {
        let mut tests: Vec<Instruction> = Vec::new();
        while self.chars.peek().filter(|&c| is_test(c)).is_some() {
            tests.push(self.test()?);
        }
        Ok(tests)
    }

    /// Parse the test operand.
    pub fn tests(&mut self) -> Result<Test, ParseError> {
        let at_beginning = self.chars.next_if_eq(&'`').is_some();
        let tests = self.many_tests()?;
        let at_end = self.chars.next_if_eq(&'~').is_some();
        if tests.is_empty() {
            Err(ParseError::EmptyTest)
        } else {
            Ok(Test {
                at_beginning,
                at_end,
                tests,
            })
        }
    }
}

/// `Display` implementation for [`Test`]
mod display {
    use super::*;
    use crate::parser::dots_to_unicode;

    impl std::fmt::Display for Test {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            if self.at_beginning {
                write!(f, "`")?;
            }

            for instruction in &self.tests {
                write!(f, "{}", instruction)?;
            }

            if self.at_end {
                write!(f, "~")?;
            }

            Ok(())
        }
    }

    impl std::fmt::Display for Attribute {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let attr = match self {
                Attribute::Digit => "d",
                Attribute::Litdigit => "D",
                Attribute::Letter => "l",
                Attribute::Sign => "S",
                Attribute::Space => "s",
                Attribute::Math => "m",
                Attribute::Punctuation => "punctuation",
                Attribute::Uppercase => "U",
                Attribute::Lowercase => "u",
                Attribute::Class1 => "w",
                Attribute::Class2 => "x",
                Attribute::Class3 => "y",
                Attribute::Class4 => "z",
                Attribute::Any => "a",
            };
            write!(f, "{}", attr)
        }
    }

    impl std::fmt::Display for Operator {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let op = match self {
                Operator::Eq => "=",
                Operator::Gt => ">",
                Operator::Lt => "<",
                Operator::GtEq => ">=",
                Operator::LtEq => "<=",
            };
            write!(f, "{}", op)
        }
    }

    impl std::fmt::Display for Quantifier {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Quantifier::Number(number) => write!(f, "{}", number),
                Quantifier::Range(from, to) => write!(f, "{}-{}", from, to),
                Quantifier::Any => write!(f, "."),
            }
        }
    }

    impl std::fmt::Display for Instruction {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Instruction::Lookback { len } => {
                    write!(f, "_{}", len)
                }
                Instruction::Variable { var, op, operand } => {
                    write!(f, "#{}{}{}", var, op, operand)
                }
                Instruction::String { s } => {
                    write!(f, "\"{}\"", s)
                }
                Instruction::Dots { dots } => {
                    write!(f, "@{}", dots_to_unicode(dots))
                }
                Instruction::Attributes { attrs, quantifier } => {
                    write!(f, "$")?;
                    for attr in attrs {
                        write!(f, "{}", attr)?;
                    }
                    if let Some(q) = quantifier {
                        write!(f, "{}", q)?;
                    }
                    Ok(())
                }
                Instruction::Class { name, quantifier } => {
                    write!(f, "%{}", name)?;
                    if let Some(q) = quantifier {
                        write!(f, "{}", q)?;
                    }
                    Ok(())
                }
                Instruction::Negate { test } => {
                    write!(f, "!{}", test)
                }
                Instruction::Replace { tests } => {
                    write!(f, "[")?;
                    for test in tests {
                        write!(f, "{}", test)?;
                    }
                    write!(f, "]")
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use enumset::enum_set;

    use super::braille::BrailleDot;
    use super::*;

    #[test]
    fn ascii_number() {
        assert_eq!(Parser::new("123").ascii_number(), Ok(123));
        assert_eq!(Parser::new("1").ascii_number(), Ok(1));
        assert_eq!(Parser::new("1 ").ascii_number(), Ok(1));
        assert_eq!(Parser::new("1abc").ascii_number(), Ok(1));
        assert_ne!(Parser::new(" 1").ascii_number(), Ok(1));
        assert_ne!(Parser::new("a1").ascii_number(), Ok(1));
    }

    #[test]
    fn dots() {
        assert_eq!(
            Parser::new("@123").dots(),
            Ok(Instruction::Dots {
                dots: vec![enum_set!(
                    BrailleDot::Dot1 | BrailleDot::Dot2 | BrailleDot::Dot3
                )]
            })
        );
        assert_eq!(
            Parser::new("@").dots(),
            Err(ParseError::InvalidBraille(
                braille::ParseError::InvalidBraille { character: None }
            ))
        );
        assert_eq!(
            Parser::new("@-").dots(),
            Err(ParseError::InvalidBraille(
                braille::ParseError::InvalidBraille { character: None }
            ))
        );
        assert_eq!(
            Parser::new("@1-2").dots(),
            Ok(Instruction::Dots {
                dots: vec![enum_set!(BrailleDot::Dot1), enum_set!(BrailleDot::Dot2)]
            })
        );
    }

    #[test]
    fn string() {
        assert_eq!(
            Parser::new(r#""test""#).string(),
            Ok(Instruction::String { s: "test".into() })
        );
        assert_eq!(
            Parser::new(r#"",_""#).string(),
            Ok(Instruction::String { s: ",_".into() })
        );
        assert_eq!(
            Parser::new(r#"", ""#).string(),
            Ok(Instruction::String { s: ", ".into() })
        );
        assert_eq!(
            Parser::new(r#""-\"""#).string(),
            Ok(Instruction::String { s: "-\"".into() })
        );
        assert_eq!(
            Parser::new(r#"".\s\"""#).string(),
            Ok(Instruction::String { s: ". \"".into() })
        );
    }

    #[test]
    fn lookback() {
        assert_eq!(
            Parser::new("_123").lookback(),
            Ok(Instruction::Lookback { len: 123 })
        );
        assert_eq!(
            Parser::new("_7").lookback(),
            Ok(Instruction::Lookback { len: 7 })
        );
        assert_eq!(
            Parser::new("_ ").lookback(),
            Ok(Instruction::Lookback { len: 1 })
        );
        assert_eq!(
            Parser::new("_abc").lookback(),
            Ok(Instruction::Lookback { len: 1 })
        );
        assert_eq!(
            Parser::new("_12abc").lookback(),
            Ok(Instruction::Lookback { len: 12 })
        );
        assert_eq!(
            Parser::new(" _ ").lookback(),
            Err(ParseError::CharExpected {
                expected: '_',
                found: Some(' ')
            })
        );
    }

    #[test]
    fn attributes() {
        assert_eq!(
            Parser::new("$a").attributes(),
            Ok(Instruction::Attributes {
                attrs: HashSet::from([Attribute::Any]),
                quantifier: None
            })
        );
        assert_eq!(
            Parser::new("$ay").attributes(),
            Ok(Instruction::Attributes {
                attrs: HashSet::from([Attribute::Any, Attribute::Class3]),
                quantifier: None
            })
        );
        assert_eq!(
            Parser::new("$").attributes(),
            Err(ParseError::InvalidAttribute { found: None })
        );
        assert_eq!(
            Parser::new("$h").attributes(),
            Err(ParseError::InvalidAttribute { found: None })
        );
    }

    #[test]
    fn class() {
        assert_eq!(
            Parser::new("%foo").class(),
            Ok(Instruction::Class {
                name: "foo".into(),
                quantifier: None
            })
        );
        assert_eq!(
            Parser::new("%3").class(),
            Ok(Instruction::Class {
                name: "3".into(),
                quantifier: None
            })
        );
        assert_eq!(Parser::new("%").class(), Err(ParseError::InvalidClass));
        assert_eq!(Parser::new("% ").class(), Err(ParseError::InvalidClass));
        assert_eq!(Parser::new("%🐂").class(), Err(ParseError::InvalidClass));
        assert_eq!(Parser::new("%8").class(), Err(ParseError::InvalidClass));
    }

    #[test]
    fn quantifier() {
        assert_eq!(
            Parser::new("$a ").attributes(),
            Ok(Instruction::Attributes {
                attrs: HashSet::from([Attribute::Any]),
                quantifier: None
            })
        );
        assert_eq!(
            Parser::new("$a.").attributes(),
            Ok(Instruction::Attributes {
                attrs: HashSet::from([Attribute::Any]),
                quantifier: Some(Quantifier::Any)
            })
        );
        assert_eq!(
            Parser::new("$a3").attributes(),
            Ok(Instruction::Attributes {
                attrs: HashSet::from([Attribute::Any]),
                quantifier: Some(Quantifier::Number(3))
            })
        );
        assert_eq!(
            Parser::new("$a3-5").attributes(),
            Ok(Instruction::Attributes {
                attrs: HashSet::from([Attribute::Any]),
                quantifier: Some(Quantifier::Range(3, 5))
            })
        );
        assert_eq!(
            Parser::new("$a1-").attributes(),
            Err(ParseError::InvalidQuantifier)
        );
        assert_eq!(
            Parser::new("$a1-a").attributes(),
            Err(ParseError::InvalidQuantifier)
        );
    }

    #[test]
    fn variable() {
        assert_eq!(
            Parser::new("#1<2").variable(),
            Ok(Instruction::Variable {
                var: 1,
                op: Operator::Lt,
                operand: 2
            })
        );
        assert_eq!(
            Parser::new("#1<=2").variable(),
            Ok(Instruction::Variable {
                var: 1,
                op: Operator::LtEq,
                operand: 2
            })
        );
        assert_eq!(
            Parser::new("#1=3").variable(),
            Ok(Instruction::Variable {
                var: 1,
                op: Operator::Eq,
                operand: 3
            })
        );
        assert_eq!(
            Parser::new("#a=3").variable(),
            Err(ParseError::InvalidVariableName)
        );
        assert_ne!(
            Parser::new("#3=a").variable(),
            Ok(Instruction::Variable {
                var: 1,
                op: Operator::Eq,
                operand: 3
            })
        );
        assert_eq!(
            Parser::new("#").variable(),
            Err(ParseError::InvalidVariableName)
        );
        assert_eq!(
            Parser::new("#3").variable(),
            Err(ParseError::InvalidOperator { found: None }),
        );
        assert_ne!(
            Parser::new("#3=").variable(),
            //Err(ParseError::InvalidNumber(ParseIntError { kind: Empty })),
            Ok(Instruction::Variable {
                var: 1,
                op: Operator::Eq,
                operand: 3
            })
        );
    }

    #[test]
    fn replacement() {
        assert_eq!(
            Parser::new("[$d]").replacement(),
            Ok(Instruction::Replace {
                tests: vec![Instruction::Attributes {
                    attrs: HashSet::from([Attribute::Digit]),
                    quantifier: None
                }]
            })
        );
        assert_eq!(
            Parser::new("[%foo]").replacement(),
            Ok(Instruction::Replace {
                tests: vec![Instruction::Class {
                    name: "foo".into(),
                    quantifier: None
                }]
            })
        );
    }

    #[test]
    fn test() {
        assert_eq!(
            Parser::new("$d[\"hello\"]%digitletter").tests(),
            Ok(Test {
                at_beginning: false,
                at_end: false,
                tests: vec![
                    Instruction::Attributes {
                        attrs: HashSet::from([Attribute::Digit]),
                        quantifier: None
                    },
                    Instruction::Replace {
                        tests: vec![Instruction::String { s: "hello".into() }]
                    },
                    Instruction::Class {
                        name: "digitletter".into(),
                        quantifier: None
                    }
                ]
            })
        );
    }
}
