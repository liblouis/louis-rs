//! A Parser for the action operand of context and multipass opcodes
use std::{iter::Peekable, str::Chars};

use super::braille::{self, BrailleChars, braille_chars, is_braille_dot};
use crate::parser::multipass::ConversionError;
use crate::parser::multipass::IsLiteral;
use crate::parser::multipass::ParseError;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Action {
    actions: Vec<Instruction>,
}

impl Action {
    pub fn new(actions: Vec<Instruction>) -> Self {
        Self { actions }
    }
}

impl IsLiteral for Action {
    fn is_literal(&self) -> bool {
        self.actions
            .iter()
            .all(|instruction| instruction.is_literal())
    }
}

impl TryFrom<&Action> for String {
    type Error = ConversionError;

    fn try_from(action: &Action) -> Result<String, Self::Error> {
        if action.is_literal() {
            Ok(action
                .actions
                .iter()
                .map(|instruction| instruction.try_into().unwrap())
                .collect::<Vec<String>>()
                .join(""))
        } else {
            Err(ConversionError::ActionNotLiteral)
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Instruction {
    String { s: String },
    Dots { dots: BrailleChars },
    // FIXME: This seems weird. What is the meaning of a class in the action part of a context rule?
    Class { name: String },
    Assignment { variable: u8, value: u8 },
    Increment { variable: u8 },
    Decrement { variable: u8 },
    Replace,
    Ignore,
}

impl IsLiteral for Instruction {
    fn is_literal(&self) -> bool {
        match self {
            Instruction::String { .. } => true,
            Instruction::Ignore => true,
            _ => false,
        }
    }
}

impl TryFrom<&Instruction> for String {
    type Error = ConversionError;

    fn try_from(instruction: &Instruction) -> Result<String, Self::Error> {
        match instruction {
            Instruction::String { s } => Ok(s.clone()),
            Instruction::Ignore => Ok("".to_string()),
            _ => Err(ConversionError::ActionNotLiteral),
        }
    }
}

fn is_action(c: &char) -> bool {
    matches!(c, '@' | '"' | '%' | '#' | '*' | '?')
}

pub struct Parser<'a> {
    chars: Peekable<Chars<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            chars: source.chars().peekable(),
        }
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

    fn replace(&mut self) -> Result<Instruction, ParseError> {
        self.consume('*')?;
        Ok(Instruction::Replace)
    }

    fn ignore(&mut self) -> Result<Instruction, ParseError> {
        self.consume('?')?;
        Ok(Instruction::Ignore)
    }

    fn ascii_number(&mut self) -> Result<u8, ParseError> {
        let mut s = String::new();
        while self.chars.peek().filter(|c| c.is_ascii_digit()).is_some() {
            s.push(self.chars.next().unwrap());
        }
        let number = s.parse::<u8>()?;
        Ok(number)
    }

    fn variable(&mut self) -> Result<Instruction, ParseError> {
        self.consume('#')?;
        let variable = self
            .ascii_number()
            .map_err(|_| ParseError::InvalidVariableName)?;
        match self.chars.peek() {
            Some('=') => {
                self.consume('=')?;
                Ok(Instruction::Assignment {
                    variable,
                    value: self.ascii_number()?,
                })
            }
            Some('+') => Ok(Instruction::Increment { variable }),
            Some('-') => Ok(Instruction::Decrement { variable }),
            c => Err(ParseError::InvalidOperator { found: c.copied() }),
        }
    }

    fn class(&mut self) -> Result<Instruction, ParseError> {
        self.consume('%')?;
        let mut name = String::new();
        while self
            .chars
            .peek()
            .filter(|c| c.is_ascii_alphabetic())
            .is_some()
        {
            name.push(self.chars.next().unwrap());
        }
        if name.is_empty() {
            Err(ParseError::InvalidClass)
        } else {
            Ok(Instruction::Class { name })
        }
    }

    fn action(&mut self) -> Result<Instruction, ParseError> {
        match self.chars.peek() {
            Some('@') => Ok(self.dots()?),
            Some('"') => Ok(self.string()?),
            Some('*') => Ok(self.replace()?),
            Some('?') => Ok(self.ignore()?),
            Some('#') => Ok(self.variable()?),
            Some('%') => Ok(self.class()?),
            Some(c) => Err(ParseError::InvalidAction { found: Some(*c) }),
            _ => Err(ParseError::InvalidAction { found: None }),
        }
    }

    fn many_actions(&mut self) -> Result<Vec<Instruction>, ParseError> {
        let mut actions: Vec<Instruction> = Vec::new();
        while self.chars.peek().filter(|&c| is_action(c)).is_some() {
            actions.push(self.action()?);
        }
        if actions.is_empty() {
            Err(ParseError::EmptyAction)
        } else {
            Ok(actions)
        }
    }

    pub fn actions(&mut self) -> Result<Action, ParseError> {
        let actions = self.many_actions()?;
        Ok(Action { actions })
    }
}

/// `Display` implementation for [`Action`]
mod display {
    use super::*;
    use crate::parser::dots_to_unicode;

    impl std::fmt::Display for Action {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            for instruction in &self.actions {
                write!(f, "{}", instruction)?;
            }
            Ok(())
        }
    }

    impl std::fmt::Display for Instruction {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Instruction::String { s } => write!(f, "{}", s),
                Instruction::Dots { dots } => write!(f, "{}", dots_to_unicode(dots)),
                Instruction::Replace => write!(f, "*"),
                Instruction::Ignore => write!(f, "?"),
                Instruction::Class { name } => write!(f, "%{}", name),
                Instruction::Assignment { variable, value } => write!(f, "#{}={}", variable, value),
                Instruction::Increment { variable } => write!(f, "#{}+", variable),
                Instruction::Decrement { variable } => write!(f, "#{}-", variable),
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
    fn replace() {
        assert_eq!(Parser::new("*").replace(), Ok(Instruction::Replace));
    }

    #[test]
    fn ignore() {
        assert_eq!(Parser::new("?").ignore(), Ok(Instruction::Ignore));
    }

    #[test]
    fn variable() {
        assert_eq!(
            Parser::new("#1=42").action(),
            Ok(Instruction::Assignment {
                variable: 1,
                value: 42
            })
        );
        assert_eq!(
            Parser::new("#42=1").action(),
            Ok(Instruction::Assignment {
                variable: 42,
                value: 1
            })
        );
        assert_eq!(
            Parser::new("#5+").action(),
            Ok(Instruction::Increment { variable: 5 })
        );
        assert_eq!(
            Parser::new("#5-").action(),
            Ok(Instruction::Decrement { variable: 5 })
        );
        assert_ne!(
            Parser::new("#1=").action(),
            //Err(ParseError::InvalidNumber(ParseIntError { kind: Empty })),
            Ok(Instruction::Assignment {
                variable: 1,
                value: 42
            })
        );
        assert_eq!(
            Parser::new("#5<").action(),
            Err(ParseError::InvalidOperator { found: Some('<') }),
        );
        assert_eq!(
            Parser::new("#a=1").action(),
            Err(ParseError::InvalidVariableName),
        );
    }

    #[test]
    fn class() {
        assert_eq!(
            Parser::new("%foo").action(),
            Ok(Instruction::Class { name: "foo".into() })
        );
        assert_ne!(
            Parser::new("%hallöchen").action(),
            Ok(Instruction::Class {
                name: "hallöchen".into()
            })
        );
        assert_eq!(Parser::new("%京").action(), Err(ParseError::InvalidClass));
        assert_eq!(Parser::new("%42").action(), Err(ParseError::InvalidClass));
    }

    #[test]
    fn action() {
        assert_eq!(
            Parser::new(r#"?"haha""#).actions(),
            Ok(Action {
                actions: vec![
                    Instruction::Ignore,
                    Instruction::String {
                        s: "haha".to_string()
                    }
                ]
            })
        );
    }
}
