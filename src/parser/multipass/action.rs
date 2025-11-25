use std::{iter::Peekable, str::Chars};

use crate::parser::multipass::ConversionError;
use crate::parser::multipass::IsLiteral;
use crate::parser::multipass::ParseError;

use super::braille::{self, BrailleChars, braille_chars, is_braille_dot};

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

    fn try_from(test: &Action) -> Result<String, Self::Error> {
        if test.is_literal() {
            Ok(test
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
    Replace,
    Ignore,
}

impl IsLiteral for Instruction {
    fn is_literal(&self) -> bool {
        match self {
            &Instruction::String { .. } => true,
            _ => false,
        }
    }
}

impl TryFrom<&Instruction> for String {
    type Error = ConversionError;

    fn try_from(instruction: &Instruction) -> Result<String, Self::Error> {
        match instruction {
            Instruction::String { s } => Ok(s.clone()),
            _ => Err(ConversionError::ActionNotLiteral),
        }
    }
}

fn is_action(c: &char) -> bool {
    matches!(c, '@' | '"' | '*' | '?')
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

    fn action(&mut self) -> Result<Instruction, ParseError> {
        match self.chars.peek() {
            Some('@') => Ok(self.dots()?),
            Some('"') => Ok(self.string()?),
            Some('*') => Ok(self.replace()?),
            Some('?') => Ok(self.ignore()?),
            Some(c) => Err(ParseError::InvalidAction { found: Some(*c) }),
            _ => Err(ParseError::InvalidTest { found: None }),
        }
    }

    fn many_actions(&mut self) -> Result<Vec<Instruction>, ParseError> {
        let mut tests: Vec<Instruction> = Vec::new();
        while self.chars.peek().filter(|&c| is_action(c)).is_some() {
            tests.push(self.action()?);
        }
        Ok(tests)
    }

    pub fn actions(&mut self) -> Result<Action, ParseError> {
        let actions = self.many_actions()?;
        Ok(Action { actions })
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
