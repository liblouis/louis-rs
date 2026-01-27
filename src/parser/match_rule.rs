use crate::parser::{Attribute, CharacterClass};
use std::{collections::HashSet, iter::Peekable, str::Chars};

#[derive(thiserror::Error, Debug, PartialEq)]
pub enum ParseError {
    #[error("Expected {expected:?}, got {found:?}")]
    CharExpected { expected: char, found: Option<char> },
    #[error("Invalid attribute {0:?}")]
    InvalidAttribute(Option<char>),
    #[error("Quantifier '{0}' not allowed without pattern")]
    MissingPatternBeforeQuantifier(char),
    #[error("Either not allowed without pattern")]
    MissingPatternBeforeEither,
    #[error("Pattern cannot be empty")]
    EmptyPattern,
    #[error("Group cannot be empty")]
    EmptyGroup,
    #[error("invalid escape sequence")]
    InvalidEscape,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Pattern {
    Empty,
    Characters(String),
    Boundary,
    Any,
    Set(HashSet<char>),
    Attributes(HashSet<Attribute>),
    Group(Patterns),
    Negate(Box<Pattern>),
    Optional(Box<Pattern>),
    ZeroOrMore(Box<Pattern>),
    OneOrMore(Box<Pattern>),
    Either(Box<Pattern>, Box<Pattern>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Patterns(Vec<Pattern>);

impl std::ops::Deref for Patterns {
    type Target = Vec<Pattern>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for Patterns {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl std::fmt::Display for Patterns {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for pattern in &self.0 {
            write!(f, "{}", pattern)?;
        }
        Ok(())
    }
}

impl std::fmt::Display for Pattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Pattern::Empty => write!(f, "-"),
            Pattern::Characters(s) => write!(f, "{}", s),
            Pattern::Boundary => todo!(),
            Pattern::Any => write!(f, "."),
            Pattern::Set(chars) => {
                write!(f, "[")?;
                for c in chars {
                    write!(f, "{}", c)?;
                }
                write!(f, "]")?;
                Ok(())
            }
            Pattern::Attributes(attributes) => {
                for attr in attributes {
                    write!(f, "{}", attr)?;
                }
                Ok(())
            }
            Pattern::Group(patterns) => write!(f, "({})", patterns),
            Pattern::Negate(pattern) => write!(f, "!{}", pattern),
            Pattern::Optional(pattern) => write!(f, "{}?", pattern),
            Pattern::ZeroOrMore(pattern) => write!(f, "{}*", pattern),
            Pattern::OneOrMore(pattern) => write!(f, "{}+", pattern),
            Pattern::Either(p1, p2) => write!(f, "{}|{}", p1, p2),
        }
    }
}

pub struct PatternParser<'a> {
    chars: Peekable<Chars<'a>>,
}

fn char_is_special(c: char) -> bool {
    ".*%^$![]()\\?*+|".contains(c)
}

impl<'a> PatternParser<'a> {
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

    fn attribute(&mut self) -> Result<Attribute, ParseError> {
        match self.chars.next() {
            Some('_') => Ok(Attribute::Class(CharacterClass::Space)),
            Some('#') => Ok(Attribute::Class(CharacterClass::Digit)),
            Some('a') => Ok(Attribute::Class(CharacterClass::Letter)),
            Some('u') => Ok(Attribute::Class(CharacterClass::Uppercase)),
            Some('l') => Ok(Attribute::Class(CharacterClass::Lowercase)),
            Some('.') => Ok(Attribute::Class(CharacterClass::Punctuation)),
            Some('$') => Ok(Attribute::Class(CharacterClass::Sign)),
            Some('~') => Ok(Attribute::Class(CharacterClass::Seqdelimiter)),
            Some('<') => Ok(Attribute::Class(CharacterClass::Seqbeforechars)),
            Some('>') => Ok(Attribute::Class(CharacterClass::Seqafterchars)),
            Some('^') => Ok(Attribute::Boundary),
            Some('0') => Ok(Attribute::ByOrder(0)),
            Some('1') => Ok(Attribute::ByOrder(1)),
            Some('2') => Ok(Attribute::ByOrder(2)),
            Some('3') => Ok(Attribute::ByOrder(3)),
            Some('4') => Ok(Attribute::ByOrder(4)),
            Some('5') => Ok(Attribute::ByOrder(5)),
            Some('6') => Ok(Attribute::ByOrder(6)),
            Some('7') => Ok(Attribute::ByOrder(7)),
            Some(c) => Err(ParseError::InvalidAttribute(Some(c))),
            _ => Err(ParseError::InvalidAttribute(None)),
        }
    }

    fn characters(&mut self) -> Result<Pattern, ParseError> {
        let mut characters: String = String::new();
        while let Some(c) = self.chars.next_if(|&c| !char_is_special(c)) {
            characters.push(c);
        }
        if characters.is_empty() {
            Err(ParseError::EmptyPattern)
        } else {
            Ok(Pattern::Characters(characters))
        }
    }

    fn set(&mut self) -> Result<Pattern, ParseError> {
        self.consume('[')?;
        let mut characters = HashSet::new();
        while let Some(&c) = self.chars.peek() {
            if c == ']' {
                break;
            } else if c == '\\' {
                // Handle escape sequence
                self.chars.next(); // consume the backslash
                if let Some(escaped_char) =
                    self.chars.next_if(|&c| c == ']' || c == '(' || c == ')')
                {
                    characters.insert(escaped_char);
                } else {
                    return Err(ParseError::InvalidEscape);
                }
            } else {
                // Regular character
                characters.insert(c);
                self.chars.next();
            }
        }
        self.consume(']')?;
        if characters.is_empty() {
            Err(ParseError::EmptyPattern)
        } else {
            Ok(Pattern::Set(characters))
        }
    }

    fn attributes(&mut self) -> Result<Pattern, ParseError> {
        self.consume('%')?;
        if self.chars.peek() == Some(&'[') {
            self.consume('[')?;
            let mut attrs: HashSet<Attribute> = HashSet::new();
            while self.chars.peek() != Some(&']') {
                attrs.insert(self.attribute()?);
            }
            self.consume(']')?;
            if attrs.is_empty() {
                Err(ParseError::InvalidAttribute(None))
            } else {
                Ok(Pattern::Attributes(attrs))
            }
        } else {
            let attr = self.attribute()?;
            Ok(Pattern::Attributes(HashSet::from([attr])))
        }
    }

    fn any(&mut self) -> Result<Pattern, ParseError> {
        self.consume('.')?;
        Ok(Pattern::Any)
    }

    fn group(&mut self) -> Result<Pattern, ParseError> {
        self.consume('(')?;
        let mut patterns = Patterns(Vec::new());
        while self.chars.peek() != Some(&')') {
            patterns.push(self.either()?);
        }
        self.consume(')')?;
        if patterns.is_empty() {
            Err(ParseError::EmptyGroup)
        } else {
            Ok(Pattern::Group(patterns))
        }
    }

    fn negate(&mut self) -> Result<Pattern, ParseError> {
        self.consume('!')?;
        let pattern = self.pattern_with_quantifier()?;
        Ok(Pattern::Negate(Box::new(pattern)))
    }

    fn start_boundary(&mut self) -> Result<Pattern, ParseError> {
        self.consume('^')?;
        Ok(Pattern::Boundary)
    }

    fn end_boundary(&mut self) -> Result<Pattern, ParseError> {
        self.consume('$')?;
        Ok(Pattern::Boundary)
    }

    fn inner_pattern(&mut self) -> Result<Pattern, ParseError> {
        match self.chars.peek() {
            Some('.') => self.any(),
            Some('%') => self.attributes(),
            Some('^') => self.start_boundary(),
            Some('$') => self.end_boundary(),
            Some('!') => self.negate(),
            Some('[') => self.set(),
            Some('(') => self.group(),
            // FIXME: handle escaped special chars
            Some('|') => Err(ParseError::MissingPatternBeforeEither),
            Some(c @ ('?' | '*' | '+')) => Err(ParseError::MissingPatternBeforeQuantifier(*c)),
            Some(_) => self.characters(),
            None => Err(ParseError::EmptyPattern),
        }
    }

    fn pattern_with_quantifier(&mut self) -> Result<Pattern, ParseError> {
        let inner = self.inner_pattern()?;
        if self.chars.next_if(|&c| c == '?').is_some() {
            return Ok(Pattern::Optional(Box::new(inner)));
        } else if self.chars.next_if(|&c| c == '*').is_some() {
            return Ok(Pattern::ZeroOrMore(Box::new(inner)));
        } else if self.chars.next_if(|&c| c == '+').is_some() {
            return Ok(Pattern::OneOrMore(Box::new(inner)));
        }
        Ok(inner)
    }

    fn either(&mut self) -> Result<Pattern, ParseError> {
        let mut left = self.pattern_with_quantifier()?;
        while self.chars.next_if(|&c| c == '|').is_some() {
            let right = self.pattern_with_quantifier()?;
            left = Pattern::Either(Box::new(left), Box::new(right));
        }
        Ok(left)
    }

    pub fn pattern(&mut self) -> Result<Patterns, ParseError> {
        let mut patterns = Patterns(Vec::new());
        if self.chars.next_if(|&c| c == '-').is_some() {
            patterns.push(Pattern::Empty);
        } else {
            while self.chars.peek().is_some() {
                patterns.push(self.either()?);
            }
        }
        Ok(patterns)
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn attribute() {
        assert_eq!(
            PatternParser::new("%[al.]").attributes(),
            Ok(Pattern::Attributes(HashSet::from([
                Attribute::Class(CharacterClass::Letter),
                Attribute::Class(CharacterClass::Lowercase),
                Attribute::Class(CharacterClass::Punctuation)
            ])))
        );
        assert_eq!(
            PatternParser::new("%[a]").attributes(),
            Ok(Pattern::Attributes(HashSet::from([Attribute::Class(
                CharacterClass::Letter
            ),])))
        );
        assert_eq!(
            PatternParser::new("%[]").attributes(),
            Err(ParseError::InvalidAttribute(None))
        );
        assert_eq!(
            PatternParser::new("%[a.").attributes(),
            Err(ParseError::InvalidAttribute(None))
        );
        assert_eq!(
            PatternParser::new("%[[]").attributes(),
            Err(ParseError::InvalidAttribute(Some('[')))
        );
        assert_eq!(
            PatternParser::new("%a").attributes(),
            Ok(Pattern::Attributes(HashSet::from([Attribute::Class(
                CharacterClass::Letter
            ),])))
        );
    }

    #[test]
    fn characters() {
        assert_eq!(
            PatternParser::new("abc").characters(),
            Ok(Pattern::Characters("abc".into()))
        );
    }

    #[test]
    fn set() {
        assert_eq!(
            PatternParser::new("[abc]").set(),
            Ok(Pattern::Set(HashSet::from(['a', 'b', 'c'])))
        );
        assert_eq!(
            PatternParser::new("[abc").set(),
            Err(ParseError::CharExpected {
                expected: ']',
                found: None
            })
        );
    }

    #[test]
    fn set_with_escape() {
        assert_eq!(
            PatternParser::new(r"[abc\]]").set(),
            Ok(Pattern::Set(HashSet::from(['a', 'b', 'c', ']'])))
        );
        assert_eq!(
            PatternParser::new(r"[)}\]]").set(),
            Ok(Pattern::Set(HashSet::from([')', '}', ']'])))
        );
    }

    #[test]
    #[should_panic(expected = "InvalidEscape")]
    fn set_with_invalid_escape() {
        assert_eq!(
            PatternParser::new(r"[\a]]").set(),
            Ok(Pattern::Set(HashSet::from(['a'])))
        );
    }

    #[test]
    fn group() {
        assert_eq!(
            PatternParser::new("(abc)").group(),
            Ok(Pattern::Group(Patterns(vec![Pattern::Characters(
                "abc".into()
            )])))
        );
        assert_eq!(
            PatternParser::new("([abc])").group(),
            Ok(Pattern::Group(Patterns(vec![Pattern::Set(HashSet::from(
                ['a', 'b', 'c']
            ))])))
        );
        assert_eq!(
            PatternParser::new("()").group(),
            Err(ParseError::EmptyGroup)
        );
    }

    #[test]
    fn either() {
        assert_eq!(
            PatternParser::new("a|b").either(),
            Ok(Pattern::Either(
                Box::new(Pattern::Characters("a".into())),
                Box::new(Pattern::Characters("b".into()))
            ))
        );
        assert_eq!(
            PatternParser::new("a|b|c").either(),
            Ok(Pattern::Either(
                Box::new(Pattern::Either(
                    Box::new(Pattern::Characters("a".into())),
                    Box::new(Pattern::Characters("b".into()))
                )),
                Box::new(Pattern::Characters("c".into())),
            ))
        );
        assert_eq!(
            PatternParser::new("a+|[bc]?").either(),
            Ok(Pattern::Either(
                Box::new(Pattern::OneOrMore(Box::new(Pattern::Characters(
                    "a".into()
                )))),
                Box::new(Pattern::Optional(Box::new(Pattern::Set(HashSet::from([
                    'b', 'c'
                ])))))
            ))
        );
        assert_eq!(
            PatternParser::new("(a|b)").either(),
            Ok(Pattern::Group(Patterns(vec![Pattern::Either(
                Box::new(Pattern::Characters("a".into())),
                Box::new(Pattern::Characters("b".into()))
            )])))
        );
        assert_eq!(
            PatternParser::new("|a").either(),
            Err(ParseError::MissingPatternBeforeEither)
        );
        assert_eq!(
            PatternParser::new("a|").either(),
            Err(ParseError::EmptyPattern)
        );
    }

    #[test]
    fn pattern() {
        assert_eq!(
            PatternParser::new("(abc)").pattern(),
            Ok(Patterns(vec![Pattern::Group(Patterns(vec![
                Pattern::Characters("abc".into())
            ]))]))
        );
        assert_eq!(
            PatternParser::new("(abc)?").pattern(),
            Ok(Patterns(vec![Pattern::Optional(Box::new(Pattern::Group(
                Patterns(vec![Pattern::Characters("abc".into())])
            )))]))
        );
        assert_eq!(
            PatternParser::new("(abc)+").pattern(),
            Ok(Patterns(vec![Pattern::OneOrMore(Box::new(
                Pattern::Group(Patterns(vec![Pattern::Characters("abc".into())]))
            ))]))
        );
        assert_eq!(
            PatternParser::new("(abc)*").pattern(),
            Ok(Patterns(vec![Pattern::ZeroOrMore(Box::new(
                Pattern::Group(Patterns(vec![Pattern::Characters("abc".into())]))
            ))]))
        );
        assert_eq!(
            PatternParser::new("a**").pattern(),
            Err(ParseError::MissingPatternBeforeQuantifier('*'))
        );
        assert_eq!(
            PatternParser::new("-").pattern(),
            Ok(Patterns(vec![Pattern::Empty]))
        );
    }
}
