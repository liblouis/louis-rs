use std::{collections::HashSet, iter::Peekable, str::Chars};

#[derive(thiserror::Error, Debug, PartialEq)]
pub enum ParseError {
    #[error("Expected {expected:?}, got {found:?}")]
    CharExpected { expected: char, found: Option<char> },
    #[error("Invalid attribute {0:?}")]
    InvalidAttribute(Option<char>),
    #[error("Quantifier not allowed without pattern")]
    MissingPatternBeforeQuantifier,
    #[error("Pattern cannot be empty")]
    EmptyPattern,
    #[error("Group cannot be empty")]
    EmptyGroup,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Attribute {
    Space,
    Digit,
    Letter,
    Uppercase,
    Lowercase,
    Punctuation,
    Sign,
    Seqdelimiter,
    Seqbeforechars,
    Seqafterchars,
    Boundary,
    UserDefined(u8),
}

#[derive(Debug, PartialEq)]
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

pub type Patterns = Vec<Pattern>;

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
            Some('_') => Ok(Attribute::Space),
            Some('#') => Ok(Attribute::Digit),
            Some('a') => Ok(Attribute::Letter),
            Some('u') => Ok(Attribute::Uppercase),
            Some('l') => Ok(Attribute::Lowercase),
            Some('.') => Ok(Attribute::Punctuation),
            Some('$') => Ok(Attribute::Sign),
            Some('~') => Ok(Attribute::Seqdelimiter),
            Some('<') => Ok(Attribute::Seqbeforechars),
            Some('>') => Ok(Attribute::Seqafterchars),
            Some('^') => Ok(Attribute::Boundary),
            Some('0') => Ok(Attribute::UserDefined(0)),
            Some('1') => Ok(Attribute::UserDefined(1)),
            Some('2') => Ok(Attribute::UserDefined(2)),
            Some('3') => Ok(Attribute::UserDefined(3)),
            Some('4') => Ok(Attribute::UserDefined(4)),
            Some('5') => Ok(Attribute::UserDefined(5)),
            Some('6') => Ok(Attribute::UserDefined(6)),
            Some('7') => Ok(Attribute::UserDefined(7)),
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
        while let Some(c) = self.chars.next_if(|&c| c != ']') {
            characters.insert(c);
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
        let mut patterns: Patterns = Vec::new();
        while self.chars.peek() != Some(&')') {
            patterns.push(self.pattern_with_quantifier()?);
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
            Some('?') | Some('*') | Some('+') | Some('|') => {
                Err(ParseError::MissingPatternBeforeQuantifier)
            }
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
        } else if self.chars.next_if(|&c| c == '|').is_some() {
            let outer = self.pattern_with_quantifier()?;
            return Ok(Pattern::Either(Box::new(inner), Box::new(outer)));
        }
        Ok(inner)
    }

    pub fn pattern(&mut self) -> Result<Patterns, ParseError> {
        let mut patterns: Patterns = Vec::new();
        if self.chars.next_if(|&c| c == '-').is_some() {
            patterns.push(Pattern::Empty);
        } else {
            while self.chars.peek().is_some() {
                patterns.push(self.pattern_with_quantifier()?);
            }
        }
        Ok(patterns)
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn attribute_test() {
        assert_eq!(
            PatternParser::new("%[al.]").attributes(),
            Ok(Pattern::Attributes(HashSet::from([
                Attribute::Letter,
                Attribute::Lowercase,
                Attribute::Punctuation
            ])))
        );
        assert_eq!(
            PatternParser::new("%[a]").attributes(),
            Ok(Pattern::Attributes(HashSet::from([Attribute::Letter,])))
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
            Ok(Pattern::Attributes(HashSet::from([Attribute::Letter,])))
        );
    }

    #[test]
    fn characters_test() {
        assert_eq!(
            PatternParser::new("abc").characters(),
            Ok(Pattern::Characters("abc".into()))
        );
    }

    #[test]
    fn set_test() {
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
    fn group_test() {
        assert_eq!(
            PatternParser::new("(abc)").group(),
            Ok(Pattern::Group(vec![Pattern::Characters("abc".into())]))
        );
        assert_eq!(
            PatternParser::new("([abc])").group(),
            Ok(Pattern::Group(vec![Pattern::Set(HashSet::from([
                'a', 'b', 'c'
            ]))]))
        );
        assert_eq!(
            PatternParser::new("()").group(),
            Err(ParseError::EmptyGroup)
        );
    }

    #[test]
    fn pattern_test() {
        assert_eq!(
            PatternParser::new("(abc)").pattern(),
            Ok(vec![Pattern::Group(vec![Pattern::Characters(
                "abc".into()
            )])])
        );
        assert_eq!(
            PatternParser::new("(abc)?").pattern(),
            Ok(vec![Pattern::Optional(Box::new(Pattern::Group(vec![
                Pattern::Characters("abc".into())
            ])))])
        );
        assert_eq!(
            PatternParser::new("(abc)+").pattern(),
            Ok(vec![Pattern::OneOrMore(Box::new(Pattern::Group(vec![
                Pattern::Characters("abc".into())
            ])))])
        );
        assert_eq!(
            PatternParser::new("(abc)*").pattern(),
            Ok(vec![Pattern::ZeroOrMore(Box::new(Pattern::Group(vec![
                Pattern::Characters("abc".into())
            ])))])
        );
        assert_eq!(
            PatternParser::new("a**").pattern(),
            Err(ParseError::MissingPatternBeforeQuantifier)
        );
        assert_eq!(PatternParser::new("-").pattern(), Ok(vec![Pattern::Empty]));
    }
}
