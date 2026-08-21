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
    #[error("double negation ('!!') is not allowed")]
    DoubleNegation,
    #[error("unexpected {0:?} after pattern")]
    TrailingInput(char),
}

/// Which side of a `match` rule (`pre` or `post`) a pattern was parsed for.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Side {
    Pre,
    Post,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Pattern {
    Empty,
    Characters(String),
    Boundary(Side),
    Any,
    Set(HashSet<char>),
    Attributes(HashSet<Attribute>),
    Group(Patterns),
    Negate(Box<Pattern>),
    Optional(Box<Pattern>),
    ZeroOrMore(Box<Pattern>),
    OneOrMore(Box<Pattern>),
    Either(Patterns, Patterns),
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
            // `^`/`$` are documented as interchangeable (see `Side`), so which one we print
            // back doesn't change the meaning -- pick the conventional spelling for each side.
            Pattern::Boundary(Side::Pre) => write!(f, "^"),
            Pattern::Boundary(Side::Post) => write!(f, "$"),
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
    side: Side,
}

fn char_is_special(c: char) -> bool {
    ".*%^$![]()\\?*+|".contains(c)
}

impl<'a> PatternParser<'a> {
    pub fn new(source: &'a str, side: Side) -> Self {
        Self {
            chars: source.chars().peekable(),
            side,
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
            // for some weird historical reason 0..7 are also allowed as class names
            Some(c @ '0') | Some(c @ '1') | Some(c @ '2') | Some(c @ '3') | Some(c @ '4')
            | Some(c @ '5') | Some(c @ '6') | Some(c @ '7') => {
                Ok(Attribute::Class(CharacterClass::UserDefined(c.to_string())))
            }
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
        let mut attrs = if self.chars.peek() == Some(&'[') {
            self.consume('[')?;
            let mut attrs = HashSet::new();
            while self.chars.peek() != Some(&']') {
                attrs.insert(self.attribute()?);
            }
            self.consume(']')?;
            if attrs.is_empty() {
                return Err(ParseError::InvalidAttribute(None));
            }
            attrs
        } else {
            HashSet::from([self.attribute()?])
        };
        // `^` inside a `%[...]` set means the same boundary as standalone `^`/`$` (see `Side`),
        // but it's expressed via the shared `Attribute` type rather than `Pattern` itself, since
        // `Attribute` is also used by the unrelated multipass `Test` grammar. Pull it out here
        // and re-express it as an ordinary `Pattern::Boundary`, so nothing downstream ever sees
        // `Attribute::Boundary` inside a `Pattern::Attributes` set.
        if attrs.remove(&Attribute::Boundary) {
            let boundary = Pattern::Boundary(self.side);
            if attrs.is_empty() {
                Ok(boundary)
            } else {
                Ok(Pattern::Either(
                    Patterns(vec![Pattern::Attributes(attrs)]),
                    Patterns(vec![boundary]),
                ))
            }
        } else {
            Ok(Pattern::Attributes(attrs))
        }
    }

    fn any(&mut self) -> Result<Pattern, ParseError> {
        self.consume('.')?;
        Ok(Pattern::Any)
    }

    fn group(&mut self) -> Result<Pattern, ParseError> {
        self.consume('(')?;
        let patterns = self.either()?;
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
        if matches!(pattern, Pattern::Negate(_)) {
            return Err(ParseError::DoubleNegation);
        }
        Ok(Pattern::Negate(Box::new(pattern)))
    }

    fn start_boundary(&mut self) -> Result<Pattern, ParseError> {
        self.consume('^')?;
        Ok(Pattern::Boundary(self.side))
    }

    fn end_boundary(&mut self) -> Result<Pattern, ParseError> {
        self.consume('$')?;
        Ok(Pattern::Boundary(self.side))
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

    /// Parses a run of concatenated patterns, stopping at `|`, `)`, or end of input.
    fn sequence(&mut self) -> Result<Patterns, ParseError> {
        let mut patterns = Patterns(Vec::new());
        while !matches!(self.chars.peek(), None | Some('|') | Some(')')) {
            patterns.push(self.pattern_with_quantifier()?);
        }
        Ok(patterns)
    }

    /// Parses `sequence ('|' sequence)*`, left-associative, matching the lowest-precedence
    /// alternation semantics of a normal regular expression (concatenation binds tighter
    /// than `|`).
    fn either(&mut self) -> Result<Patterns, ParseError> {
        if self.chars.peek() == Some(&'|') {
            return Err(ParseError::MissingPatternBeforeEither);
        }
        let mut left = self.sequence()?;
        while self.chars.next_if(|&c| c == '|').is_some() {
            let right = self.sequence()?;
            if right.is_empty() {
                return Err(ParseError::EmptyPattern);
            }
            left = Patterns(vec![Pattern::Either(left, right)]);
        }
        Ok(left)
    }

    pub fn pattern(&mut self) -> Result<Patterns, ParseError> {
        if self.chars.next_if(|&c| c == '-').is_some() {
            return Ok(Patterns(vec![Pattern::Empty]));
        }
        if self.chars.peek().is_none() {
            return Ok(Patterns(Vec::new()));
        }
        let patterns = self.either()?;
        if let Some(&c) = self.chars.peek() {
            return Err(ParseError::TrailingInput(c));
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
            PatternParser::new("%[al.]", Side::Pre).attributes(),
            Ok(Pattern::Attributes(HashSet::from([
                Attribute::Class(CharacterClass::Letter),
                Attribute::Class(CharacterClass::Lowercase),
                Attribute::Class(CharacterClass::Punctuation)
            ])))
        );
        assert_eq!(
            PatternParser::new("%[a]", Side::Pre).attributes(),
            Ok(Pattern::Attributes(HashSet::from([Attribute::Class(
                CharacterClass::Letter
            ),])))
        );
        assert_eq!(
            PatternParser::new("%[]", Side::Pre).attributes(),
            Err(ParseError::InvalidAttribute(None))
        );
        assert_eq!(
            PatternParser::new("%[a.", Side::Pre).attributes(),
            Err(ParseError::InvalidAttribute(None))
        );
        assert_eq!(
            PatternParser::new("%[[]", Side::Pre).attributes(),
            Err(ParseError::InvalidAttribute(Some('[')))
        );
        assert_eq!(
            PatternParser::new("%a", Side::Pre).attributes(),
            Ok(Pattern::Attributes(HashSet::from([Attribute::Class(
                CharacterClass::Letter
            ),])))
        );
    }

    #[test]
    fn boundary() {
        assert_eq!(
            PatternParser::new("^", Side::Pre).pattern(),
            Ok(Patterns(vec![Pattern::Boundary(Side::Pre)]))
        );
        assert_eq!(
            PatternParser::new("$", Side::Post).pattern(),
            Ok(Patterns(vec![Pattern::Boundary(Side::Post)]))
        );
        // `^`/`$` are interchangeable (see `Side`) -- which side resolves the meaning, not
        // which of the two was actually written.
        assert_eq!(
            PatternParser::new("$", Side::Pre).pattern(),
            Ok(Patterns(vec![Pattern::Boundary(Side::Pre)]))
        );
        // `%[^]` alone collapses straight to a plain boundary, not a pointless `Either` with an
        // empty character class.
        assert_eq!(
            PatternParser::new("%[^]", Side::Post).attributes(),
            Ok(Pattern::Boundary(Side::Post))
        );
        // `%[u^]` mixes a real attribute with the boundary marker.
        assert_eq!(
            PatternParser::new("%[u^]", Side::Pre).attributes(),
            Ok(Pattern::Either(
                Patterns(vec![Pattern::Attributes(HashSet::from([
                    Attribute::Class(CharacterClass::Uppercase)
                ]))]),
                Patterns(vec![Pattern::Boundary(Side::Pre)]),
            ))
        );
    }

    #[test]
    fn characters() {
        assert_eq!(
            PatternParser::new("abc", Side::Pre).characters(),
            Ok(Pattern::Characters("abc".into()))
        );
    }

    #[test]
    fn set() {
        assert_eq!(
            PatternParser::new("[abc]", Side::Pre).set(),
            Ok(Pattern::Set(HashSet::from(['a', 'b', 'c'])))
        );
        assert_eq!(
            PatternParser::new("[abc", Side::Pre).set(),
            Err(ParseError::CharExpected {
                expected: ']',
                found: None
            })
        );
    }

    #[test]
    fn set_with_escape() {
        assert_eq!(
            PatternParser::new(r"[abc\]]", Side::Pre).set(),
            Ok(Pattern::Set(HashSet::from(['a', 'b', 'c', ']'])))
        );
        assert_eq!(
            PatternParser::new(r"[)}\]]", Side::Pre).set(),
            Ok(Pattern::Set(HashSet::from([')', '}', ']'])))
        );
    }

    #[test]
    #[should_panic(expected = "InvalidEscape")]
    fn set_with_invalid_escape() {
        assert_eq!(
            PatternParser::new(r"[\a]]", Side::Pre).set(),
            Ok(Pattern::Set(HashSet::from(['a'])))
        );
    }

    #[test]
    fn negate() {
        assert_eq!(
            PatternParser::new("!a", Side::Pre).pattern_with_quantifier(),
            Ok(Pattern::Negate(Box::new(Pattern::Characters("a".into()))))
        );
        assert_eq!(
            PatternParser::new("!!a", Side::Pre).pattern_with_quantifier(),
            Err(ParseError::DoubleNegation)
        );
        assert_eq!(
            PatternParser::new("!!a+", Side::Pre).pattern_with_quantifier(),
            Err(ParseError::DoubleNegation)
        );
    }

    #[test]
    fn group() {
        assert_eq!(
            PatternParser::new("(abc)", Side::Pre).group(),
            Ok(Pattern::Group(Patterns(vec![Pattern::Characters(
                "abc".into()
            )])))
        );
        assert_eq!(
            PatternParser::new("([abc])", Side::Pre).group(),
            Ok(Pattern::Group(Patterns(vec![Pattern::Set(HashSet::from(
                ['a', 'b', 'c']
            ))])))
        );
        assert_eq!(
            PatternParser::new("()", Side::Pre).group(),
            Err(ParseError::EmptyGroup)
        );
    }

    #[test]
    fn either() {
        assert_eq!(
            PatternParser::new("a|b", Side::Pre).either(),
            Ok(Patterns(vec![Pattern::Either(
                Patterns(vec![Pattern::Characters("a".into())]),
                Patterns(vec![Pattern::Characters("b".into())])
            )]))
        );
        assert_eq!(
            PatternParser::new("a|b|c", Side::Pre).either(),
            Ok(Patterns(vec![Pattern::Either(
                Patterns(vec![Pattern::Either(
                    Patterns(vec![Pattern::Characters("a".into())]),
                    Patterns(vec![Pattern::Characters("b".into())])
                )]),
                Patterns(vec![Pattern::Characters("c".into())]),
            )]))
        );
        assert_eq!(
            PatternParser::new("a+|[bc]?", Side::Pre).either(),
            Ok(Patterns(vec![Pattern::Either(
                Patterns(vec![Pattern::OneOrMore(Box::new(Pattern::Characters(
                    "a".into()
                )))]),
                Patterns(vec![Pattern::Optional(Box::new(Pattern::Set(
                    HashSet::from(['b', 'c'])
                )))])
            )]))
        );
        assert_eq!(
            PatternParser::new("(a|b)", Side::Pre).either(),
            Ok(Patterns(vec![Pattern::Group(Patterns(vec![
                Pattern::Either(
                    Patterns(vec![Pattern::Characters("a".into())]),
                    Patterns(vec![Pattern::Characters("b".into())])
                )
            ]))]))
        );
        assert_eq!(
            PatternParser::new("|a", Side::Pre).either(),
            Err(ParseError::MissingPatternBeforeEither)
        );
        assert_eq!(
            PatternParser::new("a|", Side::Pre).either(),
            Err(ParseError::EmptyPattern)
        );
        assert_eq!(
            PatternParser::new("e%[^_.]|end", Side::Pre).either(),
            Ok(Patterns(vec![Pattern::Either(
                Patterns(vec![
                    Pattern::Characters("e".into()),
                    Pattern::Either(
                        Patterns(vec![Pattern::Attributes(HashSet::from([
                            Attribute::Class(CharacterClass::Space),
                            Attribute::Class(CharacterClass::Punctuation),
                        ]))]),
                        Patterns(vec![Pattern::Boundary(Side::Pre)]),
                    )
                ]),
                Patterns(vec![Pattern::Characters("end".into())])
            )]))
        );
    }

    #[test]
    fn pattern() {
        assert_eq!(
            PatternParser::new("(abc)", Side::Pre).pattern(),
            Ok(Patterns(vec![Pattern::Group(Patterns(vec![
                Pattern::Characters("abc".into())
            ]))]))
        );
        assert_eq!(
            PatternParser::new("(abc)?", Side::Pre).pattern(),
            Ok(Patterns(vec![Pattern::Optional(Box::new(Pattern::Group(
                Patterns(vec![Pattern::Characters("abc".into())])
            )))]))
        );
        assert_eq!(
            PatternParser::new("(abc)+", Side::Pre).pattern(),
            Ok(Patterns(vec![Pattern::OneOrMore(Box::new(
                Pattern::Group(Patterns(vec![Pattern::Characters("abc".into())]))
            ))]))
        );
        assert_eq!(
            PatternParser::new("(abc)*", Side::Pre).pattern(),
            Ok(Patterns(vec![Pattern::ZeroOrMore(Box::new(
                Pattern::Group(Patterns(vec![Pattern::Characters("abc".into())]))
            ))]))
        );
        assert_eq!(
            PatternParser::new("a**", Side::Pre).pattern(),
            Err(ParseError::MissingPatternBeforeQuantifier('*'))
        );
        assert_eq!(
            PatternParser::new("-", Side::Pre).pattern(),
            Ok(Patterns(vec![Pattern::Empty]))
        );
        // Regression test: a stray ')' with no enclosing group used to be silently accepted,
        // producing an empty Patterns instead of an error.
        assert_eq!(
            PatternParser::new(")", Side::Pre).pattern(),
            Err(ParseError::TrailingInput(')'))
        );
        assert_eq!(
            PatternParser::new("a)", Side::Pre).pattern(),
            Err(ParseError::TrailingInput(')'))
        );
    }
}
