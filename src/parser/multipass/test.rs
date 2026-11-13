//! A Parser for the test operand of context and multipass opcodes
use crate::parser::{Attribute, CharacterClass, attribute::ContextAttribute};
use std::collections::{HashSet, VecDeque};

use crate::parser::multipass::{ConversionError, IsLiteral, ParseError, Scanner};

use super::braille::{self, BrailleChars, is_braille_dot};

/// The test operand of a `context` or multipass rule.
///
/// liblouis's `_N` steps its cursor back N characters, so a test can look at text that is
/// not part of what the rule matches from its position. Only two uses of it make sense as
/// a regular expression, and the parser turns them into parts of the test: a `_N` before
/// anything that consumes characters looks behind the rule's position, and a `_N` after the
/// last such test looks ahead of the match. Any other `_N` is rejected.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Test {
    /// Checked before the rule's position, never consumed
    lookbehind: Vec<TestInstruction>,
    /// The N of the `_N` the look-behind steps back over, 0 without one
    lookbehind_len: u8,
    /// What the rule matches from its position, brackets included
    body: Vec<TestInstruction>,
    /// Checked after the match, never consumed
    lookahead: Vec<TestInstruction>,
    /// The N of the `_N` after the look-ahead, 0 without one
    lookahead_len: u8,
}

impl Test {
    pub fn new(body: Vec<TestInstruction>) -> Self {
        Self {
            lookbehind: Vec::new(),
            lookbehind_len: 0,
            body,
            lookahead: Vec::new(),
            lookahead_len: 0,
        }
    }

    pub fn lookbehind(&self) -> &[TestInstruction] {
        &self.lookbehind
    }

    pub fn body(&self) -> &[TestInstruction] {
        &self.body
    }

    pub fn lookahead(&self) -> &[TestInstruction] {
        &self.lookahead
    }

    fn has_context(&self) -> bool {
        !self.lookbehind.is_empty() || !self.lookahead.is_empty()
    }

    /// The text this test matches when it is only literals, ignoring the replacement
    /// brackets, so `[@5-46-3456]` gives ⠐⠨⠼.
    pub fn literal_text(&self) -> Option<String> {
        if self.has_context() {
            return None;
        }
        self.body
            .iter()
            .map(TestInstruction::literal_text)
            .collect()
    }

    fn has_replace(&self) -> bool {
        self.body
            .iter()
            .any(|t| matches!(t, TestInstruction::Replace { .. }))
    }

    /// The length of the literal a forward `context` rule's test starts with at the rule's
    /// position. liblouis indexes the rule by these characters (`passFindCharacters`), and
    /// it competes as a rule of that length. It is the first literal of the body, skipping
    /// variable tests, `!`, the brackets and anchors. Anything else first, such as a class,
    /// gives 0, and the rule is then tried at every position, ahead of the rule chosen there.
    pub fn leading_literal_len(&self) -> usize {
        self.body
            .iter()
            .find_map(TestInstruction::leading_literal_len)
            .unwrap_or(0)
    }

    /// How many characters the test spells out literally (`"..."` or `@...`), anywhere in it.
    pub fn literal_count(&self) -> usize {
        [&self.lookbehind, &self.body, &self.lookahead]
            .into_iter()
            .flatten()
            .map(TestInstruction::literal_count)
            .sum()
    }

    /// Without brackets, the whole body is replaced
    pub fn add_implicit_replace(self) -> Self {
        if self.has_replace() {
            return self;
        }
        Self {
            body: vec![TestInstruction::Replace { tests: self.body }],
            ..self
        }
    }

    /// Turn the tests and `_N`s as written into a [`Test`]. Variable tests don't depend on
    /// the position, so they go to the body wherever they are.
    fn from_raw(raw: Vec<RawInstruction>) -> Result<Self, ParseError> {
        let mut raw = VecDeque::from(raw);
        let mut before = take_variables(&mut raw, Side::Front);
        let (lookbehind, lookbehind_len) = match raw.front() {
            Some(&RawInstruction::Lookback(len)) => {
                raw.pop_front();
                let (lookbehind, variables) = take_context(&mut raw, len, Side::Front)?;
                before.extend(variables);
                (lookbehind, len)
            }
            _ => (Vec::new(), 0),
        };
        let mut after = take_variables(&mut raw, Side::Back);
        let (lookahead, lookahead_len) = match raw.back() {
            Some(&RawInstruction::Lookback(len)) => {
                raw.pop_back();
                let (lookahead, variables) = take_context(&mut raw, len, Side::Back)?;
                after = variables.into_iter().chain(after).collect();
                (lookahead, len)
            }
            _ => (Vec::new(), 0),
        };
        let mut body = before;
        for instruction in raw {
            match instruction {
                RawInstruction::Test(test) => body.push(test),
                RawInstruction::Lookback(_) => return Err(ParseError::MisplacedLookback),
            }
        }
        body.extend(after);
        Ok(Self {
            lookbehind,
            lookbehind_len,
            body,
            lookahead,
            lookahead_len,
        })
    }
}

/// A test or a `_N` as written in the operand, before [`Test::from_raw`] sorts them
#[derive(Debug)]
enum RawInstruction {
    Lookback(u8),
    Test(TestInstruction),
}

impl RawInstruction {
    fn into_test(self) -> Option<TestInstruction> {
        match self {
            RawInstruction::Test(test) => Some(test),
            RawInstruction::Lookback(_) => None,
        }
    }
}

#[derive(Clone, Copy)]
enum Side {
    Front,
    Back,
}

impl Side {
    fn peek(self, raw: &VecDeque<RawInstruction>) -> Option<&RawInstruction> {
        match self {
            Side::Front => raw.front(),
            Side::Back => raw.back(),
        }
    }

    fn pop(self, raw: &mut VecDeque<RawInstruction>) -> Option<RawInstruction> {
        match self {
            Side::Front => raw.pop_front(),
            Side::Back => raw.pop_back(),
        }
    }

    fn push(self, raw: &mut VecDeque<RawInstruction>, test: TestInstruction) {
        match self {
            Side::Front => raw.push_front(RawInstruction::Test(test)),
            Side::Back => raw.push_back(RawInstruction::Test(test)),
        }
    }

    /// Put tests taken from this side back into the order they were written in
    fn in_order(self, mut tests: Vec<TestInstruction>) -> Vec<TestInstruction> {
        if let Side::Back = self {
            tests.reverse();
        }
        tests
    }
}

/// Take the variable tests at the `side` of `raw`, in the order they were written in
fn take_variables(raw: &mut VecDeque<RawInstruction>, side: Side) -> Vec<TestInstruction> {
    let mut variables = Vec::new();
    while let Some(RawInstruction::Test(TestInstruction::Variable { .. })) = side.peek(raw) {
        variables.push(side.pop(raw).and_then(RawInstruction::into_test).unwrap());
    }
    side.in_order(variables)
}

/// Take the tests spanning `len` characters from the `side` of `raw`: the look-behind or
/// look-ahead, and the variable tests among them, both in the order they were written in.
/// A string or dots that reach past `len` are split. Anything of open-ended width, a
/// bracket, an anchor or another `_N` in the way, or too few tests, is a misplaced `_N`.
fn take_context(
    raw: &mut VecDeque<RawInstruction>,
    len: u8,
    side: Side,
) -> Result<(Vec<TestInstruction>, Vec<TestInstruction>), ParseError> {
    let mut context = Vec::new();
    let mut variables = Vec::new();
    let mut remaining = len as usize;
    while remaining > 0 {
        let Some(RawInstruction::Test(test)) = side.pop(raw) else {
            return Err(ParseError::MisplacedLookback);
        };
        if let TestInstruction::Variable { .. } = test {
            variables.push(test);
            continue;
        }
        let width = test.width().ok_or(ParseError::MisplacedLookback)?;
        if width <= remaining {
            remaining -= width;
            context.push(test);
            continue;
        }
        let (inside, outside) = match side {
            Side::Front => test.split_literal(remaining),
            Side::Back => test
                .split_literal(width - remaining)
                .map(|(outside, inside)| (inside, outside)),
        }
        .ok_or(ParseError::MisplacedLookback)?;
        context.push(inside);
        side.push(raw, outside);
        remaining = 0;
    }
    Ok((side.in_order(context), side.in_order(variables)))
}

impl IsLiteral for Test {
    fn is_literal(&self) -> bool {
        !self.has_context() && self.body.iter().all(|instruction| instruction.is_literal())
    }
}

impl TryFrom<&Test> for String {
    type Error = ConversionError;

    fn try_from(test: &Test) -> Result<String, Self::Error> {
        if test.is_literal() {
            Ok(test
                .body
                .iter()
                .map(|instruction| instruction.try_into().unwrap())
                .collect::<Vec<String>>()
                .join(""))
        } else {
            Err(ConversionError::TestNotLiteral)
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
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
    OneOrMore,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TestInstruction {
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
    /// `` ` `` (accent mark): true only when the match has consumed no input yet and the whole
    /// test is being evaluated at the true beginning of the original string being translated.
    AtBeginning,
    /// `~` (tilde): true only when there is no more input left, i.e. the match has reached the
    /// true end of the original string being translated.
    AtEnd,
}

impl TestInstruction {
    /// How many characters this spells out literally (`"..."` or `@...`). A negated literal
    /// does not count: it says what must *not* be there.
    fn literal_count(&self) -> usize {
        match self {
            TestInstruction::String { s } => s.chars().count(),
            TestInstruction::Dots { dots } => dots.len(),
            TestInstruction::Replace { tests } => tests.iter().map(Self::literal_count).sum(),
            TestInstruction::Variable { .. }
            | TestInstruction::Attributes { .. }
            | TestInstruction::Class { .. }
            | TestInstruction::Negate { .. }
            | TestInstruction::AtBeginning
            | TestInstruction::AtEnd => 0,
        }
    }

    /// [`Test::leading_literal_len`] for this instruction: `Some` once the length is
    /// decided, `None` to go on with the next instruction.
    fn leading_literal_len(&self) -> Option<usize> {
        match self {
            TestInstruction::String { .. } | TestInstruction::Dots { .. } => {
                Some(self.literal_count())
            }
            TestInstruction::Attributes { .. } | TestInstruction::Class { .. } => Some(0),
            TestInstruction::Negate { test } => test.leading_literal_len(),
            TestInstruction::Replace { tests } => tests.iter().find_map(Self::leading_literal_len),
            TestInstruction::Variable { .. }
            | TestInstruction::AtBeginning
            | TestInstruction::AtEnd => None,
        }
    }

    /// How many characters this matches, if that is fixed. Not for a bracket or an anchor,
    /// which a `_N` must not step over.
    fn width(&self) -> Option<usize> {
        match self {
            TestInstruction::String { s } => Some(s.chars().count()),
            TestInstruction::Dots { dots } => Some(dots.len()),
            TestInstruction::Attributes { quantifier, .. }
            | TestInstruction::Class { quantifier, .. } => match quantifier {
                None => Some(1),
                Some(Quantifier::Number(n)) => Some(*n as usize),
                Some(_) => None,
            },
            TestInstruction::Negate { test } => test.width(),
            TestInstruction::Variable { .. } => Some(0),
            TestInstruction::AtBeginning
            | TestInstruction::AtEnd
            | TestInstruction::Replace { .. } => None,
        }
    }

    /// A string or dots split into its first `at` characters and the rest
    fn split_literal(self, at: usize) -> Option<(TestInstruction, TestInstruction)> {
        match self {
            TestInstruction::String { s } => {
                let (head, tail) = s.split_at(s.char_indices().nth(at)?.0);
                Some((
                    TestInstruction::String { s: head.into() },
                    TestInstruction::String { s: tail.into() },
                ))
            }
            TestInstruction::Dots { dots } => {
                let head = dots.iter().take(at).cloned().collect();
                let tail = dots.iter().skip(at).cloned().collect();
                Some((
                    TestInstruction::Dots { dots: head },
                    TestInstruction::Dots { dots: tail },
                ))
            }
            _ => None,
        }
    }

    /// The text this instruction matches when it is only literals, looking inside
    /// replacement brackets.
    pub fn literal_text(&self) -> Option<String> {
        match self {
            TestInstruction::Replace { tests } => tests.iter().map(Self::literal_text).collect(),
            _ => String::try_from(self).ok(),
        }
    }
}

impl IsLiteral for TestInstruction {
    fn is_literal(&self) -> bool {
        matches!(
            self,
            TestInstruction::String { .. } | TestInstruction::Dots { .. }
        )
    }
}

impl TryFrom<&TestInstruction> for String {
    type Error = ConversionError;

    fn try_from(instruction: &TestInstruction) -> Result<String, Self::Error> {
        match instruction {
            TestInstruction::String { s } => Ok(s.clone()),
            TestInstruction::Dots { dots } => Ok(dots.to_string()),
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
    matches!(c, '_' | '%' | '@' | '"' | '$' | '[' | '#' | '!' | '`' | '~')
}

fn is_class_digit(c: &char) -> bool {
    matches!(c, '1'..='7')
}

/// A Parser for the test operand of context and multipass opcodes
pub struct Parser<'a> {
    chars: Scanner<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            chars: Scanner::new(source),
        }
    }

    fn maybe_quantifier(&mut self) -> Result<Option<Quantifier>, ParseError> {
        match self.chars.peek() {
            Some('.') => {
                self.chars.next();
                Ok(Some(Quantifier::OneOrMore))
            }
            Some(c) if c.is_ascii_digit() => {
                let min = self.chars.ascii_number()?;
                if self.chars.next_if_eq(&'-').is_some() {
                    let max = match self.chars.ascii_number() {
                        Ok(n) => n,
                        _ => {
                            return Err(ParseError::InvalidQuantifier);
                        }
                    };
                    if min >= max {
                        Err(ParseError::InvalidRange { min, max })
                    } else {
                        Ok(Some(Quantifier::Range(min, max)))
                    }
                } else {
                    Ok(Some(Quantifier::Number(min)))
                }
            }
            _ => Ok(None),
        }
    }

    fn class(&mut self) -> Result<TestInstruction, ParseError> {
        self.chars.consume('%')?;
        let mut name = String::new();
        // for some stupid reason classes can be just a number
        if self.chars.peek().filter(|c| is_class_digit(c)).is_some() {
            name.push(self.chars.next().unwrap());
            Ok(TestInstruction::Class {
                name,
                quantifier: None,
            })
        } else {
            // but usually they are just ascii identifiers
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
                Ok(TestInstruction::Class {
                    name,
                    quantifier: self.maybe_quantifier()?,
                })
            }
        }
    }

    fn dots(&mut self) -> Result<TestInstruction, ParseError> {
        self.chars.consume('@')?;
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
                dots: BrailleChars::try_from(dots.as_str())?,
            })
        }
    }

    fn string(&mut self) -> Result<TestInstruction, ParseError> {
        self.chars.consume('"')?;
        let mut s = String::new();
        while self.chars.peek().filter(|&c| *c != '"').is_some() {
            let raw = self.chars.next().unwrap();
            let c = self.chars.handle_escape_sequence(raw)?;
            s.push(c);
        }
        self.chars.consume('"')?;
        Ok(TestInstruction::String { s })
    }

    /// One or more `_N`, added up: `__` steps back 2
    fn lookback(&mut self) -> Result<u8, ParseError> {
        self.chars.consume('_')?;
        let mut len: u8 = 0;
        loop {
            let n = match self.chars.peek() {
                Some(c) if c.is_ascii_digit() => self.chars.ascii_number()?,
                _ => 1,
            };
            len = len.saturating_add(n);
            if self.chars.next_if_eq(&'_').is_none() {
                return Ok(len);
            }
        }
    }

    fn attribute(&mut self) -> Result<Attribute, ParseError> {
        match self.chars.next() {
            Some('a') => Ok(Attribute::Any),
            Some('d') => Ok(Attribute::Class(CharacterClass::Digit)),
            Some('D') => Ok(Attribute::Class(CharacterClass::Litdigit)),
            Some('l') => Ok(Attribute::Class(CharacterClass::Letter)),
            Some('m') => Ok(Attribute::Class(CharacterClass::Math)),
            Some('p') => Ok(Attribute::Class(CharacterClass::Punctuation)),
            Some('S') => Ok(Attribute::Class(CharacterClass::Sign)),
            Some('s') => Ok(Attribute::Class(CharacterClass::Space)),
            Some('U') => Ok(Attribute::Class(CharacterClass::Uppercase)),
            Some('u') => Ok(Attribute::Class(CharacterClass::Lowercase)),
            Some('w') => Ok(Attribute::ByOrder(1)),
            Some('x') => Ok(Attribute::ByOrder(2)),
            Some('y') => Ok(Attribute::ByOrder(3)),
            Some('z') => Ok(Attribute::ByOrder(4)),
            Some(c) => Err(ParseError::InvalidAttribute { found: Some(c) }),
            _ => Err(ParseError::InvalidAttribute { found: None }),
        }
    }

    fn attributes(&mut self) -> Result<TestInstruction, ParseError> {
        self.chars.consume('$')?;
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
        self.chars.consume('[')?;
        let tests = self.many_tests()?;
        self.chars.consume(']')?;
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
        self.chars.consume('#')?;
        Ok(TestInstruction::Variable {
            var: self
                .chars
                .ascii_number()
                .map_err(|_| ParseError::InvalidVariableName)?,
            op: self.operator()?,
            operand: self.chars.ascii_number()?,
        })
    }

    fn negate(&mut self) -> Result<TestInstruction, ParseError> {
        self.chars.consume('!')?;
        let test = self.test()?;
        if matches!(test, TestInstruction::Negate { .. }) {
            return Err(ParseError::DoubleNegation);
        }
        if matches!(test, TestInstruction::Replace { .. }) {
            return Err(ParseError::NegatedReplacement);
        }
        Ok(TestInstruction::Negate {
            test: Box::new(test),
        })
    }

    fn at_beginning(&mut self) -> Result<TestInstruction, ParseError> {
        self.chars.consume('`')?;
        Ok(TestInstruction::AtBeginning)
    }

    fn at_end(&mut self) -> Result<TestInstruction, ParseError> {
        self.chars.consume('~')?;
        Ok(TestInstruction::AtEnd)
    }

    fn test(&mut self) -> Result<TestInstruction, ParseError> {
        match self.chars.peek() {
            // only at the top level of a test, see `raw_instructions`
            Some('_') => Err(ParseError::MisplacedLookback),
            Some('%') => Ok(self.class()?),
            Some('@') => Ok(self.dots()?),
            Some('"') => Ok(self.string()?),
            Some('$') => Ok(self.attributes()?),
            Some('[') => Ok(self.replacement()?),
            Some('#') => Ok(self.variable()?),
            Some('!') => Ok(self.negate()?),
            Some('`') => Ok(self.at_beginning()?),
            Some('~') => Ok(self.at_end()?),
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

    /// The tests and `_N`s of the operand as written
    fn raw_instructions(&mut self) -> Result<Vec<RawInstruction>, ParseError> {
        let mut raw = Vec::new();
        while self.chars.peek().filter(|&c| is_test(c)).is_some() {
            if self.chars.peek() == Some(&'_') {
                raw.push(RawInstruction::Lookback(self.lookback()?));
            } else {
                raw.push(RawInstruction::Test(self.test()?));
            }
        }
        Ok(raw)
    }

    /// Parse the test operand.
    pub fn tests(&mut self) -> Result<Test, ParseError> {
        let raw = self.raw_instructions()?;
        if raw.is_empty() {
            Err(ParseError::EmptyTest)
        } else if let Some(c) = self.chars.next() {
            // we haven't consumed all the input. There are some invalid chars
            Err(ParseError::InvalidTest { found: Some(c) })
        } else {
            Test::from_raw(raw)
        }
    }
}

/// `Display` implementation for [`Test`]
mod display {
    use super::*;

    /// Normalized: the look-behind as `_N` and its tests first, then the body, then the
    /// look-ahead and its `_N`, whatever order the table wrote them in
    impl std::fmt::Display for Test {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            if self.lookbehind_len > 0 {
                write!(f, "_{}", self.lookbehind_len)?;
            }
            for instruction in self
                .lookbehind
                .iter()
                .chain(&self.body)
                .chain(&self.lookahead)
            {
                write!(f, "{}", instruction)?;
            }
            if self.lookahead_len > 0 {
                write!(f, "_{}", self.lookahead_len)?;
            }
            Ok(())
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
                Quantifier::OneOrMore => write!(f, "."),
            }
        }
    }

    impl std::fmt::Display for TestInstruction {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                TestInstruction::Variable { var, op, operand } => {
                    write!(f, "#{}{}{}", var, op, operand)
                }
                TestInstruction::String { s } => {
                    write!(f, "\"{}\"", s)
                }
                TestInstruction::Dots { dots } => {
                    write!(f, "@{}", dots)
                }
                TestInstruction::Attributes { attrs, quantifier } => {
                    write!(f, "$")?;
                    for attr in attrs {
                        write!(f, "{}", ContextAttribute(attr))?;
                    }
                    if let Some(q) = quantifier {
                        write!(f, "{}", q)?;
                    }
                    Ok(())
                }
                TestInstruction::Class { name, quantifier } => {
                    write!(f, "%{}", name)?;
                    if let Some(q) = quantifier {
                        write!(f, "{}", q)?;
                    }
                    Ok(())
                }
                TestInstruction::Negate { test } => {
                    write!(f, "!{}", test)
                }
                TestInstruction::Replace { tests } => {
                    write!(f, "[")?;
                    for test in tests {
                        write!(f, "{}", test)?;
                    }
                    write!(f, "]")
                }
                TestInstruction::AtBeginning => write!(f, "`"),
                TestInstruction::AtEnd => write!(f, "~"),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use enumset::enum_set;

    use crate::parser::braille::BrailleChar;

    use super::braille::BrailleDot;
    use super::*;

    #[test]
    fn leading_literal_len() {
        let chars = |s| Parser::new(s).tests().unwrap().leading_literal_len();
        // the first literal, skipping variable tests and brackets
        assert_eq!(chars(r#""เ"[$l]"าะ""#), 1);
        assert_eq!(chars(r#"#1=1["ab"]"#), 2);
        // a class first: tried at every position instead
        assert_eq!(chars(r#"[$l"z"]$s"lesz""#), 0);
        // `_2` steps back over a fixed-width class and part of the string
        assert_eq!(chars(r#"_2$l"abc""#), 2);
    }

    #[test]
    fn lookbehind_and_lookahead() {
        let parts = |s| {
            let test = Parser::new(s).tests().unwrap();
            let show = |tests: &[TestInstruction]| tests.iter().map(|t| t.to_string()).collect();
            (
                show(test.lookbehind()),
                show(test.body()),
                show(test.lookahead()),
            )
        };
        let strings = |v: &[&str]| -> Vec<String> { v.iter().map(|s| s.to_string()).collect() };
        // th-g2.ctb
        assert_eq!(
            parts(r#""ใคร"!%ToneAndVowels_"#),
            (
                strings(&[]),
                strings(&[r#""ใคร""#]),
                strings(&["!%ToneAndVowels"])
            )
        );
        // variable tests go to the body wherever they are, quotation-marks.uti
        assert_eq!(
            parts(r#"#1=0_$s["'"]!$s"#),
            (
                strings(&["$s"]),
                strings(&["#1=0", r#"["'"]"#, "!$s"]),
                strings(&[])
            )
        );
        // consecutive `_`s add up, nl-NL-g0.utb
        assert_eq!(
            parts("__$d@2[@6]%digitletter"),
            (
                strings(&["$d", "@⠂"]),
                strings(&["[@⠠]", "%digitletter"]),
                strings(&[])
            )
        );
        // a string or dots are split where the `_N` ends
        assert_eq!(
            parts(r#"_2$l"abc""#),
            (
                strings(&["$l", r#""a""#]),
                strings(&[r#""bc""#]),
                strings(&[])
            )
        );
        assert_eq!(
            parts(r#""ab"@1-2_1"#),
            (strings(&[]), strings(&[r#""ab""#, "@⠁"]), strings(&["@⠂"]))
        );
        assert_eq!(
            Parser::new(r#"#1=0_$s["'"]!$s"#)
                .tests()
                .unwrap()
                .to_string(),
            r#"_1$s#1=0["'"]!$s"#
        );
        assert_eq!(
            Parser::new(r#""ใคร"!%ToneAndVowels_"#)
                .tests()
                .unwrap()
                .to_string(),
            r#""ใคร"!%ToneAndVowels_1"#
        );
    }

    #[test]
    fn misplaced_lookback() {
        for test in [
            // in the middle, it-it-comp6.utb before liblouis/liblouis#2196
            "$s[@26a]@3456%mathnumber_$u.%mathop",
            // into a bracket
            r#"_["fu"]"#,
            r#"[$d5]_5"#,
            // over a class of open-ended width
            r#"_1$l."abc""#,
            // past the anchor
            "`_$d[@1]",
            // inside a bracket or negated
            r#"["a"_]"#,
            r#""a"!_"#,
            // more than there is
            r#""ab"_3"#,
        ] {
            assert_eq!(
                Parser::new(test).tests(),
                Err(ParseError::MisplacedLookback),
                "{test}"
            );
        }
    }

    #[test]
    fn literal_count() {
        let count = |s| Parser::new(s).tests().unwrap().literal_count();
        assert_eq!(count("@1-2"), 2);
        assert_eq!(count("$a"), 0);
        assert_eq!(count("#1=1[@1]$d\"ab\""), 3);
        assert_eq!(count("!\"ab\"@1"), 1);
    }

    #[test]
    fn dots() {
        assert_eq!(
            Parser::new("@123").dots(),
            Ok(TestInstruction::Dots {
                dots: BrailleChars::from(vec![BrailleChar::from(enum_set!(
                    BrailleDot::Dot1 | BrailleDot::Dot2 | BrailleDot::Dot3
                ))])
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
            Ok(TestInstruction::Dots {
                dots: BrailleChars::from(vec![
                    BrailleChar::from(enum_set!(BrailleDot::Dot1)),
                    BrailleChar::from(enum_set!(BrailleDot::Dot2))
                ])
            })
        );
    }

    #[test]
    fn string() {
        assert_eq!(
            Parser::new(r#""test""#).string(),
            Ok(TestInstruction::String { s: "test".into() })
        );
        assert_eq!(
            Parser::new(r#"",_""#).string(),
            Ok(TestInstruction::String { s: ",_".into() })
        );
        assert_eq!(
            Parser::new(r#"", ""#).string(),
            Ok(TestInstruction::String { s: ", ".into() })
        );
        assert_eq!(
            Parser::new(r#""-\"""#).string(),
            Ok(TestInstruction::String { s: "-\"".into() })
        );
        assert_eq!(
            Parser::new(r#"".\s\"""#).string(),
            Ok(TestInstruction::String { s: ". \"".into() })
        );
        // an escaped backslash is one backslash, and must not swallow the
        // closing quote (`noback context "\\" @16`)
        assert_eq!(
            Parser::new(r#""\\""#).string(),
            Ok(TestInstruction::String { s: "\\".into() })
        );
        assert_eq!(
            Parser::new(r#""a\\b""#).string(),
            Ok(TestInstruction::String { s: "a\\b".into() })
        );
    }

    #[test]
    fn negate() {
        assert_eq!(
            Parser::new(r#"!"a""#).negate(),
            Ok(TestInstruction::Negate {
                test: Box::new(TestInstruction::String { s: "a".into() })
            })
        );
        assert_eq!(
            Parser::new(r#"!!"a""#).negate(),
            Err(ParseError::DoubleNegation)
        );
        assert_eq!(
            Parser::new("![$d]").negate(),
            Err(ParseError::NegatedReplacement)
        );
    }

    #[test]
    fn lookback() {
        assert_eq!(Parser::new("_123").lookback(), Ok(123));
        assert_eq!(Parser::new("_7").lookback(), Ok(7));
        assert_eq!(Parser::new("_ ").lookback(), Ok(1));
        assert_eq!(Parser::new("_abc").lookback(), Ok(1));
        assert_eq!(Parser::new("_12abc").lookback(), Ok(12));
        assert_eq!(Parser::new("__$d").lookback(), Ok(2));
        assert_eq!(Parser::new("_2_3").lookback(), Ok(5));
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
            Ok(TestInstruction::Attributes {
                attrs: HashSet::from([Attribute::Any]),
                quantifier: None
            })
        );
        assert_eq!(
            Parser::new("$ay").attributes(),
            Ok(TestInstruction::Attributes {
                attrs: HashSet::from([Attribute::Any, Attribute::ByOrder(3)]),
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
            Ok(TestInstruction::Class {
                name: "foo".into(),
                quantifier: None
            })
        );
        assert_eq!(
            Parser::new("%3").class(),
            Ok(TestInstruction::Class {
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
    fn class_with_quantifier() {
        assert_eq!(
            Parser::new("%foo.").class(),
            Ok(TestInstruction::Class {
                name: "foo".into(),
                quantifier: Some(Quantifier::OneOrMore)
            })
        );
        assert_eq!(
            Parser::new("%foo20").class(),
            Ok(TestInstruction::Class {
                name: "foo".into(),
                quantifier: Some(Quantifier::Number(20))
            })
        );
        assert_eq!(
            Parser::new("%foo1-20").class(),
            Ok(TestInstruction::Class {
                name: "foo".into(),
                quantifier: Some(Quantifier::Range(1, 20))
            })
        );
        assert_eq!(
            Parser::new("%foo20-200").class(),
            Ok(TestInstruction::Class {
                name: "foo".into(),
                quantifier: Some(Quantifier::Range(20, 200))
            })
        );
        assert_eq!(
            Parser::new("%foo200-20").class(),
            Err(ParseError::InvalidRange { min: 200, max: 20 }),
        );
    }

    #[test]
    fn quantifier() {
        assert_eq!(
            Parser::new("$a ").attributes(),
            Ok(TestInstruction::Attributes {
                attrs: HashSet::from([Attribute::Any]),
                quantifier: None
            })
        );
        assert_eq!(
            Parser::new("$a.").attributes(),
            Ok(TestInstruction::Attributes {
                attrs: HashSet::from([Attribute::Any]),
                quantifier: Some(Quantifier::OneOrMore)
            })
        );
        assert_eq!(
            Parser::new("$a3").attributes(),
            Ok(TestInstruction::Attributes {
                attrs: HashSet::from([Attribute::Any]),
                quantifier: Some(Quantifier::Number(3))
            })
        );
        assert_eq!(
            Parser::new("$a3-5").attributes(),
            Ok(TestInstruction::Attributes {
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
            Ok(TestInstruction::Variable {
                var: 1,
                op: Operator::Lt,
                operand: 2
            })
        );
        assert_eq!(
            Parser::new("#1<=2").variable(),
            Ok(TestInstruction::Variable {
                var: 1,
                op: Operator::LtEq,
                operand: 2
            })
        );
        assert_eq!(
            Parser::new("#1=3").variable(),
            Ok(TestInstruction::Variable {
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
            Ok(TestInstruction::Variable {
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
            Ok(TestInstruction::Variable {
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
            Ok(TestInstruction::Replace {
                tests: vec![TestInstruction::Attributes {
                    attrs: HashSet::from([Attribute::Class(CharacterClass::Digit)]),
                    quantifier: None
                }]
            })
        );
        assert_eq!(
            Parser::new("[%foo]").replacement(),
            Ok(TestInstruction::Replace {
                tests: vec![TestInstruction::Class {
                    name: "foo".into(),
                    quantifier: None
                }]
            })
        );
        assert_eq!(
            Parser::new("[,,,]").replacement(),
            Err(ParseError::CharExpected {
                expected: ']',
                found: Some(',')
            }),
        );
    }

    #[test]
    fn test() {
        assert_eq!(
            Parser::new("$d[\"hello\"]%digitletter").tests(),
            Ok(Test::new(vec![
                TestInstruction::Attributes {
                    attrs: HashSet::from([Attribute::Class(CharacterClass::Digit)]),
                    quantifier: None
                },
                TestInstruction::Replace {
                    tests: vec![TestInstruction::String { s: "hello".into() }]
                },
                TestInstruction::Class {
                    name: "digitletter".into(),
                    quantifier: None
                }
            ]))
        );
    }

    #[test]
    fn consume_all() {
        assert_eq!(
            Parser::new("@123#1=0&too much input").tests(),
            Err(ParseError::InvalidTest { found: Some('&') }),
        );
    }

    #[test]
    fn at_beginning_and_at_end() {
        assert_eq!(
            Parser::new("`\"a\"").tests(),
            Ok(Test::new(vec![
                TestInstruction::AtBeginning,
                TestInstruction::String { s: "a".into() }
            ]))
        );
        assert_eq!(
            Parser::new("\"a\"~").tests(),
            Ok(Test::new(vec![
                TestInstruction::String { s: "a".into() },
                TestInstruction::AtEnd
            ]))
        );
        // liblouis/liblouis#2071: `quotation-marks.uti:26` places `` ` `` after `#1=0` rather
        // than first -- the manual says it must be first, but the compiler doesn't enforce
        // that, and this should still parse.
        assert_eq!(
            Parser::new("#1=0`[\"'\"]").tests(),
            Ok(Test::new(vec![
                TestInstruction::Variable {
                    var: 1,
                    op: Operator::Eq,
                    operand: 0
                },
                TestInstruction::AtBeginning,
                TestInstruction::Replace {
                    tests: vec![TestInstruction::String { s: "'".into() }]
                }
            ]))
        );
        assert_eq!(
            Parser::new("!`").tests(),
            Ok(Test::new(vec![TestInstruction::Negate {
                test: Box::new(TestInstruction::AtBeginning)
            }]))
        );
        assert_eq!(
            Parser::new("!~").tests(),
            Ok(Test::new(vec![TestInstruction::Negate {
                test: Box::new(TestInstruction::AtEnd)
            }]))
        );
    }
}
