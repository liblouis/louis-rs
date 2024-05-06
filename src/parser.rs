use std::{
    collections::HashSet,
    ffi::OsStr,
    fs::read_to_string,
    io,
    iter::Peekable,
    num::ParseIntError,
    path::{Path, PathBuf},
    str::{Chars, SplitWhitespace},
};

use enumset::enum_set;
use enumset::{EnumSet, EnumSetType};

use search_path::SearchPath;

use self::{
    braille::{braille_chars, chars_to_dots, BrailleChars},
    multipass::Test,
};

pub use braille::dots_to_unicode;
pub use braille::fallback;

mod braille;
mod multipass;

#[derive(EnumSetType, Debug)]
pub enum Constraint {
    Nofor,
    Noback,
    Nocross,
}

type Constraints = EnumSet<Constraint>;

const DIRECTIONS: Constraints = enum_set!(Constraint::Nofor | Constraint::Noback);

#[derive(EnumSetType, Debug, clap::ValueEnum)]
pub enum Direction {
    Forward,
    Backward,
}

#[derive(PartialEq, Debug)]
pub enum WithClass {
    Before { class: String },
    After { class: String },
}

type WithClasses = Vec<WithClass>;

#[derive(Hash, PartialEq, Eq, Debug)]
pub enum WithMatch {
    Before,
    After,
}

type WithMatches = HashSet<WithMatch>;

#[derive(thiserror::Error, Debug, PartialEq)]
pub enum ParseError {
    #[error("Expected {expected:?}, got {found:?}")]
    TokenExpected {
        expected: String,
        found: Option<String>,
    },
    #[error("Invalid braille")]
    InvalidBraille(#[from] braille::ParseError),
    #[error("Braille expected")]
    DotsExpected,
    #[error("Comma separated tuple of Braille expected")]
    DotsTupleExpected,
    #[error("invalid unicode literal {found:?}")]
    InvalidUnicodeLiteral { found: Option<String> },
    #[error("invalid number")]
    InvalidNumber(#[from] ParseIntError),
    #[error("invalid escape sequence")]
    InvalidEscape,
    #[error("Names can only contain a-z and A-Z, got {name:?}")]
    InvalidName { name: String },
    #[error("Constraints '{constraints:?}' not allowed for opcode {opcode:?}.")]
    InvalidConstraints {
        constraints: Constraints,
        opcode: Opcode,
    },
    #[error("Expected classname, got {found:?}")]
    ClassNameExpected { found: Option<String> },
    #[error("Opcode expected, got {found:?}")]
    OpcodeExpected { found: Option<String> },
    #[error("Name expected")]
    NameExpected,
    #[error("Characters expected")]
    CharsExpected,
    #[error("Filename expected")]
    FilenameExpected,
    #[error("Number expected, got {found:?}")]
    NumberExpected { found: Option<String> },
    #[error("Multipass test expected")]
    MultipassTestExpected,
    #[error("Multipass action expected")]
    MultipassActionExpected,
    #[error("Invalid multipass test: {0}")]
    InvalidMultipassTest(#[from] multipass::ParseError),
    #[error("Match pre-pattern expected")]
    MatchPreExpected,
    #[error("Match post-pattern expected")]
    MatchPostExpected,
    #[error("Expected a single char, got {found:?}")]
    SingleCharExpected { found: Option<String> },
}

#[derive(PartialEq, Debug)]
pub enum Opcode {
    Include,
    Undefined,
    Display,
    Multind,

    // Character-Definition Opcodes
    Space,
    Punctuation,
    Digit,
    Grouping,
    Letter,
    Base,
    Lowercase,
    Uppercase,
    Litdigit,
    Sign,
    Math,

    // Braille Indicator Opcodes
    Modeletter,
    Capsletter,
    Begmodeword,
    Begcapsword,
    Endcapsword,
    Capsmodechars,
    Begcaps,
    Endcaps,
    Begcapsphrase,
    Endcapsphrase,
    Lencapsphrase,
    Letsign,
    Noletsign,
    Noletsignbefore,
    Noletsignafter,
    Nocontractsign,
    Numsign,
    Nonumsign,
    Numericnocontchars,
    Numericmodechars,
    Midendnumericmodechars,

    Begmodephrase,
    Endmodephrase,
    Lenmodephrase,

    // Opcodes for Standing Alone Sequences
    Seqdelimiter,
    Seqbeforechars,
    Seqafterchars,
    Seqafterpattern,
    Seqafterexpression,

    // Emphasis Opcodes
    Class,
    Emphclass,
    Begemph,
    Endemph,
    Noemphchars,
    Emphletter,
    Begemphword,
    Endemphword,
    Emphmodechars,
    Begemphphrase,
    Endemphphrase,
    Lenemphphrase,

    // Special Symbol Opcodes
    Decpoint,
    Hyphen,

    // Special Processing Opcodes
    Capsnocont,

    // Translation Opcodes
    Compbrl,
    Comp6,
    Nocont,
    Replace,
    Always,
    Repeated,
    Repword,
    Rependword,
    Largesign,
    Word,
    Syllable,
    Joinword,
    Lowword,
    Contraction,
    Sufword,
    Prfword,
    Begword,
    Begmidword,
    Midword,
    Midendword,
    Endword,
    Partword,
    Exactdots,
    Prepunc,
    Postpunc,
    Begnum,
    Midnum,
    Endnum,
    Joinnum,

    // Computer braille
    Begcomp,
    Endcomp,

    // Character-Class Opcodes
    Attribute,

    // Swap Opcodes
    Swapcd,
    Swapdd,
    Swapcc,

    // Context and Multipass Opcodes
    Context,
    Pass2,
    Pass3,
    Pass4,

    // The correct Opcode
    Correct,

    // The match Opcode
    Match,
    Literal,
}

#[derive(PartialEq, Debug)]
pub enum Rule {
    Include {
        file: String,
    },
    Undefined {
        dots: BrailleChars,
    },
    Display {
        character: char,
        dots: BrailleChars,
        constraints: Constraints,
    },
    Multind {
        dots: BrailleChars,
        names: Vec<String>,
        constraints: Constraints,
    },

    Space {
        character: char,
        dots: BrailleChars,
        constraints: Constraints,
    },
    Punctuation {
        character: char,
        dots: BrailleChars,
        constraints: Constraints,
    },
    Digit {
        character: char,
        dots: BrailleChars,
        constraints: Constraints,
    },
    Grouping {
        name: String,
        chars: String,
        dots: BrailleChars,
    },
    Letter {
        character: char,
        dots: Braille,
        constraints: Constraints,
    },
    Base {
        name: String,
        derived: char,
        base: char,
    },
    Lowercase {
        character: char,
        dots: BrailleChars,
        constraints: Constraints,
    },
    Uppercase {
        character: char,
        dots: BrailleChars,
        constraints: Constraints,
    },
    Litdigit {
        character: char,
        dots: BrailleChars,
        constraints: Constraints,
    },
    Sign {
        character: char,
        dots: BrailleChars,
        constraints: Constraints,
    },
    Math {
        character: char,
        dots: BrailleChars,
        constraints: Constraints,
    },

    Modeletter {
        name: String,
        dots: BrailleChars,
        constraints: Constraints,
    },
    Capsletter {
        dots: BrailleChars,
        constraints: Constraints,
    },
    Begmodeword {
        name: String,
        dots: BrailleChars,
        constraints: Constraints,
    },
    Begcapsword {
        dots: BrailleChars,
        constraints: Constraints,
    },
    Endcapsword {
        dots: BrailleChars,
        constraints: Constraints,
    },
    Capsmodechars {
        chars: String,
    },
    Begcaps {
        dots: BrailleChars,
    },
    Endcaps {
        dots: BrailleChars,
    },
    Begcapsphrase {
        dots: BrailleChars,
    },
    Endcapsphrase {
        dots: BrailleChars,
        position: Position,
    },
    Lencapsphrase {
        number: i32,
    },
    Letsign {
        dots: BrailleChars,
    },
    Noletsign {
        chars: String,
    },
    Noletsignbefore {
        chars: String,
    },
    Noletsignafter {
        chars: String,
    },
    Nocontractsign {
        dots: BrailleChars,
    },
    Numsign {
        dots: BrailleChars,
    },
    Nonumsign {
        dots: BrailleChars,
    },
    Numericnocontchars {
        chars: String,
    },
    Numericmodechars {
        chars: String,
    },
    Midendnumericmodechars {
        chars: String,
    },

    Begmodephrase {
        name: String,
        dots: BrailleChars,
    },
    Endmodephrase {
        name: String,
        dots: BrailleChars,
        position: Position,
    },
    Lenmodephrase {
        name: String,
        number: i32,
    },

    Seqdelimiter {
        chars: String,
    },
    Seqbeforechars {
        chars: String,
    },
    Seqafterchars {
        chars: String,
    },
    Seqafterpattern {
        chars: String,
    },
    Seqafterexpression {
        chars: String,
    },

    Class {
        name: String,
        chars: String,
    },
    Emphclass {
        name: String,
    },
    Begemph {
        name: String,
        dots: BrailleChars,
        constraints: Constraints,
    },
    Endemph {
        name: String,
        dots: BrailleChars,
        constraints: Constraints,
    },
    Noemphchars {
        name: String,
        chars: String,
    },
    Emphletter {
        name: String,
        dots: BrailleChars,
    },
    Begemphword {
        name: String,
        dots: BrailleChars,
    },
    Endemphword {
        name: String,
        dots: BrailleChars,
    },
    Emphmodechars {
        name: String,
        chars: String,
    },
    Begemphphrase {
        name: String,
        dots: BrailleChars,
    },
    Endemphphrase {
        name: String,
        dots: BrailleChars,
        position: Position,
    },
    Lenemphphrase {
        name: String,
        number: i32,
    },

    Begcomp {
        dots: BrailleChars,
        constraints: Constraints,
    },
    Endcomp {
        dots: BrailleChars,
        constraints: Constraints,
    },

    Decpoint {
        chars: String,
        dots: BrailleChars,
    },
    Hyphen {
        chars: String,
        dots: BrailleChars,
        constraints: Constraints,
    },

    Capsnocont {},

    Compbrl {
        chars: String,
        constraints: Constraints,
    },
    Comp6 {
        chars: String,
        dots: Braille,
    },
    Nocont {
        chars: String,
    },
    Replace {
        chars: String,
        replacement: Option<String>,
    },
    Always {
        chars: String,
        dots: Braille,
        constraints: Constraints,
    },
    Repeated {
        chars: String,
        dots: BrailleChars,
        constraints: Constraints,
    },
    Repword {
        chars: String,
        dots: BrailleChars,
    },
    Rependword {
        chars: String,
        dots: BrailleChars,
        other: BrailleChars,
    },
    Largesign {
        chars: String,
        dots: BrailleChars,
    },
    Word {
        chars: String,
        dots: Braille,
        constraints: Constraints,
    },
    Syllable {
        chars: String,
        dots: Braille,
    },
    Joinword {
        chars: String,
        dots: BrailleChars,
    },
    Lowword {
        chars: String,
        dots: BrailleChars,
        constraints: Constraints,
    },
    Contraction {
        chars: String,
    },
    Sufword {
        chars: String,
        dots: Braille,
        constraints: Constraints,
    },
    Prfword {
        chars: String,
        dots: Braille,
        constraints: Constraints,
    },
    Begword {
        chars: String,
        dots: Braille,
        constraints: Constraints,
    },
    Begmidword {
        chars: String,
        dots: Braille,
        constraints: Constraints,
    },
    Midword {
        chars: String,
        dots: Braille,
        constraints: Constraints,
    },
    Midendword {
        chars: String,
        dots: Braille,
        constraints: Constraints,
    },
    Endword {
        chars: String,
        dots: Braille,
        constraints: Constraints,
    },
    Partword {
        chars: String,
        dots: Braille,
        constraints: Constraints,
    },
    Exactdots {
        chars: String,
    },
    Prepunc {
        chars: String,
        dots: BrailleChars,
        constraints: Constraints,
    },
    Postpunc {
        chars: String,
        dots: BrailleChars,
        constraints: Constraints,
    },
    Begnum {
        chars: String,
        dots: BrailleChars,
        constraints: Constraints,
    },
    Midnum {
        chars: String,
        dots: BrailleChars,
        constraints: Constraints,
    },
    Endnum {
        chars: String,
        dots: Braille,
        constraints: Constraints,
    },
    Joinnum {
        chars: String,
        dots: BrailleChars,
        constraints: Constraints,
    },

    Swapcd {
        name: String,
        chars: String,
        dots: Vec<BrailleChars>,
    },
    Swapdd {
        name: String,
        dots: Vec<BrailleChars>,
        replacement: Vec<BrailleChars>,
    },
    Swapcc {
        name: String,
        chars: String,
        replacement: String,
    },

    Attribute {
        name: String,
        chars: String,
    },
    Context {
        test: multipass::Test,
        action: String,
        constraints: Constraints,
    },
    Pass2 {
        test: multipass::Test,
        action: String,
        constraints: Constraints,
    },
    Pass3 {
        test: multipass::Test,
        action: String,
        constraints: Constraints,
    },
    Pass4 {
        test: multipass::Test,
        action: String,
        constraints: Constraints,
    },
    Correct {
        test: multipass::Test,
        action: String,
        constraints: Constraints,
    },

    Match {
        pre: String,
        chars: String,
        post: String,
        dots: Braille,
        constraints: Constraints,
        matches: Option<WithMatches>,
    },
    Literal {
        chars: String,
    },
}

impl Rule {
    pub fn is_direction(&self, direction: Direction) -> bool {
        match self {
            Rule::Display { constraints, .. }
            | Rule::Multind { constraints, .. }
            | Rule::Space { constraints, .. }
            | Rule::Punctuation { constraints, .. }
            | Rule::Digit { constraints, .. }
            | Rule::Letter { constraints, .. }
            | Rule::Lowercase { constraints, .. }
            | Rule::Uppercase { constraints, .. }
            | Rule::Litdigit { constraints, .. }
            | Rule::Sign { constraints, .. }
            | Rule::Math { constraints, .. }
            | Rule::Modeletter { constraints, .. }
            | Rule::Capsletter { constraints, .. }
            | Rule::Begmodeword { constraints, .. }
            | Rule::Begcapsword { constraints, .. }
            | Rule::Endcapsword { constraints, .. }
            | Rule::Begemph { constraints, .. }
            | Rule::Endemph { constraints, .. }
            | Rule::Begcomp { constraints, .. }
            | Rule::Endcomp { constraints, .. }
            | Rule::Hyphen { constraints, .. }
            | Rule::Compbrl { constraints, .. }
            | Rule::Always { constraints, .. }
            | Rule::Repeated { constraints, .. }
            | Rule::Word { constraints, .. }
            | Rule::Lowword { constraints, .. }
            | Rule::Sufword { constraints, .. }
            | Rule::Prfword { constraints, .. }
            | Rule::Begword { constraints, .. }
            | Rule::Begmidword { constraints, .. }
            | Rule::Midword { constraints, .. }
            | Rule::Midendword { constraints, .. }
            | Rule::Endword { constraints, .. }
            | Rule::Partword { constraints, .. }
            | Rule::Prepunc { constraints, .. }
            | Rule::Postpunc { constraints, .. }
            | Rule::Begnum { constraints, .. }
            | Rule::Midnum { constraints, .. }
            | Rule::Endnum { constraints, .. }
            | Rule::Joinnum { constraints, .. }
            | Rule::Context { constraints, .. }
            | Rule::Pass2 { constraints, .. }
            | Rule::Pass3 { constraints, .. }
            | Rule::Pass4 { constraints, .. }
            | Rule::Correct { constraints, .. }
            | Rule::Match { constraints, .. } => {
                if direction == Direction::Forward {
                    !constraints.contains(Constraint::Nofor)
                } else {
                    !constraints.contains(Constraint::Noback)
                }
            }
            _ => true,
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum Braille {
    Implicit,
    Explicit(BrailleChars),
}

#[derive(PartialEq, Debug)]
pub enum Position {
    Before,
    After,
}

fn unescape_unicode(chars: &mut Chars, len: u8) -> Result<char, ParseError> {
    let mut s = String::new();

    for _ in 0..len {
        match chars.next() {
            Some(c) => s.push(c),
            _ => return Err(ParseError::InvalidUnicodeLiteral { found: None }),
        }
    }

    if let Ok(n) = u32::from_str_radix(&s, 16) {
        if let Some(c) = char::from_u32(n) {
            return Ok(c);
        }
    }
    Err(ParseError::InvalidUnicodeLiteral { found: Some(s) })
}

fn unescape(s: &str) -> Result<String, ParseError> {
    let mut iter = s.chars();
    let mut new = String::new();

    while let Some(c) = iter.next() {
        if c != '\\' {
            new.push(c);
            continue;
        }

        match iter.next() {
            Some('f') => new.push('\u{000C}'),
            Some('n') => new.push('\n'),
            Some('r') => new.push('\r'),
            Some('t') => new.push('\t'),
            Some('s') => new.push(' '),
            Some('v') => new.push('\u{000B}'),
            Some('e') => new.push('\u{001B}'),
            Some('\\') => new.push('\\'),
            Some('x') => new.push(unescape_unicode(&mut iter, 4)?),
            Some('y') => new.push(unescape_unicode(&mut iter, 5)?),
            _ => return Err(ParseError::InvalidEscape),
        };
    }
    Ok(new)
}

/// Return an error if actual contains more constraints than expected
fn fail_if_invalid_constraints(
    expected: Constraints,
    actual: Constraints,
    opcode: Opcode,
) -> Result<(), ParseError> {
    if !actual.is_subset(expected) {
        Err(ParseError::InvalidConstraints {
            constraints: actual.difference(expected),
            opcode,
        })
    } else {
        Ok(())
    }
}

pub struct RuleParser<'a> {
    tokens: Peekable<SplitWhitespace<'a>>,
}

impl<'a> RuleParser<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            tokens: source.split_whitespace().peekable(),
        }
    }

    fn nofor(&mut self) -> bool {
        self.tokens.next_if_eq(&"nofor").is_some()
    }

    fn noback(&mut self) -> bool {
        self.tokens.next_if_eq(&"noback").is_some()
    }

    fn nocross(&mut self) -> bool {
        self.tokens.next_if_eq(&"nocross").is_some()
    }

    fn constraints(&mut self) -> Constraints {
        let mut constraints = Constraints::EMPTY;
        if self.nofor() {
            constraints.insert(Constraint::Nofor);
        } else if self.noback() {
            constraints.insert(Constraint::Noback);
        }
        if self.nocross() {
            constraints.insert(Constraint::Nocross);
        }
        constraints
    }

    fn with_class(&mut self) -> Result<Option<WithClass>, ParseError> {
        if self.tokens.next_if_eq(&"before").is_some() {
            match self.tokens.next() {
                Some(class) => {
                    return Ok(Some(WithClass::Before {
                        class: class.into(),
                    }))
                }
                None => return Err(ParseError::ClassNameExpected { found: None }),
            }
        }
        if self.tokens.next_if_eq(&"after").is_some() {
            match self.tokens.next() {
                Some(class) => {
                    return Ok(Some(WithClass::After {
                        class: class.into(),
                    }))
                }
                None => return Err(ParseError::ClassNameExpected { found: None }),
            }
        }
        Ok(None)
    }

    fn with_classes(&mut self) -> Result<Option<WithClasses>, ParseError> {
        let mut classes = WithClasses::new();
        while self.tokens.peek() == Some(&"before") || self.tokens.peek() == Some(&"after") {
            if let Some(class) = self.with_class()? {
                classes.push(class);
            }
        }
        Ok(Some(classes))
    }

    fn with_matches(&mut self) -> Option<WithMatches> {
        let mut matches = WithMatches::new();
        // TODO: make sure "empmatchbefore empmatchbefore" is not allowed
        while self.tokens.peek() == Some(&"empmatchbefore")
            || self.tokens.peek() == Some(&"empmatchafter")
        {
            if self.tokens.next_if_eq(&"empmatchbefore").is_some() {
                matches.insert(WithMatch::Before);
            }
            if self.tokens.next_if_eq(&"empmatchafter").is_some() {
                matches.insert(WithMatch::After);
            }
        }
        if !matches.is_empty() {
            Some(matches)
        } else {
            None
        }
    }

    fn opcode(&mut self) -> Result<Opcode, ParseError> {
        if let Some(token) = self.tokens.next() {
            match token {
                "include" => Ok(Opcode::Include),
                "undefined" => Ok(Opcode::Undefined),
                "display" => Ok(Opcode::Display),
                "multind" => Ok(Opcode::Multind),
                "space" => Ok(Opcode::Space),
                "punctuation" => Ok(Opcode::Punctuation),
                "digit" => Ok(Opcode::Digit),
                "grouping" => Ok(Opcode::Grouping),
                "letter" => Ok(Opcode::Letter),
                "base" => Ok(Opcode::Base),
                "lowercase" => Ok(Opcode::Lowercase),
                "uppercase" => Ok(Opcode::Uppercase),
                "litdigit" => Ok(Opcode::Litdigit),
                "sign" => Ok(Opcode::Sign),
                "math" => Ok(Opcode::Math),

                // Braille Indicator Opcodes
                "modeletter" => Ok(Opcode::Modeletter),
                "capsletter" => Ok(Opcode::Capsletter),
                "begmodeword" => Ok(Opcode::Begmodeword),
                "begcapsword" => Ok(Opcode::Begcapsword),
                "endcapsword" => Ok(Opcode::Endcapsword),
                "capsmodechars" => Ok(Opcode::Capsmodechars),
                "begcaps" => Ok(Opcode::Begcaps),
                "endcaps" => Ok(Opcode::Endcaps),
                "begcapsphrase" => Ok(Opcode::Begcapsphrase),
                "endcapsphrase" => Ok(Opcode::Endcapsphrase),
                "lencapsphrase" => Ok(Opcode::Lencapsphrase),
                "letsign" => Ok(Opcode::Letsign),
                "noletsign" => Ok(Opcode::Noletsign),
                "noletsignbefore" => Ok(Opcode::Noletsignbefore),
                "noletsignafter" => Ok(Opcode::Noletsignafter),
                "nocontractsign" => Ok(Opcode::Nocontractsign),
                "numsign" => Ok(Opcode::Numsign),
                "nonumsign" => Ok(Opcode::Nonumsign),
                "numericnocontchars" => Ok(Opcode::Numericnocontchars),
                "numericmodechars" => Ok(Opcode::Numericmodechars),
                "midendnumericmodechars" => Ok(Opcode::Midendnumericmodechars),

                "begmodephrase" => Ok(Opcode::Begmodephrase),
                "endmodephrase" => Ok(Opcode::Endmodephrase),
                "lenmodephrase" => Ok(Opcode::Lenmodephrase),

                // Opcodes for Standing Alone Sequences
                "seqdelimiter" => Ok(Opcode::Seqdelimiter),
                "seqbeforechars" => Ok(Opcode::Seqbeforechars),
                "seqafterchars" => Ok(Opcode::Seqafterchars),
                "seqafterpattern" => Ok(Opcode::Seqafterpattern),
                "seqafterexpression" => Ok(Opcode::Seqafterexpression),

                // Emphasis Opcodes
                "class" => Ok(Opcode::Class),
                "emphclass" => Ok(Opcode::Emphclass),
                "begemph" => Ok(Opcode::Begemph),
                "endemph" => Ok(Opcode::Endemph),
                "noemphchars" => Ok(Opcode::Noemphchars),
                "emphletter" => Ok(Opcode::Emphletter),
                "begemphword" => Ok(Opcode::Begemphword),
                "endemphword" => Ok(Opcode::Endemphword),
                "emphmodechars" => Ok(Opcode::Emphmodechars),
                "begemphphrase" => Ok(Opcode::Begemphphrase),
                "endemphphrase" => Ok(Opcode::Endemphphrase),
                "lenemphphrase" => Ok(Opcode::Lenemphphrase),

                // Special Symbol Opcodes
                "decpoint" => Ok(Opcode::Decpoint),
                "hyphen" => Ok(Opcode::Hyphen),

                // Special Processing Opcodes
                "capsnocont" => Ok(Opcode::Capsnocont),

                // Translation Opcodes
                "compbrl" => Ok(Opcode::Compbrl),
                "comp6" => Ok(Opcode::Comp6),
                "nocont" => Ok(Opcode::Nocont),
                "replace" => Ok(Opcode::Replace),
                "always" => Ok(Opcode::Always),
                "repeated" => Ok(Opcode::Repeated),
                "repword" => Ok(Opcode::Repword),
                "rependword" => Ok(Opcode::Rependword),
                "largesign" => Ok(Opcode::Largesign),
                "word" => Ok(Opcode::Word),
                "syllable" => Ok(Opcode::Syllable),
                "joinword" => Ok(Opcode::Joinword),
                "lowword" => Ok(Opcode::Lowword),
                "contraction" => Ok(Opcode::Contraction),
                "sufword" => Ok(Opcode::Sufword),
                "prfword" => Ok(Opcode::Prfword),
                "begword" => Ok(Opcode::Begword),
                "begmidword" => Ok(Opcode::Begmidword),
                "midword" => Ok(Opcode::Midword),
                "midendword" => Ok(Opcode::Midendword),
                "endword" => Ok(Opcode::Endword),
                "partword" => Ok(Opcode::Partword),
                "exactdots" => Ok(Opcode::Exactdots),
                "prepunc" => Ok(Opcode::Prepunc),
                "postpunc" => Ok(Opcode::Postpunc),
                "begnum" => Ok(Opcode::Begnum),
                "midnum" => Ok(Opcode::Midnum),
                "endnum" => Ok(Opcode::Endnum),
                "joinnum" => Ok(Opcode::Joinnum),

                // Computer braille
                "begcomp" => Ok(Opcode::Begcomp),
                "endcomp" => Ok(Opcode::Endcomp),

                // Character-Class Opcodes
                "attribute" => Ok(Opcode::Attribute),

                // Swap Opcodes
                "swapcd" => Ok(Opcode::Swapcd),
                "swapdd" => Ok(Opcode::Swapdd),
                "swapcc" => Ok(Opcode::Swapcc),

                // Context and Multipass Opcodes
                "context" => Ok(Opcode::Context),
                "pass2" => Ok(Opcode::Pass2),
                "pass3" => Ok(Opcode::Pass3),
                "pass4" => Ok(Opcode::Pass4),

                // The correct Opcode
                "correct" => Ok(Opcode::Correct),

                // The match Opcode
                "match" => Ok(Opcode::Match),
                "literal" => Ok(Opcode::Literal),
                unknown => Err(ParseError::OpcodeExpected {
                    found: Some(unknown.into()),
                }),
            }
        } else {
            Err(ParseError::OpcodeExpected { found: None })
        }
    }

    fn name(&mut self) -> Result<String, ParseError> {
        let name = self
            .tokens
            .next()
            .ok_or(ParseError::NameExpected)
            .map(|s| s.to_string())?;
        if name.chars().all(|c| c.is_ascii_alphanumeric()) {
            Ok(name)
        } else {
            Err(ParseError::InvalidName { name })
        }
    }

    fn many_names(&mut self) -> Result<Vec<String>, ParseError> {
        let mut names: Vec<String> = Vec::new();
        while let Some(name) = self.tokens.next() {
            names.push(name.into());
        }
        if names.len() > 1 {
            Ok(names)
        } else {
            Err(ParseError::TokenExpected {
                expected: "At least one name expected".into(),
                found: None,
            })
        }
    }

    fn one_char(&mut self) -> Result<char, ParseError> {
        let s = self
            .tokens
            .next()
            .ok_or(ParseError::SingleCharExpected { found: None })?;
        let s = unescape(s)?;
        if s.chars().count() == 1 {
            Ok(s.chars().next().unwrap())
        } else {
            Err(ParseError::SingleCharExpected { found: Some(s) })
        }
    }

    fn number(&mut self) -> Result<i32, ParseError> {
        let s = self
            .tokens
            .next()
            .ok_or(ParseError::NumberExpected { found: None })?;
        let number = s.parse::<i32>()?;
        Ok(number)
    }

    fn chars(&mut self) -> Result<String, ParseError> {
        let s = self.tokens.next().ok_or(ParseError::CharsExpected)?;
        unescape(s)
    }

    fn maybe_chars(&mut self) -> Option<String> {
        self.tokens.next().map(|s| s.to_string())
    }

    fn filename(&mut self) -> Result<String, ParseError> {
        self.tokens
            .next()
            .ok_or(ParseError::FilenameExpected)
            .map(|s| s.to_string())
    }

    fn position(&mut self) -> Result<Position, ParseError> {
        if self.tokens.next_if_eq(&"before").is_some() {
            return Ok(Position::Before);
        }
        if self.tokens.next_if_eq(&"after").is_some() {
            return Ok(Position::After);
        }
        Err(ParseError::TokenExpected {
            expected: "Before/After".into(),
            found: self.tokens.peek().map(|&s| s.into()),
        })
    }

    fn dots(&mut self) -> Result<Braille, ParseError> {
        let token = self.tokens.next().ok_or(ParseError::DotsExpected)?;
        if token == "=" {
            Ok(Braille::Implicit)
        } else {
            Ok(Braille::Explicit(braille_chars(token)?))
        }
    }

    fn explicit_dots(&mut self) -> Result<BrailleChars, ParseError> {
        self.tokens
            .next()
            .ok_or(ParseError::DotsExpected)?
            .split('-')
            .map(chars_to_dots)
            // FIXME: it feels really weird that we manually have to
            // map braille::ParseError to ParseError. I thought the
            // thisError crate implements the from trait between the
            // two?
            .map(|r| r.map_err(ParseError::InvalidBraille))
            .collect()
    }

    fn many_dots(&mut self) -> Result<Vec<BrailleChars>, ParseError> {
        self.tokens
            .next()
            .ok_or(ParseError::DotsExpected)?
            .split(',')
            .map(|chars| {
                chars
                    .split('-')
                    .map(chars_to_dots)
                    // FIXME: it feels really weird that we manually have to
                    // map braille::ParseError to ParseError.
                    .map(|r| r.map_err(ParseError::InvalidBraille))
                    .collect()
            })
            .collect()
    }

    fn multipass_test(&mut self) -> Result<Test, ParseError> {
        self.tokens
            .next()
            .ok_or(ParseError::MultipassTestExpected)
            .map(|s| s.to_string())
            .map(|s| {
                multipass::TestParser::new(&s)
                    .tests()
                    .map_err(ParseError::InvalidMultipassTest)
            })?
    }

    fn multipass_action(&mut self) -> Result<String, ParseError> {
        self.tokens
            .next()
            .ok_or(ParseError::MultipassActionExpected)
            .map(|s| s.to_string())
    }

    fn match_pre(&mut self) -> Result<String, ParseError> {
        self.tokens
            .next()
            .ok_or(ParseError::MatchPreExpected)
            .map(|s| s.to_string())
    }

    fn match_post(&mut self) -> Result<String, ParseError> {
        self.tokens
            .next()
            .ok_or(ParseError::MatchPostExpected)
            .map(|s| s.to_string())
    }

    pub fn rule(&mut self) -> Result<Rule, ParseError> {
        let constraints = self.constraints();
        let _classes = self.with_classes();
        let matches = self.with_matches();
        let opcode = self.opcode()?;
        let rule = match opcode {
            Opcode::Include => {
                fail_if_invalid_constraints(Constraints::EMPTY, constraints, opcode)?;
                Rule::Include {
                    file: self.filename()?,
                }
            }
            Opcode::Undefined => {
                fail_if_invalid_constraints(Constraints::EMPTY, constraints, opcode)?;
                Rule::Undefined {
                    dots: self.explicit_dots()?,
                }
            }
            Opcode::Display => {
                fail_if_invalid_constraints(DIRECTIONS, constraints, opcode)?;
                Rule::Display {
                    character: self.one_char()?,
                    dots: self.explicit_dots()?,
                    constraints,
                }
            }
            Opcode::Multind => {
                fail_if_invalid_constraints(DIRECTIONS, constraints, opcode)?;
                Rule::Multind {
                    dots: self.explicit_dots()?,
                    names: self.many_names()?,
                    constraints,
                }
            }
            Opcode::Space => {
                fail_if_invalid_constraints(DIRECTIONS, constraints, opcode)?;
                Rule::Space {
                    character: self.one_char()?,
                    dots: self.explicit_dots()?,
                    constraints,
                }
            }
            Opcode::Punctuation => {
                fail_if_invalid_constraints(DIRECTIONS, constraints, opcode)?;
                Rule::Punctuation {
                    character: self.one_char()?,
                    dots: self.explicit_dots()?,
                    constraints,
                }
            }
            Opcode::Digit => {
                fail_if_invalid_constraints(DIRECTIONS, constraints, opcode)?;
                Rule::Digit {
                    character: self.one_char()?,
                    dots: self.explicit_dots()?,
                    constraints,
                }
            }
            Opcode::Grouping => {
                fail_if_invalid_constraints(DIRECTIONS, constraints, opcode)?;
                Rule::Grouping {
                    name: self.name()?,
                    chars: self.chars()?,
                    dots: self.explicit_dots()?,
                }
            }
            Opcode::Letter => {
                fail_if_invalid_constraints(DIRECTIONS, constraints, opcode)?;
                Rule::Letter {
                    character: self.one_char()?,
                    dots: self.dots()?,
                    constraints,
                }
            }
            Opcode::Base => {
                fail_if_invalid_constraints(Constraints::EMPTY, constraints, opcode)?;
                Rule::Base {
                    name: self.name()?,
                    derived: self.one_char()?,
                    base: self.one_char()?,
                }
            }
            Opcode::Lowercase => {
                fail_if_invalid_constraints(DIRECTIONS, constraints, opcode)?;
                Rule::Lowercase {
                    character: self.one_char()?,
                    dots: self.explicit_dots()?,
                    constraints,
                }
            }
            Opcode::Uppercase => {
                fail_if_invalid_constraints(DIRECTIONS, constraints, opcode)?;
                Rule::Uppercase {
                    character: self.one_char()?,
                    dots: self.explicit_dots()?,
                    constraints,
                }
            }
            Opcode::Litdigit => {
                fail_if_invalid_constraints(DIRECTIONS, constraints, opcode)?;
                Rule::Litdigit {
                    character: self.one_char()?,
                    dots: self.explicit_dots()?,
                    constraints,
                }
            }
            Opcode::Sign => {
                fail_if_invalid_constraints(DIRECTIONS, constraints, opcode)?;
                Rule::Sign {
                    character: self.one_char()?,
                    dots: self.explicit_dots()?,
                    constraints,
                }
            }
            Opcode::Math => {
                fail_if_invalid_constraints(DIRECTIONS, constraints, opcode)?;
                Rule::Math {
                    character: self.one_char()?,
                    dots: self.explicit_dots()?,
                    constraints,
                }
            }

            Opcode::Modeletter => {
                fail_if_invalid_constraints(DIRECTIONS, constraints, opcode)?;
                Rule::Modeletter {
                    name: self.name()?,
                    dots: self.explicit_dots()?,
                    constraints,
                }
            }
            Opcode::Capsletter => {
                fail_if_invalid_constraints(DIRECTIONS, constraints, opcode)?;
                Rule::Capsletter {
                    dots: self.explicit_dots()?,
                    constraints,
                }
            }
            Opcode::Begmodeword => {
                fail_if_invalid_constraints(DIRECTIONS, constraints, opcode)?;
                Rule::Begmodeword {
                    name: self.name()?,
                    dots: self.explicit_dots()?,
                    constraints,
                }
            }
            Opcode::Begcapsword => {
                fail_if_invalid_constraints(DIRECTIONS, constraints, opcode)?;
                Rule::Begcapsword {
                    dots: self.explicit_dots()?,
                    constraints,
                }
            }
            Opcode::Endcapsword => {
                fail_if_invalid_constraints(DIRECTIONS, constraints, opcode)?;
                Rule::Endcapsword {
                    dots: self.explicit_dots()?,
                    constraints,
                }
            }
            Opcode::Capsmodechars => {
                fail_if_invalid_constraints(Constraints::EMPTY, constraints, opcode)?;
                Rule::Capsmodechars {
                    chars: self.chars()?,
                }
            }
            Opcode::Begcaps => {
                fail_if_invalid_constraints(Constraints::EMPTY, constraints, opcode)?;
                Rule::Begcaps {
                    dots: self.explicit_dots()?,
                }
            }
            Opcode::Endcaps => {
                fail_if_invalid_constraints(Constraints::EMPTY, constraints, opcode)?;
                Rule::Endcaps {
                    dots: self.explicit_dots()?,
                }
            }
            Opcode::Begcapsphrase => {
                fail_if_invalid_constraints(Constraints::EMPTY, constraints, opcode)?;
                Rule::Begcapsphrase {
                    dots: self.explicit_dots()?,
                }
            }
            Opcode::Endcapsphrase => {
                fail_if_invalid_constraints(Constraints::EMPTY, constraints, opcode)?;
                Rule::Endcapsphrase {
                    position: self.position()?,
                    dots: self.explicit_dots()?,
                }
            }
            Opcode::Lencapsphrase => {
                fail_if_invalid_constraints(Constraints::EMPTY, constraints, opcode)?;
                Rule::Lencapsphrase {
                    number: self.number()?,
                }
            }
            Opcode::Letsign => {
                fail_if_invalid_constraints(Constraints::EMPTY, constraints, opcode)?;
                Rule::Letsign {
                    dots: self.explicit_dots()?,
                }
            }
            Opcode::Noletsign => {
                fail_if_invalid_constraints(Constraints::EMPTY, constraints, opcode)?;
                Rule::Noletsign {
                    chars: self.chars()?,
                }
            }
            Opcode::Noletsignbefore => {
                fail_if_invalid_constraints(Constraints::EMPTY, constraints, opcode)?;
                Rule::Noletsignbefore {
                    chars: self.chars()?,
                }
            }
            Opcode::Noletsignafter => {
                fail_if_invalid_constraints(Constraints::EMPTY, constraints, opcode)?;
                Rule::Noletsignafter {
                    chars: self.chars()?,
                }
            }
            Opcode::Nocontractsign => {
                fail_if_invalid_constraints(Constraints::EMPTY, constraints, opcode)?;
                Rule::Nocontractsign {
                    dots: self.explicit_dots()?,
                }
            }
            Opcode::Numsign => {
                fail_if_invalid_constraints(Constraints::EMPTY, constraints, opcode)?;
                Rule::Numsign {
                    dots: self.explicit_dots()?,
                }
            }
            Opcode::Nonumsign => {
                fail_if_invalid_constraints(Constraints::EMPTY, constraints, opcode)?;
                Rule::Nonumsign {
                    dots: self.explicit_dots()?,
                }
            }
            Opcode::Numericnocontchars => {
                fail_if_invalid_constraints(Constraints::EMPTY, constraints, opcode)?;
                Rule::Numericnocontchars {
                    chars: self.chars()?,
                }
            }
            Opcode::Numericmodechars => {
                fail_if_invalid_constraints(Constraints::EMPTY, constraints, opcode)?;
                Rule::Numericmodechars {
                    chars: self.chars()?,
                }
            }
            Opcode::Midendnumericmodechars => {
                fail_if_invalid_constraints(Constraints::EMPTY, constraints, opcode)?;
                Rule::Midendnumericmodechars {
                    chars: self.chars()?,
                }
            }

            Opcode::Begmodephrase => {
                fail_if_invalid_constraints(Constraints::EMPTY, constraints, opcode)?;
                Rule::Begmodephrase {
                    name: self.name()?,
                    dots: self.explicit_dots()?,
                }
            }
            Opcode::Endmodephrase => {
                fail_if_invalid_constraints(Constraints::EMPTY, constraints, opcode)?;
                Rule::Endmodephrase {
                    name: self.name()?,
                    position: self.position()?,
                    dots: self.explicit_dots()?,
                }
            }
            Opcode::Lenmodephrase => {
                fail_if_invalid_constraints(Constraints::EMPTY, constraints, opcode)?;
                Rule::Lenmodephrase {
                    name: self.name()?,
                    number: self.number()?,
                }
            }

            Opcode::Seqdelimiter => {
                fail_if_invalid_constraints(Constraints::EMPTY, constraints, opcode)?;
                Rule::Seqdelimiter {
                    chars: self.chars()?,
                }
            }
            Opcode::Seqbeforechars => {
                fail_if_invalid_constraints(Constraints::EMPTY, constraints, opcode)?;
                Rule::Seqbeforechars {
                    chars: self.chars()?,
                }
            }
            Opcode::Seqafterchars => {
                fail_if_invalid_constraints(Constraints::EMPTY, constraints, opcode)?;
                Rule::Seqafterchars {
                    chars: self.chars()?,
                }
            }
            Opcode::Seqafterpattern => {
                fail_if_invalid_constraints(Constraints::EMPTY, constraints, opcode)?;
                Rule::Seqafterpattern {
                    chars: self.chars()?,
                }
            }
            Opcode::Seqafterexpression => {
                fail_if_invalid_constraints(Constraints::EMPTY, constraints, opcode)?;
                Rule::Seqafterexpression {
                    chars: self.chars()?,
                }
            }

            Opcode::Class => {
                fail_if_invalid_constraints(Constraints::EMPTY, constraints, opcode)?;
                Rule::Class {
                    name: self.name()?,
                    chars: self.chars()?,
                }
            }
            Opcode::Emphclass => {
                fail_if_invalid_constraints(Constraints::EMPTY, constraints, opcode)?;
                Rule::Emphclass { name: self.name()? }
            }
            Opcode::Begemph => {
                fail_if_invalid_constraints(DIRECTIONS, constraints, opcode)?;
                Rule::Begemph {
                    name: self.name()?,
                    dots: self.explicit_dots()?,
                    constraints,
                }
            }
            Opcode::Endemph => {
                fail_if_invalid_constraints(DIRECTIONS, constraints, opcode)?;
                Rule::Endemph {
                    name: self.name()?,
                    dots: self.explicit_dots()?,
                    constraints,
                }
            }
            Opcode::Noemphchars => {
                fail_if_invalid_constraints(Constraints::EMPTY, constraints, opcode)?;
                Rule::Noemphchars {
                    name: self.name()?,
                    chars: self.chars()?,
                }
            }
            Opcode::Emphletter => {
                fail_if_invalid_constraints(Constraints::EMPTY, constraints, opcode)?;
                Rule::Emphletter {
                    name: self.name()?,
                    dots: self.explicit_dots()?,
                }
            }
            Opcode::Begemphword => {
                fail_if_invalid_constraints(Constraints::EMPTY, constraints, opcode)?;
                Rule::Begemphword {
                    name: self.name()?,
                    dots: self.explicit_dots()?,
                }
            }
            Opcode::Endemphword => {
                fail_if_invalid_constraints(Constraints::EMPTY, constraints, opcode)?;
                Rule::Endemphword {
                    name: self.name()?,
                    dots: self.explicit_dots()?,
                }
            }
            Opcode::Emphmodechars => {
                fail_if_invalid_constraints(Constraints::EMPTY, constraints, opcode)?;
                Rule::Emphmodechars {
                    name: self.name()?,
                    chars: self.chars()?,
                }
            }
            Opcode::Begemphphrase => {
                fail_if_invalid_constraints(Constraints::EMPTY, constraints, opcode)?;
                Rule::Begemphphrase {
                    name: self.name()?,
                    dots: self.explicit_dots()?,
                }
            }
            Opcode::Endemphphrase => {
                fail_if_invalid_constraints(Constraints::EMPTY, constraints, opcode)?;
                Rule::Endemphphrase {
                    name: self.name()?,
                    position: self.position()?,
                    dots: self.explicit_dots()?,
                }
            }
            Opcode::Lenemphphrase => {
                fail_if_invalid_constraints(Constraints::EMPTY, constraints, opcode)?;
                Rule::Lenemphphrase {
                    name: self.name()?,
                    number: self.number()?,
                }
            }

            Opcode::Begcomp => {
                fail_if_invalid_constraints(DIRECTIONS, constraints, opcode)?;
                Rule::Begcomp {
                    dots: self.explicit_dots()?,
                    constraints,
                }
            }
            Opcode::Endcomp => {
                fail_if_invalid_constraints(DIRECTIONS, constraints, opcode)?;
                Rule::Endcomp {
                    dots: self.explicit_dots()?,
                    constraints,
                }
            }

            Opcode::Decpoint => {
                fail_if_invalid_constraints(Constraints::EMPTY, constraints, opcode)?;
                Rule::Decpoint {
                    chars: self.chars()?,
                    dots: self.explicit_dots()?,
                }
            }
            Opcode::Hyphen => {
                fail_if_invalid_constraints(DIRECTIONS, constraints, opcode)?;
                Rule::Hyphen {
                    chars: self.chars()?,
                    dots: self.explicit_dots()?,
                    constraints,
                }
            }

            Opcode::Capsnocont => {
                fail_if_invalid_constraints(Constraints::EMPTY, constraints, opcode)?;
                Rule::Capsnocont {}
            }

            Opcode::Compbrl => {
                fail_if_invalid_constraints(DIRECTIONS, constraints, opcode)?;
                Rule::Compbrl {
                    chars: self.chars()?,
                    constraints,
                }
            }
            Opcode::Comp6 => {
                fail_if_invalid_constraints(Constraints::EMPTY, constraints, opcode)?;
                Rule::Comp6 {
                    chars: self.chars()?,
                    dots: self.dots()?,
                }
            }
            Opcode::Nocont => {
                fail_if_invalid_constraints(Constraints::EMPTY, constraints, opcode)?;
                Rule::Nocont {
                    chars: self.chars()?,
                }
            }
            Opcode::Replace => {
                fail_if_invalid_constraints(Constraints::EMPTY, constraints, opcode)?;
                Rule::Replace {
                    chars: self.chars()?,
                    replacement: self.maybe_chars(),
                }
            }
            Opcode::Always => Rule::Always {
                chars: self.chars()?,
                dots: self.dots()?,
                constraints,
            },
            Opcode::Repeated => {
                fail_if_invalid_constraints(DIRECTIONS, constraints, opcode)?;
                Rule::Repeated {
                    chars: self.chars()?,
                    dots: self.explicit_dots()?,
                    constraints,
                }
            }
            Opcode::Repword => {
                fail_if_invalid_constraints(Constraints::EMPTY, constraints, opcode)?;
                Rule::Repword {
                    chars: self.chars()?,
                    dots: self.explicit_dots()?,
                }
            }
            Opcode::Rependword => {
                fail_if_invalid_constraints(Constraints::EMPTY, constraints, opcode)?;
                let chars = self.chars()?;
                let many_dots = self.many_dots()?;
                if many_dots.len() != 2 {
                    return Err(ParseError::DotsTupleExpected);
                }
                let mut iterator = many_dots.into_iter();
                let dots = iterator.next().unwrap();
                let other = iterator.next().unwrap();
                Rule::Rependword { chars, dots, other }
            }
            Opcode::Largesign => {
                fail_if_invalid_constraints(Constraints::EMPTY, constraints, opcode)?;
                Rule::Largesign {
                    chars: self.chars()?,
                    dots: self.explicit_dots()?,
                }
            }
            Opcode::Word => {
                fail_if_invalid_constraints(DIRECTIONS, constraints, opcode)?;
                Rule::Word {
                    chars: self.chars()?,
                    dots: self.dots()?,
                    constraints,
                }
            }
            Opcode::Syllable => {
                fail_if_invalid_constraints(Constraints::EMPTY, constraints, opcode)?;
                Rule::Syllable {
                    chars: self.chars()?,
                    dots: self.dots()?,
                }
            }
            Opcode::Joinword => {
                fail_if_invalid_constraints(Constraints::EMPTY, constraints, opcode)?;
                Rule::Joinword {
                    chars: self.chars()?,
                    dots: self.explicit_dots()?,
                }
            }
            Opcode::Lowword => {
                fail_if_invalid_constraints(DIRECTIONS, constraints, opcode)?;
                Rule::Lowword {
                    chars: self.chars()?,
                    dots: self.explicit_dots()?,
                    constraints,
                }
            }
            Opcode::Contraction => {
                fail_if_invalid_constraints(Constraints::EMPTY, constraints, opcode)?;
                Rule::Contraction {
                    chars: self.chars()?,
                }
            }
            Opcode::Sufword => Rule::Sufword {
                chars: self.chars()?,
                dots: self.dots()?,
                constraints,
            },
            Opcode::Prfword => Rule::Prfword {
                chars: self.chars()?,
                dots: self.dots()?,
                constraints,
            },
            Opcode::Begword => Rule::Begword {
                chars: self.chars()?,
                dots: self.dots()?,
                constraints,
            },
            Opcode::Begmidword => Rule::Begmidword {
                chars: self.chars()?,
                dots: self.dots()?,
                constraints,
            },
            Opcode::Midword => Rule::Midword {
                chars: self.chars()?,
                dots: self.dots()?,
                constraints,
            },
            Opcode::Midendword => Rule::Midendword {
                chars: self.chars()?,
                dots: self.dots()?,
                constraints,
            },
            Opcode::Endword => Rule::Endword {
                chars: self.chars()?,
                dots: self.dots()?,
                constraints,
            },
            Opcode::Partword => Rule::Partword {
                chars: self.chars()?,
                dots: self.dots()?,
                constraints,
            },
            Opcode::Exactdots => {
                fail_if_invalid_constraints(Constraints::EMPTY, constraints, opcode)?;
                Rule::Exactdots {
                    chars: self.chars()?,
                }
            }
            Opcode::Prepunc => Rule::Prepunc {
                chars: self.chars()?,
                dots: self.explicit_dots()?,
                constraints,
            },
            Opcode::Postpunc => Rule::Postpunc {
                chars: self.chars()?,
                dots: self.explicit_dots()?,
                constraints,
            },
            Opcode::Begnum => Rule::Begnum {
                chars: self.chars()?,
                dots: self.explicit_dots()?,
                constraints,
            },
            Opcode::Midnum => Rule::Midnum {
                chars: self.chars()?,
                dots: self.explicit_dots()?,
                constraints,
            },
            Opcode::Endnum => Rule::Endnum {
                chars: self.chars()?,
                dots: self.dots()?,
                constraints,
            },
            Opcode::Joinnum => Rule::Joinnum {
                chars: self.chars()?,
                dots: self.explicit_dots()?,
                constraints,
            },

            Opcode::Attribute => {
                fail_if_invalid_constraints(Constraints::EMPTY, constraints, opcode)?;
                Rule::Attribute {
                    name: self.name()?,
                    chars: self.chars()?,
                }
            }

            Opcode::Swapcd => {
                fail_if_invalid_constraints(Constraints::EMPTY, constraints, opcode)?;
                Rule::Swapcd {
                    name: self.name()?,
                    chars: self.chars()?,
                    dots: self.many_dots()?,
                }
            }
            Opcode::Swapdd => {
                fail_if_invalid_constraints(Constraints::EMPTY, constraints, opcode)?;
                Rule::Swapdd {
                    name: self.name()?,
                    dots: self.many_dots()?,
                    replacement: self.many_dots()?,
                }
            }
            Opcode::Swapcc => {
                fail_if_invalid_constraints(Constraints::EMPTY, constraints, opcode)?;
                Rule::Swapcc {
                    name: self.name()?,
                    chars: self.chars()?,
                    replacement: self.chars()?,
                }
            }

            Opcode::Context => {
                fail_if_invalid_constraints(DIRECTIONS, constraints, opcode)?;
                Rule::Context {
                    test: self.multipass_test()?,
                    action: self.multipass_action()?,
                    constraints,
                }
            }
            Opcode::Pass2 => {
                fail_if_invalid_constraints(DIRECTIONS, constraints, opcode)?;
                Rule::Pass2 {
                    test: self.multipass_test()?,
                    action: self.multipass_action()?,
                    constraints,
                }
            }
            Opcode::Pass3 => {
                fail_if_invalid_constraints(DIRECTIONS, constraints, opcode)?;
                Rule::Pass3 {
                    test: self.multipass_test()?,
                    action: self.multipass_action()?,
                    constraints,
                }
            }
            Opcode::Pass4 => {
                fail_if_invalid_constraints(DIRECTIONS, constraints, opcode)?;
                Rule::Pass4 {
                    test: self.multipass_test()?,
                    action: self.multipass_action()?,
                    constraints,
                }
            }
            Opcode::Correct => {
                fail_if_invalid_constraints(DIRECTIONS, constraints, opcode)?;
                Rule::Correct {
                    test: self.multipass_test()?,
                    action: self.multipass_action()?,
                    constraints,
                }
            }

            Opcode::Match => {
                fail_if_invalid_constraints(DIRECTIONS, constraints, opcode)?;
                Rule::Match {
                    pre: self.match_pre()?,
                    chars: self.chars()?,
                    post: self.match_post()?,
                    dots: self.dots()?,
                    matches,
                    constraints,
                }
            }
            Opcode::Literal => {
                fail_if_invalid_constraints(Constraints::EMPTY, constraints, opcode)?;
                Rule::Literal {
                    chars: self.chars()?,
                }
            }
        };
        Ok(rule)
    }
}

#[derive(thiserror::Error, Debug)]
pub enum TableError {
    #[error("Parse error {error:?} on line {line:?}")]
    ParseError {
        path: Option<PathBuf>,
        line: usize,
        error: ParseError,
    },
    #[error("Cannot read table")]
    TableNotReadable(#[from] io::Error),
    #[error("Cannot find table {0:?}")]
    TableNotFound(PathBuf),
    #[error("Table format not supported {0:?}")]
    FormatNotSupported(PathBuf),
}

#[derive(PartialEq, Debug)]
pub struct AnchoredRule {
    pub rule: Rule,
    pub path: Option<PathBuf>,
    pub line: usize,
}

impl From<Rule> for AnchoredRule {
    fn from(rule: Rule) -> Self {
        Self {
            rule,
            path: None,
            line: 0,
        }
    }
}

pub fn table(table: &str, path: Option<PathBuf>) -> Result<Vec<AnchoredRule>, Vec<TableError>> {
    let (rules, errors): (Vec<_>, Vec<_>) = table
        .lines()
        .enumerate()
        // drop comments and empty lines
        .filter(|(_, line)| !line.trim().starts_with('#') && !line.trim().is_empty())
        .map(|(line_no, line)| {
            // parse the line
            RuleParser::new(line)
                .rule()
                .map(|rule| AnchoredRule {
                    rule,
                    path: path.clone(),
                    line: line_no,
                })
                .map_err(|e| TableError::ParseError {
                    path: path.clone(),
                    line: line_no,
                    error: e,
                })
        })
        .partition(Result::is_ok);
    let rules: Vec<_> = rules.into_iter().map(Result::unwrap).collect();
    let errors: Vec<_> = errors.into_iter().map(Result::unwrap_err).collect();
    if errors.is_empty() {
        Ok(rules)
    } else {
        Err(errors)
    }
}

pub fn table_file(path: &Path) -> Result<Vec<AnchoredRule>, Vec<TableError>> {
    let text = read_to_string(path).map_err(|e| vec![TableError::TableNotReadable(e)])?;
    table(&text, Some(path.into()))
}

pub fn table_expanded(file: &Path) -> Result<Vec<AnchoredRule>, Vec<TableError>> {
    let search_path = &SearchPath::new_or("LOUIS_TABLE_PATH", ".");
    let path = search_path.find_file(file);
    match path {
        Some(path) => {
            let rules = table_file(path.as_path())?;
            let rules = expand_includes(rules)?;
            Ok(rules)
        }
        _ => Err(vec![TableError::TableNotFound(file.into())]),
    }
}

fn expand_include(rule: AnchoredRule) -> Result<Vec<AnchoredRule>, Vec<TableError>> {
    let search_path = &SearchPath::new_or("LOUIS_TABLE_PATH", ".");
    if let Rule::Include { ref file } = rule.rule {
        let path = Path::new(file);
        if path.extension().and_then(OsStr::to_str) == Some("dic") {
            return Err(vec![TableError::FormatNotSupported(path.into())]);
        }
        let path = search_path
            .find_file(path)
            .ok_or(vec![TableError::TableNotFound(path.into())])?;
        let rules = table_expanded(&path)?;
        Ok(rules)
    } else {
        Ok(vec![rule])
    }
}

pub fn expand_includes(rules: Vec<AnchoredRule>) -> Result<Vec<AnchoredRule>, Vec<TableError>> {
    let (rules, errors): (Vec<_>, Vec<_>) = rules
        .into_iter()
        .map(expand_include)
        .partition(Result::is_ok);
    let rules: Vec<_> = rules.into_iter().flat_map(Result::unwrap).collect();
    let errors: Vec<_> = errors.into_iter().flat_map(Result::unwrap_err).collect();
    if errors.is_empty() {
        Ok(rules)
    } else {
        Err(errors)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use enumset::enum_set;

    #[test]
    fn nocross_test() {
        assert_eq!(true, RuleParser::new(&"nocross").nocross());
        assert_eq!(true, RuleParser::new(&"nocross nofor").nocross());
        assert_eq!(false, RuleParser::new(&"nofor nocross").nocross());
        assert_eq!(false, RuleParser::new(&"nofor").nocross());
    }

    #[test]
    fn nofor_test() {
        assert_eq!(true, RuleParser::new(&" nofor ").nofor());
        assert_eq!(true, RuleParser::new(&"nofor nocross").nofor());
        assert_eq!(false, RuleParser::new(&"nocross nofor").nofor());
        assert_eq!(false, RuleParser::new(&"").nofor());
    }

    #[test]
    fn constraints_test() {
        assert_eq!(
            enum_set!(Constraint::Nofor),
            RuleParser::new(&" nofor ").constraints()
        );
        assert_eq!(
            enum_set!(Constraint::Nofor | Constraint::Nocross),
            RuleParser::new(&"nofor nocross").constraints()
        );
    }

    #[test]
    fn fail_if_invalid_constraints_test() {
        assert_eq!(
            Ok(()),
            fail_if_invalid_constraints(Constraints::empty(), Constraints::empty(), Opcode::Space)
        );
        assert_eq!(
            Err(ParseError::InvalidConstraints {
                constraints: enum_set!(Constraint::Nofor),
                opcode: Opcode::Space
            }),
            fail_if_invalid_constraints(
                Constraints::empty(),
                enum_set!(Constraint::Nofor),
                Opcode::Space
            )
        );
        assert_eq!(
            Err(ParseError::InvalidConstraints {
                constraints: enum_set!(Constraint::Nofor | Constraint::Nocross),
                opcode: Opcode::Space
            }),
            fail_if_invalid_constraints(
                Constraints::empty(),
                enum_set!(Constraint::Nofor | Constraint::Nocross),
                Opcode::Space
            )
        );
        assert_eq!(
            Err(ParseError::InvalidConstraints {
                constraints: enum_set!(Constraint::Nocross),
                opcode: Opcode::Space
            }),
            fail_if_invalid_constraints(
                enum_set!(Constraint::Nofor | Constraint::Noback),
                enum_set!(Constraint::Nofor | Constraint::Nocross),
                Opcode::Space
            )
        );
    }

    #[test]
    fn withclass_test() {
        assert_eq!(
            Ok(Some(WithClass::Before {
                class: "foo".into()
            })),
            RuleParser::new(&"before foo ").with_class()
        );
        assert_eq!(
            Ok(Some(WithClass::After {
                class: "foo".into()
            })),
            RuleParser::new(&" after foo ").with_class()
        );
        assert_eq!(
            Err(ParseError::ClassNameExpected { found: None }),
            RuleParser::new(&" after ").with_class()
        );
    }

    #[test]
    fn withclasses_test() {
        assert_eq!(
            Ok(Some(vec![
                WithClass::Before {
                    class: "foo".into()
                },
                WithClass::After {
                    class: "bar".into()
                },
            ])),
            RuleParser::new(&"before foo after bar").with_classes()
        );

        assert_eq!(
            Err(ParseError::ClassNameExpected { found: None }),
            RuleParser::new(&"before foo after").with_classes()
        );
    }

    #[test]
    fn opcode_test() {
        assert_eq!(Ok(Opcode::Include), RuleParser::new(&"include").opcode());
        assert_eq!(
            Ok(Opcode::Always),
            RuleParser::new(&"always foo after").opcode()
        );

        assert_eq!(
            Err(ParseError::OpcodeExpected {
                found: Some("h".into())
            }),
            RuleParser::new(&"h").opcode()
        );
        assert_eq!(
            Err(ParseError::OpcodeExpected {
                found: Some("hello".into())
            }),
            RuleParser::new(&"hello world").opcode()
        );
    }

    #[test]
    fn with_matches_test() {
        assert_eq!(
            Some(HashSet::from([WithMatch::After])),
            RuleParser::new(&"empmatchafter match").with_matches()
        );
        assert_eq!(
            Some(HashSet::from([WithMatch::Before])),
            RuleParser::new(&"empmatchbefore match").with_matches()
        );
        assert_eq!(None, RuleParser::new(&"match").with_matches());
        assert_eq!(
            Some(HashSet::from([WithMatch::After, WithMatch::Before])),
            RuleParser::new(&"empmatchbefore empmatchafter match").with_matches()
        );
    }

    #[test]
    fn rule_test() {
        assert_eq!(
            Ok(Rule::Include {
                file: "foo.ctb".into()
            }),
            RuleParser::new(&"include foo.ctb").rule()
        );
        assert_eq!(
            Err(ParseError::FilenameExpected),
            RuleParser::new(&"include").rule()
        );
        assert_eq!(
            Err(ParseError::OpcodeExpected {
                found: Some("Include".into())
            }),
            RuleParser::new(&"Include foo.ctb").rule()
        );
    }

    #[test]
    fn display_test() {
        use self::braille::BrailleDot;
        assert_eq!(
            Ok(Rule::Display {
                character: 'a',
                dots: vec![enum_set!(BrailleDot::Dot1)],
                constraints: Constraints::empty()
            }),
            RuleParser::new(&"display a 1").rule()
        );
    }
}
