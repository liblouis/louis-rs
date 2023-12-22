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

use enumset::{EnumSetType, EnumSet};
use search_path::SearchPath;

use self::{
    braille::{braille_chars, chars_to_dots, BrailleChars},
    multipass::Test,
};

pub use braille::dots_to_unicode;

mod braille;
mod multipass;

#[derive(Hash, Eq, PartialEq, Debug)]
enum Constraint {
    Nofor,
    Noback,
    Nocross,
}

// type Constraints = HashSet<Constraint>;

#[derive(EnumSetType, Debug)]
pub enum Direction {
    Forward,
    Backward,
}

type Directions = EnumSet<Direction>;

#[derive(PartialEq, Debug)]
struct Constraints {
    across_syllable_boundaries: bool,
    direction: Direction,
}

// impl Default for Constraints {
//     fn default() -> Self {
//         Self {
//             across_syllable_boundaries: true,
//             direction: Direction::Both,
//         }
//     }
// }

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
    #[error("invalid digit")]
    InvalidDigit,
    #[error("invalid number")]
    InvalidNumber(#[from] ParseIntError),
    #[error("invalid escape sequence")]
    InvalidEscape,
    #[error("Names can only contain a-z and A-Z, got {name:?}")]
    InvalidName { name: String },
    #[error("Direction {directions:?} not allowed for opcode {opcode:?}")]
    InvalidDirection {
        directions: Directions,
        opcode: Opcode,
    },
    #[error("Nocross not allowed for opcode {opcode:?}")]
    InvalidNocross { opcode: Opcode },
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
        directions: Directions,
    },
    Multind {
        dots: BrailleChars,
        names: Vec<String>,
        directions: Directions,
    },

    Space {
        character: char,
        dots: BrailleChars,
        directions: Directions,
    },
    Punctuation {
        character: char,
        dots: BrailleChars,
        directions: Directions,
    },
    Digit {
        character: char,
        dots: BrailleChars,
        directions: Directions,
    },
    Grouping {
        name: String,
        chars: String,
        dots: BrailleChars,
    },
    Letter {
        character: char,
        dots: Braille,
        directions: Directions,
    },
    Base {
        name: String,
        from: char,
        to: char,
    },
    Lowercase {
        character: char,
        dots: BrailleChars,
        directions: Directions,
    },
    Uppercase {
        character: char,
        dots: BrailleChars,
        directions: Directions,
    },
    Litdigit {
        character: char,
        dots: BrailleChars,
        directions: Directions,
    },
    Sign {
        character: char,
        dots: BrailleChars,
        directions: Directions,
    },
    Math {
        character: char,
        dots: BrailleChars,
        directions: Directions,
    },

    Modeletter {
        name: String,
        dots: BrailleChars,
        directions: Directions,
    },
    Capsletter {
        dots: BrailleChars,
        directions: Directions,
    },
    Begmodeword {
        name: String,
        dots: BrailleChars,
        directions: Directions,
    },
    Begcapsword {
        dots: BrailleChars,
        directions: Directions,
    },
    Endcapsword {
        dots: BrailleChars,
        directions: Directions,
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
        directions: Directions,
    },
    Endemph {
        name: String,
        dots: BrailleChars,
        directions: Directions,
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
        directions: Directions,
    },
    Endcomp {
        dots: BrailleChars,
        directions: Directions,
    },

    Decpoint {
        chars: String,
        dots: BrailleChars,
    },
    Hyphen {
        chars: String,
        dots: BrailleChars,
        directions: Directions,
    },

    Capsnocont {},

    Compbrl {
        chars: String,
        directions: Directions,
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
        directions: Directions,
        nocross: bool,
    },
    Repeated {
        chars: String,
        dots: BrailleChars,
        directions: Directions,
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
        directions: Directions,
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
        directions: Directions,
    },
    Contraction {
        chars: String,
    },
    Sufword {
        chars: String,
        dots: Braille,
        directions: Directions,
        nocross: bool,
    },
    Prfword {
        chars: String,
        dots: Braille,
        directions: Directions,
        nocross: bool,
    },
    Begword {
        chars: String,
        dots: Braille,
        directions: Directions,
        nocross: bool,
    },
    Begmidword {
        chars: String,
        dots: Braille,
        directions: Directions,
        nocross: bool,
    },
    Midword {
        chars: String,
        dots: Braille,
        directions: Directions,
        nocross: bool,
    },
    Midendword {
        chars: String,
        dots: Braille,
        directions: Directions,
        nocross: bool,
    },
    Endword {
        chars: String,
        dots: Braille,
        directions: Directions,
        nocross: bool,
    },
    Partword {
        chars: String,
        dots: Braille,
        directions: Directions,
        nocross: bool,
    },
    Exactdots {
        chars: String,
    },
    Prepunc {
        chars: String,
        dots: BrailleChars,
        directions: Directions,
    },
    Postpunc {
        chars: String,
        dots: BrailleChars,
        directions: Directions,
    },
    Begnum {
        chars: String,
        dots: BrailleChars,
        directions: Directions,
    },
    Midnum {
        chars: String,
        dots: BrailleChars,
        directions: Directions,
    },
    Endnum {
        chars: String,
        dots: Braille,
        directions: Directions,
    },
    Joinnum {
        chars: String,
        dots: BrailleChars,
        directions: Directions,
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
        directions: Directions,
    },
    Pass2 {
        test: multipass::Test,
        action: String,
        directions: Directions,
    },
    Pass3 {
        test: multipass::Test,
        action: String,
        directions: Directions,
    },
    Pass4 {
        test: multipass::Test,
        action: String,
        directions: Directions,
    },
    Correct {
        test: multipass::Test,
        action: String,
        directions: Directions,
    },

    Match {
        pre: String,
        chars: String,
        post: String,
        dots: Braille,
        directions: Directions,
        matches: Option<WithMatches>,
    },
    Literal {
        chars: String,
    },
}

impl Rule {
    fn directions(&self) -> Directions {
        match self {
            Rule::Display { directions, .. }
            | Rule::Multind { directions, .. }
            | Rule::Space { directions, .. }
            | Rule::Punctuation { directions, .. }
            | Rule::Digit { directions, .. }
            | Rule::Letter { directions, .. }
            | Rule::Lowercase { directions, .. }
            | Rule::Uppercase { directions, .. }
            | Rule::Litdigit { directions, .. }
            | Rule::Sign { directions, .. }
            | Rule::Math { directions, .. }
            | Rule::Modeletter { directions, .. }
            | Rule::Capsletter { directions, .. }
            | Rule::Begmodeword { directions, .. }
            | Rule::Begcapsword { directions, .. }
            | Rule::Endcapsword { directions, .. }
            | Rule::Begemph { directions, .. }
            | Rule::Endemph { directions, .. }
            | Rule::Begcomp { directions, .. }
            | Rule::Endcomp { directions, .. }
            | Rule::Hyphen { directions, .. }
            | Rule::Compbrl { directions, .. }
            | Rule::Always { directions, .. }
            | Rule::Repeated { directions, .. }
            | Rule::Word { directions, .. }
            | Rule::Lowword { directions, .. }
            | Rule::Sufword { directions, .. }
            | Rule::Prfword { directions, .. }
            | Rule::Begword { directions, .. }
            | Rule::Begmidword { directions, .. }
            | Rule::Midword { directions, .. }
            | Rule::Midendword { directions, .. }
            | Rule::Endword { directions, .. }
            | Rule::Partword { directions, .. }
            | Rule::Prepunc { directions, .. }
            | Rule::Postpunc { directions, .. }
            | Rule::Begnum { directions, .. }
            | Rule::Midnum { directions, .. }
            | Rule::Endnum { directions, .. }
            | Rule::Joinnum { directions, .. }
            | Rule::Context { directions, .. }
            | Rule::Pass2 { directions, .. }
            | Rule::Pass3 { directions, .. }
            | Rule::Pass4 { directions, .. }
            | Rule::Correct { directions, .. }
            | Rule::Match { directions, .. } => directions.clone(),
            _ => Direction::Forward | Direction::Backward,
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

/// Return an error if a direction or nocross have been specified
fn fail_if_direction_or_nocross(
    directions: Directions,
    nocross: bool,
    opcode: Opcode,
) -> Result<(), ParseError> {
    if directions != Direction::Forward | Direction::Backward {
        Err(ParseError::InvalidDirection { directions, opcode })
    } else if nocross {
        Err(ParseError::InvalidNocross { opcode })
    } else {
        Ok(())
    }
}

/// Return an error if nocross has been specified
fn fail_if_nocross(nocross: bool, opcode: Opcode) -> Result<(), ParseError> {
    if nocross {
        Err(ParseError::InvalidNocross { opcode })
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

    fn nofor(&mut self) -> Option<Constraint> {
        match self.tokens.next_if_eq(&"nofor") {
            Some(_) => Some(Constraint::Nofor),
            _ => None,
        }
    }

    fn noback(&mut self) -> Option<Constraint> {
        match self.tokens.next_if_eq(&"noback") {
            Some(_) => Some(Constraint::Noback),
            _ => None,
        }
    }

    fn nocross(&mut self) -> Option<Constraint> {
        match self.tokens.next_if_eq(&"nocross") {
            Some(_) => Some(Constraint::Nocross),
            _ => None,
        }
    }

    fn direction(&mut self) -> Directions {
        if self.nofor().is_some() {
            Direction::Backward.into()
        } else if self.noback().is_some() {
            Direction::Forward.into()
        } else {
            Direction::Forward | Direction::Backward
        }
    }

    fn across_syllable_boundaries(&mut self) -> bool {
        self.nocross().is_some()
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
        let directions = self.direction();
        let nocross = self.across_syllable_boundaries();
        let _classes = self.with_classes();
        let matches = self.with_matches();
        let opcode = self.opcode()?;
        let rule = match opcode {
            Opcode::Include => {
                fail_if_direction_or_nocross(directions, nocross, opcode)?;
                Rule::Include {
                    file: self.filename()?,
                }
            }
            Opcode::Undefined => {
                fail_if_direction_or_nocross(directions, nocross, opcode)?;
                Rule::Undefined {
                    dots: self.explicit_dots()?,
                }
            }
            Opcode::Display => {
                fail_if_nocross(nocross, opcode)?;
                Rule::Display {
                    character: self.one_char()?,
                    dots: self.explicit_dots()?,
                    directions,
                }
            }
            Opcode::Multind => {
                fail_if_nocross(nocross, opcode)?;
                Rule::Multind {
                    dots: self.explicit_dots()?,
                    names: self.many_names()?,
                    directions,
                }
            }
            Opcode::Space => {
                fail_if_nocross(nocross, opcode)?;
                Rule::Space {
                    character: self.one_char()?,
                    dots: self.explicit_dots()?,
                    directions,
                }
            }
            Opcode::Punctuation => {
                fail_if_nocross(nocross, opcode)?;
                Rule::Punctuation {
                    character: self.one_char()?,
                    dots: self.explicit_dots()?,
                    directions,
                }
            }
            Opcode::Digit => {
                fail_if_nocross(nocross, opcode)?;
                Rule::Digit {
                    character: self.one_char()?,
                    dots: self.explicit_dots()?,
                    directions,
                }
            }
            Opcode::Grouping => {
                fail_if_nocross(nocross, opcode)?;
                Rule::Grouping {
                    name: self.name()?,
                    chars: self.chars()?,
                    dots: self.explicit_dots()?,
                }
            }
            Opcode::Letter => {
                fail_if_nocross(nocross, opcode)?;
                Rule::Letter {
                    character: self.one_char()?,
                    dots: self.dots()?,
                    directions,
                }
            }
            Opcode::Base => {
                fail_if_direction_or_nocross(directions, nocross, opcode)?;
                Rule::Base {
                    name: self.name()?,
                    from: self.one_char()?,
                    to: self.one_char()?,
                }
            }
            Opcode::Lowercase => {
                fail_if_nocross(nocross, opcode)?;
                Rule::Lowercase {
                    character: self.one_char()?,
                    dots: self.explicit_dots()?,
                    directions,
                }
            }
            Opcode::Uppercase => {
                fail_if_nocross(nocross, opcode)?;
                Rule::Uppercase {
                    character: self.one_char()?,
                    dots: self.explicit_dots()?,
                    directions,
                }
            }
            Opcode::Litdigit => {
                fail_if_nocross(nocross, opcode)?;
                Rule::Litdigit {
                    character: self.one_char()?,
                    dots: self.explicit_dots()?,
                    directions,
                }
            }
            Opcode::Sign => {
                fail_if_nocross(nocross, opcode)?;
                Rule::Sign {
                    character: self.one_char()?,
                    dots: self.explicit_dots()?,
                    directions,
                }
            }
            Opcode::Math => {
                fail_if_nocross(nocross, opcode)?;
                Rule::Math {
                    character: self.one_char()?,
                    dots: self.explicit_dots()?,
                    directions,
                }
            }

            Opcode::Modeletter => {
                fail_if_nocross(nocross, opcode)?;
                Rule::Modeletter {
                    name: self.name()?,
                    dots: self.explicit_dots()?,
                    directions,
                }
            }
            Opcode::Capsletter => {
                fail_if_nocross(nocross, opcode)?;
                Rule::Capsletter {
                    dots: self.explicit_dots()?,
                    directions,
                }
            }
            Opcode::Begmodeword => {
                fail_if_nocross(nocross, opcode)?;
                Rule::Begmodeword {
                    name: self.name()?,
                    dots: self.explicit_dots()?,
                    directions,
                }
            }
            Opcode::Begcapsword => {
                fail_if_nocross(nocross, opcode)?;
                Rule::Begcapsword {
                    dots: self.explicit_dots()?,
                    directions,
                }
            }
            Opcode::Endcapsword => {
                fail_if_nocross(nocross, opcode)?;
                Rule::Endcapsword {
                    dots: self.explicit_dots()?,
                    directions,
                }
            }
            Opcode::Capsmodechars => {
                fail_if_direction_or_nocross(directions, nocross, opcode)?;
                Rule::Capsmodechars {
                    chars: self.chars()?,
                }
            }
            Opcode::Begcaps => {
                fail_if_direction_or_nocross(directions, nocross, opcode)?;
                Rule::Begcaps {
                    dots: self.explicit_dots()?,
                }
            }
            Opcode::Endcaps => {
                fail_if_direction_or_nocross(directions, nocross, opcode)?;
                Rule::Endcaps {
                    dots: self.explicit_dots()?,
                }
            }
            Opcode::Begcapsphrase => {
                fail_if_direction_or_nocross(directions, nocross, opcode)?;
                Rule::Begcapsphrase {
                    dots: self.explicit_dots()?,
                }
            }
            Opcode::Endcapsphrase => {
                fail_if_direction_or_nocross(directions, nocross, opcode)?;
                Rule::Endcapsphrase {
                    position: self.position()?,
                    dots: self.explicit_dots()?,
                }
            }
            Opcode::Lencapsphrase => {
                fail_if_direction_or_nocross(directions, nocross, opcode)?;
                Rule::Lencapsphrase {
                    number: self.number()?,
                }
            }
            Opcode::Letsign => {
                fail_if_direction_or_nocross(directions, nocross, opcode)?;
                Rule::Letsign {
                    dots: self.explicit_dots()?,
                }
            }
            Opcode::Noletsign => {
                fail_if_direction_or_nocross(directions, nocross, opcode)?;
                Rule::Noletsign {
                    chars: self.chars()?,
                }
            }
            Opcode::Noletsignbefore => {
                fail_if_direction_or_nocross(directions, nocross, opcode)?;
                Rule::Noletsignbefore {
                    chars: self.chars()?,
                }
            }
            Opcode::Noletsignafter => {
                fail_if_direction_or_nocross(directions, nocross, opcode)?;
                Rule::Noletsignafter {
                    chars: self.chars()?,
                }
            }
            Opcode::Nocontractsign => {
                fail_if_direction_or_nocross(directions, nocross, opcode)?;
                Rule::Nocontractsign {
                    dots: self.explicit_dots()?,
                }
            }
            Opcode::Numsign => {
                fail_if_direction_or_nocross(directions, nocross, opcode)?;
                Rule::Numsign {
                    dots: self.explicit_dots()?,
                }
            }
            Opcode::Nonumsign => {
                fail_if_direction_or_nocross(directions, nocross, opcode)?;
                Rule::Nonumsign {
                    dots: self.explicit_dots()?,
                }
            }
            Opcode::Numericnocontchars => {
                fail_if_direction_or_nocross(directions, nocross, opcode)?;
                Rule::Numericnocontchars {
                    chars: self.chars()?,
                }
            }
            Opcode::Numericmodechars => {
                fail_if_direction_or_nocross(directions, nocross, opcode)?;
                Rule::Numericmodechars {
                    chars: self.chars()?,
                }
            }
            Opcode::Midendnumericmodechars => {
                fail_if_direction_or_nocross(directions, nocross, opcode)?;
                Rule::Midendnumericmodechars {
                    chars: self.chars()?,
                }
            }

            Opcode::Begmodephrase => {
                fail_if_direction_or_nocross(directions, nocross, opcode)?;
                Rule::Begmodephrase {
                    name: self.name()?,
                    dots: self.explicit_dots()?,
                }
            }
            Opcode::Endmodephrase => {
                fail_if_direction_or_nocross(directions, nocross, opcode)?;
                Rule::Endmodephrase {
                    name: self.name()?,
                    position: self.position()?,
                    dots: self.explicit_dots()?,
                }
            }
            Opcode::Lenmodephrase => {
                fail_if_direction_or_nocross(directions, nocross, opcode)?;
                Rule::Lenmodephrase {
                    name: self.name()?,
                    number: self.number()?,
                }
            }

            Opcode::Seqdelimiter => {
                fail_if_direction_or_nocross(directions, nocross, opcode)?;
                Rule::Seqdelimiter {
                    chars: self.chars()?,
                }
            }
            Opcode::Seqbeforechars => {
                fail_if_direction_or_nocross(directions, nocross, opcode)?;
                Rule::Seqbeforechars {
                    chars: self.chars()?,
                }
            }
            Opcode::Seqafterchars => {
                fail_if_direction_or_nocross(directions, nocross, opcode)?;
                Rule::Seqafterchars {
                    chars: self.chars()?,
                }
            }
            Opcode::Seqafterpattern => {
                fail_if_direction_or_nocross(directions, nocross, opcode)?;
                Rule::Seqafterpattern {
                    chars: self.chars()?,
                }
            }
            Opcode::Seqafterexpression => {
                fail_if_direction_or_nocross(directions, nocross, opcode)?;
                Rule::Seqafterexpression {
                    chars: self.chars()?,
                }
            }

            Opcode::Class => {
                fail_if_direction_or_nocross(directions, nocross, opcode)?;
                Rule::Class {
                    name: self.name()?,
                    chars: self.chars()?,
                }
            }
            Opcode::Emphclass => {
                fail_if_direction_or_nocross(directions, nocross, opcode)?;
                Rule::Emphclass { name: self.name()? }
            }
            Opcode::Begemph => {
                fail_if_nocross(nocross, opcode)?;
                Rule::Begemph {
                    name: self.name()?,
                    dots: self.explicit_dots()?,
                    directions,
                }
            }
            Opcode::Endemph => {
                fail_if_nocross(nocross, opcode)?;
                Rule::Endemph {
                    name: self.name()?,
                    dots: self.explicit_dots()?,
                    directions,
                }
            }
            Opcode::Noemphchars => {
                fail_if_direction_or_nocross(directions, nocross, opcode)?;
                Rule::Noemphchars {
                    name: self.name()?,
                    chars: self.chars()?,
                }
            }
            Opcode::Emphletter => {
                fail_if_direction_or_nocross(directions, nocross, opcode)?;
                Rule::Emphletter {
                    name: self.name()?,
                    dots: self.explicit_dots()?,
                }
            }
            Opcode::Begemphword => {
                fail_if_direction_or_nocross(directions, nocross, opcode)?;
                Rule::Begemphword {
                    name: self.name()?,
                    dots: self.explicit_dots()?,
                }
            }
            Opcode::Endemphword => {
                fail_if_direction_or_nocross(directions, nocross, opcode)?;
                Rule::Endemphword {
                    name: self.name()?,
                    dots: self.explicit_dots()?,
                }
            }
            Opcode::Emphmodechars => {
                fail_if_direction_or_nocross(directions, nocross, opcode)?;
                Rule::Emphmodechars {
                    name: self.name()?,
                    chars: self.chars()?,
                }
            }
            Opcode::Begemphphrase => {
                fail_if_direction_or_nocross(directions, nocross, opcode)?;
                Rule::Begemphphrase {
                    name: self.name()?,
                    dots: self.explicit_dots()?,
                }
            }
            Opcode::Endemphphrase => {
                fail_if_direction_or_nocross(directions, nocross, opcode)?;
                Rule::Endemphphrase {
                    name: self.name()?,
                    position: self.position()?,
                    dots: self.explicit_dots()?,
                }
            }
            Opcode::Lenemphphrase => {
                fail_if_direction_or_nocross(directions, nocross, opcode)?;
                Rule::Lenemphphrase {
                    name: self.name()?,
                    number: self.number()?,
                }
            }

            Opcode::Begcomp => {
                fail_if_nocross(nocross, opcode)?;
                Rule::Begcomp {
                    dots: self.explicit_dots()?,
                    directions,
                }
            }
            Opcode::Endcomp => {
                fail_if_nocross(nocross, opcode)?;
                Rule::Endcomp {
                    dots: self.explicit_dots()?,
                    directions,
                }
            }

            Opcode::Decpoint => {
                fail_if_direction_or_nocross(directions, nocross, opcode)?;
                Rule::Decpoint {
                    chars: self.chars()?,
                    dots: self.explicit_dots()?,
                }
            }
            Opcode::Hyphen => {
                fail_if_nocross(nocross, opcode)?;
                Rule::Hyphen {
                    chars: self.chars()?,
                    dots: self.explicit_dots()?,
                    directions,
                }
            }

            Opcode::Capsnocont => {
                fail_if_direction_or_nocross(directions, nocross, opcode)?;
                Rule::Capsnocont {}
            }

            Opcode::Compbrl => {
                fail_if_nocross(nocross, opcode)?;
                Rule::Compbrl {
                    chars: self.chars()?,
                    directions,
                }
            }
            Opcode::Comp6 => {
                fail_if_direction_or_nocross(directions, nocross, opcode)?;
                Rule::Comp6 {
                    chars: self.chars()?,
                    dots: self.dots()?,
                }
            }
            Opcode::Nocont => {
                fail_if_direction_or_nocross(directions, nocross, opcode)?;
                Rule::Nocont {
                    chars: self.chars()?,
                }
            }
            Opcode::Replace => {
                fail_if_direction_or_nocross(directions, nocross, opcode)?;
                Rule::Replace {
                    chars: self.chars()?,
                    replacement: self.maybe_chars(),
                }
            }
            Opcode::Always => Rule::Always {
                chars: self.chars()?,
                dots: self.dots()?,
                directions,
                nocross,
            },
            Opcode::Repeated => {
                fail_if_nocross(nocross, opcode)?;
                Rule::Repeated {
                    chars: self.chars()?,
                    dots: self.explicit_dots()?,
                    directions,
                }
            }
            Opcode::Repword => {
                fail_if_direction_or_nocross(directions, nocross, opcode)?;
                Rule::Repword {
                    chars: self.chars()?,
                    dots: self.explicit_dots()?,
                }
            }
            Opcode::Rependword => {
                fail_if_direction_or_nocross(directions, nocross, opcode)?;
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
                fail_if_direction_or_nocross(directions, nocross, opcode)?;
                Rule::Largesign {
                    chars: self.chars()?,
                    dots: self.explicit_dots()?,
                }
            }
            Opcode::Word => {
                fail_if_nocross(nocross, opcode)?;
                Rule::Word {
                    chars: self.chars()?,
                    dots: self.dots()?,
                    directions,
                }
            }
            Opcode::Syllable => {
                fail_if_direction_or_nocross(directions, nocross, opcode)?;
                Rule::Syllable {
                    chars: self.chars()?,
                    dots: self.dots()?,
                }
            }
            Opcode::Joinword => {
                fail_if_direction_or_nocross(directions, nocross, opcode)?;
                Rule::Joinword {
                    chars: self.chars()?,
                    dots: self.explicit_dots()?,
                }
            }
            Opcode::Lowword => {
                fail_if_nocross(nocross, opcode)?;
                Rule::Lowword {
                    chars: self.chars()?,
                    dots: self.explicit_dots()?,
                    directions,
                }
            }
            Opcode::Contraction => {
                fail_if_direction_or_nocross(directions, nocross, opcode)?;
                Rule::Contraction {
                    chars: self.chars()?,
                }
            }
            Opcode::Sufword => Rule::Sufword {
                chars: self.chars()?,
                dots: self.dots()?,
                directions,
                nocross,
            },
            Opcode::Prfword => Rule::Prfword {
                chars: self.chars()?,
                dots: self.dots()?,
                directions,
                nocross,
            },
            Opcode::Begword => Rule::Begword {
                chars: self.chars()?,
                dots: self.dots()?,
                directions,
                nocross,
            },
            Opcode::Begmidword => Rule::Begmidword {
                chars: self.chars()?,
                dots: self.dots()?,
                directions,
                nocross,
            },
            Opcode::Midword => Rule::Midword {
                chars: self.chars()?,
                dots: self.dots()?,
                directions,
                nocross,
            },
            Opcode::Midendword => Rule::Midendword {
                chars: self.chars()?,
                dots: self.dots()?,
                directions,
                nocross,
            },
            Opcode::Endword => Rule::Endword {
                chars: self.chars()?,
                dots: self.dots()?,
                directions,
                nocross,
            },
            Opcode::Partword => Rule::Partword {
                chars: self.chars()?,
                dots: self.dots()?,
                directions,
                nocross,
            },
            Opcode::Exactdots => {
                fail_if_direction_or_nocross(directions, nocross, opcode)?;
                Rule::Exactdots {
                    chars: self.chars()?,
                }
            }
            Opcode::Prepunc => Rule::Prepunc {
                chars: self.chars()?,
                dots: self.explicit_dots()?,
                directions,
            },
            Opcode::Postpunc => Rule::Postpunc {
                chars: self.chars()?,
                dots: self.explicit_dots()?,
                directions,
            },
            Opcode::Begnum => Rule::Begnum {
                chars: self.chars()?,
                dots: self.explicit_dots()?,
                directions,
            },
            Opcode::Midnum => Rule::Midnum {
                chars: self.chars()?,
                dots: self.explicit_dots()?,
                directions,
            },
            Opcode::Endnum => Rule::Endnum {
                chars: self.chars()?,
                dots: self.dots()?,
                directions,
            },
            Opcode::Joinnum => Rule::Joinnum {
                chars: self.chars()?,
                dots: self.explicit_dots()?,
                directions,
            },

            Opcode::Attribute => {
                fail_if_direction_or_nocross(directions, nocross, opcode)?;
                Rule::Attribute {
                    name: self.name()?,
                    chars: self.chars()?,
                }
            }

            Opcode::Swapcd => {
                fail_if_direction_or_nocross(directions, nocross, opcode)?;
                Rule::Swapcd {
                    name: self.name()?,
                    chars: self.chars()?,
                    dots: self.many_dots()?,
                }
            }
            Opcode::Swapdd => {
                fail_if_direction_or_nocross(directions, nocross, opcode)?;
                Rule::Swapdd {
                    name: self.name()?,
                    dots: self.many_dots()?,
                    replacement: self.many_dots()?,
                }
            }
            Opcode::Swapcc => {
                fail_if_direction_or_nocross(directions, nocross, opcode)?;
                Rule::Swapcc {
                    name: self.name()?,
                    chars: self.chars()?,
                    replacement: self.chars()?,
                }
            }

            Opcode::Context => {
                fail_if_nocross(nocross, opcode)?;
                Rule::Context {
                    test: self.multipass_test()?,
                    action: self.multipass_action()?,
                    directions,
                }
            }
            Opcode::Pass2 => {
                fail_if_nocross(nocross, opcode)?;
                Rule::Pass2 {
                    test: self.multipass_test()?,
                    action: self.multipass_action()?,
                    directions,
                }
            }
            Opcode::Pass3 => {
                fail_if_nocross(nocross, opcode)?;
                Rule::Pass3 {
                    test: self.multipass_test()?,
                    action: self.multipass_action()?,
                    directions,
                }
            }
            Opcode::Pass4 => {
                fail_if_nocross(nocross, opcode)?;
                Rule::Pass4 {
                    test: self.multipass_test()?,
                    action: self.multipass_action()?,
                    directions,
                }
            }
            Opcode::Correct => {
                fail_if_nocross(nocross, opcode)?;
                Rule::Correct {
                    test: self.multipass_test()?,
                    action: self.multipass_action()?,
                    directions,
                }
            }

            Opcode::Match => {
                fail_if_nocross(nocross, opcode)?;
                Rule::Match {
                    pre: self.match_pre()?,
                    chars: self.chars()?,
                    post: self.match_post()?,
                    dots: self.dots()?,
                    matches,
                    directions,
                }
            }
            Opcode::Literal => {
                fail_if_direction_or_nocross(directions, nocross, opcode)?;
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
        file: String,
        line: usize,
        error: ParseError,
    },
    #[error("Cannot read table")]
    TableNotReadable(#[from] io::Error),
    #[error("Cannot find table {path:?}")]
    TableNotFound { path: PathBuf },
    #[error("Table format not supported {path:?}")]
    FormatNotSupported { path: PathBuf },
}

pub fn table(file: &Path) -> Result<Vec<Rule>, Vec<TableError>> {
    let (rules, errors): (Vec<_>, Vec<_>) = read_to_string(file)
        .map_err(|e| vec![TableError::TableNotReadable(e)])?
        .lines()
        .enumerate()
        // drop comments and empty lines
        .filter(|(_, line)| !line.trim().starts_with('#') && !line.trim().is_empty())
        .map(|(line_no, line)| {
            // parse the line
            RuleParser::new(line)
                .rule()
                .map_err(|e| TableError::ParseError {
                    file: file.to_string_lossy().into(),
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

fn expand_include(search_path: &SearchPath, rule: Rule) -> Result<Vec<Rule>, Vec<TableError>> {
    if let Rule::Include { ref file } = rule {
        let path = Path::new(file);
        if path.extension().and_then(OsStr::to_str) == Some("dic") {
            return Err(vec![TableError::FormatNotSupported { path: path.into() }]);
        }
        let path = search_path
            .find_file(path)
            .ok_or(vec![TableError::TableNotFound { path: path.into() }])?;
        let rules = table(&path)?;
        Ok(expand_includes(search_path, rules)?)
    } else {
        Ok(vec![rule])
    }
}

pub fn expand_includes(
    search_path: &SearchPath,
    rules: Vec<Rule>,
) -> Result<Vec<Rule>, Vec<TableError>> {
    let (rules, errors): (Vec<_>, Vec<_>) = rules
        .into_iter()
        .map(|rule| expand_include(search_path, rule))
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

    #[test]
    fn nocross_test() {
        assert_eq!(
            Some(Constraint::Nocross),
            RuleParser::new(&"nocross").nocross()
        );
        assert_eq!(
            Some(Constraint::Nocross),
            RuleParser::new(&"nocross nofor").nocross()
        );
        assert_eq!(None, RuleParser::new(&"nofor nocross").nocross());
        assert_eq!(None, RuleParser::new(&"nofor").nocross());
    }

    #[test]
    fn nofor_test() {
        assert_eq!(Some(Constraint::Nofor), RuleParser::new(&" nofor ").nofor());
        assert_eq!(
            Some(Constraint::Nofor),
            RuleParser::new(&"nofor nocross").nofor()
        );
        assert_eq!(None, RuleParser::new(&"nocross nofor").nofor());
        assert_eq!(None, RuleParser::new(&"").nofor());
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
}
