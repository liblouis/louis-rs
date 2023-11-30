use std::{collections::HashSet, str::{SplitWhitespace, Chars}, iter::Peekable, num::ParseIntError, fs::read_to_string, env};

#[derive(Hash, Eq, PartialEq, Debug)]
enum Prefix {
    Nofor,
    Noback,
    Nocross,
}

#[derive(PartialEq, Debug)]
pub enum WithClass {
    Before { class: String },
    After { class: String },
}

type WithClasses = Vec<WithClass>;

#[derive(thiserror::Error, Debug, PartialEq)]
pub enum ParseError {
    #[error("Expected {expected:?} got {found:?}")]
    TokenExpected{ expected: String, found: Option<String> },
    #[error("Invalid braille {character:?}")]
    InvalidBraille {character: char},
    #[error("Braille expected")]
    DotsExpected,
    #[error("invalid unicode literal")]
    InvalidUnicodeLiteral,
    #[error("invalid digit")]
    InvalidDigit,
    #[error("invalid number")]
    InvalidNumber(#[from] ParseIntError),
    #[error("invalid prefix")]
    InvalidPrefix,
    #[error("invalid match prefix")]
    InvalidMatchPrefix,
    #[error("invalid escape sequence")]
    InvalidEscape,
    #[error("Expected classname, got {found:?}")]
    ClassNameExpected{ found: Option<String> },
    #[error("Opcode expected")]
    OpcodeExpected,
    #[error("Name expected")]
    NameExpected,
    #[error("Characters expected")]
    CharsExpected,
    #[error("Filename expected")]
    FilenameExpected,
    #[error("Number expected, got {found:?}")]
    NumberExpected { found: Option<String> },
    #[error("Multipass test expected")]
    Multitest,
    #[error("Multipass action expected")]
    MultiAction,
    #[error("Match pre-pattern expected")]
    MatchPre,
    #[error("Match post-pattern expected")]
    MatchPost,
    #[error("Expected a single char, got {found:?}")]
    SingleCharExpected{ found: Option<String> },
    #[error("unknown parser error")]
    Unparseable,
}

#[derive(PartialEq, Debug)]
enum Opcode {
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
enum Rule {
    Include { file: String },
    Undefined { chars: String, dots: BrailleChars },
    Display { dots: BrailleChars },
    Multind { dots: BrailleChars, names: Vec<String> },

    Space { chars: String, dots: BrailleChars },
    Punctuation { chars: String, dots: BrailleChars },
    Digit { character: char, dots: BrailleChars },
    Grouping { name: String, chars: String, dots: BrailleChars },
    Letter { chars: String, dots: BrailleChars },
    Base { name: String, from: char, to: char },
    Lowercase { character: char, dots: BrailleChars },
    Uppercase { character: char, dots: BrailleChars },
    Litdigit { character: char, dots: BrailleChars },
    Sign { character: char, dots: BrailleChars },
    Math { character: char, dots: BrailleChars },

    Modeletter { name: String, dots: BrailleChars },
    Capsletter { dots: BrailleChars },
    Begmodeword { name: String, dots: BrailleChars },
    Begcapsword { dots: BrailleChars },
    Endcapsword { dots: BrailleChars },
    Capsmodechars { chars: String },
    Begcaps { dots: BrailleChars },
    Endcaps { dots: BrailleChars },
    Begcapsphrase { dots: BrailleChars },
    Endcapsphrase { dots: BrailleChars, position: Position },
    Lencapsphrase { number: i32 },
    Letsign { dots: BrailleChars },
    Noletsign { chars: String },
    Noletsignbefore { chars: String },
    Noletsignafter { chars: String },
    Nocontractsign { dots: BrailleChars },
    Numsign { dots: BrailleChars },
    Nonumsign { dots: BrailleChars },
    Numericnocontchars { chars: String },
    Numericmodechars { chars: String },
    Midendnumericmodechars { chars: String },

    Begmodephrase { name: String, dots: BrailleChars },
    Endmodephrase { name: String, dots: BrailleChars, position: Position },
    Lenmodephrase { name: String, number: i32 },

    Seqdelimiter { chars: String },
    Seqbeforechars { chars: String },
    Seqafterchars { chars: String },
    Seqafterpattern { chars: String },
    Seqafterexpression { chars: String },

    Class { name: String, chars: String },
    Emphclass { name: String },
    Begemph { name: String, dots: BrailleChars },
    Endemph { name: String, dots: BrailleChars },
    Noemphchars { name: String, chars: String },
    Emphletter { name: String, dots: BrailleChars },
    Begemphword { name: String, dots: BrailleChars },
    Endemphword { name: String, dots: BrailleChars },
    Emphmodechars { name: String, chars: String},
    Begemphphrase { name: String, dots: BrailleChars },
    Endemphphrase { name: String, dots: BrailleChars, position: Position },
    Lenemphphrase { name: String, number: i32 },

    Begcomp { dots: BrailleChars },
    Endcomp { dots: BrailleChars },

    Decpoint { chars: String, dots: BrailleChars },
    Hyphen { dots: BrailleChars },

    Capsnocont {  },

    Compbrl { chars: String },
    Comp6 { chars: String, dots: BrailleChars },
    Nocont { chars: String },
    Replace { chars: String, replacement: Option<String> },
    Always { chars: String, dots: BrailleChars },
    Repeated { chars: String, dots: BrailleChars },
    Repword { chars: String, dots: BrailleChars },
    Rependword { chars: String, dots: BrailleChars },
    Largesign { chars: String, dots: BrailleChars },
    Word { chars: String, dots: BrailleChars },
    Syllable { chars: String, dots: BrailleChars },
    Joinword { chars: String, dots: BrailleChars },
    Lowword { chars: String, dots: BrailleChars },
    Contraction { chars: String },
    Sufword { chars: String, dots: BrailleChars },
    Prfword { chars: String, dots: BrailleChars },
    Begword { chars: String, dots: BrailleChars },
    Begmidword { chars: String, dots: BrailleChars },
    Midword { chars: String, dots: BrailleChars },
    Midendword { chars: String, dots: BrailleChars },
    Endword { chars: String, dots: BrailleChars },
    Partword { chars: String, dots: BrailleChars },
    Exactdots { chars: String },
    Prepunc { chars: String, dots: BrailleChars },
    Postpunc { chars: String, dots: BrailleChars },
    Begnum { chars: String, dots: BrailleChars },
    Midnum { chars: String, dots: BrailleChars },
    Endnum { chars: String, dots: BrailleChars },
    Joinnum { chars: String, dots: BrailleChars },

    Swapcd { name: String, chars: String, dots: Vec<BrailleChars> },
    Swapdd { name: String, dots: Vec<BrailleChars>, replacement: Vec<BrailleChars> },
    Swapcc { name: String, chars: String, replacement: String },

    Attribute { name: String, chars: String },
    Context { test: String, action: String },
    Pass2 { test: String, action: String },
    Pass3 { test: String, action: String },
    Pass4 { test: String, action: String },
    Correct { test: String, action: String },

    Match { pre: String, chars: String, post: String, dots: BrailleChars },
    Literal { chars: String },
}

#[derive(Hash, PartialEq, Eq, Debug)]
pub enum BrailleDot {
    DOT0,
    DOT1,
    DOT2,
    DOT3,
    DOT4,
    DOT5,
    DOT6,
    DOT7,
    DOT8,
    DOT9,
    DOTA,
    DOTB,
    DOTC,
    DOTD,
    DOTE,
    DOTF,
}

pub type BrailleChar = HashSet<BrailleDot>;
pub type BrailleChars = Vec<BrailleChar>;

#[derive(PartialEq, Debug)]
pub enum Position {
    Before,
    After,
}

// fn has_virtual_dots(char: &BrailleChar) -> bool {
//     let virtual_dots = HashSet::from([
// 	BrailleDot::DOT9,
// 	BrailleDot::DOTA,
// 	BrailleDot::DOTB,
// 	BrailleDot::DOTC,
// 	BrailleDot::DOTD,
// 	BrailleDot::DOTE,
// 	BrailleDot::DOTF]);
//     !virtual_dots.intersection(char)
// 	.collect::<HashSet<_>>()
// 	.is_empty()
// }

fn char_to_dot(char: char) -> Result<BrailleDot, ParseError> {
    match char {
        '0' => Ok(BrailleDot::DOT0),
        '1' => Ok(BrailleDot::DOT1),
        '2' => Ok(BrailleDot::DOT2),
        '3' => Ok(BrailleDot::DOT3),
        '4' => Ok(BrailleDot::DOT4),
        '5' => Ok(BrailleDot::DOT5),
        '6' => Ok(BrailleDot::DOT6),
        '7' => Ok(BrailleDot::DOT7),
        '8' => Ok(BrailleDot::DOT8),
        '9' => Ok(BrailleDot::DOT9),
        'a' => Ok(BrailleDot::DOTA),
        'b' => Ok(BrailleDot::DOTB),
        'c' => Ok(BrailleDot::DOTC),
        'd' => Ok(BrailleDot::DOTD),
        'e' => Ok(BrailleDot::DOTE),
        'f' => Ok(BrailleDot::DOTF),
        invalid => Err(ParseError::InvalidBraille {character: invalid}),
    }
}

fn chars_to_dots(chars: &str) -> Result<BrailleChar, ParseError> {
    chars.chars().map(|c| char_to_dot(c)).collect()
}

fn unescape_unicode(chars: &mut Chars) -> Result<char, ParseError> {
    let mut s = String::new();

    for _ in 0..4 {
	match chars.next() {
	    Some(c) => {s.push(c)}
	    _ => {return Err(ParseError::InvalidUnicodeLiteral)}
	}
    }

    if let Ok(n) = u32::from_str_radix(&s, 16) {
	if let Some(c) = char::from_u32(n) {
	    return Ok(c);
	}
    }
    Err(ParseError::InvalidUnicodeLiteral)
}

pub fn unescape(s: &str) -> Result<String, ParseError> {
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
            Some('x') => new.push(unescape_unicode(&mut iter)?),
            _ => return Err(ParseError::InvalidEscape)
        };
    }
    Ok(new)
}

// fn dot_to_hex(dot: &BrailleDot) -> u32 {
//     match dot {
//         BrailleDot::DOT0 => 0x0000,
//         BrailleDot::DOT1 => 0x0001,
//         BrailleDot::DOT2 => 0x0002,
//         BrailleDot::DOT3 => 0x0004,
//         BrailleDot::DOT4 => 0x0008,
//         BrailleDot::DOT5 => 0x0010,
//         BrailleDot::DOT6 => 0x0020,
//         BrailleDot::DOT7 => 0x0040,
//         BrailleDot::DOT8 => 0x0080,
//         BrailleDot::DOT9 => 0x0100,
//         BrailleDot::DOTA => 0x0200,
//         BrailleDot::DOTB => 0x0400,
//         BrailleDot::DOTC => 0x0800,
//         BrailleDot::DOTD => 0x1000,
//         BrailleDot::DOTE => 0x2000,
//         BrailleDot::DOTF => 0x4000,
//     }
// }

struct RuleParser<'a> {
    tokens: Peekable<SplitWhitespace<'a>>,
}

impl<'a> RuleParser<'a> {

    fn new(source: &'a str) -> Self {
	Self {
	    tokens: source.split_whitespace().peekable(),
	}
    }

    fn nofor(&mut self) ->Option<Prefix> {
	match self.tokens.next_if_eq(&"nofor") {
	    Some(_) => Some(Prefix::Nofor),
	    _ => None
	}
    }
    
    fn noback(&mut self) ->Option<Prefix> {
	match self.tokens.next_if_eq(&"noback") {
	    Some(_) => Some(Prefix::Noback),
	    _ => None
	}
    }
    
    fn nocross(&mut self) ->Option<Prefix> {
	match self.tokens.next_if_eq(&"nocross") {
	    Some(_) => Some(Prefix::Nocross),
	    _ => None
	}
    }
    
    fn prefix(&mut self) -> Option<HashSet<Prefix>> {
	let mut prefixes: HashSet<Prefix> = HashSet::new();
	if let Some(prefix) = self.nofor() {
	    prefixes.insert(prefix);
	} else if let Some(prefix) = self.noback() {
	    prefixes.insert(prefix);
	}
	if let Some(prefix) = self.nocross() {
	    prefixes.insert(prefix);
	}
	if prefixes.is_empty() {
	    return None
	}
	Some(prefixes)
    }

    fn with_class(&mut self) -> Result<Option<WithClass>, ParseError> {
	if self.tokens.next_if_eq(&"before").is_some() {
	    match self.tokens.next() {
		Some(class) => {return Ok(Some(WithClass::Before{class: class.into()}))},
		None => {return Err(ParseError::ClassNameExpected { found: None })}
	    }
	}
	if self.tokens.next_if_eq(&"after").is_some() {
	    match self.tokens.next() {
		Some(class) => {return Ok(Some(WithClass::After{class: class.into()}))},
		None => {return Err(ParseError::ClassNameExpected { found: None })}, 
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
		_ => Err(ParseError::OpcodeExpected)
	    }
	} else {
	    Err(ParseError::OpcodeExpected)
	}
    }

    fn name(&mut self) -> Result<String, ParseError> {
	self.tokens.next()
	    .ok_or(ParseError::NameExpected)
	    .map(|s| s.to_string())
    }
    
    fn many_names(&mut self) -> Result<Vec<String>, ParseError> {
	let mut names: Vec<String> = Vec::new();
	while let Some(name) = self.tokens.next() {
	    names.push(name.into());
	}
	if names.len() > 1 {
	    	Ok(names)
	} else {
	    Err(ParseError::TokenExpected { expected: "At least one name expected".into(), found: None })
	}
    }
    
    fn one_char(&mut self) -> Result<char, ParseError> {
	let s = self.tokens.next()
	    .ok_or(ParseError::SingleCharExpected { found: None})?;
	let s = unescape(s)?;
	if s.chars().count() == 1 {
	    Ok(s.chars().next().unwrap())
	} else {
	    Err(ParseError::SingleCharExpected { found: Some(s.into())})
	}
    }
    
    fn number(&mut self) -> Result<i32, ParseError> {
	let s = self.tokens.next()
	    .ok_or(ParseError::NumberExpected { found: None})?;
	let number = s.parse::<i32>()?;
	Ok(number)
    }
    
    fn chars(&mut self) -> Result<String, ParseError> {
	let s = self.tokens.next()
	    .ok_or(ParseError::CharsExpected)?;
	unescape(s)
    }
    
    fn maybe_chars(&mut self) -> Option<String> {
	self.tokens.next()
	    .map(|s| s.to_string())
    }
    
    fn filename(&mut self) -> Result<String, ParseError> {
	self.tokens.next()
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
	Err(ParseError::TokenExpected { expected: "Before/After".into(),
					found: self.tokens.peek().map(|&s| s.into()) })
    }
    
    fn dots(&mut self) -> Result<BrailleChars, ParseError> {
	self.tokens.next()
	    .ok_or(ParseError::DotsExpected)?
	    .split('-')
	    .map(|chars| chars_to_dots(chars))
	    .collect()
    }

    fn many_dots(&mut self) -> Result<Vec<BrailleChars>, ParseError> {
	self.tokens.next()
	    .ok_or(ParseError::DotsExpected)?
	    .split(',')
	    .map(|chars| chars.split('-')
		 .map(|chars| chars_to_dots(chars))
		 .collect())
	    .collect()
    }
    
    fn multi_test(&mut self) -> Result<String, ParseError> {
	self.tokens.next()
	    .ok_or(ParseError::Multitest)
	    .map(|s| s.to_string())
    }
    
    fn multi_action(&mut self) -> Result<String, ParseError> {
	self.tokens.next()
	    .ok_or(ParseError::MultiAction)
	    .map(|s| s.to_string())
    }
    
    fn match_pre(&mut self) -> Result<String, ParseError> {
	self.tokens.next()
	    .ok_or(ParseError::MatchPre)
	    .map(|s| s.to_string())
    }
    
    fn match_post(&mut self) -> Result<String, ParseError> {
	self.tokens.next()
	    .ok_or(ParseError::MatchPost)
	    .map(|s| s.to_string())
    }
    
    fn rule(&mut self) -> Result<Rule, ParseError> {
	let _prefix = self.prefix();
	let _classes = self.with_classes();
	let opcode = match self.opcode()? {
	    Opcode::Include => Rule::Include { file: self.filename()? },
	    Opcode::Undefined => Rule::Undefined { chars: self.chars()?, dots: self.dots()?},
	    Opcode::Display => Rule::Display { dots: self.dots()? },
	    Opcode::Multind => Rule::Multind { dots: self.dots()?, names: self.many_names()? },

	    Opcode::Space => Rule::Space { chars: self.chars()?, dots: self.dots()? },
	    Opcode::Punctuation => Rule::Punctuation { chars: self.chars()?, dots: self.dots()? },
	    Opcode::Digit => Rule::Digit { character: self.one_char()?, dots: self.dots()? },
	    Opcode::Grouping => Rule::Grouping { name: self.name()?, chars: self.chars()?, dots: self.dots()? },
	    Opcode::Letter => Rule::Letter { chars: self.chars()?, dots: self.dots()? },
	    Opcode::Base => Rule::Base { name: self.name()?, from: self.one_char()?, to: self.one_char()? },
	    Opcode::Lowercase => Rule::Lowercase { character: self.one_char()?, dots: self.dots()? },
	    Opcode::Uppercase => Rule::Uppercase { character: self.one_char()?, dots: self.dots()? },
	    Opcode::Litdigit => Rule::Litdigit { character: self.one_char()?, dots: self.dots()? },
	    Opcode::Sign => Rule::Sign { character: self.one_char()?, dots: self.dots()? },
	    Opcode::Math => Rule::Math { character: self.one_char()?, dots: self.dots()? },

	    Opcode::Modeletter => Rule::Modeletter { name: self.name()?, dots: self.dots()? },
	    Opcode::Capsletter =>  Rule::Capsletter { dots: self.dots()? },
	    Opcode::Begmodeword => Rule::Begmodeword { name: self.name()?, dots: self.dots()? },
	    Opcode::Begcapsword => Rule::Begcapsword { dots: self.dots()? },
	    Opcode::Endcapsword => Rule::Endcapsword { dots: self.dots()? },
	    Opcode::Capsmodechars => Rule::Capsmodechars { chars: self.chars()? },
	    Opcode::Begcaps => Rule::Begcaps { dots: self.dots()? },
	    Opcode::Endcaps => Rule::Endcaps { dots: self.dots()? },
	    Opcode::Begcapsphrase => Rule::Begcapsphrase { dots: self.dots()? },
	    Opcode::Endcapsphrase => Rule::Endcapsphrase { position: self.position()?, dots: self.dots()? },
	    Opcode::Lencapsphrase => Rule::Lencapsphrase { number: self.number()? },
	    Opcode::Letsign => Rule::Letsign { dots: self.dots()? },
	    Opcode::Noletsign => Rule::Noletsign { chars: self.chars()? },
	    Opcode::Noletsignbefore => Rule::Noletsignbefore { chars: self.chars()? },
	    Opcode::Noletsignafter =>  Rule::Noletsignafter { chars: self.chars()? },
	    Opcode::Nocontractsign =>  Rule::Nocontractsign { dots: self.dots()? },
	    Opcode::Numsign => Rule::Numsign { dots: self.dots()? },
	    Opcode::Nonumsign => Rule::Nonumsign { dots: self.dots()? },
	    Opcode::Numericnocontchars => Rule::Numericnocontchars { chars: self.chars()? },
	    Opcode::Numericmodechars => Rule::Numericmodechars { chars: self.chars()? },
	    Opcode::Midendnumericmodechars => Rule::Midendnumericmodechars { chars: self.chars()? },

	    Opcode::Begmodephrase => Rule::Begmodephrase { name: self.name()?, dots: self.dots()? },
	    Opcode::Endmodephrase => Rule::Endmodephrase { name: self.name()?, position: self.position()?, dots: self.dots()? },
	    Opcode::Lenmodephrase => Rule::Lenmodephrase { name: self.name()?, number: self.number()? },
	    
	    Opcode::Seqdelimiter => Rule::Seqdelimiter { chars: self.chars()? },
	    Opcode::Seqbeforechars => Rule::Seqbeforechars { chars: self.chars()? },
	    Opcode::Seqafterchars => Rule::Seqafterchars { chars: self.chars()? },
	    Opcode::Seqafterpattern => Rule::Seqafterpattern { chars: self.chars()? },
	    Opcode::Seqafterexpression => Rule::Seqafterexpression { chars: self.chars()? },
	    
	    Opcode::Class => Rule::Class { name: self.name()?, chars: self.chars()? },
	    Opcode::Emphclass => Rule::Emphclass { name: self.name()? },
	    Opcode::Begemph => Rule::Begemph { name: self.name()?, dots: self.dots()? },
	    Opcode::Endemph => Rule::Endemph { name: self.name()?, dots: self.dots()? },
	    Opcode::Noemphchars => Rule::Noemphchars { name: self.name()?, chars: self.chars()? },
	    Opcode::Emphletter => Rule::Emphletter { name: self.name()?, dots: self.dots()? },
	    Opcode::Begemphword => Rule::Begemphword { name: self.name()?, dots: self.dots()? },
	    Opcode::Endemphword => Rule::Endemphword { name: self.name()?, dots: self.dots()? },
	    Opcode::Emphmodechars => Rule::Emphmodechars { name: self.name()?, chars: self.chars()? },
	    Opcode::Begemphphrase => Rule::Begemphphrase { name: self.name()?, dots: self.dots()? },
	    Opcode::Endemphphrase => Rule::Endemphphrase { name: self.name()?, position: self.position()?, dots: self.dots()? },
	    Opcode::Lenemphphrase => Rule::Lenemphphrase { name: self.name()?, number: self.number()? },

	    Opcode::Begcomp => Rule::Begcomp { dots: self.dots()? },
	    Opcode::Endcomp => Rule::Endcomp { dots: self.dots()? },

	    Opcode::Decpoint => Rule::Decpoint { chars: self.chars()?, dots: self.dots()? },
	    Opcode::Hyphen => Rule::Hyphen { dots: self.dots()? },

	    Opcode::Capsnocont => Rule::Capsnocont {},

	    Opcode::Compbrl => Rule::Compbrl { chars: self.chars()? },
	    Opcode::Comp6 => Rule::Comp6 { chars: self.chars()?, dots: self.dots()? },
	    Opcode::Nocont => Rule::Nocont { chars: self.chars()? },
	    Opcode::Replace => Rule::Replace { chars: self.chars()?, replacement: self.maybe_chars() },
	    Opcode::Always => Rule::Always { chars: self.chars()?, dots: self.dots()? },
	    Opcode::Repeated => Rule::Repeated { chars: self.chars()?, dots: self.dots()? },
	    Opcode::Repword => Rule::Repword { chars: self.chars()?, dots: self.dots()? },
	    Opcode::Rependword => Rule::Rependword { chars: self.chars()?, dots: self.dots()? },
	    Opcode::Largesign => Rule::Largesign { chars: self.chars()?, dots: self.dots()? },
	    Opcode::Word => Rule::Word { chars: self.chars()?, dots: self.dots()? },
	    Opcode::Syllable => Rule::Syllable { chars: self.chars()?, dots: self.dots()? },
	    Opcode::Joinword => Rule::Joinword { chars: self.chars()?, dots: self.dots()? },
	    Opcode::Lowword => Rule::Lowword { chars: self.chars()?, dots: self.dots()? },
	    Opcode::Contraction => Rule::Contraction { chars: self.chars()? },
	    Opcode::Sufword => Rule::Sufword { chars: self.chars()?, dots: self.dots()? },
	    Opcode::Prfword => Rule::Prfword { chars: self.chars()?, dots: self.dots()? },
	    Opcode::Begword => Rule::Begword { chars: self.chars()?, dots: self.dots()? },
	    Opcode::Begmidword => Rule::Begmidword { chars: self.chars()?, dots: self.dots()? },
	    Opcode::Midword => Rule::Midword { chars: self.chars()?, dots: self.dots()? },
	    Opcode::Midendword => Rule::Midendword { chars: self.chars()?, dots: self.dots()? },
	    Opcode::Endword => Rule::Endword { chars: self.chars()?, dots: self.dots()? },
	    Opcode::Partword => Rule::Partword { chars: self.chars()?, dots: self.dots()? },
	    Opcode::Exactdots => Rule::Exactdots { chars: self.chars()? },
	    Opcode::Prepunc => Rule::Prepunc { chars: self.chars()?, dots: self.dots()? },
	    Opcode::Postpunc => Rule::Postpunc { chars: self.chars()?, dots: self.dots()? },
	    Opcode::Begnum => Rule::Begnum { chars: self.chars()?, dots: self.dots()? },
	    Opcode::Midnum => Rule::Midnum { chars: self.chars()?, dots: self.dots()? },
	    Opcode::Endnum => Rule::Endnum { chars: self.chars()?, dots: self.dots()? },
	    Opcode::Joinnum => Rule::Joinnum { chars: self.chars()?, dots: self.dots()? },

	    Opcode::Attribute => Rule::Attribute { name: self.name()?, chars: self.chars()? },
	    
	    Opcode::Swapcd => Rule::Swapcd { name: self.name()?, chars: self.chars()?, dots: self.many_dots()? },
	    Opcode::Swapdd => Rule::Swapdd { name: self.name()?, dots: self.many_dots()?, replacement: self.many_dots()? },
	    Opcode::Swapcc => Rule::Swapcc { name: self.name()?, chars: self.chars()?, replacement: self.chars()? },

	    Opcode::Context => Rule::Context { test: self.multi_test()?, action: self.multi_action()? },
	    Opcode::Pass2 => Rule::Pass2 { test: self.multi_test()?, action: self.multi_action()? },
	    Opcode::Pass3 => Rule::Pass3 { test: self.multi_test()?, action: self.multi_action()? },
	    Opcode::Pass4 => Rule::Pass4 { test: self.multi_test()?, action: self.multi_action()? },
	    Opcode::Correct => Rule::Correct { test: self.multi_test()?, action: self.multi_action()? },
	    
	    Opcode::Match => Rule::Match { pre: self.match_pre()?, chars: self.chars()?, post: self.match_post()?, dots: self.dots()? },
	    Opcode::Literal => Rule::Literal { chars: self.chars()? },
	};
	Ok(opcode)
    }

}

fn main() {
    let args: Vec<String> = env::args().collect();

    let filename = &args[1];
    for (line_no, line) in read_to_string(filename).unwrap().lines().enumerate() {
        // println!("{}", line);
	if !line.starts_with("#") && !line.is_empty() {
	    let rule = RuleParser::new(line).rule();
	    match rule {
		Ok(rule) => {println!("{:?}", rule)},
		Err(e) => {eprintln!("{}:{}: {:?}", filename, line_no+1, e);}
	    }
	}
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn nocross_test() {
        assert_eq!(Some(Prefix::Nocross), RuleParser::new(&"nocross").nocross());
        assert_eq!(Some(Prefix::Nocross), RuleParser::new(&"nocross nofor").nocross());
        assert_eq!(None, RuleParser::new(&"nofor nocross").nocross());
        assert_eq!(None, RuleParser::new(&"nofor").nocross());
    }

    #[test]
    fn nofor_test() {
        assert_eq!(Some(Prefix::Nofor), RuleParser::new(&" nofor ").nofor());
        assert_eq!(Some(Prefix::Nofor), RuleParser::new(&"nofor nocross").nofor());
        assert_eq!(None, RuleParser::new(&"nocross nofor").nofor());
        assert_eq!(None, RuleParser::new(&"").nofor());
    }

    #[test]
    fn withclass_test() {
        assert_eq!(Ok(Some(WithClass::Before { class: "foo".into() })), RuleParser::new(&"before foo ").with_class());
        assert_eq!(Ok(Some(WithClass::After { class: "foo".into() })), RuleParser::new(&" after foo ").with_class());
        assert_eq!(Err(ParseError::ClassNameExpected { found: None }), RuleParser::new(&" after ").with_class());
    }

    #[test]
    fn withclasses_test() {
        assert_eq!(
	    Ok(Some(vec![
		WithClass::Before { class: "foo".into() },
		WithClass::After  { class: "bar".into() },
	    ])),
	    RuleParser::new(&"before foo after bar").with_classes());
	
        assert_eq!(Err(ParseError::ClassNameExpected { found: None }), RuleParser::new(&"before foo after").with_classes());
    }

    #[test]
    fn opcode_test() {
        assert_eq!(Ok(Opcode::Include), RuleParser::new(&"include").opcode());
        assert_eq!(Ok(Opcode::Always), RuleParser::new(&"always foo after").opcode());

        assert_eq!(Err(ParseError::OpcodeExpected), RuleParser::new(&"h").opcode());
        assert_eq!(Err(ParseError::OpcodeExpected), RuleParser::new(&"hello world").opcode());
    }

    #[test]
    fn rule_test() {
        assert_eq!(Ok(Rule::Include{ file: "foo.ctb".into()}), RuleParser::new(&"include foo.ctb").rule());
        assert_eq!(Err(ParseError::FilenameExpected), RuleParser::new(&"include").rule());
        assert_eq!(Err(ParseError::OpcodeExpected), RuleParser::new(&"Include foo.ctb").rule());
}

}

