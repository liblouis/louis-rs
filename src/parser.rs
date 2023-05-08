use std::fmt;
use std::error;

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::bytes::complete::is_a;
use nom::bytes::complete::is_not;
use nom::character::complete::alpha1;
use nom::character::complete::hex_digit1;
use nom::character::complete::line_ending;
use nom::character::complete::not_line_ending;
use nom::character::complete::space0;
use nom::character::complete::space1;
use nom::character::complete::digit1;
use nom::character::complete::none_of;
use nom::combinator::all_consuming;
use nom::combinator::map;
use nom::combinator::map_res;
use nom::combinator::opt;
use nom::combinator::success;
use nom::combinator::verify;
use nom::error::ParseError;
use nom::multi::many0;
use nom::multi::separated_list1;
use nom::sequence::tuple;
use nom::error::Error;

use enumset::EnumSet;
use enumset::EnumSetType;
use enumset::enum_set;

use nom::IResult;
//use nom_unicode::complete::alpha1 as unicode_alpha1;
use nom_unicode::complete::digit1 as unicode_digit1;

#[derive(PartialEq, Debug)]
pub enum Line {
    Empty,
    Comment { comment: String },
    Rule { rule: Rule, comment: String },
}

#[derive(PartialEq, Debug)]
pub enum Rule {
    Include { filename: String },
    Undefined { dots: BrailleChars },
    Display { chars: String, dots: BrailleChars, prefixes: Prefixes },

    // Character-Definition Opcodes
    Space { ch: char, dots: BrailleChars, prefixes: Prefixes},
    Multind { dots: BrailleChars, opcodes: Vec<String>, prefixes: Prefixes },
    Punctuation { ch: char, dots: BrailleChars, prefixes: Prefixes},
    Digit { ch: char, dots: BrailleChars },
    Letter { ch: char, dots: BrailleCharsOrImplicit },
    Lowercase { word: String, dots: BrailleChars, prefixes: Prefixes },
    Uppercase { word: String, dots: BrailleChars, prefixes: Prefixes },
    Litdigit { ch: char, dots: BrailleChars },
    Sign { ch: char, dots: BrailleChars, prefixes: Prefixes },
    Math { ch: char, dots: BrailleChars },
    Grouping { name: String, chars: String, dots: Vec<BrailleChars> },
    Base { attribute: String, derived: char, base: char },
    Attribute { name: String, chars: String },

    // Braille Indicator Opcodes
    Modeletter { attribute: String, dots: BrailleChars, prefixes: Prefixes},
    Capsletter { dots: BrailleChars, prefixes: Prefixes},
    Begmodeword { attribute: String, dots: BrailleChars, prefixes: Prefixes},
    Begcapsword { dots: BrailleChars, prefixes: Prefixes},
    Endmodeword { attribute: BrailleChars, prefixes: Prefixes},
    Endcapsword { dots: BrailleChars, prefixes: Prefixes},
    Capsmodechars { chars: String},
    Begmode { attribute: String, dots: BrailleChars},
    Begcaps { dots: BrailleChars},
    Endmode { attribute: String, dots: BrailleChars},
    Endcaps { dots: BrailleChars },
    Letsign { dots: BrailleChars },
    Noletsign { characters: String },
    Noletsignbefore { characters: String },
    Noletsignafter { characters: String },
    Nocontractsign { dots: BrailleChars },
    Numsign { dots: BrailleChars },
    Nonumsign { dots: BrailleChars },
    Numericnocontchars { characters: String },
    Numericmodechars { characters: String },
    Midendnumericmodechars { characters: String },
    Begcapsphrase { dots: BrailleChars},
    Endcapsphrase { dots: BrailleChars, position: Position},
    Lencapsphrase { length: u8},

    // Standing Alone Sequences
    Seqdelimiter { characters: String },
    Seqbeforechars { characters: String },
    Seqafterchars { characters: String },
    Seqafterpattern { pattern: String },
    Seqafterexpression { expression: String },

    // Emphasis Opcodes
    Class { name: String, chars: String },
    Emphclass { name: String },
    Begemph { name: String, dots: BrailleChars, prefixes: Prefixes },
    Endemph { name: String, dots: BrailleChars, prefixes: Prefixes },
    Noemphchars { name: String, chars: String},
    Emphletter { name: String, dots: BrailleChars },
    Begemphword { name: String,  dots: BrailleChars },
    Endemphword { name: String, dots: BrailleChars },
    Emphmodechars { name: String, chars: String },
    Begemphphrase { name: String, dots: BrailleChars },
    Endemphphrase { name: String, dots: BrailleChars, position: Position },
    Lenemphphrase { name: String, length: u8 },

    // Computer braille
    Begcomp { dots: BrailleChars, prefixes: Prefixes},
    Endcomp { dots: BrailleChars, prefixes: Prefixes},

    // Special Symbol Opcodes
    Decpoint { characters: String, dots: BrailleChars},
    Hyphen { characters: String, dots: BrailleChars, prefixes: Prefixes},

    // Special Processing Opcodes
    Capsnocont,

    // Translation Opcodes
    Compbrl { characters: String},
    Comp6 { characters: String, dots: BrailleCharsOrImplicit},
    Nocont {characters: String},
    Replace {characters: String, replacement: Option<String> },
    Always {characters: String, dots: BrailleCharsOrImplicit, prefixes: Prefixes},
    Repeated {characters: String, dots: BrailleChars},
    Repword  {characters: String, dots: BrailleChars},
    Rependword  {characters: String, dots: BrailleChars, other: BrailleChars},
    Largesign  {characters: String, dots: BrailleChars},
    Word  {characters: String, dots: BrailleCharsOrImplicit},
    Syllable { word: String, dots: BrailleCharsOrImplicit },
    Joinword { word: String, dots: BrailleChars },
    Lowword  {characters: String, dots: BrailleChars},
    Contraction  {characters: String},
    Sufword  {characters: String, dots: BrailleCharsOrImplicit},
    Prfword  {characters: String, dots: BrailleCharsOrImplicit},
    Begword  {characters: String, dots: BrailleCharsOrImplicit},
    Begmidword  {characters: String, dots: BrailleCharsOrImplicit},
    Midword  {characters: String, dots: BrailleCharsOrImplicit},
    Midendword  {characters: String, dots: BrailleCharsOrImplicit, prefixes: Prefixes},
    Endword  {characters: String, dots: BrailleCharsOrImplicit},
    Partword {characters: String, dots: BrailleCharsOrImplicit},
    Prepunc {characters: String, dots: BrailleChars},
    Postpunc {characters: String, dots: BrailleChars},
    Begnum {characters: String, dots: BrailleChars},
    Midnum {characters: String, dots: BrailleChars},
    Endnum {characters: String, dots: BrailleCharsOrImplicit},
    Joinnum {characters: String, dots: BrailleChars},

    // Swap Opcodes
    Swapcd {name: String, characters: String, dots: Vec<BrailleChars> },
    Swapdd {name: String, dots: Vec<BrailleChars>, dotpattern: Vec<BrailleChars>},
    Swapcc {name: String, chars: String, replacement: String},

    // Context Opcodes
    Context {test: String, action: String, prefixes: Prefixes},
    Pass2 {test: String, action: String, prefixes: Prefixes},
    Pass3 {test: String, action: String, prefixes: Prefixes},
    Pass4 {test: String, action: String, prefixes: Prefixes},

    // Correct Opcode
    Correct {test: String, action: String, prefixes: Prefixes},

    // Match Opcode
    Match { pre: String, characters: String, post: String, dots: BrailleCharsOrImplicit, prefixes: Prefixes},

    // undocumented opcodes
    Literal {characters: String, }
}

#[derive(EnumSetType, Debug)]
pub enum Prefix {
    Noback,
    Nofor,
    Nocross,
}

#[derive(PartialEq, Debug)]
pub enum Position {
    Before,
    After,
}

type Prefixes = EnumSet<Prefix>;

#[derive(EnumSetType, Debug)]
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

type BrailleChar = EnumSet<BrailleDot>;
type BrailleChars = Vec<BrailleChar>;

#[derive(PartialEq, Debug)]
pub enum BrailleCharsOrImplicit {
    Implicit,
    Explicit(BrailleChars),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseBrailleError;

impl fmt::Display for ParseBrailleError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        "provided string was not `true` or `false`".fmt(f)
    }
}
impl error::Error for ParseBrailleError {
    fn description(&self) -> &str {
        "failed to parse Braille dot"
    }
}
fn char_to_dot(char: char) -> Result<BrailleDot, ParseBrailleError> {
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
        _ => Err(ParseBrailleError{}),
    }
}

fn chars_to_dots(chars: &str) -> BrailleChar {
    chars.chars().map(|c| char_to_dot(c).unwrap()).collect()
}

pub fn chars(input: &str) -> IResult<&str, &str> {
    is_not(" \t\r\n")(input)
    //unicode_alpha1(input)
}

pub fn unicode_literal(input: &str) -> IResult<&str, char> {
    let (i, num) = map_res(hex_digit1, |s| u32::from_str_radix(s, 16))(input)?;

    let c = char::from_u32(num).unwrap();
    Ok((i, c))
}

fn escape_sequence(input: &str) -> IResult<&str, char> {
    alt((
	map(tag(r"\\"), |_| '\\'),
	map(tag(r"\f"), |_| '\x0C'),
	map(tag(r"\n"), |_| '\n'),
	map(tag(r"\r"), |_| '\r'),
	map(tag(r"\s"), |_| ' '),
	map(tag(r"\t"), |_| '\t'),
	map(tag(r"\v"), |_| '\x0B'),
	map(tag(r"\e"), |_| '\x1B'),
    ))(input)
}

pub fn escaped_char(i: &str) -> IResult<&str, char> {
    let (input, (_, c)) = tuple((tag("\\x"), unicode_literal))(i)?;
    Ok((input, c))
}

pub fn single_char(input: &str) -> IResult<&str, char> {
    alt((
	escape_sequence,
	escaped_char,
	none_of(" \t\r\n"),
    ))(input)
}

pub fn ascii_chars(input: &str) -> IResult<&str, &str> {
    alpha1(input)
}

pub fn filename(input: &str) -> IResult<&str, &str> {
    is_a("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-.")(input)
}

pub fn braillechars_or_implicit(input: &str) -> IResult<&str, BrailleCharsOrImplicit> {
    alt((
	map(tag("="), |_| BrailleCharsOrImplicit::Implicit),
	map(dots, |dots| BrailleCharsOrImplicit::Explicit(dots))
    ))(input)
}

pub fn dots(i: &str) -> IResult<&str, BrailleChars> {
    let (input, dots) = separated_list1(tag("-"), hex_digit1)(i)?;
    let braille_chars: Vec<BrailleChar> = dots
	.iter()
	.map(|chars| chars_to_dots(chars))
	.collect();
    Ok((input, braille_chars))
}

fn single_unicode_digit(input: &str) -> IResult<&str, char> {
    alt((
	escaped_char,
	one_unicode_digit,
    ))(input)
}

fn one_unicode_digit(input: &str) -> IResult<&str, char> {
    let (input, digit) = verify(unicode_digit1, |s: &str| s.chars().count() == 1)(input)?;
    match digit.char_indices().next() {
	Some((i, c)) => match input.get(i..) {
	    Some(s) => Ok((s, c)),
	    _ => Err(nom::Err::Error(ParseError::from_error_kind(input, nom::error::ErrorKind::Digit))),
	}
	_ => Err(nom::Err::Error(ParseError::from_error_kind(input, nom::error::ErrorKind::Digit))),
    }
}

pub fn number(input: &str) -> IResult<&str, u8> {
    map_res(digit1, |s: &str| s.parse::<u8>())(input)
}

pub fn before_or_after(input: &str) -> IResult<&str, Position> {
    alt((
	map(tag("before"), |_| Position::Before),
	map(tag("after"), |_| Position::After)))(input)
}

fn prefixes(i: &str) -> IResult<&str, Prefixes> {
    alt((
	map(tuple((tag("noback"), space1, tag("nocross"), space1)), |_| Prefix::Noback | Prefix::Nocross),
	map(tuple((tag("nofor"), space1, tag("nocross"), space1)), |_| Prefix::Nofor | Prefix::Nocross),
	map(tuple((tag("nofor"), space1)), |_| enum_set!(Prefix::Nofor)),
	map(tuple((tag("noback"), space1)), |_| enum_set!(Prefix::Noback)),
	map(tuple((tag("nocross"), space1)), |_| enum_set!(Prefix::Nocross)),
	success::<_,_,Error<_>>(Prefixes::empty()),
    ))(i)
}

pub fn include(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, filename)) = tuple((tag("include"), space1, filename))(i)?;
    Ok((input, Rule::Include { filename: filename.to_string() }))
}

pub fn undefined(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, dots)) = tuple((tag("undefined"), space1, dots))(i)?;
    Ok((input, Rule::Undefined { dots }))
}

pub fn display(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, chars, _, dots)) = tuple((opt(prefixes), tag("display"), space1, chars, space1, dots))(i)?;
    Ok((input, Rule::Display { chars: chars.to_string(), dots, prefixes: prefixes.unwrap() }))
}

pub fn space(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, ch, _, dots)) = tuple((opt(prefixes), tag("space"), space1, single_char, space1, dots))(i)?;
    Ok((input, Rule::Space { ch, dots, prefixes: prefixes.unwrap() }))
}

pub fn multind(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, dots, _, opcodes)) = tuple((opt(prefixes), tag("multind"), space1, dots, space1, separated_list1(space1, chars)))(i)?;
    // FIXME: Make sure the opcodes are valid
    Ok((input, Rule::Multind { dots, opcodes: opcodes.iter().map(|s| s.to_string()).collect(), prefixes: prefixes.unwrap() }))
}

pub fn punctuation(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, ch, _, dots)) = tuple((opt(prefixes), tag("punctuation"), space1, single_char, space1, dots))(i)?;
    Ok((input, Rule::Punctuation { ch, dots, prefixes: prefixes.unwrap() }))
}

pub fn digit(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, ch, _, dots)) = tuple((tag("digit"), space1, single_char, space1, dots))(i)?;
    Ok((input, Rule::Digit { ch, dots }))
}

pub fn letter(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, ch, _, dots)) = tuple((tag("letter"), space1, single_char, space1, braillechars_or_implicit))(i)?;
    Ok((input, Rule::Letter { ch, dots }))
}

pub fn lowercase(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, word, _, dots)) = tuple((opt(prefixes), tag("lowercase"), space1, single_char, space1, dots))(i)?;
    Ok((input, Rule::Lowercase { word: word.to_string(), dots, prefixes: prefixes.unwrap() }))
}

pub fn uppercase(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, word, _, dots)) = tuple((opt(prefixes), tag("uppercase"), space1, single_char, space1, dots))(i)?;
    Ok((input, Rule::Uppercase { word: word.to_string(), dots, prefixes: prefixes.unwrap() }))
}

pub fn litdigit(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, ch, _, dots)) = tuple((tag("litdigit"), space1, single_unicode_digit, space1, dots))(i)?;
    Ok((input, Rule::Litdigit { ch, dots }))
}

pub fn sign(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, ch, _, dots)) = tuple((opt(prefixes), tag("sign"), space1, single_char, space1, dots))(i)?;
    Ok((input, Rule::Sign { ch, dots, prefixes: prefixes.unwrap() }))
}

pub fn math(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, ch, _, dots)) = tuple((tag("math"), space1, single_char, space1, dots))(i)?;
    Ok((input, Rule::Math { ch, dots }))
}

pub fn grouping(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, name, _, characters, _, dots)) = tuple((tag("grouping"), space1, ascii_chars, space1, ascii_chars, space1, separated_list1(tag(","), dots)))(i)?;
    Ok((input, Rule::Grouping { name: name.to_string(), chars: characters.to_string(), dots}))
}

pub fn base(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, attribute, _, derived, _, base)) = tuple((tag("base"), space1, ascii_chars, space1, single_char, space1, single_char))(i)?;
    Ok((input, Rule::Base { attribute: attribute.to_string(), derived, base }))
}

pub fn attribute(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, name, _, chars)) = tuple((tag("attribute"), space1, ascii_chars, space1, chars))(i)?;
    Ok((input, Rule::Attribute { name: name.to_string(), chars: chars.to_string() }))
}

pub fn modeletter(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, chars, _, dots)) = tuple((opt(prefixes), tag("modeletter"), space1, ascii_chars, space1, dots))(i)?;
    Ok((input, Rule::Modeletter { attribute: chars.to_string(), dots, prefixes: prefixes.unwrap() }))
}

pub fn capsletter(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, dots)) = tuple((opt(prefixes), tag("capsletter"), space1, dots))(i)?;
    Ok((input, Rule::Capsletter { dots, prefixes: prefixes.unwrap() }))
}

pub fn begmodeword(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, chars, _, dots)) = tuple((opt(prefixes), tag("begmodeword"), space1, ascii_chars, space1, dots))(i)?;
    Ok((input, Rule::Begmodeword { attribute: chars.to_string(), dots, prefixes: prefixes.unwrap() }))
}

pub fn begcapsword(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, dots)) = tuple((opt(prefixes), tag("begcapsword"), space1, dots))(i)?;
    Ok((input, Rule::Begcapsword { dots, prefixes: prefixes.unwrap() }))
}

pub fn endcapsword(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, dots)) = tuple((opt(prefixes), tag("endcapsword"), space1, dots))(i)?;
    Ok((input, Rule::Endcapsword { dots, prefixes: prefixes.unwrap() }))
}

pub fn capsmodechars(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, chars)) = tuple((tag("capsmodechars"), space1, chars))(i)?;
    Ok((input, Rule::Capsmodechars { chars: chars.to_string() }))
}

pub fn begcaps(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, dots)) = tuple((tag("begcaps"), space1, dots))(i)?;
    Ok((input, Rule::Begcaps { dots }))
}

pub fn endcaps(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, dots)) = tuple((tag("endcaps"), space1, dots))(i)?;
    Ok((input, Rule::Endcaps { dots }))
}

pub fn letsign(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, dots)) = tuple((tag("letsign"), space1, dots))(i)?;
    Ok((input, Rule::Letsign { dots }))
}

pub fn noletsign(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, characters)) = tuple((tag("noletsign"), space1, chars))(i)?;
    Ok((input, Rule::Noletsign { characters: characters.to_string() }))
}

pub fn noletsignbefore(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, characters)) = tuple((tag("noletsignbefore"), space1, chars))(i)?;
    Ok((input, Rule::Noletsignbefore { characters: characters.to_string() }))
}

pub fn noletsignafter(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, characters)) = tuple((tag("noletsignafter"), space1, chars))(i)?;
    Ok((input, Rule::Noletsignafter { characters: characters.to_string() }))
}

pub fn nocontractsign(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, dots)) = tuple((tag("nocontractsign"), space1, dots))(i)?;
    Ok((input, Rule::Nocontractsign { dots }))
}

pub fn numsign(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, dots)) = tuple((tag("numsign"), space1, dots))(i)?;
    Ok((input, Rule::Numsign { dots }))
}

fn nonumsign(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, dots)) = tuple((tag("nonumsign"), space1, dots))(i)?;
    Ok((input, Rule::Nonumsign { dots }))
}

pub fn numericnocontchars(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, characters)) = tuple((tag("numericnocontchars"), space1, chars))(i)?;
    Ok((input, Rule::Numericnocontchars { characters: characters.to_string() }))
}

pub fn numericmodechars(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, characters)) = tuple((tag("numericmodechars"), space1, chars))(i)?;
    Ok((input, Rule::Numericmodechars { characters: characters.to_string() }))
}

pub fn midendnumericmodechars(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, characters)) = tuple((tag("midendnumericmodechars"), space1, chars))(i)?;
    Ok((input, Rule::Midendnumericmodechars { characters: characters.to_string() }))
}

pub fn begcapsphrase(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, dots)) = tuple((tag("begcapsphrase"), space1, dots))(i)?;
    Ok((input, Rule::Begcapsphrase { dots }))
}

pub fn endcapsphrase(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, position, _, dots)) = tuple((tag("endcapsphrase"), space1, before_or_after, space1, dots))(i)?;
    Ok((input, Rule::Endcapsphrase { dots, position }))
}

pub fn lencapsphrase(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, length)) = tuple((tag("lencapsphrase"), space1, number))(i)?;
    Ok((input, Rule::Lencapsphrase { length }))
}

pub fn seqdelimiter(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, characters)) = tuple((tag("seqdelimiter"), space1, chars))(i)?;
    Ok((input, Rule::Seqdelimiter { characters: characters.to_string() }))
}

pub fn seqbeforechars(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, characters)) = tuple((tag("seqbeforechars"), space1, chars))(i)?;
    Ok((input, Rule::Seqbeforechars { characters: characters.to_string() }))
}

pub fn seqafterchars(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, characters)) = tuple((tag("seqafterchars"), space1, chars))(i)?;
    Ok((input, Rule::Seqafterchars { characters: characters.to_string() }))
}

pub fn seqafterpattern(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, characters)) = tuple((tag("seqafterpattern"), space1, chars))(i)?;
    Ok((input, Rule::Seqafterpattern { pattern: characters.to_string() }))
}

pub fn seqafterexpression(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, expression)) = tuple((tag("seqafterexpression"), space1, chars))(i)?;
    Ok((input, Rule::Seqafterexpression { expression: expression.to_string() }))
}

pub fn class(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, name, _, chars)) = tuple((tag("class"), space1, chars, space1, chars))(i)?;
    Ok((input, Rule::Class { name: name.to_string(), chars: chars.to_string() }))
}

pub fn emphclass(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, name)) = tuple((tag("emphclass"), space1, chars))(i)?;
    Ok((input, Rule::Emphclass { name: name.to_string() }))
}

pub fn begemph(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, name, _, dots)) = tuple((opt(prefixes), tag("begemph"), space1, chars, space1, dots))(i)?;
    Ok((input, Rule::Begemph { name: name.to_string(), dots, prefixes: prefixes.unwrap() }))
}

pub fn endemph(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, name, _, dots)) = tuple((opt(prefixes), tag("endemph"), space1, chars, space1, dots))(i)?;
    Ok((input, Rule::Endemph { name: name.to_string(), dots, prefixes: prefixes.unwrap() }))
}

pub fn noemphchars(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, name, _, chars)) = tuple((tag("noemphchars"), space1, chars, space1, chars))(i)?;
    Ok((input, Rule::Noemphchars { name: name.to_string(), chars: chars.to_string() }))
}

pub fn emphletter(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, name, _, dots)) = tuple((tag("emphletter"), space1, chars, space1, dots))(i)?;
    Ok((input, Rule::Emphletter { name: name.to_string(), dots }))
}

pub fn begemphword(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, name, _, dots)) = tuple((tag("begemphword"), space1, chars, space1, dots))(i)?;
    Ok((input, Rule::Begemphword { name: name.to_string(), dots }))
}

pub fn endemphword(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, name, _, dots)) = tuple((tag("endemphword"), space1, chars, space1, dots))(i)?;
    Ok((input, Rule::Endemphword { name: name.to_string(), dots }))
}

pub fn emphmodechars(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, name, _, chars)) = tuple((tag("emphmodechars"), space1, chars, space1, chars))(i)?;
    Ok((input, Rule::Emphmodechars { name: name.to_string(), chars: chars.to_string() }))
}

pub fn begemphphrase(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, name, _, dots)) = tuple((tag("begemphphrase"), space1, chars, space1, dots))(i)?;
    Ok((input, Rule::Begemphphrase { name: name.to_string(), dots }))
}

pub fn endemphphrase(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, name, _, position, _, dots)) = tuple((tag("endemphphrase"), space1, chars, space1, before_or_after, space1, dots))(i)?;
    Ok((input, Rule::Endemphphrase { name: name.to_string(), dots, position }))
}

pub fn lenemphphrase(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, name, _, length)) = tuple((tag("lenemphphrase"), space1, chars, space1, number))(i)?;
    Ok((input, Rule::Lenemphphrase { name: name.to_string(), length }))
}

pub fn begcomp(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, dots)) = tuple((opt(prefixes), tag("begcomp"), space1, dots))(i)?;
    Ok((input, Rule::Begcomp { dots, prefixes: prefixes.unwrap() }))
}

pub fn endcomp(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, dots)) = tuple((opt(prefixes), tag("endcomp"), space1, dots))(i)?;
    Ok((input, Rule::Endcomp { dots, prefixes: prefixes.unwrap() }))
}

pub fn decpoint(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, chars, _, dots)) = tuple((tag("decpoint"), space1, chars, space1, dots))(i)?;
    Ok((input, Rule::Decpoint { characters: chars.to_string(), dots }))
}

pub fn hyphen(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, chars, _, dots)) = tuple((opt(prefixes), tag("hyphen"), space1, chars, space1, dots))(i)?;
    Ok((input, Rule::Hyphen { characters: chars.to_string(), dots, prefixes: prefixes.unwrap() }))
}

pub fn capsnocont(i: &str) -> IResult<&str, Rule> {
    let (input, _) = tag("capsnocont")(i)?;
    Ok((input, Rule::Capsnocont))
}

pub fn compbrl(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, chars)) = tuple((tag("compbrl"), space1, chars))(i)?;
    Ok((input, Rule::Compbrl { characters: chars.to_string() }))
}

pub fn comp6(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, chars, _, dots)) = tuple((tag("comp6"), space1, chars, space1, braillechars_or_implicit))(i)?;
    Ok((input, Rule::Comp6 { characters: chars.to_string(), dots }))
}

pub fn nocont(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, chars)) = tuple((tag("nocont"), space1, chars))(i)?;
    Ok((input, Rule::Compbrl { characters: chars.to_string() }))
}

pub fn replace(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, chars, replacement)) = tuple((tag("replace"), space1, chars, opt(tuple((space1, chars)))))(i)?;
    let replacement = match replacement {
	Some((_, replacement)) => Some(replacement.to_string()),
	None => None,
    };
    Ok((input, Rule::Replace { characters: chars.to_string(), replacement }))
}

pub fn always(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, chars, _, dots)) = tuple((opt(prefixes), tag("always"), space1, chars, space1, braillechars_or_implicit))(i)?;
    Ok((input, Rule::Always { characters: chars.to_string(), dots, prefixes: prefixes.unwrap() }))
}

pub fn repeated(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, chars, _, dots)) = tuple((tag("repeated"), space1, chars, space1, dots))(i)?;
    Ok((input, Rule::Repeated { characters: chars.to_string(), dots }))
}

pub fn repword(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, chars, _, dots)) = tuple((tag("repword"), space1, chars, space1, dots))(i)?;
    Ok((input, Rule::Repword { characters: chars.to_string(), dots }))
}

pub fn rependword(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, chars, _, dots, _, other)) = tuple((tag("repword"), space1, chars, space1, dots, space1, dots))(i)?;
    Ok((input, Rule::Rependword { characters: chars.to_string(), dots, other }))
}

pub fn largesign(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, chars, _, dots)) = tuple((tag("largesign"), space1, chars, space1, dots))(i)?;
    Ok((input, Rule::Largesign { characters: chars.to_string(), dots }))
}

pub fn word(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, chars, _, dots)) = tuple((tag("word"), space1, chars, space1, braillechars_or_implicit))(i)?;
    Ok((input, Rule::Word { characters: chars.to_string(), dots }))
}

pub fn syllable(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, word, _, dots)) = tuple((tag("syllable"), space1, chars, space1, braillechars_or_implicit))(i)?;
    Ok((input, Rule::Syllable { word: word.to_string(), dots }))
}

pub fn joinword(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, word, _, dots)) = tuple((
        tag("joinword"), space1, chars, space1, dots,
    ))(i)?;
    Ok((input, Rule::Joinword { word: word.to_string(), dots }))
}

pub fn lowword(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, chars, _, dots)) = tuple((tag("lowword"), space1, chars, space1, dots))(i)?;
    Ok((input, Rule::Lowword { characters: chars.to_string(), dots }))
}

pub fn contraction(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, chars)) = tuple((tag("contraction"), space1, chars))(i)?;
    Ok((input, Rule::Contraction { characters: chars.to_string() }))
}

pub fn sufword(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, chars, _, dots)) = tuple((tag("sufword"), space1, chars, space1, braillechars_or_implicit))(i)?;
    Ok((input, Rule::Sufword { characters: chars.to_string(), dots }))
}

pub fn prfword(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, chars, _, dots)) = tuple((tag("prfword"), space1, chars, space1, braillechars_or_implicit))(i)?;
    Ok((input, Rule::Prfword { characters: chars.to_string(), dots }))
}

pub fn begword(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, chars, _, dots)) = tuple((tag("begword"), space1, chars, space1, braillechars_or_implicit))(i)?;
    Ok((input, Rule::Begword { characters: chars.to_string(), dots }))
}

pub fn begmidword(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, chars, _, dots)) = tuple((tag("begmidword"), space1, chars, space1, braillechars_or_implicit))(i)?;
    Ok((input, Rule::Begmidword { characters: chars.to_string(), dots }))
}

pub fn midword(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, chars, _, dots)) = tuple((tag("midword"), space1, chars, space1, braillechars_or_implicit))(i)?;
    Ok((input, Rule::Midword { characters: chars.to_string(), dots }))
}

pub fn midendword(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, chars, _, dots)) = tuple((opt(prefixes), tag("midendword"), space1, chars, space1, braillechars_or_implicit))(i)?;
    Ok((input, Rule::Midendword { characters: chars.to_string(), dots , prefixes: prefixes.unwrap()}))
}

pub fn endword(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, chars, _, dots)) = tuple((tag("endword"), space1, chars, space1, braillechars_or_implicit))(i)?;
    Ok((input, Rule::Endword { characters: chars.to_string(), dots }))
}

pub fn partword(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, chars, _, dots)) = tuple((tag("partword"), space1, chars, space1, braillechars_or_implicit))(i)?;
    Ok((input, Rule::Partword { characters: chars.to_string(), dots }))
}

pub fn prepunc(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, chars, _, dots)) = tuple((tag("prepunc"), space1, chars, space1, dots))(i)?;
    Ok((input, Rule::Prepunc { characters: chars.to_string(), dots }))
}

pub fn postpunc(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, chars, _, dots)) = tuple((tag("postpunc"), space1, chars, space1, dots))(i)?;
    Ok((input, Rule::Postpunc { characters: chars.to_string(), dots }))
}

pub fn begnum(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, chars, _, dots)) = tuple((tag("begnum"), space1, chars, space1, dots))(i)?;
    Ok((input, Rule::Begnum { characters: chars.to_string(), dots }))
}

pub fn midnum(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, chars, _, dots)) = tuple((tag("midnum"), space1, chars, space1, dots))(i)?;
    Ok((input, Rule::Midnum { characters: chars.to_string(), dots }))
}

pub fn endnum(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, chars, _, dots)) = tuple((tag("endnum"), space1, chars, space1, braillechars_or_implicit))(i)?;
    Ok((input, Rule::Endnum { characters: chars.to_string(), dots }))
}

pub fn joinnum(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, chars, _, dots)) = tuple((tag("joinnum"), space1, chars, space1, dots))(i)?;
    Ok((input, Rule::Joinnum { characters: chars.to_string(), dots }))
}

pub fn swapcd(i: &str) -> IResult<&str, Rule> {
    let (input, ( _, _, name, _, characters, _, dots)) = tuple((tag("swapcd"), space1, ascii_chars, space1, chars, space1, separated_list1(tag(","), dots)))(i)?;
    Ok((input, Rule::Swapcd { name: name.to_string(), characters: characters.to_string(), dots }))
}

pub fn swapdd(i: &str) -> IResult<&str, Rule> {
    let (input, ( _, _, name, _, dots, _, dotpattern)) = tuple((tag("swapdd"), space1, ascii_chars, space1, separated_list1(tag(","), dots), space1, separated_list1(tag(","), dots)))(i)?;
    Ok((input, Rule::Swapdd { name: name.to_string(), dots, dotpattern }))
}

pub fn swapcc(i: &str) -> IResult<&str, Rule> {
    let (input, ( _, _, name, _, characters, _, replacement)) = tuple((tag("swapcc"), space1, ascii_chars, space1, chars, space1, chars))(i)?;
    Ok((input, Rule::Swapcc { name: name.to_string(), chars: characters.to_string(), replacement: replacement.to_string() }))
}

pub fn context(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, test, _, action)) = tuple((opt(prefixes), tag("context"), space1, chars, space1, chars))(i)?;
    Ok((input, Rule::Context { test: test.to_string(), action: action.to_string(), prefixes: prefixes.unwrap() }))
}

pub fn pass2(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, test, _, action)) = tuple((opt(prefixes), tag("pass2"), space1, chars, space1, chars))(i)?;
    Ok((input, Rule::Pass2 { test: test.to_string(), action: action.to_string(), prefixes: prefixes.unwrap() }))
}

pub fn pass3(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, test, _, action)) = tuple((opt(prefixes), tag("pass3"), space1, chars, space1, chars))(i)?;
    Ok((input, Rule::Pass3 { test: test.to_string(), action: action.to_string(), prefixes: prefixes.unwrap() }))
}

pub fn pass4(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, test, _, action)) = tuple((opt(prefixes), tag("pass4"), space1, chars, space1, chars))(i)?;
    Ok((input, Rule::Pass4 { test: test.to_string(), action: action.to_string(), prefixes: prefixes.unwrap() }))
}

pub fn correct(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, test, _, action)) = tuple((opt(prefixes), tag("correct"), space1, chars, space1, chars))(i)?;
    Ok((input, Rule::Correct { test: test.to_string(), action: action.to_string(), prefixes: prefixes.unwrap() }))
}

pub fn match_opcode(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, pre, _, chars, _, post, _, dots)) = tuple((opt(prefixes), tag("match"), space1, chars, space1, chars, space1, chars, space1, braillechars_or_implicit))(i)?;
    Ok((input, Rule::Match { pre: pre.to_string(), characters: chars.to_string(), post: post.to_string(), dots, prefixes: prefixes.unwrap() }))
}

fn literal(input: &str) -> IResult<&str, Rule> {
    let (input, ( _, _, characters)) = tuple((tag("literal"), space1, chars))(input)?;
    Ok((input, Rule::Literal { characters: characters.to_string() }))
}

fn end_comment(i: &str) -> IResult<&str, &str> {
    let (input, (_, _, comment)) = tuple((space1, opt(tag("#")), not_line_ending))(i)?;
    Ok((input, comment))
}

pub fn rule_line(i: &str) -> IResult<&str, Line> {
    let (input, (rule, comment, _)) = tuple((
	// for some reason alt only allows for 21 choices. As a
	// workaround we need to nest the alt calls, see
	// https://github.com/rust-bakery/nom/issues/1144#issuecomment-629774957
        alt((
	    alt((
		include,
		undefined,
		display,
		space,
		multind,
		punctuation,
		digit,
		letter,
		lowercase,
		uppercase,
		litdigit,
		sign,
		math,
		grouping,
		base,
		attribute,
            )),
	    alt((
		modeletter,
		capsletter,
		begmodeword,
		begcapsword,
		endcapsword,
		capsmodechars,
		begcaps,
		endcaps,
		letsign,
		noletsign,
		noletsignbefore,
		noletsignafter,
		nocontractsign,
		numsign,
		nonumsign,
		numericnocontchars,
		numericmodechars,
		midendnumericmodechars,
		begcapsphrase,
		endcapsphrase,
		lencapsphrase,
	    )),
	    alt((
		seqdelimiter,
		seqbeforechars,
		seqafterchars,
		seqafterpattern,
		seqafterexpression,
	    )),
	    alt((
		class,
		emphclass,
		begemph,
		endemph,
		noemphchars,
		emphletter,
		begemphword,
		endemphword,
		emphmodechars,
		begemphphrase,
		endemphphrase,
		lenemphphrase,
		begcomp,
		endcomp,
	    )),
	    alt((
		decpoint,
		hyphen,
		capsnocont,
		compbrl,
		comp6,
		nocont,
		replace,
		always,
		repeated,
		repword,
		rependword,
		largesign,
		word,
		syllable,
		joinword,
		lowword,
		contraction,
		sufword,
		prfword,
	    )),
	    alt((
		begword,
		begmidword,
		midword,
		midendword,
		endword,
		partword,
		prepunc,
		postpunc,
		begnum,
		midnum,
		endnum,
		joinnum,
		swapcd,
		swapdd,
		swapcc,
		context,
		pass2,
		pass3,
		pass4,
		correct,
		match_opcode,
	    )),
	    alt((
		literal,
	    ))
	)),
        alt((end_comment, space0)),
        line_ending,
    ))(i)?;
    Ok((input, Line::Rule { rule, comment: comment.to_string()}))
}

pub fn comment_line(i: &str) -> IResult<&str, Line> {
    let (input, (_, comment, _)) = tuple((tag("#"), not_line_ending, line_ending))(i)?;
    Ok((input, Line::Comment { comment: comment.to_string() }))
}

pub fn empty_line(i: &str) -> IResult<&str, Line> {
    let (input, (_, _)) = tuple((space0, line_ending))(i)?;
    Ok((input, Line::Empty))
}

pub fn line(i: &str) -> IResult<&str, Line> {
    let (input, rule) = alt((
	rule_line,
	comment_line,
	empty_line,
    ))(i)?;
    Ok((input, rule))
}

pub fn table(i: &str) -> IResult<&str, Vec<Line>> {
    all_consuming(many0(line))(i)
}

#[cfg(test)]
mod tests {
    use super::*;
    use nom::error::Error;
    use nom::error::ErrorKind;
    use nom::Err;

    #[test]
    fn char_to_dot_test() {
        assert_eq!(char_to_dot('8'), Ok(BrailleDot::DOT8));
        assert_eq!(char_to_dot('F'), Err(ParseBrailleError{}));
        assert_eq!(char_to_dot('z'), Err(ParseBrailleError{}));
    }

    #[test]
    fn character_test() {
        assert_eq!(ascii_chars("hallo"), Ok(("", "hallo")));
        assert_eq!(ascii_chars("haLlo"), Ok(("", "haLlo")));
    }

    #[test]
    fn unicode_literal_test() {
        assert_eq!(unicode_literal("00AD"), Ok(("", '­')));
    }

    #[test]
    fn escaped_char_test() {
        assert_eq!(escaped_char("\\x00AD"), Ok(("", '­')));
        assert_eq!(escaped_char("\\x04D8"), Ok(("", 'Ә')));
    }

    #[test]
    fn one_unicode_digit_test() {
        assert_eq!(one_unicode_digit("1"), Ok(("", '1')));
        assert_eq!(one_unicode_digit("12"), Err(Err::Error(Error { input: "12", code: ErrorKind::Verify })));
        assert_eq!(one_unicode_digit("b"), Err(Err::Error(Error { input: "b", code: ErrorKind::Digit })));
        assert_eq!(one_unicode_digit("1b"), Ok(("b", '1')));
        assert_eq!(one_unicode_digit(""), Err(Err::Error(Error { input: "", code: ErrorKind::Digit })));
        assert_eq!(one_unicode_digit("໑"), Ok(("", '໑')));
    }

    #[test]
    fn single_char_test() {
        assert_eq!(single_char("a"), Ok(("", 'a')));
        assert_eq!(single_char("b"), Ok(("", 'b')));
        assert_eq!(single_char("\\x00AD"), Ok(("", '­')));
    }

    #[test]
    fn characters_test() {
        assert_eq!(chars("foo"), Ok(("", "foo")));
        assert_eq!(chars("foo bar"), Ok((" bar", "foo")));
	// FIXME: I guess that should parse as a single unicode char
        assert_eq!(chars(r"\x04D8"), Ok(("", r"\x04D8")));
    }

    #[test]
    fn dots_test() {
        assert_eq!(dots("123"), Ok(("",  vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3] )));
        assert_eq!(dots("1f"), Ok(("", vec![BrailleDot::DOT1 | BrailleDot::DOTF])));
        assert_eq!(dots("123-1f"), Ok(("", vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3,
						BrailleDot::DOT1 | BrailleDot::DOTF])));
        assert_eq!(dots("123-1f-78"),
		   Ok(("", vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3,
				BrailleDot::DOT1 | BrailleDot::DOTF,
				BrailleDot::DOT7 | BrailleDot::DOT8,
		   ])));
        assert_eq!(dots("huhu"), Err(Err::Error(Error {input: "huhu", code: ErrorKind::HexDigit}))
        );
    }

    #[test]
    fn braillechars_or_implicit_test() {
        assert_eq!(braillechars_or_implicit("123"), Ok(("", BrailleCharsOrImplicit::Explicit(vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3]))));
        assert_eq!(braillechars_or_implicit("="), Ok(("", BrailleCharsOrImplicit::Implicit)));
        assert_eq!(dots("m"), Err(Err::Error(Error { input: "m", code: ErrorKind::HexDigit })));
    }

    #[test]
    fn include_test() {
        assert_eq!(include("include filename.tbl"), Ok(("", Rule::Include { filename: "filename.tbl".to_string() })));
    }

    #[test]
    fn undefined_test() {
        assert_eq!(undefined("undefined 12"), Ok(("", Rule::Undefined { dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2] })));
    }

    #[test]
    fn display_test() {
        assert_eq!(display("display haha 122"), Ok(("", Rule::Display { chars: "haha".to_string(),
									dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2],
									prefixes: Prefixes::empty() })));
    }

    #[test]
    fn space_test() {
        assert_eq!(space("space . 0"),
		   Ok(("", Rule::Space { ch: '.', dots: vec![enum_set!(BrailleDot::DOT0)], prefixes: Prefixes::empty() })));
    }

    #[test]
    fn punctuation_test() {
        assert_eq!(punctuation("punctuation . 46"),
		   Ok(("", Rule::Punctuation { ch: '.',
					       dots: vec![BrailleDot::DOT4 | BrailleDot::DOT6],
					       prefixes: Prefixes::empty() })));
    }

    #[test]
    fn digit_test() {
        assert_eq!(digit("digit 1 278"),
		   Ok(("", Rule::Digit { ch: '1',
					 dots: vec![BrailleDot::DOT2 | BrailleDot::DOT7 | BrailleDot::DOT8] })));
        assert_eq!(digit("digit ۲ 1278"),
		   Ok(("", Rule::Digit { ch: '۲',
					 dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT7 | BrailleDot::DOT8] })));
    }

    #[test]
    fn litdigit_test() {
        assert_eq!(litdigit("litdigit 0 245"),
		   Ok(("", Rule::Litdigit { ch: '0', dots: vec![BrailleDot::DOT2 | BrailleDot::DOT4 | BrailleDot::DOT5] })));
    }

    #[test]
    fn grouping_test() {
        assert_eq!(grouping("grouping mfrac ab 3e,4e"),
		   Ok(("", Rule::Grouping { name: "mfrac".to_string(), chars: "ab".to_string(), dots: vec![vec![BrailleDot::DOT3 | BrailleDot::DOTE], vec![BrailleDot::DOT4 | BrailleDot::DOTE]] })));
    }

    #[test]
    fn modeletter_test() {
        assert_eq!(modeletter("modeletter uppercase 6"),
		   Ok(("", Rule::Modeletter { attribute: "uppercase".to_string(),
					      dots: vec![enum_set!(BrailleDot::DOT6)],
					      prefixes: Prefixes::empty()})));
    }

    #[test]
    fn capsletter_test() {
        assert_eq!(capsletter("capsletter 6"),
		   Ok(("", Rule::Capsletter { dots: vec![enum_set!(BrailleDot::DOT6)],
					      prefixes: Prefixes::empty()})));
    }

    #[test]
    fn begmodeword_test() {
        assert_eq!(begmodeword("begmodeword uppercase 6"),
		   Ok(("", Rule::Begmodeword { attribute: "uppercase".to_string(),
					       dots: vec![enum_set!(BrailleDot::DOT6)],
					       prefixes: Prefixes::empty()})));
    }

    #[test]
    fn begcapsword_test() {
        assert_eq!(begcapsword("begcapsword 6-6"),
		   Ok(("", Rule::Begcapsword { dots: vec![enum_set!(BrailleDot::DOT6),
							  enum_set!(BrailleDot::DOT6)],
					       prefixes: Prefixes::empty()})));
    }

    #[test]
    fn endcapsword_test() {
        assert_eq!(endcapsword("endcapsword 6-3"),
		   Ok(("", Rule::Endcapsword { dots: vec![enum_set!(BrailleDot::DOT6),
							  enum_set!(BrailleDot::DOT3)],
					       prefixes: Prefixes::empty()})));
    }

    #[test]
    fn capsmodechars_test() {
        assert_eq!(capsmodechars("capsmodechars -/"),
		   Ok(("", Rule::Capsmodechars { chars: "-/".to_string()})));
    }

    #[test]
    fn begcaps_test() {
        assert_eq!(begcaps("begcaps 6-6-6"),
		   Ok(("", Rule::Begcaps { dots: vec![enum_set!(BrailleDot::DOT6),
						      enum_set!(BrailleDot::DOT6),
						      enum_set!(BrailleDot::DOT6)]})));
    }

    #[test]
    fn endcaps_test() {
        assert_eq!(endcaps("endcaps 6-3"),
		   Ok(("", Rule::Endcaps { dots: vec![enum_set!(BrailleDot::DOT6),
							  enum_set!(BrailleDot::DOT3)]})));
    }

    #[test]
    fn begcapsphrase_test() {
        assert_eq!(begcapsphrase("begcapsphrase 45-45"),
		   Ok(("", Rule::Begcapsphrase { dots: vec![enum_set!(BrailleDot::DOT4 | BrailleDot::DOT5),
							    enum_set!(BrailleDot::DOT4 | BrailleDot::DOT5)]})));
    }

    #[test]
    fn endcapsphrase_test() {
        assert_eq!(endcapsphrase("endcapsphrase before 45"),
		   Ok(("", Rule::Endcapsphrase { dots: vec![BrailleDot::DOT4 | BrailleDot::DOT5],
						 position: Position::Before})));
        assert_eq!(endcapsphrase("endcapsphrase after 45"),
		   Ok(("", Rule::Endcapsphrase { dots: vec![BrailleDot::DOT4 | BrailleDot::DOT5],
						 position: Position::After})));
        assert_eq!(endcapsphrase("endcapsphrase foo 45"),
		   Err(Err::Error(Error::new("foo 45", ErrorKind::Tag))));
    }

    #[test]
    fn lencapsphrase_test() {
        assert_eq!(lencapsphrase("lencapsphrase 4"),
		   Ok(("", Rule::Lencapsphrase { length: 4 })));
    }

    #[test]
    fn prefixes_test() {
        assert_eq!(display("nocross display haha 122"),
		   Ok(("", Rule::Display { chars: "haha".to_string(),
					   dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2],
					   prefixes: enum_set!(Prefix::Nocross) })));
        assert_eq!(display("noback nocross display haha 122"),
		   Ok(("", Rule::Display { chars: "haha".to_string(),
					   dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2],
					   prefixes: Prefix::Noback | Prefix::Nocross })));
    }

    #[test]
    fn largesign_test() {
        assert_eq!(
            largesign("largesign überall 123"),
            Ok(("", Rule::Largesign { characters: "überall".to_string(), dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3] })));
        assert_eq!(
            largesign("largesign அஇ 123"),
            Ok(("", Rule::Largesign { characters: "அஇ".to_string(), dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3] })));
    }

    #[test]
    fn joinword_test() {
        assert_eq!(
            joinword("joinword haha 123"),
            Ok(("", Rule::Joinword { word: "haha".to_string(), dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3] })));
        assert_eq!(
            joinword("joinword அஇ 123"),
            Ok(("", Rule::Joinword { word: "அஇ".to_string(), dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3] })));
    }

    #[test]
    fn uppercase_test() {
        assert_eq!(
            uppercase("uppercase f 123"),
            Ok(("", Rule::Uppercase { word: "f".to_string(), dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3], prefixes: Prefixes::empty()  })));
        assert_eq!(
            uppercase("uppercase அ 123"),
            Ok(("", Rule::Uppercase { word: "அ".to_string(), dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3], prefixes: Prefixes::empty()  })));
        assert_eq!(
            uppercase("uppercase \\x04D8 34579"),
            Ok(("", Rule::Uppercase { word: "Ә".to_string(), dots: vec![BrailleDot::DOT3 | BrailleDot::DOT4 | BrailleDot::DOT5 | BrailleDot::DOT7 | BrailleDot::DOT9], prefixes: Prefixes::empty() })));
    }

    #[test]
    fn lowercase_test() {
        assert_eq!(
            lowercase("lowercase f 123"),
            Ok(("", Rule::Lowercase { word: "f".to_string(), dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3], prefixes: Prefixes::empty() })));
        assert_eq!(
            lowercase("lowercase அ 123"),
            Ok(("", Rule::Lowercase { word: "அ".to_string(), dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3], prefixes: Prefixes::empty() })));
    }

    #[test]
    fn multind_test() {
        assert_eq!(
            multind("multind 123 lowercase uppercase"),
            Ok(("", Rule::Multind { dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3],
				    opcodes: vec!["lowercase".to_string(), "uppercase".to_string()],
				    prefixes: Prefixes::empty() })));
	// FIXME: test multind with an end comment
	// FIXME: test multind with invalid opcodes
    }

    #[test]
    fn match_test() {
        assert_eq!(
            match_opcode("match ab xyz cd 1346-13456"),
            Ok(("", Rule::Match {pre: "ab".to_string(),
				 characters: "xyz".to_string(),
				 post: "cd".to_string(),
				 dots: BrailleCharsOrImplicit::Explicit(vec![BrailleDot::DOT1 | BrailleDot::DOT3 | BrailleDot::DOT4 | BrailleDot::DOT6, BrailleDot::DOT1 | BrailleDot::DOT3 | BrailleDot::DOT4 | BrailleDot::DOT5 | BrailleDot::DOT6]),
				 prefixes: Prefixes::empty() })));
    }

    #[test]
    fn rule_line_test() {
        assert_eq!(
            rule_line("joinword haha 123\n"),
            Ok(("", Line::Rule { rule: Rule::Joinword { word: "haha".to_string(),
							dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3] },
				 comment: "".to_string() })));
        assert_eq!(
            rule_line("largesign அஇ 123\n"),
            Ok(("", Line::Rule { rule: Rule::Largesign { characters: "அஇ".to_string(),
							 dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3] },
				 comment: "".to_string() })));
        assert_eq!(
            rule_line("syllable haha 123\n"),
            Ok(("", Line::Rule { rule: Rule::Syllable { word: "haha".to_string(),
							dots: BrailleCharsOrImplicit::Explicit(vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3]) },
				 comment: "".to_string() })));
        assert_eq!(
            rule_line("base uppercase A a\n"),
            Ok(("", Line::Rule { rule: Rule::Base { attribute: "uppercase".to_string(), derived: 'A', base: 'a' }, comment: "".to_string() })));
    }

    #[test]
    fn empty_line_test() {
        assert_eq!(
            empty_line("       \n"),
            Ok(("", Line::Empty)));
        assert_eq!(
            empty_line("\n"),
            Ok(("", Line::Empty)));
    }

    #[test]
    fn comment_line_test() {
        assert_eq!(
            comment_line("# haha 1234    \n"),
            Ok(("", Line::Comment { comment: " haha 1234    ".to_string()})));
        assert_eq!(
            comment_line("# haha 1234    "),
            Err(Err::Error(Error::new("", ErrorKind::CrLf))));
    }

    #[test]
    fn end_comment_test() {
	// assert_eq!(
	//     end_comment("an end comment\n"),
	//     Err(Err::Error(Error::new("an end comment\n", ErrorKind::Space))));
	// assert_eq!(end_comment(" an end comment\n"), Ok(("\n", "an end comment")));
        assert_eq!(
            rule_line("joinword haha 123 # comment \n"),
            Ok(("", Line::Rule { rule: Rule::Joinword { word: "haha".to_string(),
							dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3] },
				 comment: " comment ".to_string() })));
    }

    #[test]
    fn table_test() {
        assert_eq!(
            table(concat!("       \n",
			  "joinword haha 123\n",
			  "syllable haha 123-1f\n")),
            Ok(("", vec![Line::Empty,
			 Line::Rule { rule: Rule::Joinword { word: "haha".to_string(),
							     dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3] },
				      comment: "".to_string() },
			 Line::Rule { rule: Rule::Syllable { word: "haha".to_string(),
							     dots: BrailleCharsOrImplicit::Explicit(vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3, BrailleDot::DOT1 | BrailleDot::DOTF]) },
				      comment: "".to_string() }])));
        assert_eq!(
            table(concat!("       \n",
			  "# just testing\n",
			  "nocross multind 123 always syllable\n",
			  "joinword haha 123\n",
			  "syllable haha =\n")),
            Ok(("", vec![Line::Empty,
			 Line::Comment { comment: " just testing".to_string() },
			 Line::Rule { rule: Rule::Multind { opcodes: vec!["always".to_string(), "syllable".to_string()],
							    dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3],
							    prefixes: enum_set!(Prefix::Nocross) },
				      comment: "".to_string() },
			 Line::Rule { rule: Rule::Joinword { word: "haha".to_string(),
							     dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3] },
				      comment: "".to_string() },
			 Line::Rule { rule: Rule::Syllable { word: "haha".to_string(),
							     dots: BrailleCharsOrImplicit::Implicit },
				      comment: "".to_string() }])));
    }
}
