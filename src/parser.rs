use std::error;
use std::fmt;
use std::fs;

use nom::branch::alt;
use nom::bytes::complete::is_a;
use nom::bytes::complete::is_not;
use nom::bytes::complete::tag;
use nom::character::complete::alpha1;
use nom::character::complete::digit1;
use nom::character::complete::hex_digit1;
use nom::character::complete::line_ending;
use nom::character::complete::none_of;
use nom::character::complete::not_line_ending;
use nom::character::complete::one_of;
use nom::character::complete::space0;
use nom::character::complete::space1;
use nom::combinator::all_consuming;
use nom::combinator::map;
use nom::combinator::map_res;
use nom::combinator::opt;
use nom::combinator::success;
use nom::combinator::verify;
use nom::error::Error;
use nom::error::ParseError;
use nom::multi::many0;
use nom::multi::separated_list1;
use nom::sequence::tuple;

use enumset::enum_set;
use enumset::EnumSet;
use enumset::EnumSetType;

use nom::IResult;
//use nom_unicode::complete::alpha1 as unicode_alpha1;
use nom_unicode::complete::digit1 as unicode_digit1;

#[derive(PartialEq, Debug)]
pub enum Line {
    Empty,
    Comment { comment: String },
    Rule { rule: Rule, comment: String },
}

impl Line {
    pub fn as_rule(self) -> Option<Rule> {
        match self {
            Line::Rule { rule, .. } => Some(rule),
            _ => None,
        }
    }
}

#[rustfmt::skip]
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
    Letter { ch: char, dots: BrailleCharsOrImplicit, prefixes: Prefixes },
    Lowercase { ch: char, dots: BrailleChars, prefixes: Prefixes },
    Uppercase { ch: char, dots: BrailleChars, prefixes: Prefixes },
    Litdigit { ch: char, dots: BrailleChars },
    Sign { ch: char, dots: BrailleChars, prefixes: Prefixes },
    Math { ch: char, dots: BrailleChars, prefixes: Prefixes },
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
    Begmodephrase { name: String, dots: BrailleChars},
    Endmodephrase { name: String, dots: BrailleChars, position: Position},
    Lenmodephrase { name: String, length: u8},

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
    Compbrl { characters: String, prefixes: Prefixes},
    Comp6 { characters: String, dots: BrailleCharsOrImplicit},
    Nocont {characters: String},
    Replace {characters: String, replacement: Option<String> },
    Always {characters: String, dots: BrailleCharsOrImplicit, prefixes: Prefixes, classes: WithClasses},
    Repeated {characters: String, dots: BrailleChars, prefixes: Prefixes},
    Repword  {characters: String, dots: BrailleChars},
    Rependword  {characters: String, dots: BrailleChars, other: BrailleChars},
    Largesign  {characters: String, dots: BrailleChars},
    Word  {characters: String, dots: BrailleCharsOrImplicit, prefixes: Prefixes, classes: WithClasses},
    Syllable { word: String, dots: BrailleCharsOrImplicit },
    Joinword { word: String, dots: BrailleChars },
    Lowword  {characters: String, dots: BrailleChars, prefixes: Prefixes},
    Contraction  {characters: String},
    Sufword  {characters: String, dots: BrailleCharsOrImplicit, prefixes: Prefixes, classes: WithClasses},
    Prfword  {characters: String, dots: BrailleCharsOrImplicit, prefixes: Prefixes, classes: WithClasses},
    Begword  {characters: String, dots: BrailleCharsOrImplicit, prefixes: Prefixes, classes: WithClasses},
    Begmidword  {characters: String, dots: BrailleCharsOrImplicit, prefixes: Prefixes, classes: WithClasses},
    Midword  {characters: String, dots: BrailleCharsOrImplicit, prefixes: Prefixes, classes: WithClasses},
    Midendword  {characters: String, dots: BrailleCharsOrImplicit, prefixes: Prefixes, classes: WithClasses},
    Endword  {characters: String, dots: BrailleCharsOrImplicit, prefixes: Prefixes, classes: WithClasses},
    Partword {characters: String, dots: BrailleCharsOrImplicit, prefixes: Prefixes, classes: WithClasses},
    Prepunc {characters: String, dots: BrailleChars, prefixes: Prefixes},
    Postpunc {characters: String, dots: BrailleChars, prefixes: Prefixes},
    Begnum {characters: String, dots: BrailleChars, prefixes: Prefixes},
    Midnum {characters: String, dots: BrailleChars, prefixes: Prefixes},
    Endnum {characters: String, dots: BrailleCharsOrImplicit, prefixes: Prefixes},
    Joinnum {characters: String, dots: BrailleChars, prefixes: Prefixes},

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
    Match { pre: String, characters: String, post: String, dots: BrailleCharsOrImplicit, prefixes: Prefixes, classes: WithClasses, positions: WithMatches},

    // deprecated opcodes
    Literal {characters: String, }
}

#[derive(EnumSetType, Debug)]
pub enum Prefix {
    Noback,
    Nofor,
    Nocross,
}

pub type Prefixes = EnumSet<Prefix>;

#[derive(PartialEq, Debug)]
pub enum WithClass {
    Before { class: String },
    After { class: String },
}

type WithClasses = Vec<WithClass>;

#[derive(EnumSetType, Debug)]
pub enum WithMatch {
    Before,
    After,
}

type WithMatches = EnumSet<WithMatch>;

#[derive(PartialEq, Debug)]
pub enum Position {
    Before,
    After,
}

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

pub type BrailleChar = EnumSet<BrailleDot>;
pub type BrailleChars = Vec<BrailleChar>;

#[derive(PartialEq, Debug)]
pub enum BrailleCharsOrImplicit {
    Implicit,
    Explicit(BrailleChars),
}

fn dot_to_hex(dot: BrailleDot) -> u32 {
    match dot {
        BrailleDot::DOT1 => 0x0001,
        BrailleDot::DOT2 => 0x0002,
        BrailleDot::DOT3 => 0x0004,
        BrailleDot::DOT4 => 0x0008,
        BrailleDot::DOT5 => 0x0010,
        BrailleDot::DOT6 => 0x0020,
        BrailleDot::DOT7 => 0x0040,
        BrailleDot::DOT8 => 0x0080,
        // all other braille dots cannot be mapped to unicode, so they
        // return the identity bit pattern
        _ => 0x0000,
    }
}

// FIXME: the following two functions should be defined as associated
// functions, i.e. inside an impl block for BrailleChar or as an
// implementation of the From trait. Both solutions would probably
// require the newtype pattern as we do not own these types, see
// https://doc.rust-lang.org/book/ch19-03-advanced-traits.html#using-the-newtype-pattern-to-implement-external-traits-on-external-types
fn dot_to_unicode(dot: BrailleChar) -> char {
    let unicode = dot
        .iter()
        .map(|d| dot_to_hex(d))
        .fold(0x2800, |acc, x| acc | x);
    char::from_u32(unicode).unwrap()
}

pub fn dots_to_unicode(dots: BrailleChars) -> String {
    dots.into_iter().map(|d| dot_to_unicode(d)).collect()
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
        _ => Err(ParseBrailleError {}),
    }
}

fn chars_to_dots(chars: &str) -> BrailleChar {
    chars.chars().map(|c| char_to_dot(c).unwrap()).collect()
}

fn chars(input: &str) -> IResult<&str, &str> {
    is_not(" \t\r\n")(input)
    //unicode_alpha1(input)
}

fn unicode_literal(input: &str) -> IResult<&str, char> {
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

fn escaped_char(i: &str) -> IResult<&str, char> {
    let (input, (_, c)) = tuple((tag("\\x"), unicode_literal))(i)?;
    Ok((input, c))
}

fn single_char(input: &str) -> IResult<&str, char> {
    alt((escape_sequence, escaped_char, none_of(" \t\r\n")))(input)
}

fn filename(input: &str) -> IResult<&str, &str> {
    is_a("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-.")(input)
}

fn braillechars_or_implicit(input: &str) -> IResult<&str, BrailleCharsOrImplicit> {
    alt((
        map(tag("="), |_| BrailleCharsOrImplicit::Implicit),
        map(dots, |dots| BrailleCharsOrImplicit::Explicit(dots)),
    ))(input)
}

fn dots(i: &str) -> IResult<&str, BrailleChars> {
    let (input, dots) = separated_list1(tag("-"), hex_digit1)(i)?;
    let braille_chars: Vec<BrailleChar> = dots.iter().map(|chars| chars_to_dots(chars)).collect();
    Ok((input, braille_chars))
}

fn single_unicode_digit(input: &str) -> IResult<&str, char> {
    alt((escaped_char, one_unicode_digit))(input)
}

fn one_unicode_digit(input: &str) -> IResult<&str, char> {
    let (input, digit) = verify(unicode_digit1, |s: &str| s.chars().count() == 1)(input)?;
    match digit.char_indices().next() {
        Some((i, c)) => match input.get(i..) {
            Some(s) => Ok((s, c)),
            _ => Err(nom::Err::Error(ParseError::from_error_kind(
                input,
                nom::error::ErrorKind::Digit,
            ))),
        },
        _ => Err(nom::Err::Error(ParseError::from_error_kind(
            input,
            nom::error::ErrorKind::Digit,
        ))),
    }
}

fn number(input: &str) -> IResult<&str, u8> {
    map_res(digit1, |s: &str| s.parse::<u8>())(input)
}

fn before_or_after(input: &str) -> IResult<&str, Position> {
    alt((
        map(tag("before"), |_| Position::Before),
        map(tag("after"), |_| Position::After),
    ))(input)
}

fn prefixes(i: &str) -> IResult<&str, Prefixes> {
    alt((
        map(
            tuple((tag("noback"), space1, tag("nocross"), space1)),
            |_| Prefix::Noback | Prefix::Nocross,
        ),
        map(
            tuple((tag("nofor"), space1, tag("nocross"), space1)),
            |_| Prefix::Nofor | Prefix::Nocross,
        ),
        map(tuple((tag("nofor"), space1)), |_| enum_set!(Prefix::Nofor)),
        map(tuple((tag("noback"), space1)), |_| {
            enum_set!(Prefix::Noback)
        }),
        map(tuple((tag("nocross"), space1)), |_| {
            enum_set!(Prefix::Nocross)
        }),
        success::<_, _, Error<_>>(Prefixes::empty()),
    ))(i)
}

fn include(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, filename)) = tuple((tag("include"), space1, filename))(i)?;
    Ok((
        input,
        Rule::Include {
            filename: filename.to_string(),
        },
    ))
}

fn undefined(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, dots)) = tuple((tag("undefined"), space1, dots))(i)?;
    Ok((input, Rule::Undefined { dots }))
}

fn display(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, chars, _, dots)) =
        tuple((prefixes, tag("display"), space1, chars, space1, dots))(i)?;
    Ok((
        input,
        Rule::Display {
            chars: chars.to_string(),
            dots,
            prefixes,
        },
    ))
}

fn space(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, ch, _, dots)) =
        tuple((prefixes, tag("space"), space1, single_char, space1, dots))(i)?;
    Ok((input, Rule::Space { ch, dots, prefixes }))
}

fn multind(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, dots, _, opcodes)) = tuple((
        prefixes,
        tag("multind"),
        space1,
        dots,
        space1,
        separated_list1(space1, chars),
    ))(i)?;
    // FIXME: Make sure the opcodes are valid
    Ok((
        input,
        Rule::Multind {
            dots,
            opcodes: opcodes.iter().map(|s| s.to_string()).collect(),
            prefixes,
        },
    ))
}

fn punctuation(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, ch, _, dots)) = tuple((
        prefixes,
        tag("punctuation"),
        space1,
        single_char,
        space1,
        dots,
    ))(i)?;
    Ok((input, Rule::Punctuation { ch, dots, prefixes }))
}

fn digit(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, ch, _, dots)) = tuple((tag("digit"), space1, single_char, space1, dots))(i)?;
    Ok((input, Rule::Digit { ch, dots }))
}

fn letter(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, ch, _, dots)) = tuple((
        prefixes,
        tag("letter"),
        space1,
        single_char,
        space1,
        braillechars_or_implicit,
    ))(i)?;
    Ok((input, Rule::Letter { ch, dots, prefixes }))
}

fn lowercase(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, ch, _, dots)) = tuple((
        prefixes,
        tag("lowercase"),
        space1,
        single_char,
        space1,
        dots,
    ))(i)?;
    Ok((input, Rule::Lowercase { ch, dots, prefixes }))
}

fn uppercase(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, ch, _, dots)) = tuple((
        prefixes,
        tag("uppercase"),
        space1,
        single_char,
        space1,
        dots,
    ))(i)?;
    Ok((input, Rule::Uppercase { ch, dots, prefixes }))
}

fn litdigit(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, ch, _, dots)) =
        tuple((tag("litdigit"), space1, single_unicode_digit, space1, dots))(i)?;
    Ok((input, Rule::Litdigit { ch, dots }))
}

fn sign(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, ch, _, dots)) =
        tuple((prefixes, tag("sign"), space1, single_char, space1, dots))(i)?;
    Ok((input, Rule::Sign { ch, dots, prefixes }))
}

fn math(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, ch, _, dots)) =
        tuple((prefixes, tag("math"), space1, single_char, space1, dots))(i)?;
    Ok((input, Rule::Math { ch, dots, prefixes }))
}

fn grouping(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, name, _, characters, _, dots)) = tuple((
        tag("grouping"),
        space1,
        alpha1,
        space1,
        alpha1,
        space1,
        separated_list1(tag(","), dots),
    ))(i)?;
    Ok((
        input,
        Rule::Grouping {
            name: name.to_string(),
            chars: characters.to_string(),
            dots,
        },
    ))
}

fn base(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, attribute, _, derived, _, base)) = tuple((
        tag("base"),
        space1,
        alpha1,
        space1,
        single_char,
        space1,
        single_char,
    ))(i)?;
    Ok((
        input,
        Rule::Base {
            attribute: attribute.to_string(),
            derived,
            base,
        },
    ))
}

fn attribute_name(input: &str) -> IResult<&str, String> {
    alt((
        map(one_of("01234567"), |c| c.to_string()),
        map(alpha1, |s: &str| s.to_string()),
    ))(input)
}

fn attribute(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, name, _, chars)) =
        tuple((tag("attribute"), space1, attribute_name, space1, chars))(i)?;
    Ok((
        input,
        Rule::Attribute {
            name,
            chars: chars.to_string(),
        },
    ))
}

fn modeletter(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, chars, _, dots)) =
        tuple((prefixes, tag("modeletter"), space1, alpha1, space1, dots))(i)?;
    Ok((
        input,
        Rule::Modeletter {
            attribute: chars.to_string(),
            dots,
            prefixes,
        },
    ))
}

fn capsletter(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, dots)) = tuple((prefixes, tag("capsletter"), space1, dots))(i)?;
    Ok((input, Rule::Capsletter { dots, prefixes }))
}

fn begmodeword(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, chars, _, dots)) =
        tuple((prefixes, tag("begmodeword"), space1, alpha1, space1, dots))(i)?;
    Ok((
        input,
        Rule::Begmodeword {
            attribute: chars.to_string(),
            dots,
            prefixes,
        },
    ))
}

fn begcapsword(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, dots)) = tuple((prefixes, tag("begcapsword"), space1, dots))(i)?;
    Ok((input, Rule::Begcapsword { dots, prefixes }))
}

fn endcapsword(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, dots)) = tuple((prefixes, tag("endcapsword"), space1, dots))(i)?;
    Ok((input, Rule::Endcapsword { dots, prefixes }))
}

fn capsmodechars(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, chars)) = tuple((tag("capsmodechars"), space1, chars))(i)?;
    Ok((
        input,
        Rule::Capsmodechars {
            chars: chars.to_string(),
        },
    ))
}

fn begcaps(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, dots)) = tuple((tag("begcaps"), space1, dots))(i)?;
    Ok((input, Rule::Begcaps { dots }))
}

fn endcaps(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, dots)) = tuple((tag("endcaps"), space1, dots))(i)?;
    Ok((input, Rule::Endcaps { dots }))
}

fn letsign(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, dots)) = tuple((tag("letsign"), space1, dots))(i)?;
    Ok((input, Rule::Letsign { dots }))
}

fn noletsign(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, characters)) = tuple((tag("noletsign"), space1, chars))(i)?;
    Ok((
        input,
        Rule::Noletsign {
            characters: characters.to_string(),
        },
    ))
}

fn noletsignbefore(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, characters)) = tuple((tag("noletsignbefore"), space1, chars))(i)?;
    Ok((
        input,
        Rule::Noletsignbefore {
            characters: characters.to_string(),
        },
    ))
}

fn noletsignafter(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, characters)) = tuple((tag("noletsignafter"), space1, chars))(i)?;
    Ok((
        input,
        Rule::Noletsignafter {
            characters: characters.to_string(),
        },
    ))
}

fn nocontractsign(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, dots)) = tuple((tag("nocontractsign"), space1, dots))(i)?;
    Ok((input, Rule::Nocontractsign { dots }))
}

fn numsign(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, dots)) = tuple((tag("numsign"), space1, dots))(i)?;
    Ok((input, Rule::Numsign { dots }))
}

fn nonumsign(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, dots)) = tuple((tag("nonumsign"), space1, dots))(i)?;
    Ok((input, Rule::Nonumsign { dots }))
}

fn numericnocontchars(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, characters)) = tuple((tag("numericnocontchars"), space1, chars))(i)?;
    Ok((
        input,
        Rule::Numericnocontchars {
            characters: characters.to_string(),
        },
    ))
}

fn numericmodechars(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, characters)) = tuple((tag("numericmodechars"), space1, chars))(i)?;
    Ok((
        input,
        Rule::Numericmodechars {
            characters: characters.to_string(),
        },
    ))
}

fn midendnumericmodechars(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, characters)) = tuple((tag("midendnumericmodechars"), space1, chars))(i)?;
    Ok((
        input,
        Rule::Midendnumericmodechars {
            characters: characters.to_string(),
        },
    ))
}

fn begcapsphrase(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, dots)) = tuple((tag("begcapsphrase"), space1, dots))(i)?;
    Ok((input, Rule::Begcapsphrase { dots }))
}

fn endcapsphrase(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, position, _, dots)) =
        tuple((tag("endcapsphrase"), space1, before_or_after, space1, dots))(i)?;
    Ok((input, Rule::Endcapsphrase { dots, position }))
}

fn lencapsphrase(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, length)) = tuple((tag("lencapsphrase"), space1, number))(i)?;
    Ok((input, Rule::Lencapsphrase { length }))
}

fn begmodephrase(input: &str) -> IResult<&str, Rule> {
    let (input, (_, _, name, _, dots)) =
        tuple((tag("begmodephrase"), space1, alpha1, space1, dots))(input)?;
    Ok((
        input,
        Rule::Begmodephrase {
            name: name.to_string(),
            dots,
        },
    ))
}

fn endmodephrase(input: &str) -> IResult<&str, Rule> {
    let (input, (_, _, name, _, position, _, dots)) = tuple((
        tag("endmodephrase"),
        space1,
        alpha1,
        space1,
        before_or_after,
        space1,
        dots,
    ))(input)?;
    Ok((
        input,
        Rule::Endmodephrase {
            name: name.to_string(),
            position,
            dots,
        },
    ))
}

fn lenmodephrase(input: &str) -> IResult<&str, Rule> {
    let (input, (_, _, name, _, length)) =
        tuple((tag("lenmodephrase"), space1, alpha1, space1, number))(input)?;
    Ok((
        input,
        Rule::Lenmodephrase {
            name: name.to_string(),
            length,
        },
    ))
}

fn seqdelimiter(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, characters)) = tuple((tag("seqdelimiter"), space1, chars))(i)?;
    Ok((
        input,
        Rule::Seqdelimiter {
            characters: characters.to_string(),
        },
    ))
}

fn seqbeforechars(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, characters)) = tuple((tag("seqbeforechars"), space1, chars))(i)?;
    Ok((
        input,
        Rule::Seqbeforechars {
            characters: characters.to_string(),
        },
    ))
}

fn seqafterchars(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, characters)) = tuple((tag("seqafterchars"), space1, chars))(i)?;
    Ok((
        input,
        Rule::Seqafterchars {
            characters: characters.to_string(),
        },
    ))
}

fn seqafterpattern(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, characters)) = tuple((tag("seqafterpattern"), space1, chars))(i)?;
    Ok((
        input,
        Rule::Seqafterpattern {
            pattern: characters.to_string(),
        },
    ))
}

fn seqafterexpression(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, expression)) = tuple((tag("seqafterexpression"), space1, chars))(i)?;
    Ok((
        input,
        Rule::Seqafterexpression {
            expression: expression.to_string(),
        },
    ))
}

fn class(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, name, _, chars)) = tuple((tag("class"), space1, chars, space1, chars))(i)?;
    Ok((
        input,
        Rule::Class {
            name: name.to_string(),
            chars: chars.to_string(),
        },
    ))
}

fn emphclass(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, name)) = tuple((tag("emphclass"), space1, chars))(i)?;
    Ok((
        input,
        Rule::Emphclass {
            name: name.to_string(),
        },
    ))
}

fn begemph(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, name, _, dots)) =
        tuple((prefixes, tag("begemph"), space1, chars, space1, dots))(i)?;
    Ok((
        input,
        Rule::Begemph {
            name: name.to_string(),
            dots,
            prefixes,
        },
    ))
}

fn endemph(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, name, _, dots)) =
        tuple((prefixes, tag("endemph"), space1, chars, space1, dots))(i)?;
    Ok((
        input,
        Rule::Endemph {
            name: name.to_string(),
            dots,
            prefixes,
        },
    ))
}

fn noemphchars(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, name, _, chars)) =
        tuple((tag("noemphchars"), space1, chars, space1, chars))(i)?;
    Ok((
        input,
        Rule::Noemphchars {
            name: name.to_string(),
            chars: chars.to_string(),
        },
    ))
}

fn emphletter(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, name, _, dots)) =
        tuple((tag("emphletter"), space1, chars, space1, dots))(i)?;
    Ok((
        input,
        Rule::Emphletter {
            name: name.to_string(),
            dots,
        },
    ))
}

fn begemphword(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, name, _, dots)) =
        tuple((tag("begemphword"), space1, chars, space1, dots))(i)?;
    Ok((
        input,
        Rule::Begemphword {
            name: name.to_string(),
            dots,
        },
    ))
}

fn endemphword(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, name, _, dots)) =
        tuple((tag("endemphword"), space1, chars, space1, dots))(i)?;
    Ok((
        input,
        Rule::Endemphword {
            name: name.to_string(),
            dots,
        },
    ))
}

fn emphmodechars(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, name, _, chars)) =
        tuple((tag("emphmodechars"), space1, chars, space1, chars))(i)?;
    Ok((
        input,
        Rule::Emphmodechars {
            name: name.to_string(),
            chars: chars.to_string(),
        },
    ))
}

fn begemphphrase(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, name, _, dots)) =
        tuple((tag("begemphphrase"), space1, chars, space1, dots))(i)?;
    Ok((
        input,
        Rule::Begemphphrase {
            name: name.to_string(),
            dots,
        },
    ))
}

fn endemphphrase(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, name, _, position, _, dots)) = tuple((
        tag("endemphphrase"),
        space1,
        chars,
        space1,
        before_or_after,
        space1,
        dots,
    ))(i)?;
    Ok((
        input,
        Rule::Endemphphrase {
            name: name.to_string(),
            dots,
            position,
        },
    ))
}

fn lenemphphrase(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, name, _, length)) =
        tuple((tag("lenemphphrase"), space1, chars, space1, number))(i)?;
    Ok((
        input,
        Rule::Lenemphphrase {
            name: name.to_string(),
            length,
        },
    ))
}

fn begcomp(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, dots)) = tuple((prefixes, tag("begcomp"), space1, dots))(i)?;
    Ok((input, Rule::Begcomp { dots, prefixes }))
}

fn endcomp(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, dots)) = tuple((prefixes, tag("endcomp"), space1, dots))(i)?;
    Ok((input, Rule::Endcomp { dots, prefixes }))
}

fn decpoint(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, chars, _, dots)) = tuple((tag("decpoint"), space1, chars, space1, dots))(i)?;
    Ok((
        input,
        Rule::Decpoint {
            characters: chars.to_string(),
            dots,
        },
    ))
}

fn hyphen(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, chars, _, dots)) =
        tuple((prefixes, tag("hyphen"), space1, chars, space1, dots))(i)?;
    Ok((
        input,
        Rule::Hyphen {
            characters: chars.to_string(),
            dots,
            prefixes,
        },
    ))
}

fn capsnocont(i: &str) -> IResult<&str, Rule> {
    let (input, _) = tag("capsnocont")(i)?;
    Ok((input, Rule::Capsnocont))
}

fn compbrl(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, chars)) = tuple((prefixes, tag("compbrl"), space1, chars))(i)?;
    Ok((
        input,
        Rule::Compbrl {
            characters: chars.to_string(),
            prefixes,
        },
    ))
}

fn comp6(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, chars, _, dots)) = tuple((
        tag("comp6"),
        space1,
        chars,
        space1,
        braillechars_or_implicit,
    ))(i)?;
    Ok((
        input,
        Rule::Comp6 {
            characters: chars.to_string(),
            dots,
        },
    ))
}

fn nocont(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, chars)) = tuple((tag("nocont"), space1, chars))(i)?;
    Ok((
        input,
        Rule::Nocont {
            characters: chars.to_string(),
        },
    ))
}

fn replace(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, chars, replacement)) =
        tuple((tag("replace"), space1, chars, opt(tuple((space1, chars)))))(i)?;
    let replacement = match replacement {
        Some((_, replacement)) => Some(replacement.to_string()),
        None => None,
    };
    Ok((
        input,
        Rule::Replace {
            characters: chars.to_string(),
            replacement,
        },
    ))
}

fn always(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, classes, _, _, chars, _, dots)) = tuple((
        prefixes,
        with_classes,
        tag("always"),
        space1,
        chars,
        space1,
        braillechars_or_implicit,
    ))(i)?;
    Ok((
        input,
        Rule::Always {
            characters: chars.to_string(),
            dots,
            prefixes,
            classes,
        },
    ))
}

fn repeated(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, chars, _, dots)) =
        tuple((prefixes, tag("repeated"), space1, chars, space1, dots))(i)?;
    Ok((
        input,
        Rule::Repeated {
            characters: chars.to_string(),
            dots,
            prefixes,
        },
    ))
}

fn repword(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, chars, _, dots)) = tuple((tag("repword"), space1, chars, space1, dots))(i)?;
    Ok((
        input,
        Rule::Repword {
            characters: chars.to_string(),
            dots,
        },
    ))
}

fn rependword(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, chars, _, dots, _, other)) = tuple((
        tag("rependword"),
        space1,
        chars,
        space1,
        dots,
        tag(","),
        dots,
    ))(i)?;
    Ok((
        input,
        Rule::Rependword {
            characters: chars.to_string(),
            dots,
            other,
        },
    ))
}

fn largesign(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, chars, _, dots)) =
        tuple((tag("largesign"), space1, chars, space1, dots))(i)?;
    Ok((
        input,
        Rule::Largesign {
            characters: chars.to_string(),
            dots,
        },
    ))
}

fn word(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, classes, _, _, chars, _, dots)) = tuple((
        prefixes,
        with_classes,
        tag("word"),
        space1,
        chars,
        space1,
        braillechars_or_implicit,
    ))(i)?;
    Ok((
        input,
        Rule::Word {
            characters: chars.to_string(),
            dots,
            prefixes,
            classes,
        },
    ))
}

fn syllable(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, word, _, dots)) = tuple((
        tag("syllable"),
        space1,
        chars,
        space1,
        braillechars_or_implicit,
    ))(i)?;
    Ok((
        input,
        Rule::Syllable {
            word: word.to_string(),
            dots,
        },
    ))
}

fn joinword(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, word, _, dots)) = tuple((tag("joinword"), space1, chars, space1, dots))(i)?;
    Ok((
        input,
        Rule::Joinword {
            word: word.to_string(),
            dots,
        },
    ))
}

fn lowword(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, chars, _, dots)) =
        tuple((prefixes, tag("lowword"), space1, chars, space1, dots))(i)?;
    Ok((
        input,
        Rule::Lowword {
            characters: chars.to_string(),
            dots,
            prefixes,
        },
    ))
}

fn contraction(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, chars)) = tuple((tag("contraction"), space1, chars))(i)?;
    Ok((
        input,
        Rule::Contraction {
            characters: chars.to_string(),
        },
    ))
}

fn sufword(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, classes, _, _, chars, _, dots)) = tuple((
        prefixes,
        with_classes,
        tag("sufword"),
        space1,
        chars,
        space1,
        braillechars_or_implicit,
    ))(i)?;
    Ok((
        input,
        Rule::Sufword {
            characters: chars.to_string(),
            dots,
            prefixes,
            classes,
        },
    ))
}

fn prfword(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, classes, _, _, chars, _, dots)) = tuple((
        prefixes,
        with_classes,
        tag("prfword"),
        space1,
        chars,
        space1,
        braillechars_or_implicit,
    ))(i)?;
    Ok((
        input,
        Rule::Prfword {
            characters: chars.to_string(),
            dots,
            prefixes,
            classes,
        },
    ))
}

fn begword(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, classes, _, _, chars, _, dots)) = tuple((
        prefixes,
        with_classes,
        tag("begword"),
        space1,
        chars,
        space1,
        braillechars_or_implicit,
    ))(i)?;
    Ok((
        input,
        Rule::Begword {
            characters: chars.to_string(),
            dots,
            prefixes,
            classes,
        },
    ))
}

fn begmidword(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, classes, _, _, chars, _, dots)) = tuple((
        prefixes,
        with_classes,
        tag("begmidword"),
        space1,
        chars,
        space1,
        braillechars_or_implicit,
    ))(i)?;
    Ok((
        input,
        Rule::Begmidword {
            characters: chars.to_string(),
            dots,
            prefixes,
            classes,
        },
    ))
}

fn midword(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, classes, _, _, chars, _, dots)) = tuple((
        prefixes,
        with_classes,
        tag("midword"),
        space1,
        chars,
        space1,
        braillechars_or_implicit,
    ))(i)?;
    Ok((
        input,
        Rule::Midword {
            characters: chars.to_string(),
            dots,
            prefixes,
            classes,
        },
    ))
}

fn midendword(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, classes, _, _, chars, _, dots)) = tuple((
        prefixes,
        with_classes,
        tag("midendword"),
        space1,
        chars,
        space1,
        braillechars_or_implicit,
    ))(i)?;
    Ok((
        input,
        Rule::Midendword {
            characters: chars.to_string(),
            dots,
            prefixes,
            classes,
        },
    ))
}

fn endword(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, classes, _, _, chars, _, dots)) = tuple((
        prefixes,
        with_classes,
        tag("endword"),
        space1,
        chars,
        space1,
        braillechars_or_implicit,
    ))(i)?;
    Ok((
        input,
        Rule::Endword {
            characters: chars.to_string(),
            dots,
            prefixes,
            classes,
        },
    ))
}

fn partword(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, classes, _, _, chars, _, dots)) = tuple((
        prefixes,
        with_classes,
        tag("partword"),
        space1,
        chars,
        space1,
        braillechars_or_implicit,
    ))(i)?;
    Ok((
        input,
        Rule::Partword {
            characters: chars.to_string(),
            dots,
            prefixes,
            classes,
        },
    ))
}

fn prepunc(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, chars, _, dots)) =
        tuple((prefixes, tag("prepunc"), space1, chars, space1, dots))(i)?;
    Ok((
        input,
        Rule::Prepunc {
            characters: chars.to_string(),
            dots,
            prefixes,
        },
    ))
}

fn postpunc(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, chars, _, dots)) =
        tuple((prefixes, tag("postpunc"), space1, chars, space1, dots))(i)?;
    Ok((
        input,
        Rule::Postpunc {
            characters: chars.to_string(),
            dots,
            prefixes,
        },
    ))
}

fn begnum(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, chars, _, dots)) =
        tuple((prefixes, tag("begnum"), space1, chars, space1, dots))(i)?;
    Ok((
        input,
        Rule::Begnum {
            characters: chars.to_string(),
            dots,
            prefixes,
        },
    ))
}

fn midnum(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, chars, _, dots)) =
        tuple((prefixes, tag("midnum"), space1, chars, space1, dots))(i)?;
    Ok((
        input,
        Rule::Midnum {
            characters: chars.to_string(),
            dots,
            prefixes,
        },
    ))
}

fn endnum(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, chars, _, dots)) = tuple((
        prefixes,
        tag("endnum"),
        space1,
        chars,
        space1,
        braillechars_or_implicit,
    ))(i)?;
    Ok((
        input,
        Rule::Endnum {
            characters: chars.to_string(),
            dots,
            prefixes,
        },
    ))
}

fn joinnum(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, chars, _, dots)) =
        tuple((prefixes, tag("joinnum"), space1, chars, space1, dots))(i)?;
    Ok((
        input,
        Rule::Joinnum {
            characters: chars.to_string(),
            dots,
            prefixes,
        },
    ))
}

fn swapcd(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, name, _, characters, _, dots)) = tuple((
        tag("swapcd"),
        space1,
        alpha1,
        space1,
        chars,
        space1,
        separated_list1(tag(","), dots),
    ))(i)?;
    Ok((
        input,
        Rule::Swapcd {
            name: name.to_string(),
            characters: characters.to_string(),
            dots,
        },
    ))
}

fn swapdd(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, name, _, dots, _, dotpattern)) = tuple((
        tag("swapdd"),
        space1,
        alpha1,
        space1,
        separated_list1(tag(","), dots),
        space1,
        separated_list1(tag(","), dots),
    ))(i)?;
    Ok((
        input,
        Rule::Swapdd {
            name: name.to_string(),
            dots,
            dotpattern,
        },
    ))
}

fn swapcc(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, name, _, characters, _, replacement)) =
        tuple((tag("swapcc"), space1, alpha1, space1, chars, space1, chars))(i)?;
    Ok((
        input,
        Rule::Swapcc {
            name: name.to_string(),
            chars: characters.to_string(),
            replacement: replacement.to_string(),
        },
    ))
}

fn context(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, test, _, action)) =
        tuple((prefixes, tag("context"), space1, chars, space1, chars))(i)?;
    Ok((
        input,
        Rule::Context {
            test: test.to_string(),
            action: action.to_string(),
            prefixes,
        },
    ))
}

fn pass2(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, test, _, action)) =
        tuple((prefixes, tag("pass2"), space1, chars, space1, chars))(i)?;
    Ok((
        input,
        Rule::Pass2 {
            test: test.to_string(),
            action: action.to_string(),
            prefixes,
        },
    ))
}

fn pass3(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, test, _, action)) =
        tuple((prefixes, tag("pass3"), space1, chars, space1, chars))(i)?;
    Ok((
        input,
        Rule::Pass3 {
            test: test.to_string(),
            action: action.to_string(),
            prefixes,
        },
    ))
}

fn pass4(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, test, _, action)) =
        tuple((prefixes, tag("pass4"), space1, chars, space1, chars))(i)?;
    Ok((
        input,
        Rule::Pass4 {
            test: test.to_string(),
            action: action.to_string(),
            prefixes,
        },
    ))
}

fn correct(i: &str) -> IResult<&str, Rule> {
    // FIXME: make sure the prefixes are mandatory here
    let (input, (prefixes, _, _, test, _, action)) =
        tuple((prefixes, tag("correct"), space1, chars, space1, chars))(i)?;
    Ok((
        input,
        Rule::Correct {
            test: test.to_string(),
            action: action.to_string(),
            prefixes,
        },
    ))
}

fn match_opcode(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, classes, positions, _, _, pre, _, chars, _, post, _, dots)) = tuple((
        prefixes,
        with_classes,
        with_matches,
        tag("match"),
        space1,
        chars,
        space1,
        chars,
        space1,
        chars,
        space1,
        braillechars_or_implicit,
    ))(i)?;
    Ok((
        input,
        Rule::Match {
            pre: pre.to_string(),
            characters: chars.to_string(),
            post: post.to_string(),
            dots,
            classes,
            positions,
            prefixes,
        },
    ))
}

fn literal(input: &str) -> IResult<&str, Rule> {
    let (input, (_, _, characters)) = tuple((tag("literal"), space1, chars))(input)?;
    Ok((
        input,
        Rule::Literal {
            characters: characters.to_string(),
        },
    ))
}

fn with_class_before(input: &str) -> IResult<&str, WithClass> {
    let (input, (_, _, class, _)) = tuple((tag("before"), space1, alpha1, space1))(input)?;
    Ok((
        input,
        WithClass::Before {
            class: class.to_string(),
        },
    ))
}

fn with_class_after(input: &str) -> IResult<&str, WithClass> {
    let (input, (_, _, class, _)) = tuple((tag("after"), space1, alpha1, space1))(input)?;
    Ok((
        input,
        WithClass::After {
            class: class.to_string(),
        },
    ))
}

fn with_classes(input: &str) -> IResult<&str, Vec<WithClass>> {
    many0(alt((with_class_before, with_class_after)))(input)
}

fn with_matches(input: &str) -> IResult<&str, WithMatches> {
    alt((
        map(
            tuple((tag("empmatchbefore"), space1, tag("empmatchafter"), space1)),
            |_| WithMatch::Before | WithMatch::After,
        ),
        map(
            tuple((tag("empmatchafter"), space1, tag("empmatchbefore"), space1)),
            |_| WithMatch::After | WithMatch::Before,
        ),
        map(tuple((tag("empmatchbefore"), space1)), |_| {
            enum_set!(WithMatch::Before)
        }),
        map(tuple((tag("empmatchafter"), space1)), |_| {
            enum_set!(WithMatch::After)
        }),
        success::<_, _, Error<_>>(WithMatches::empty()),
    ))(input)
}

fn end_comment(i: &str) -> IResult<&str, &str> {
    let (input, (_, _, comment)) = tuple((space1, opt(tag("#")), not_line_ending))(i)?;
    Ok((input, comment))
}

fn rule_line(i: &str) -> IResult<&str, Line> {
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
                begmodephrase,
                endmodephrase,
                lenmodephrase,
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
            alt((literal,)),
        )),
        alt((end_comment, space0)),
        line_ending,
    ))(i)?;
    Ok((
        input,
        Line::Rule {
            rule,
            comment: comment.to_string(),
        },
    ))
}

fn comment_line(i: &str) -> IResult<&str, Line> {
    let (input, (_, comment, _)) = tuple((tag("#"), not_line_ending, line_ending))(i)?;
    Ok((
        input,
        Line::Comment {
            comment: comment.to_string(),
        },
    ))
}

fn empty_line(i: &str) -> IResult<&str, Line> {
    let (input, (_, _)) = tuple((space0, line_ending))(i)?;
    Ok((input, Line::Empty))
}

fn line(i: &str) -> IResult<&str, Line> {
    let (input, rule) = alt((rule_line, comment_line, empty_line))(i)?;
    Ok((input, rule))
}

pub fn table(i: &str) -> IResult<&str, Vec<Line>> {
    all_consuming(many0(line))(i)
}

fn expand_include(line: Line) -> Vec<Line> {
    if let Line::Rule {
        rule: Rule::Include { filename },
        ..
    } = line
    {
        // FIXME: how do we upstream io and parsing errors?
        let included = fs::read_to_string(filename).unwrap();
        let (_, lines) = table(&included).unwrap();
        return lines;
    }
    vec![line]
}

pub fn expand_includes(lines: Vec<Line>) -> Vec<Line> {
    lines
        .into_iter()
        .flat_map(|line| expand_include(line))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use nom::error::Error;
    use nom::error::ErrorKind;
    use nom::Err;

    #[test]
    fn dot_to_unicode_test() {
        assert_eq!(
            dot_to_unicode(enum_set!(BrailleDot::DOT1 | BrailleDot::DOT8)),
            '⢁'
        );
    }

    #[test]
    fn dots_to_unicode_test() {
        assert_eq!(
            dots_to_unicode(vec![
                enum_set!(BrailleDot::DOT1),
                enum_set!(BrailleDot::DOT8)
            ]),
            "⠁⢀".to_string()
        );
    }

    #[test]
    fn char_to_dot_test() {
        assert_eq!(char_to_dot('8'), Ok(BrailleDot::DOT8));
        assert_eq!(char_to_dot('F'), Err(ParseBrailleError {}));
        assert_eq!(char_to_dot('z'), Err(ParseBrailleError {}));
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
        assert_eq!(
            one_unicode_digit("12"),
            Err(Err::Error(Error {
                input: "12",
                code: ErrorKind::Verify
            }))
        );
        assert_eq!(
            one_unicode_digit("b"),
            Err(Err::Error(Error {
                input: "b",
                code: ErrorKind::Digit
            }))
        );
        assert_eq!(one_unicode_digit("1b"), Ok(("b", '1')));
        assert_eq!(
            one_unicode_digit(""),
            Err(Err::Error(Error {
                input: "",
                code: ErrorKind::Digit
            }))
        );
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
        assert_eq!(
            dots("123"),
            Ok((
                "",
                vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3]
            ))
        );
        assert_eq!(
            dots("1f"),
            Ok(("", vec![BrailleDot::DOT1 | BrailleDot::DOTF]))
        );
        assert_eq!(
            dots("123-1f"),
            Ok((
                "",
                vec![
                    BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3,
                    BrailleDot::DOT1 | BrailleDot::DOTF
                ]
            ))
        );
        assert_eq!(
            dots("123-1f-78"),
            Ok((
                "",
                vec![
                    BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3,
                    BrailleDot::DOT1 | BrailleDot::DOTF,
                    BrailleDot::DOT7 | BrailleDot::DOT8,
                ]
            ))
        );
        assert_eq!(
            dots("huhu"),
            Err(Err::Error(Error {
                input: "huhu",
                code: ErrorKind::HexDigit
            }))
        );
    }

    #[test]
    fn braillechars_or_implicit_test() {
        assert_eq!(
            braillechars_or_implicit("123"),
            Ok((
                "",
                BrailleCharsOrImplicit::Explicit(vec![
                    BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3
                ])
            ))
        );
        assert_eq!(
            braillechars_or_implicit("="),
            Ok(("", BrailleCharsOrImplicit::Implicit))
        );
        assert_eq!(
            dots("m"),
            Err(Err::Error(Error {
                input: "m",
                code: ErrorKind::HexDigit
            }))
        );
    }

    #[test]
    fn with_classes_test() {
        assert_eq!(
            with_classes("before italics "),
            Ok((
                "",
                vec![WithClass::Before {
                    class: "italics".to_string()
                }]
            ))
        );
        assert_eq!(
            with_classes("before italics after foo "),
            Ok((
                "",
                vec![
                    WithClass::Before {
                        class: "italics".to_string()
                    },
                    WithClass::After {
                        class: "foo".to_string()
                    }
                ]
            ))
        );
        assert_eq!(with_classes(""), Ok(("", vec![])));
    }

    #[test]
    fn with_matches_test() {
        assert_eq!(
            with_matches("empmatchbefore empmatchafter "),
            Ok(("", enum_set![WithMatch::Before | WithMatch::After]))
        );
        assert_eq!(
            with_matches("empmatchafter empmatchbefore "),
            Ok(("", enum_set![WithMatch::Before | WithMatch::After]))
        );
        assert_eq!(
            with_matches("empmatchbefore "),
            Ok(("", enum_set![WithMatch::Before]))
        );
        assert_eq!(
            with_matches("empmatchafter "),
            Ok(("", enum_set![WithMatch::After]))
        );
        assert_eq!(with_matches(""), Ok(("", enum_set![])));
    }

    #[test]
    fn include_test() {
        assert_eq!(
            include("include filename.tbl"),
            Ok((
                "",
                Rule::Include {
                    filename: "filename.tbl".to_string()
                }
            ))
        );
    }

    #[test]
    fn undefined_test() {
        assert_eq!(
            undefined("undefined 12"),
            Ok((
                "",
                Rule::Undefined {
                    dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2]
                }
            ))
        );
    }

    #[test]
    fn display_test() {
        assert_eq!(
            display("display haha 122"),
            Ok((
                "",
                Rule::Display {
                    chars: "haha".to_string(),
                    dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2],
                    prefixes: Prefixes::empty()
                }
            ))
        );
    }

    #[test]
    fn space_test() {
        assert_eq!(
            space("space . 0"),
            Ok((
                "",
                Rule::Space {
                    ch: '.',
                    dots: vec![enum_set!(BrailleDot::DOT0)],
                    prefixes: Prefixes::empty()
                }
            ))
        );
    }

    #[test]
    fn punctuation_test() {
        assert_eq!(
            punctuation("punctuation . 46"),
            Ok((
                "",
                Rule::Punctuation {
                    ch: '.',
                    dots: vec![BrailleDot::DOT4 | BrailleDot::DOT6],
                    prefixes: Prefixes::empty()
                }
            ))
        );
    }

    #[test]
    fn digit_test() {
        assert_eq!(
            digit("digit 1 278"),
            Ok((
                "",
                Rule::Digit {
                    ch: '1',
                    dots: vec![BrailleDot::DOT2 | BrailleDot::DOT7 | BrailleDot::DOT8]
                }
            ))
        );
        assert_eq!(
            digit("digit ۲ 1278"),
            Ok((
                "",
                Rule::Digit {
                    ch: '۲',
                    dots: vec![
                        BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT7 | BrailleDot::DOT8
                    ]
                }
            ))
        );
    }

    #[test]
    fn litdigit_test() {
        assert_eq!(
            litdigit("litdigit 0 245"),
            Ok((
                "",
                Rule::Litdigit {
                    ch: '0',
                    dots: vec![BrailleDot::DOT2 | BrailleDot::DOT4 | BrailleDot::DOT5]
                }
            ))
        );
    }

    #[test]
    fn grouping_test() {
        assert_eq!(
            grouping("grouping mfrac ab 3e,4e"),
            Ok((
                "",
                Rule::Grouping {
                    name: "mfrac".to_string(),
                    chars: "ab".to_string(),
                    dots: vec![
                        vec![BrailleDot::DOT3 | BrailleDot::DOTE],
                        vec![BrailleDot::DOT4 | BrailleDot::DOTE]
                    ]
                }
            ))
        );
    }

    #[test]
    fn modeletter_test() {
        assert_eq!(
            modeletter("modeletter uppercase 6"),
            Ok((
                "",
                Rule::Modeletter {
                    attribute: "uppercase".to_string(),
                    dots: vec![enum_set!(BrailleDot::DOT6)],
                    prefixes: Prefixes::empty()
                }
            ))
        );
    }

    #[test]
    fn capsletter_test() {
        assert_eq!(
            capsletter("capsletter 6"),
            Ok((
                "",
                Rule::Capsletter {
                    dots: vec![enum_set!(BrailleDot::DOT6)],
                    prefixes: Prefixes::empty()
                }
            ))
        );
    }

    #[test]
    fn begmodeword_test() {
        assert_eq!(
            begmodeword("begmodeword uppercase 6"),
            Ok((
                "",
                Rule::Begmodeword {
                    attribute: "uppercase".to_string(),
                    dots: vec![enum_set!(BrailleDot::DOT6)],
                    prefixes: Prefixes::empty()
                }
            ))
        );
    }

    #[test]
    fn begcapsword_test() {
        assert_eq!(
            begcapsword("begcapsword 6-6"),
            Ok((
                "",
                Rule::Begcapsword {
                    dots: vec![enum_set!(BrailleDot::DOT6), enum_set!(BrailleDot::DOT6)],
                    prefixes: Prefixes::empty()
                }
            ))
        );
    }

    #[test]
    fn endcapsword_test() {
        assert_eq!(
            endcapsword("endcapsword 6-3"),
            Ok((
                "",
                Rule::Endcapsword {
                    dots: vec![enum_set!(BrailleDot::DOT6), enum_set!(BrailleDot::DOT3)],
                    prefixes: Prefixes::empty()
                }
            ))
        );
    }

    #[test]
    fn capsmodechars_test() {
        assert_eq!(
            capsmodechars("capsmodechars -/"),
            Ok((
                "",
                Rule::Capsmodechars {
                    chars: "-/".to_string()
                }
            ))
        );
    }

    #[test]
    fn begcaps_test() {
        assert_eq!(
            begcaps("begcaps 6-6-6"),
            Ok((
                "",
                Rule::Begcaps {
                    dots: vec![
                        enum_set!(BrailleDot::DOT6),
                        enum_set!(BrailleDot::DOT6),
                        enum_set!(BrailleDot::DOT6)
                    ]
                }
            ))
        );
    }

    #[test]
    fn endcaps_test() {
        assert_eq!(
            endcaps("endcaps 6-3"),
            Ok((
                "",
                Rule::Endcaps {
                    dots: vec![enum_set!(BrailleDot::DOT6), enum_set!(BrailleDot::DOT3)]
                }
            ))
        );
    }

    #[test]
    fn begcapsphrase_test() {
        assert_eq!(
            begcapsphrase("begcapsphrase 45-45"),
            Ok((
                "",
                Rule::Begcapsphrase {
                    dots: vec![
                        enum_set!(BrailleDot::DOT4 | BrailleDot::DOT5),
                        enum_set!(BrailleDot::DOT4 | BrailleDot::DOT5)
                    ]
                }
            ))
        );
    }

    #[test]
    fn endcapsphrase_test() {
        assert_eq!(
            endcapsphrase("endcapsphrase before 45"),
            Ok((
                "",
                Rule::Endcapsphrase {
                    dots: vec![BrailleDot::DOT4 | BrailleDot::DOT5],
                    position: Position::Before
                }
            ))
        );
        assert_eq!(
            endcapsphrase("endcapsphrase after 45"),
            Ok((
                "",
                Rule::Endcapsphrase {
                    dots: vec![BrailleDot::DOT4 | BrailleDot::DOT5],
                    position: Position::After
                }
            ))
        );
        assert_eq!(
            endcapsphrase("endcapsphrase foo 45"),
            Err(Err::Error(Error::new("foo 45", ErrorKind::Tag)))
        );
    }

    #[test]
    fn lencapsphrase_test() {
        assert_eq!(
            lencapsphrase("lencapsphrase 4"),
            Ok(("", Rule::Lencapsphrase { length: 4 }))
        );
    }

    #[test]
    fn prefixes_test() {
        assert_eq!(
            display("nocross display haha 122"),
            Ok((
                "",
                Rule::Display {
                    chars: "haha".to_string(),
                    dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2],
                    prefixes: enum_set!(Prefix::Nocross)
                }
            ))
        );
        assert_eq!(
            display("noback nocross display haha 122"),
            Ok((
                "",
                Rule::Display {
                    chars: "haha".to_string(),
                    dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2],
                    prefixes: Prefix::Noback | Prefix::Nocross
                }
            ))
        );
    }

    #[test]
    fn largesign_test() {
        assert_eq!(
            largesign("largesign überall 123"),
            Ok((
                "",
                Rule::Largesign {
                    characters: "überall".to_string(),
                    dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3]
                }
            ))
        );
        assert_eq!(
            largesign("largesign அஇ 123"),
            Ok((
                "",
                Rule::Largesign {
                    characters: "அஇ".to_string(),
                    dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3]
                }
            ))
        );
    }

    #[test]
    fn joinword_test() {
        assert_eq!(
            joinword("joinword haha 123"),
            Ok((
                "",
                Rule::Joinword {
                    word: "haha".to_string(),
                    dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3]
                }
            ))
        );
        assert_eq!(
            joinword("joinword அஇ 123"),
            Ok((
                "",
                Rule::Joinword {
                    word: "அஇ".to_string(),
                    dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3]
                }
            ))
        );
    }

    #[test]
    fn uppercase_test() {
        assert_eq!(
            uppercase("uppercase f 123"),
            Ok((
                "",
                Rule::Uppercase {
                    ch: 'f',
                    dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3],
                    prefixes: Prefixes::empty()
                }
            ))
        );
        assert_eq!(
            uppercase("uppercase அ 123"),
            Ok((
                "",
                Rule::Uppercase {
                    ch: 'அ',
                    dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3],
                    prefixes: Prefixes::empty()
                }
            ))
        );
        assert_eq!(
            uppercase("uppercase \\x04D8 34579"),
            Ok((
                "",
                Rule::Uppercase {
                    ch: 'Ә',
                    dots: vec![
                        BrailleDot::DOT3
                            | BrailleDot::DOT4
                            | BrailleDot::DOT5
                            | BrailleDot::DOT7
                            | BrailleDot::DOT9
                    ],
                    prefixes: Prefixes::empty()
                }
            ))
        );
    }

    #[test]
    fn lowercase_test() {
        assert_eq!(
            lowercase("lowercase f 123"),
            Ok((
                "",
                Rule::Lowercase {
                    ch: 'f',
                    dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3],
                    prefixes: Prefixes::empty()
                }
            ))
        );
        assert_eq!(
            lowercase("lowercase அ 123"),
            Ok((
                "",
                Rule::Lowercase {
                    ch: 'அ',
                    dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3],
                    prefixes: Prefixes::empty()
                }
            ))
        );
    }

    #[test]
    fn multind_test() {
        assert_eq!(
            multind("multind 123 lowercase uppercase"),
            Ok((
                "",
                Rule::Multind {
                    dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3],
                    opcodes: vec!["lowercase".to_string(), "uppercase".to_string()],
                    prefixes: Prefixes::empty()
                }
            ))
        );
        // FIXME: test multind with an end comment
        // FIXME: test multind with invalid opcodes
    }

    #[test]
    fn always_test() {
        assert_eq!(
            always("always world 123"),
            Ok((
                "",
                Rule::Always {
                    characters: "world".to_string(),
                    dots: BrailleCharsOrImplicit::Explicit(vec![
                        BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3
                    ]),
                    prefixes: Prefixes::empty(),
                    classes: vec![]
                }
            ))
        );
        assert_eq!(
            always("before number always world 123"),
            Ok((
                "",
                Rule::Always {
                    characters: "world".to_string(),
                    dots: BrailleCharsOrImplicit::Explicit(vec![
                        BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3
                    ]),
                    prefixes: Prefixes::empty(),
                    classes: vec![WithClass::Before {
                        class: "number".to_string()
                    }]
                }
            ))
        );
    }

    #[test]
    fn match_test() {
        assert_eq!(
            match_opcode("match ab xyz cd 1346-13456"),
            Ok((
                "",
                Rule::Match {
                    pre: "ab".to_string(),
                    characters: "xyz".to_string(),
                    post: "cd".to_string(),
                    positions: WithMatches::empty(),
                    classes: vec![],
                    dots: BrailleCharsOrImplicit::Explicit(vec![
                        BrailleDot::DOT1 | BrailleDot::DOT3 | BrailleDot::DOT4 | BrailleDot::DOT6,
                        BrailleDot::DOT1
                            | BrailleDot::DOT3
                            | BrailleDot::DOT4
                            | BrailleDot::DOT5
                            | BrailleDot::DOT6
                    ]),
                    prefixes: Prefixes::empty()
                }
            ))
        );
    }

    #[test]
    fn rule_line_test() {
        assert_eq!(
            rule_line("joinword haha 123\n"),
            Ok((
                "",
                Line::Rule {
                    rule: Rule::Joinword {
                        word: "haha".to_string(),
                        dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3]
                    },
                    comment: "".to_string()
                }
            ))
        );
        assert_eq!(
            rule_line("largesign அஇ 123\n"),
            Ok((
                "",
                Line::Rule {
                    rule: Rule::Largesign {
                        characters: "அஇ".to_string(),
                        dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3]
                    },
                    comment: "".to_string()
                }
            ))
        );
        assert_eq!(
            rule_line("syllable haha 123\n"),
            Ok((
                "",
                Line::Rule {
                    rule: Rule::Syllable {
                        word: "haha".to_string(),
                        dots: BrailleCharsOrImplicit::Explicit(vec![
                            BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3
                        ])
                    },
                    comment: "".to_string()
                }
            ))
        );
        assert_eq!(
            rule_line("base uppercase A a\n"),
            Ok((
                "",
                Line::Rule {
                    rule: Rule::Base {
                        attribute: "uppercase".to_string(),
                        derived: 'A',
                        base: 'a'
                    },
                    comment: "".to_string()
                }
            ))
        );
    }

    #[test]
    fn empty_line_test() {
        assert_eq!(empty_line("       \n"), Ok(("", Line::Empty)));
        assert_eq!(empty_line("\n"), Ok(("", Line::Empty)));
    }

    #[test]
    fn comment_line_test() {
        assert_eq!(
            comment_line("# haha 1234    \n"),
            Ok((
                "",
                Line::Comment {
                    comment: " haha 1234    ".to_string()
                }
            ))
        );
        assert_eq!(
            comment_line("# haha 1234    "),
            Err(Err::Error(Error::new("", ErrorKind::CrLf)))
        );
    }

    #[test]
    fn end_comment_test() {
        // assert_eq!(
        //     end_comment("an end comment\n"),
        //     Err(Err::Error(Error::new("an end comment\n", ErrorKind::Space))));
        // assert_eq!(end_comment(" an end comment\n"), Ok(("\n", "an end comment")));
        assert_eq!(
            rule_line("joinword haha 123 # comment \n"),
            Ok((
                "",
                Line::Rule {
                    rule: Rule::Joinword {
                        word: "haha".to_string(),
                        dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3]
                    },
                    comment: " comment ".to_string()
                }
            ))
        );
    }

    #[test]
    fn table_test() {
        assert_eq!(
            table(concat!(
                "       \n",
                "joinword haha 123\n",
                "syllable haha 123-1f\n"
            )),
            Ok((
                "",
                vec![
                    Line::Empty,
                    Line::Rule {
                        rule: Rule::Joinword {
                            word: "haha".to_string(),
                            dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3]
                        },
                        comment: "".to_string()
                    },
                    Line::Rule {
                        rule: Rule::Syllable {
                            word: "haha".to_string(),
                            dots: BrailleCharsOrImplicit::Explicit(vec![
                                BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3,
                                BrailleDot::DOT1 | BrailleDot::DOTF
                            ])
                        },
                        comment: "".to_string()
                    }
                ]
            ))
        );
        assert_eq!(
            table(concat!(
                "       \n",
                "# just testing\n",
                "nocross multind 123 always syllable\n",
                "joinword haha 123\n",
                "syllable haha =\n"
            )),
            Ok((
                "",
                vec![
                    Line::Empty,
                    Line::Comment {
                        comment: " just testing".to_string()
                    },
                    Line::Rule {
                        rule: Rule::Multind {
                            opcodes: vec!["always".to_string(), "syllable".to_string()],
                            dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3],
                            prefixes: enum_set!(Prefix::Nocross)
                        },
                        comment: "".to_string()
                    },
                    Line::Rule {
                        rule: Rule::Joinword {
                            word: "haha".to_string(),
                            dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3]
                        },
                        comment: "".to_string()
                    },
                    Line::Rule {
                        rule: Rule::Syllable {
                            word: "haha".to_string(),
                            dots: BrailleCharsOrImplicit::Implicit
                        },
                        comment: "".to_string()
                    }
                ]
            ))
        );
    }
}
