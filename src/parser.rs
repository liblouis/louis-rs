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
    Multind { chars: String, dots: BrailleChars, prefixes: Prefixes },
    Punctuation { ch: char, dots: BrailleChars, prefixes: Prefixes},
    Digit { ch: char, dots: BrailleChars },
    Letter { ch: char, dots: BrailleChars },
    Lowercase { word: String, dots: BrailleChars },
    Uppercase { word: String, dots: BrailleChars },
    Litdigit { chars: String, dots: BrailleChars },
    Sign { ch: char, dots: BrailleChars },
    Math { ch: char, dots: BrailleChars },
    Grouping { name: String, chars: String, dots: Vec<BrailleChars> },
    Base { attribute: String, derived: char, base: char },
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
    Noletsign { letters: String },
    Noletsignbefore { characters: String },
    Noletsignafter { characters: String },
    Nocontractsign { dots: BrailleChars },
    Numsign { dots: BrailleChars },
    Numericnocontchars { characters: String },
    Numericmodechars { characters: String },
    Midendnumericmodechars { characters: String },
    // Standing Alone Sequences
    Begcapsphrase { dots: BrailleChars},
    Endcapsphrase { dots: BrailleChars, position: Position},
    Lencapsphrase { length: u8},
    Largesign { word: String, dots: BrailleChars },
    Syllable { word: String, dots: BrailleChars },
    Joinword { word: String, dots: BrailleChars },
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
    let (i, digits) = hex_digit1(input)?;
    let num = digits.parse::<u32>().unwrap();
    let c = char::from_u32(num).unwrap();
    Ok((i, c))
}

pub fn escaped_char(i: &str) -> IResult<&str, char> {
    let (input, (_, c)) = tuple((tag("\\x"), unicode_literal))(i)?;
    Ok((input, c))
}

pub fn single_char(input: &str) -> IResult<&str, char> {
    none_of(" \t\r\n")(input)
}

pub fn ascii_chars(input: &str) -> IResult<&str, &str> {
    alpha1(input)
}

pub fn filename(input: &str) -> IResult<&str, &str> {
    is_a("abcdefghijklmnopqrstuvwxyz0123456789_-.")(input)
}

pub fn dots(i: &str) -> IResult<&str, BrailleChars> {
    let (input, dots) = separated_list1(tag("-"), hex_digit1)(i)?;
    let braille_chars: Vec<BrailleChar> = dots
	.iter()
	.map(|chars| chars_to_dots(chars))
	.collect();
    Ok((input, braille_chars))
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
    let (input, (prefixes, _, _, chars, _, dots)) = tuple((opt(prefixes), tag("multind"), space1, chars, space1, dots))(i)?;
    Ok((input, Rule::Multind { chars: chars.to_string(), dots, prefixes: prefixes.unwrap() }))
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
    let (input, (_, _, ch, _, dots)) = tuple((tag("letter"), space1, single_char, space1, dots))(i)?;
    Ok((input, Rule::Letter { ch, dots }))
}

pub fn lowercase(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, word, _, dots)) = tuple((
        tag("lowercase"), space1, single_char, space1, dots,
    ))(i)?;
    Ok((input, Rule::Lowercase { word: word.to_string(), dots }))
}

pub fn uppercase(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, word, _, dots)) = tuple((
        tag("uppercase"), space1, single_char, space1, dots,
    ))(i)?;
    Ok((input, Rule::Uppercase { word: word.to_string(), dots }))
}

pub fn litdigit(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, chars, _, dots)) = tuple((tag("litdigit"), space1, unicode_digit1, space1, dots))(i)?;
    Ok((input, Rule::Litdigit { chars: chars.to_string(), dots }))
}

pub fn sign(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, ch, _, dots)) = tuple((tag("sign"), space1, single_char, space1, dots))(i)?;
    Ok((input, Rule::Sign { ch, dots }))
}

pub fn math(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, ch, _, dots)) = tuple((tag("math"), space1, single_char, space1, dots))(i)?;
    Ok((input, Rule::Math { ch, dots }))
}

pub fn grouping(i: &str) -> IResult<&str, Rule> {
    // FIXME: handle n dots
    let (input, (_, _, name, _, characters, _, dots)) = tuple((tag("grouping"), space1, ascii_chars, space1, ascii_chars, space1, dots))(i)?;
    Ok((input, Rule::Grouping { name: name.to_string(), chars: characters.to_string(), dots: vec![dots]}))
}

pub fn base(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, attribute, _, derived, _, base)) = tuple((tag("base"), space1, ascii_chars, space1, single_char, space1, single_char))(i)?;
    Ok((input, Rule::Base { attribute: attribute.to_string(), derived, base }))
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

pub fn largesign(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, word, _, dots)) = tuple((
        tag("largesign"), space1, chars, space1, dots,
    ))(i)?;
    Ok((input, Rule::Largesign { word: word.to_string(), dots }))
}

pub fn syllable(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, word, _, dots)) = tuple((
        tag("syllable"), space1, chars, space1, dots,
    ))(i)?;
    Ok((input, Rule::Syllable { word: word.to_string(), dots }))
}

pub fn joinword(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, word, _, dots)) = tuple((
        tag("joinword"), space1, chars, space1, dots,
    ))(i)?;
    Ok((input, Rule::Joinword { word: word.to_string(), dots }))
}

fn end_comment(i: &str) -> IResult<&str, &str> {
    let (input, (_, _, comment)) = tuple((space1, tag("#"), not_line_ending))(i)?;
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
		begcapsphrase,
		endcapsphrase,
		largesign,
		syllable,
		joinword,
	    )),
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
        assert_eq!(dots("huhu"),
		   Err(Err::Error(Error::new("huhu", ErrorKind::HexDigit)))
        );
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
		   Ok(("", Rule::Litdigit { chars: "0".to_string(), dots: vec![BrailleDot::DOT2 | BrailleDot::DOT4 | BrailleDot::DOT5] })));
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
    fn _test() {
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
            Ok(("", Rule::Largesign { word: "überall".to_string(), dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3] })));
        assert_eq!(
            largesign("largesign அஇ 123"),
            Ok(("", Rule::Largesign { word: "அஇ".to_string(), dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3] })));
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
            Ok(("", Rule::Uppercase { word: "f".to_string(), dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3] })));
        assert_eq!(
            uppercase("uppercase அ 123"),
            Ok(("", Rule::Uppercase { word: "அ".to_string(), dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3] })));
        assert_eq!(
            uppercase(r"uppercase \x04D8 34579"),
            Ok(("", Rule::Uppercase { word: "Ә".to_string(), dots: vec![BrailleDot::DOT3 | BrailleDot::DOT4 | BrailleDot::DOT5 | BrailleDot::DOT7 | BrailleDot::DOT9] })));
    }

    #[test]
    fn lowercase_test() {
        assert_eq!(
            lowercase("lowercase f 123"),
            Ok(("", Rule::Lowercase { word: "f".to_string(), dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3] })));
        assert_eq!(
            lowercase("lowercase அ 123"),
            Ok(("", Rule::Lowercase { word: "அ".to_string(), dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3] })));
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
            Ok(("", Line::Rule { rule: Rule::Largesign { word: "அஇ".to_string(),
							 dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3] },
				 comment: "".to_string() })));
        assert_eq!(
            rule_line("syllable haha 123\n"),
            Ok(("", Line::Rule { rule: Rule::Syllable { word: "haha".to_string(),
							dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3] },
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
							     dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3,
									BrailleDot::DOT1 | BrailleDot::DOTF] },
				      comment: "".to_string() }])));
        assert_eq!(
            table(concat!("       \n",
			  "# just testing\n",
			  "nocross multind hehe 123\n",
			  "joinword haha 123\n",
			  "syllable haha 123\n")),
            Ok(("", vec![Line::Empty,
			 Line::Comment { comment: " just testing".to_string() },
			 Line::Rule { rule: Rule::Multind { chars: "hehe".to_string(),
							    dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3],
							    prefixes: enum_set!(Prefix::Nocross) },
				      comment: "".to_string() },
			 Line::Rule { rule: Rule::Joinword { word: "haha".to_string(),
							     dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3] },
				      comment: "".to_string() },
			 Line::Rule { rule: Rule::Syllable { word: "haha".to_string(),
							     dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3] },
				      comment: "".to_string() }])));
    }
}
