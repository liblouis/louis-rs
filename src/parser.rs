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
pub enum Line<'a> {
    Empty,
    Comment { comment: &'a str },
    Rule { rule: Rule<'a>, comment: &'a str },
}

#[derive(PartialEq, Debug)]
pub enum Rule<'a> {
    Include { filename: &'a str },
    Undefined { dots: BrailleChars },
    Display { chars: &'a str, dots: BrailleChars, prefixes: Prefixes },
    Space { ch: char, dots: BrailleChars, prefixes: Prefixes},
    Multind { chars: &'a str, dots: BrailleChars, prefixes: Prefixes },
    Punctuation { ch: char, dots: BrailleChars, prefixes: Prefixes},
    Digit { ch: char, dots: BrailleChars },
    Litdigit { chars: &'a str, dots: BrailleChars },
    Modeletter { chars: &'a str, dots: BrailleChars, prefixes: Prefixes},
    Capsletter { dots: BrailleChars, prefixes: Prefixes},
    Begmodeword { chars: &'a str, dots: BrailleChars, prefixes: Prefixes},
    Begcapsword { dots: BrailleChars, prefixes: Prefixes},
    Endcapsword { dots: BrailleChars, prefixes: Prefixes},
    Capsmodechars { chars: &'a str},
    Begcaps { dots: BrailleChars},
    Endcaps { dots: BrailleChars},
    Begcapsphrase { dots: BrailleChars},
    Endcapsphrase { dots: BrailleChars, position: Position},
    Lencapsphrase { length: u8},
    Largesign { word: &'a str, dots: BrailleChars },
    Syllable { word: &'a str, dots: BrailleChars },
    Joinword { word: &'a str, dots: BrailleChars },
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

fn char_to_dot(char: char) -> Option<BrailleDot> {
    match char {
        '0' => Some(BrailleDot::DOT0),
        '1' => Some(BrailleDot::DOT1),
        '2' => Some(BrailleDot::DOT2),
        '3' => Some(BrailleDot::DOT3),
        '4' => Some(BrailleDot::DOT4),
        '5' => Some(BrailleDot::DOT5),
        '6' => Some(BrailleDot::DOT6),
        '7' => Some(BrailleDot::DOT7),
        '8' => Some(BrailleDot::DOT8),
        '9' => Some(BrailleDot::DOT9),
        'a' => Some(BrailleDot::DOTA),
        'b' => Some(BrailleDot::DOTB),
        'c' => Some(BrailleDot::DOTC),
        'd' => Some(BrailleDot::DOTD),
        'e' => Some(BrailleDot::DOTE),
        'f' => Some(BrailleDot::DOTF),
        _ => None,
    }
}

fn chars_to_dots(chars: &str) -> BrailleChar {
    chars.chars().map(|c| char_to_dot(c).unwrap()).collect()
}

pub fn chars(input: &str) -> IResult<&str, &str> {
    is_not(" \t\r\n")(input)
    //unicode_alpha1(input)
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
    Ok((input, Rule::Include { filename: filename }))
}

pub fn undefined(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, dots)) = tuple((tag("undefined"), space1, dots))(i)?;
    Ok((input, Rule::Undefined { dots: dots }))
}

pub fn display(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, chars, _, dots)) = tuple((opt(prefixes), tag("display"), space1, chars, space1, dots))(i)?;
    Ok((input, Rule::Display { chars: chars, dots: dots, prefixes: prefixes.unwrap() }))
}

pub fn space(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, c, _, dots)) = tuple((opt(prefixes), tag("space"), space1, single_char, space1, dots))(i)?;
    Ok((input, Rule::Space { ch: c, dots: dots, prefixes: prefixes.unwrap() }))
}

pub fn multind(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, chars, _, dots)) = tuple((opt(prefixes), tag("multind"), space1, chars, space1, dots))(i)?;
    Ok((input, Rule::Multind { chars: chars, dots: dots, prefixes: prefixes.unwrap() }))
}

pub fn punctuation(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, c, _, dots)) = tuple((opt(prefixes), tag("punctuation"), space1, single_char, space1, dots))(i)?;
    Ok((input, Rule::Punctuation { ch: c, dots: dots, prefixes: prefixes.unwrap() }))
}

pub fn digit(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, digit, _, dots)) = tuple((tag("digit"), space1, single_char, space1, dots))(i)?;
    Ok((input, Rule::Digit { ch: digit, dots: dots }))
}

pub fn litdigit(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, chars, _, dots)) = tuple((tag("litdigit"), space1, unicode_digit1, space1, dots))(i)?;
    Ok((input, Rule::Litdigit { chars: chars, dots: dots }))
}

pub fn modeletter(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, chars, _, dots)) = tuple((opt(prefixes), tag("modeletter"), space1, ascii_chars, space1, dots))(i)?;
    Ok((input, Rule::Modeletter { chars: chars, dots: dots, prefixes: prefixes.unwrap() }))
}

pub fn capsletter(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, dots)) = tuple((opt(prefixes), tag("capsletter"), space1, dots))(i)?;
    Ok((input, Rule::Capsletter { dots: dots, prefixes: prefixes.unwrap() }))
}

pub fn begmodeword(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, chars, _, dots)) = tuple((opt(prefixes), tag("begmodeword"), space1, ascii_chars, space1, dots))(i)?;
    Ok((input, Rule::Begmodeword { chars: chars, dots: dots, prefixes: prefixes.unwrap() }))
}

pub fn begcapsword(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, dots)) = tuple((opt(prefixes), tag("begcapsword"), space1, dots))(i)?;
    Ok((input, Rule::Begcapsword { dots: dots, prefixes: prefixes.unwrap() }))
}

pub fn endcapsword(i: &str) -> IResult<&str, Rule> {
    let (input, (prefixes, _, _, dots)) = tuple((opt(prefixes), tag("endcapsword"), space1, dots))(i)?;
    Ok((input, Rule::Endcapsword { dots: dots, prefixes: prefixes.unwrap() }))
}

pub fn capsmodechars(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, chars)) = tuple((tag("capsmodechars"), space1, chars))(i)?;
    Ok((input, Rule::Capsmodechars { chars: chars }))
}

pub fn begcaps(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, dots)) = tuple((tag("begcaps"), space1, dots))(i)?;
    Ok((input, Rule::Begcaps { dots: dots }))
}

pub fn endcaps(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, dots)) = tuple((tag("endcaps"), space1, dots))(i)?;
    Ok((input, Rule::Endcaps { dots: dots }))
}

pub fn begcapsphrase(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, dots)) = tuple((tag("begcapsphrase"), space1, dots))(i)?;
    Ok((input, Rule::Begcapsphrase { dots: dots }))
}

pub fn endcapsphrase(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, position, _, dots)) = tuple((tag("endcapsphrase"), space1, before_or_after, space1, dots))(i)?;
    Ok((input, Rule::Endcapsphrase { dots: dots, position: position }))
}

pub fn lencapsphrase(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, length)) = tuple((tag("lencapsphrase"), space1, number))(i)?;
    Ok((input, Rule::Lencapsphrase { length: length }))
}

pub fn largesign(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, word, _, dots)) = tuple((
        tag("largesign"), space1, chars, space1, dots,
    ))(i)?;
    Ok((input, Rule::Largesign { word: word, dots: dots }))
}

pub fn syllable(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, word, _, dots)) = tuple((
        tag("syllable"), space1, chars, space1, dots,
    ))(i)?;
    Ok((input, Rule::Syllable { word: word, dots: dots }))
}

pub fn joinword(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, word, _, dots)) = tuple((
        tag("joinword"), space1, chars, space1, dots,
    ))(i)?;
    Ok((input, Rule::Joinword { word: word, dots: dots }))
}

pub fn end_comment(i: &str) -> IResult<&str, &str> {
    let (input, (_, comment)) = tuple((space1, not_line_ending))(i)?;
    Ok((input, comment))
}

pub fn rule_line(i: &str) -> IResult<&str, Line> {
    let (input, (rule, comment, _)) = tuple((
        alt((
	    include,
	    undefined,
	    display,
	    multind,
	    largesign,
	    joinword,
	    syllable)),
	alt((end_comment, space0)),
	line_ending,
    ))(i)?;
    Ok((input, Line::Rule { rule: rule, comment: comment}))
}

pub fn comment_line(i: &str) -> IResult<&str, Line> {
    let (input, (_, comment, _)) = tuple((tag("#"), not_line_ending, line_ending))(i)?;
    Ok((input, Line::Comment { comment: comment }))
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
    many0(line)(i)
}

#[cfg(test)]
mod tests {
    use super::*;
    use nom::error::Error;
    use nom::error::ErrorKind;
    use nom::Err;

    #[test]
    fn char_to_dot_test() {
        assert_eq!(char_to_dot('8'), Some(BrailleDot::DOT8));
        assert_eq!(char_to_dot('F'), None);
        assert_eq!(char_to_dot('z'), None);
    }

    #[test]
    fn character_test() {
        assert_eq!(ascii_chars("hallo"), Ok(("", "hallo")));
        assert_eq!(ascii_chars("haLlo"), Ok(("", "haLlo")));
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
        assert_eq!(include("include filename.tbl"), Ok(("", Rule::Include { filename: "filename.tbl" })));
    }

    #[test]
    fn undefined_test() {
        assert_eq!(undefined("undefined 12"), Ok(("", Rule::Undefined { dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2] })));
    }

    #[test]
    fn display_test() {
        assert_eq!(display("display haha 122"), Ok(("", Rule::Display { chars: "haha",
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
		   Ok(("", Rule::Litdigit { chars: "0", dots: vec![BrailleDot::DOT2 | BrailleDot::DOT4 | BrailleDot::DOT5] })));
    }

    #[test]
    fn modeletter_test() {
        assert_eq!(modeletter("modeletter uppercase 6"),
		   Ok(("", Rule::Modeletter { chars: "uppercase",
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
		   Ok(("", Rule::Begmodeword { chars: "uppercase",
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
		   Ok(("", Rule::Capsmodechars { chars: "-/"})));
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
		   Ok(("", Rule::Display { chars: "haha",
					   dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2],
					   prefixes: enum_set!(Prefix::Nocross) })));
        assert_eq!(display("noback nocross display haha 122"),
		   Ok(("", Rule::Display { chars: "haha",
					   dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2],
					   prefixes: Prefix::Noback | Prefix::Nocross })));
    }

    #[test]
    fn largesign_test() {
        assert_eq!(
            largesign("largesign überall 123"),
            Ok(("", Rule::Largesign { word: "überall", dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3] })));
        assert_eq!(
            largesign("largesign அஇ 123"),
            Ok(("", Rule::Largesign { word: "அஇ", dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3] })));
    }

    #[test]
    fn joinword_test() {
        assert_eq!(
            joinword("joinword haha 123"),
            Ok(("", Rule::Joinword { word: "haha", dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3] })));
        assert_eq!(
            joinword("joinword அஇ 123"),
            Ok(("", Rule::Joinword { word: "அஇ", dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3] })));
    }

    #[test]
    fn rule_line_test() {
        assert_eq!(
            rule_line("joinword haha 123\n"),
            Ok(("", Line::Rule { rule: Rule::Joinword { word: "haha",
							dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3] },
				 comment: "" })));
        assert_eq!(
            rule_line("largesign அஇ 123\n"),
            Ok(("", Line::Rule { rule: Rule::Largesign { word: "அஇ",
							 dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3] },
				 comment: "" })));
        assert_eq!(
            rule_line("syllable haha 123\n"),
            Ok(("", Line::Rule { rule: Rule::Syllable { word: "haha",
							dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3] },
				 comment: "" })));
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
            Ok(("", Line::Comment { comment: " haha 1234    "})));
        assert_eq!(
            comment_line("# haha 1234    "),
            Err(Err::Error(Error::new("", ErrorKind::CrLf))));
    }

    #[test]
    fn end_comment_test() {
	assert_eq!(
	    end_comment("an end comment\n"),
	    Err(Err::Error(Error::new("an end comment\n", ErrorKind::Space))));
	assert_eq!(end_comment(" an end comment\n"), Ok(("\n", "an end comment")));
        assert_eq!(
            rule_line("joinword haha 123 comment \n"),
            Ok(("", Line::Rule { rule: Rule::Joinword { word: "haha",
							dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3] },
				 comment: "comment " })));
    }

    #[test]
    fn table_test() {
        assert_eq!(
            table(concat!("       \n",
			  "joinword haha 123\n",
			  "syllable haha 123-1f\n")),
            Ok(("", vec![Line::Empty,
			 Line::Rule { rule: Rule::Joinword { word: "haha",
							     dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3] },
				      comment: "" },
			 Line::Rule { rule: Rule::Syllable { word: "haha",
							     dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3,
									BrailleDot::DOT1 | BrailleDot::DOTF] },
				      comment: "" }])));
        assert_eq!(
            table(concat!("       \n",
			  "# just testing\n",
			  "nocross multind hehe 123\n",
			  "joinword haha 123\n",
			  "syllable haha 123\n")),
            Ok(("", vec![Line::Empty,
			 Line::Comment { comment: " just testing" },
			 Line::Rule { rule: Rule::Multind { chars: "hehe",
							    dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3],
							    prefixes: enum_set!(Prefix::Nocross) },
				      comment: "" },
			 Line::Rule { rule: Rule::Joinword { word: "haha",
							     dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3] },
				      comment: "" },
			 Line::Rule { rule: Rule::Syllable { word: "haha",
							     dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3] },
				      comment: "" }])));
    }
}
