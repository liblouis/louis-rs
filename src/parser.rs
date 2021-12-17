use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::alpha1;
use nom::character::complete::hex_digit1;
use nom::character::complete::line_ending;
use nom::character::complete::space0;
use nom::character::complete::space1;
use nom::sequence::tuple;
use nom::multi::many0;
use nom::multi::separated_list1;
use nom::character::complete::not_line_ending;

use enumset::EnumSetType;
use enumset::EnumSet;

use nom::IResult;
use nom_unicode::complete::alpha1 as unicode_alpha1;

#[derive(PartialEq, Debug)]
pub enum Line<'a> {
    Empty,
    Comment { comment: &'a str },
    Rule { rule: Rule<'a>, comment: &'a str }
}

#[derive(PartialEq, Debug)]
pub enum Rule<'a> {
    Include { filename: &'a str },
    Undefined { dots: BrailleChars },
    Display { chars: &'a str, dots: BrailleChars },
    Largesign { word: &'a str, dots: BrailleChars },
    Syllable { word: &'a str, dots: BrailleChars },
    Joinword { word: &'a str, dots: BrailleChars },
}

#[derive(EnumSetType, Debug)]
pub enum BrailleDot {
    DOT0, DOT1, DOT2, DOT3, DOT4, DOT5, DOT6,
    DOT7, DOT8, DOT9, DOTA, DOTB, DOTC, DOTD,
    DOTE, DOTF,
}

type BrailleChar = EnumSet<BrailleDot>;
type BrailleChars = Vec<BrailleChar>;

fn char_to_dot(char: char) -> BrailleDot {
    match char {
	'0' => BrailleDot::DOT0,
	'1' => BrailleDot::DOT1,
	'2' => BrailleDot::DOT2,
	'3' => BrailleDot::DOT3,
	'4' => BrailleDot::DOT4,
	'5' => BrailleDot::DOT5,
	'6' => BrailleDot::DOT6,
	'7' => BrailleDot::DOT7,
	'8' => BrailleDot::DOT8,
	'9' => BrailleDot::DOT9,
	'a' => BrailleDot::DOTA,
	'b' => BrailleDot::DOTB,
	'c' => BrailleDot::DOTC,
	'd' => BrailleDot::DOTD,
	'e' => BrailleDot::DOTE,
	'f' => BrailleDot::DOTF,
	 _  => BrailleDot::DOT0, // FIXME: might be better to return an opt
    }
}

fn chars_to_dots(chars: &str) -> BrailleChar {
    chars
	.chars()
	.map(|c| char_to_dot(c))
	.collect()
}

pub fn chars(input: &str) -> IResult<&str, &str> {
    unicode_alpha1(input)
}

pub fn ascii_chars(input: &str) -> IResult<&str, &str> {
    alpha1(input)
}

pub fn dots(i: &str) -> IResult<&str, BrailleChars> {
    let (input, dots) = separated_list1(tag("-"), hex_digit1)(i)?;
    let braille_chars: Vec<BrailleChar> = dots
	.iter()
	.map(|chars| chars_to_dots(chars))
	.collect();
    Ok((input, braille_chars))
}

pub fn include(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, filename)) = tuple((tag("include"), space1, chars))(i)?;
    Ok((input, Rule::Include { filename: filename }))
}

pub fn undefined(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, dots)) = tuple((tag("undefined"), space1, dots))(i)?;
    Ok((input, Rule::Undefined { dots: dots }))
}

pub fn display(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _, chars, _, dots)) = tuple((tag("display"), space1, chars, space1, dots))(i)?;
    Ok((input, Rule::Display { chars: chars, dots: dots }))
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
    let (input, (_, comment)) = tuple((
        space1, not_line_ending,
    ))(i)?;
    Ok((input, comment))
}

pub fn rule_line(i: &str) -> IResult<&str, Line> {
    let (input, (rule, comment, _)) = tuple((
        alt((largesign, joinword, syllable)),
	alt((end_comment, space0)),
	line_ending,
    ))(i)?;
    Ok((input, Line::Rule { rule: rule, comment: comment}))
}

pub fn comment_line(i: &str) -> IResult<&str, Line> {
    let (input, (_, comment, _)) = tuple((tag("#"), not_line_ending, line_ending,))(i)?;
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
        assert_eq!(include("include filename"), Ok(("", Rule::Include { filename: "filename" })));
    }

    #[test]
    fn undefined_test() {
        assert_eq!(undefined("undefined 12"), Ok(("", Rule::Undefined { dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2] })));
    }

    #[test]
    fn display_test() {
        assert_eq!(display("display haha 122"), Ok(("", Rule::Display { chars: "haha", dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2] })));
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
            Ok(("", Line::Rule { rule: Rule::Joinword { word: "haha", dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3] }, comment: "" })));
        assert_eq!(
            rule_line("largesign அஇ 123\n"),
            Ok(("", Line::Rule { rule: Rule::Largesign { word: "அஇ", dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3] }, comment: "" })));
        assert_eq!(
            rule_line("syllable haha 123\n"),
            Ok(("", Line::Rule { rule: Rule::Syllable { word: "haha", dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3] }, comment: "" })));
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
            Ok(("", Line::Rule { rule: Rule::Joinword { word: "haha", dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3] }, comment: "comment " })));
    }

    #[test]
    fn table_test() {
        assert_eq!(
            table(concat!("       \n",
			  "joinword haha 123\n",
			  "syllable haha 123\n")),
            Ok(("", vec![Line::Empty,
			 Line::Rule { rule: Rule::Joinword { word: "haha", dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3] }, comment: "" },
			 Line::Rule { rule: Rule::Syllable { word: "haha", dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3] }, comment: "" }])));
        assert_eq!(
            table(concat!("       \n",
			  "# just testing\n",
			  "joinword haha 123\n",
			  "syllable haha 123\n")),
            Ok(("", vec![Line::Empty,
			 Line::Comment { comment: " just testing" },
			 Line::Rule { rule: Rule::Joinword { word: "haha", dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3] }, comment: "" },
			 Line::Rule { rule: Rule::Syllable { word: "haha", dots: vec![BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3] }, comment: "" }])));
    }
}
