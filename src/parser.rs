use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::alpha1;
use nom::character::complete::hex_digit1;
use nom::character::complete::line_ending;
use nom::character::complete::space0;
use nom::character::complete::space1;
use nom::sequence::tuple;
use nom::multi::many0;
use nom::character::complete::not_line_ending;

use nom::IResult;
use nom_unicode::complete::alpha1 as unicode_alpha1;

#[derive(PartialEq, Debug)]
pub enum Rule<'a> {
    Empty,
    Comment { comment: &'a str },
    Largesign { word: &'a str, dots: &'a str },
    Syllable { word: &'a str, dots: &'a str },
    Joinword { word: &'a str, dots: &'a str},
}

pub fn chars(input: &str) -> IResult<&str, &str> {
    unicode_alpha1(input)
}

pub fn ascii_chars(input: &str) -> IResult<&str, &str> {
    alpha1(input)
}

pub fn dots(input: &str) -> IResult<&str, &str> {
    hex_digit1(input)
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

pub fn rule_line(i: &str) -> IResult<&str, Rule> {
    let (input, (rule, _, _)) = tuple((
        alt((largesign, joinword, syllable)), space0, line_ending,
    ))(i)?;
    Ok((input, rule))
}

pub fn comment_line(i: &str) -> IResult<&str, Rule> {
    let (input, (_, comment, _)) = tuple((tag("#"), not_line_ending, line_ending,))(i)?;
    Ok((input, Rule::Comment {comment: comment}))
}

pub fn empty_line(i: &str) -> IResult<&str, Rule> {
    let (input, (_, _)) = tuple((space0, line_ending))(i)?;
    Ok((input, Rule::Empty))
}

pub fn line(i: &str) -> IResult<&str, Rule> {
    let (input, rule) = alt((
	rule_line,
	comment_line,
	empty_line,
    ))(i)?;
    Ok((input, rule))
}

pub fn table(i: &str) -> IResult<&str, Vec<Rule>> {
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
        assert_eq!(dots("123"), Ok(("", "123")));
        assert_eq!(dots("1f"), Ok(("", "1f")));
        assert_eq!(
            dots("not valid"),
            Err(Err::Error(Error::new("not valid", ErrorKind::HexDigit)))
        );
    }

    #[test]
    fn largesign_test() {
        assert_eq!(
            largesign("largesign überall 123"),
            Ok(("", Rule::Largesign { word: "überall", dots: "123" })));
        assert_eq!(
            largesign("largesign அஇ 123"),
            Ok(("", Rule::Largesign { word: "அஇ", dots: "123" })));
    }

    #[test]
    fn joinword_test() {
        assert_eq!(
            joinword("joinword haha 123"),
            Ok(("", Rule::Joinword { word: "haha", dots: "123" })));
        assert_eq!(
            joinword("joinword அஇ 123"),
            Ok(("", Rule::Joinword { word: "அஇ", dots: "123" })));
    }

    #[test]
    fn rule_line_test() {
        assert_eq!(
            rule_line("joinword haha 123\n"),
            Ok(("", Rule::Joinword { word: "haha", dots: "123" })));
        assert_eq!(
            rule_line("largesign அஇ 123\n"),
            Ok(("", Rule::Largesign { word: "அஇ", dots: "123" })));
        assert_eq!(
            rule_line("syllable haha 123\n"),
            Ok(("", Rule::Syllable { word: "haha", dots: "123" })));
    }

    #[test]
    fn empty_line_test() {
        assert_eq!(
            empty_line("       \n"),
            Ok(("", Rule::Empty)));
        assert_eq!(
            empty_line("\n"),
            Ok(("", Rule::Empty)));
    }

    #[test]
    fn comment_line_test() {
        assert_eq!(
            comment_line("# haha 1234    \n"),
            Ok(("", Rule::Comment { comment: " haha 1234    "})));
        assert_eq!(
            comment_line("# haha 1234    "),
            Err(Err::Error(Error::new("", ErrorKind::CrLf))));
    }

    #[test]
    fn table_test() {
        assert_eq!(
            table(concat!("       \n",
			  "joinword haha 123\n",
			  "syllable haha 123\n")),
            Ok(("", vec![Rule::Empty,
			 Rule::Joinword { word: "haha", dots: "123" },
			 Rule::Syllable { word: "haha", dots: "123" }])));
        assert_eq!(
            table(concat!("       \n",
			  "# just testing\n",
			  "joinword haha 123\n",
			  "syllable haha 123\n")),
            Ok(("", vec![Rule::Empty,
			 Rule::Comment { comment: " just testing" },
			 Rule::Joinword { word: "haha", dots: "123" },
			 Rule::Syllable { word: "haha", dots: "123" }])));
    }
}
