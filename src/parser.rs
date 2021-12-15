use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::alpha1;
use nom::character::complete::hex_digit1;
use nom::character::complete::newline;
use nom::character::complete::space0;
use nom::character::complete::space1;
use nom::sequence::tuple;
use nom::IResult;
use nom_unicode::complete::alpha1 as unicode_alpha1;

#[derive(PartialEq, Debug)]
pub enum Rule<'a> {
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

pub fn line(i: &str) -> IResult<&str, Rule> {
    let (input, (rule, _, _)) = tuple((
        alt((largesign, joinword, syllable)), space0, newline,
    ))(i)?;
    Ok((input, rule))
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
    fn line_test() {
        assert_eq!(
            line("joinword haha 123\n"),
            Ok(("", Rule::Joinword { word: "haha", dots: "123" })));
        assert_eq!(
            line("largesign அஇ 123\n"),
            Ok(("", Rule::Largesign { word: "அஇ", dots: "123" })));
        assert_eq!(
            line("syllable haha 123\n"),
            Ok(("", Rule::Syllable { word: "haha", dots: "123" })));
    }
}
