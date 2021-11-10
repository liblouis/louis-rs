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
pub struct Rule<'a> {
    opcode: &'a str,
    word: &'a str,
    dots: &'a str,
}

pub fn unicode_characters(input: &str) -> IResult<&str, &str> {
    unicode_alpha1(input)
}

pub fn ascii_characters(input: &str) -> IResult<&str, &str> {
    alpha1(input)
}

pub fn dots(input: &str) -> IResult<&str, &str> {
    hex_digit1(input)
}
pub fn opcode(input: &str) -> IResult<&str, &str> {
    alt((tag("always"),
	 tag("word")))(input)
}

pub fn rule(i: &str) -> IResult<&str, Rule> {
    let (input, (opcode, _, word, _, dots, _, _)) = tuple((
        opcode,
        space1,
        ascii_characters,
        space1,
        dots,
        space0,
        newline,
    ))(i)?;
    Ok((input, Rule { opcode, word, dots }))
}

pub fn largesign(i: &str)  -> IResult<&str, Rule> {
    let (input, (opcode, _, word, _, dots, _, _)) = tuple((
        tag("largesign"),
        space1,
        unicode_characters,
        space1,
        dots,
        space0,
        newline,
    ))(i)?;
    Ok((input, Rule { opcode, word, dots }))
}

#[cfg(test)]
mod tests {
    use super::*;
    use nom::error::Error;
    use nom::error::ErrorKind;
    use nom::Err;

    #[test]
    fn character_test() {
        assert_eq!(ascii_characters("hallo"), Ok(("", "hallo")));
        assert_eq!(ascii_characters("haLlo"), Ok(("", "haLlo")));
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
    fn rule_test() {
        assert_eq!(
            rule("always haha 123\n"),
            Ok(("", Rule { opcode: "always", word: "haha", dots: "123" })));
        assert_eq!(
            rule("word haha 123\n"),
            Ok(("", Rule { opcode: "word", word: "haha", dots: "123" })));
        assert_eq!(
            rule("foo haha 123\n"),
            Err(Err::Error(Error{input: "foo haha 123\n", code: ErrorKind::Tag})));
    }

    #[test]
    fn largesign_test() {
        assert_eq!(
            largesign("largesign überall 123\n"),
            Ok(("", Rule { opcode: "largesign", word: "überall", dots: "123" })));
        assert_eq!(
            largesign("largesign அஇ 123\n"),
            Ok(("", Rule { opcode: "largesign", word: "அஇ", dots: "123" })));
    }
}
