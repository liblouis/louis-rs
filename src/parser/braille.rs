use enumset::{EnumSet, EnumSetType, enum_set};

#[derive(thiserror::Error, Debug, PartialEq)]
pub enum ParseError {
    #[error("Invalid braille {character:?}")]
    InvalidBraille { character: Option<char> },
}

#[derive(EnumSetType, Debug)]
pub enum BrailleDot {
    Dot0,
    Dot1,
    Dot2,
    Dot3,
    Dot4,
    Dot5,
    Dot6,
    Dot7,
    Dot8,
    Dot9,
    DotA,
    DotB,
    DotC,
    DotD,
    DotE,
    DotF,
}

impl BrailleDot {
    fn to_hex(self) -> u32 {
        match self {
            BrailleDot::Dot0 => 0x0000,
            BrailleDot::Dot1 => 0x0001,
            BrailleDot::Dot2 => 0x0002,
            BrailleDot::Dot3 => 0x0004,
            BrailleDot::Dot4 => 0x0008,
            BrailleDot::Dot5 => 0x0010,
            BrailleDot::Dot6 => 0x0020,
            BrailleDot::Dot7 => 0x0040,
            BrailleDot::Dot8 => 0x0080,
            BrailleDot::Dot9 => 0x0100,
            BrailleDot::DotA => 0x0200,
            BrailleDot::DotB => 0x0400,
            BrailleDot::DotC => 0x0800,
            BrailleDot::DotD => 0x1000,
            BrailleDot::DotE => 0x2000,
            BrailleDot::DotF => 0x4000,
        }
    }

    /// The character liblouis uses for this dot when it spells dots out, or `None` for
    /// [`BrailleDot::Dot0`], which stands for no dot at all.
    fn to_char(self) -> Option<char> {
        match self {
            BrailleDot::Dot0 => None,
            BrailleDot::Dot1 => Some('1'),
            BrailleDot::Dot2 => Some('2'),
            BrailleDot::Dot3 => Some('3'),
            BrailleDot::Dot4 => Some('4'),
            BrailleDot::Dot5 => Some('5'),
            BrailleDot::Dot6 => Some('6'),
            BrailleDot::Dot7 => Some('7'),
            BrailleDot::Dot8 => Some('8'),
            BrailleDot::Dot9 => Some('9'),
            BrailleDot::DotA => Some('A'),
            BrailleDot::DotB => Some('B'),
            BrailleDot::DotC => Some('C'),
            BrailleDot::DotD => Some('D'),
            BrailleDot::DotE => Some('E'),
            BrailleDot::DotF => Some('F'),
        }
    }
}

impl TryFrom<char> for BrailleDot {
    type Error = ParseError;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            '0' => Ok(BrailleDot::Dot0),
            '1' => Ok(BrailleDot::Dot1),
            '2' => Ok(BrailleDot::Dot2),
            '3' => Ok(BrailleDot::Dot3),
            '4' => Ok(BrailleDot::Dot4),
            '5' => Ok(BrailleDot::Dot5),
            '6' => Ok(BrailleDot::Dot6),
            '7' => Ok(BrailleDot::Dot7),
            '8' => Ok(BrailleDot::Dot8),
            '9' => Ok(BrailleDot::Dot9),
            'a' => Ok(BrailleDot::DotA),
            'b' => Ok(BrailleDot::DotB),
            'c' => Ok(BrailleDot::DotC),
            'd' => Ok(BrailleDot::DotD),
            'e' => Ok(BrailleDot::DotE),
            'f' => Ok(BrailleDot::DotF),
            invalid => Err(ParseError::InvalidBraille {
                character: Some(invalid),
            }),
        }
    }
}

/// Where cells without virtual dots live: the Unicode braille patterns block
const BRAILLE_PATTERNS: u32 = 0x2800;

/// Where cells with virtual dots live: Unicode Supplementary Private Use Area-A
const VIRTUAL_DOTS: u32 = 0xF0000;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BrailleChar(EnumSet<BrailleDot>);

impl From<EnumSet<BrailleDot>> for BrailleChar {
    fn from(value: EnumSet<BrailleDot>) -> Self {
        BrailleChar(value)
    }
}

impl BrailleChar {
    fn has_virtual_dots(&self) -> bool {
        let virtual_dots = enum_set!(
            BrailleDot::Dot9 |
            BrailleDot::DotA |
            BrailleDot::DotB |
            BrailleDot::DotC |
                BrailleDot::DotD |
                BrailleDot::DotE |
                BrailleDot::DotF |
        );
        !virtual_dots.intersection(self.0).is_empty()
    }

    pub fn to_unicode(&self) -> char {
        let unicode_plane = if self.has_virtual_dots() {
            VIRTUAL_DOTS
        } else {
            BRAILLE_PATTERNS
        };
        let unicode = self
            .0
            .iter()
            .map(|dot| dot.to_hex())
            .fold(unicode_plane, |acc, x| acc | x);
        char::from_u32(unicode).unwrap()
    }

    /// The inverse of [`BrailleChar::to_unicode`]. Returns `None` for a character that
    /// `to_unicode` never produces.
    pub fn from_unicode(c: char) -> Option<Self> {
        let bits = match c as u32 {
            c @ BRAILLE_PATTERNS..=0x28FF => c - BRAILLE_PATTERNS,
            // a cell lands here only if it has a virtual dot, i.e. bit 0x100 or above
            c @ 0xF0100..=0xF7FFF => c - VIRTUAL_DOTS,
            _ => return None,
        };
        let cell: BrailleChar = EnumSet::<BrailleDot>::all()
            .iter()
            .filter(|dot| bits & dot.to_hex() != 0)
            .collect();
        Some(cell)
    }

    /// The dot numbers of this cell the way liblouis spells them out, e.g. `1258`, with
    /// `A` to `F` for the virtual dots and `0` for a cell with no dots
    pub fn dot_numbers(&self) -> String {
        let dots: String = self.0.iter().filter_map(BrailleDot::to_char).collect();
        if dots.is_empty() { "0".into() } else { dots }
    }
}

impl std::fmt::Display for BrailleChar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_unicode())
    }
}

impl TryFrom<&str> for BrailleChar {
    type Error = ParseError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        if value.is_empty() {
            Err(ParseError::InvalidBraille { character: None })
        } else {
            value.chars().map(BrailleDot::try_from).collect()
        }
    }
}

impl FromIterator<BrailleDot> for BrailleChar {
    fn from_iter<T: IntoIterator<Item = BrailleDot>>(iter: T) -> Self {
        BrailleChar(EnumSet::from_iter(iter))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BrailleChars(Vec<BrailleChar>);

impl BrailleChars {
    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn iter(&self) -> std::slice::Iter<'_, BrailleChar> {
        self.0.iter()
    }
}

impl From<Vec<BrailleChar>> for BrailleChars {
    fn from(value: Vec<BrailleChar>) -> Self {
        BrailleChars(value)
    }
}

impl TryFrom<&str> for BrailleChars {
    type Error = ParseError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        value.split('-').map(BrailleChar::try_from).collect()
    }
}

impl std::fmt::Display for BrailleChars {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.0.iter().map(|b| b.to_unicode()).collect::<String>()
        )
    }
}

impl FromIterator<BrailleChar> for BrailleChars {
    fn from_iter<T: IntoIterator<Item = BrailleChar>>(iter: T) -> Self {
        BrailleChars(iter.into_iter().collect())
    }
}

pub fn is_braille_dot(c: char) -> bool {
    matches!(c, '0'..='9' | 'a'..='f')
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_chars_to_dots() {
        assert_eq!(
            BrailleChar::try_from("123"),
            Ok(BrailleChar(enum_set!(
                BrailleDot::Dot1 | BrailleDot::Dot2 | BrailleDot::Dot3
            )))
        );
        assert_eq!(
            BrailleChar::try_from("1a"),
            Ok(BrailleChar(enum_set!(BrailleDot::Dot1 | BrailleDot::DotA)))
        );
        assert_eq!(
            BrailleChar::try_from("a"),
            Ok(BrailleChar(enum_set!(BrailleDot::DotA)))
        );
        assert_eq!(
            BrailleChar::try_from("z"),
            Err(ParseError::InvalidBraille {
                character: Some('z')
            })
        );
    }

    #[test]
    fn test_braille_chars() {
        assert_eq!(
            BrailleChars::try_from("1-1"),
            Ok(BrailleChars(vec![
                BrailleChar(enum_set!(BrailleDot::Dot1)),
                BrailleChar(enum_set!(BrailleDot::Dot1))
            ]))
        );
        assert_eq!(
            BrailleChars::try_from("1-"),
            Err(ParseError::InvalidBraille { character: None })
        );
        assert_eq!(
            BrailleChars::try_from("-1"),
            Err(ParseError::InvalidBraille { character: None })
        );
        assert_eq!(
            BrailleChars::try_from("-"),
            Err(ParseError::InvalidBraille { character: None })
        );
        assert_eq!(
            BrailleChars::try_from(""),
            Err(ParseError::InvalidBraille { character: None })
        );
    }

    #[test]
    fn test_dots_to_unicode() {
        assert_eq!(
            BrailleChars(vec![BrailleChar(enum_set!(
                BrailleDot::Dot1 | BrailleDot::Dot8
            ))])
            .to_string(),
            "⢁".to_string()
        );
        assert_eq!(
            BrailleChars(vec![BrailleChar(enum_set!(
                BrailleDot::Dot1 | BrailleDot::Dot9
            ))])
            .to_string(),
            "\u{f0101}".to_string()
        );
    }

    #[test]
    fn test_unicode_round_trip() {
        for dots in ["1", "12345678", "19", "2a", "f"] {
            let cell = BrailleChar::try_from(dots).unwrap();
            assert_eq!(BrailleChar::from_unicode(cell.to_unicode()), Some(cell));
        }
        // dot 1 alone is U+2801, never U+F0001: `to_unicode` uses the private use area
        // only for cells with a virtual dot, so everything below U+F0100 is not a cell
        assert_eq!(BrailleChar::from_unicode('\u{f0001}'), None);
        // outside both the braille patterns block and the private use area
        assert_eq!(BrailleChar::from_unicode('x'), None);
    }

    #[test]
    fn test_dot_numbers() {
        assert_eq!(BrailleChar::try_from("1258").unwrap().dot_numbers(), "1258");
        assert_eq!(BrailleChar::try_from("19af").unwrap().dot_numbers(), "19AF");
        assert_eq!(BrailleChar::try_from("0").unwrap().dot_numbers(), "0");
    }
}
