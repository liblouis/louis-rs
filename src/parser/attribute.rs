use crate::parser::CharacterClass;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Attribute {
    Class(CharacterClass),
    ByOrder(u8),
    Boundary,
    Any,
}

pub struct MatchAttribute(Attribute);
pub struct ContextAttribute(Attribute);

impl std::fmt::Display for ContextAttribute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.0 {
            Attribute::Class(class) => match class {
                CharacterClass::Space => write!(f, "s"),
                CharacterClass::Digit => write!(f, "d"),
                CharacterClass::Litdigit => write!(f, "D"),
                CharacterClass::Letter => write!(f, "l"),
                CharacterClass::Uppercase => write!(f, "U"),
                CharacterClass::Lowercase => write!(f, "u"),
                CharacterClass::Punctuation => write!(f, "p"),
                CharacterClass::Sign => write!(f, "S"),
                CharacterClass::Math => write!(f, "m"),
                CharacterClass::UserDefined(name) => write!(f, "{}", name),
                CharacterClass::Seqdelimiter
                | CharacterClass::Seqbeforechars
                | CharacterClass::Seqafterchars => unreachable!(),
            },
            Attribute::ByOrder(order) => match order {
                1 => write!(f, "w"),
                2 => write!(f, "x"),
                3 => write!(f, "y"),
                4 => write!(f, "z"),
                _ => todo!(),
            },
            Attribute::Any => write!(f, "a"),
            Attribute::Boundary => unreachable!(),
        }
    }
}
