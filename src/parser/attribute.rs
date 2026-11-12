use crate::parser::CharacterClass;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Attribute {
    Class(CharacterClass),
    ByOrder(u8),
    Boundary,
    Any,
}

/// An [`Attribute`] as written in a `context` or multipass rule, e.g. `s` for space in `$s`
pub struct ContextAttribute<'a>(pub &'a Attribute);

impl std::fmt::Display for ContextAttribute<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
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
                CharacterClass::Seqdelimiter => write!(f, "~"),
                CharacterClass::Seqbeforechars => write!(f, "<"),
                CharacterClass::Seqafterchars => write!(f, ">"),
                CharacterClass::UserDefined(name) => write!(f, "{}", name),
            },
            Attribute::ByOrder(order) => match order {
                1 => write!(f, "w"),
                2 => write!(f, "x"),
                3 => write!(f, "y"),
                4 => write!(f, "z"),
                _ => todo!(),
            },
            Attribute::Any => write!(f, "a"),
            Attribute::Boundary => write!(f, "^"),
        }
    }
}

/// An [`Attribute`] as written in a `match` pattern, e.g. `_` for space in `%[_#]`
pub struct MatchAttribute<'a>(pub &'a Attribute);

impl std::fmt::Display for MatchAttribute<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            Attribute::Class(class) => match class {
                CharacterClass::Space => write!(f, "_"),
                CharacterClass::Digit => write!(f, "#"),
                CharacterClass::Letter => write!(f, "a"),
                CharacterClass::Uppercase => write!(f, "u"),
                CharacterClass::Lowercase => write!(f, "l"),
                CharacterClass::Punctuation => write!(f, "."),
                CharacterClass::Sign => write!(f, "$"),
                CharacterClass::Seqdelimiter => write!(f, "~"),
                CharacterClass::Seqbeforechars => write!(f, "<"),
                CharacterClass::Seqafterchars => write!(f, ">"),
                // the user-defined classes 0 to 7
                CharacterClass::UserDefined(name) => write!(f, "{}", name),
                // match patterns have no syntax for these
                CharacterClass::Litdigit | CharacterClass::Math => unreachable!(),
            },
            Attribute::Boundary => write!(f, "^"),
            Attribute::ByOrder(_) | Attribute::Any => unreachable!(),
        }
    }
}
