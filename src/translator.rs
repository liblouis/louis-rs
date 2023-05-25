use std::collections::HashMap;

const UNDEFINED: &str = "⣿";

type Corrections = HashMap<String, String>;
type CharacterDefinitions = HashMap<char, String>;
type Translations = HashMap<String, String>;

#[derive(Debug)]
pub struct TranslationTable {
    undefined: String,
    corrections: Corrections,
    character_definitions: CharacterDefinitions,
    translations: Translations,
}

impl TranslationTable {
    fn translate(&self, input: &str) -> String {
	input.chars().map(|c| self.char_to_braille(c)).collect()
    }

    fn char_to_braille(&self, c: char) -> String {
	match self.character_definitions.get(&c) {
	    Some(replacement) => replacement.to_string(),
	    None => self.undefined.to_string(),
	}
    }
}

// implementing the builder pattern, see https://doc.rust-lang.org/1.0.0/style/ownership/builders.html
impl TranslationTable {
    fn new() -> Self {
        Self { undefined: UNDEFINED.to_string(),
               corrections: HashMap::new(),
               character_definitions: HashMap::new(),
	       translations: HashMap::new(),
	}
    }

    fn character_definitions(self, defs: CharacterDefinitions) -> TranslationTable {
        Self {character_definitions: defs, ..self}
    }

    fn corrections(self, corrections: Corrections) -> TranslationTable {
        Self {corrections, ..self}
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;


    #[test]
    fn translate_test() {
	let char_defs = HashMap::from([('a', "⠁".to_string()), ('b', "⠂".to_string())]);
	let table = TranslationTable::new().character_definitions(char_defs);
        assert_eq!(table.translate("ab"), "⠁⠂");
    }

    #[test]
    fn undefined_test() {
	let table = TranslationTable::new();
        assert_eq!(table.translate("x"), UNDEFINED);
    }
}
