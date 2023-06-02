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
	// apply the corrections
	// apply the translations
	// finally use the character definitions for the remaining text
	input.chars().map(|c| self.char_to_braille(c)).collect()
    }

    fn char_to_braille(&self, c: char) -> String {
	match self.character_definitions.get(&c) {
	    Some(replacement) => replacement.to_string(),
	    None => self.undefined.to_string(),
	}
    }
    fn apply_translations<'a>(&self, input: &'a str) -> (&'a str, String) {
	match self.longest_matching_translation(input) {
	    Some((k,v)) => (input.split_at(k.len()).1, v.to_string()),
	    None => (input, "".to_string())
	}
    }

    // This is a very naive implementation. It basically goes through
    // all translation opcodes and checks if the match the given
    // input. This is O(m*n) with m translation opcodes and n the
    // average length of an opcode. A much better implementation will
    // be using a [trie](https://en.wikipedia.org/wiki/Trie), such as
    // the one provided in the [fst crate](https://crates.io/crates/fst)
    fn longest_matching_translation(&self, input: &str) -> Option<(&String, &String)> {
	self.translations
	    .iter()
	    .filter(|(k,_)| input.starts_with(&**k))
	    .max_by(|a,b| a.0.chars().count().cmp(&b.0.chars().count()))
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

    fn corrections(self, corrections: Corrections) -> TranslationTable {
        Self {corrections, ..self}
    }
    fn character_definitions(self, defs: CharacterDefinitions) -> TranslationTable {
        Self {character_definitions: defs, ..self}
    }

    fn translations(self, trans: Translations) -> TranslationTable {
        Self {translations: trans, ..self}
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;

    #[test]
    fn longest_matching_translation_test() {
	let translations = HashMap::from([("haha".to_string(), "".to_string()),
					  ("ha".to_string(), "".to_string()),
					  ("hahaha".to_string(), "".to_string()),
					  ("hahahi".to_string(), "".to_string())]);
	let table = TranslationTable::new().translations(translations);
	let ignore = String::new();
        assert_eq!(table.longest_matching_translation("haha"), Some((&"haha".to_string(), &ignore)));
        assert_eq!(table.longest_matching_translation("hahaha"), Some((&"hahaha".to_string(), &ignore)));
        assert_eq!(table.longest_matching_translation("hahaho"), Some((&"haha".to_string(), &ignore)));
    }

    #[test]
    fn apply_translation_test() {
	let translations = HashMap::from([("haha".to_string(), "HA".to_string()),
					  ("ha".to_string(), "H".to_string()),
					  ("hahaha".to_string(), "HAA".to_string()),
					  ("hahahi".to_string(), "HAI".to_string())]);
	let table = TranslationTable::new().translations(translations);
        assert_eq!(table.apply_translations("haha"), (&""[..], "HA".to_string()));
        assert_eq!(table.apply_translations("hahaha"), (&""[..], "HAA".to_string()));
        assert_eq!(table.apply_translations("hahaho"), (&"ho"[..], "HA".to_string()));
    }


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
