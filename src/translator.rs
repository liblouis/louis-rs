use trie::Trie;

use crate::parser::{dots_to_unicode, Braille, Rule};

mod trie;

#[derive(Debug, PartialEq, Clone)]
pub struct Translation {
    from: String,
    to: String,
}

/// Mapping of an input char to the translated output
#[derive(Debug, PartialEq)]
pub struct TranslationMapping<'a> {
    input: &'a str,
    output: &'a str,
}

impl<'a> TranslationMapping<'a> {
    fn new(input: &'a str, output: &'a str) -> TranslationMapping<'a> {
        TranslationMapping { input, output }
    }
}

#[derive(Debug)]
pub struct TranslationTable {
    undefined: Translation,
    trie: Trie,
}

impl TranslationTable {
    pub fn compile(rules: &Vec<Rule>) -> Self {
        let mut undefined = Translation {
            from: "".into(),
            to: "_".into(),
        };
        let mut trie = Trie::new();

        for rule in rules {
            match rule {
                Rule::Undefined { dots } => {
                    undefined = Translation {
                        from: "".into(),
                        to: dots_to_unicode(dots),
                    };
                }
                Rule::Space {
                    character, dots, ..
                }
                | Rule::Punctuation {
                    character, dots, ..
                }
                | Rule::Digit {
                    character, dots, ..
                }
                | Rule::Letter {
                    character,
                    dots: Braille::Explicit(dots),
                    ..
                }
                | Rule::Lowercase {
                    character, dots, ..
                }
                | Rule::Uppercase {
                    character, dots, ..
                }
                | Rule::Litdigit {
                    character, dots, ..
                }
                | Rule::Sign {
                    character, dots, ..
                }
                | Rule::Math {
                    character, dots, ..
                } => trie.insert(
                    &character.to_string(),
                    Translation {
                        from: character.to_string(),
                        to: dots_to_unicode(dots),
                    },
                ),
                Rule::Comp6 {
                    chars,
                    dots: Braille::Explicit(dots),
                    ..
                }
                | Rule::Always {
                    chars,
                    dots: Braille::Explicit(dots),
                    ..
                }
                | Rule::Word {
                    chars,
                    dots: Braille::Explicit(dots),
                    ..
                }
                | Rule::Begword {
                    chars,
                    dots: Braille::Explicit(dots),
                    ..
                }
                | Rule::Endword {
                    chars,
                    dots: Braille::Explicit(dots),
                    ..
                } => trie.insert(
                    chars,
                    Translation {
                        from: chars.into(),
                        to: dots_to_unicode(dots),
                    },
                ),
                _ => (),
            }
        }
        TranslationTable { undefined, trie }
    }

    pub fn translate(&self, input: &str) -> String {
        let mut translations: Vec<Translation> = Vec::new();
        let mut current = input;
        loop {
            let candidates = self.trie.find_translations(current);
            let translation = if let Some(longest) = candidates.last() {
                *longest
            } else {
                match current.chars().next() {
                    Some(c) => Translation {
                        from: c.to_string(),
                        to: self.undefined.to,
                    },
                    _ => {
                        break;
                    }
                }
            };
            translations.push(translation.clone());
            current = match current.strip_prefix(&translation.from) {
                Some(i) => i,
                None => {
                    break;
                }
            };
        }
        translations.into_iter().map(|t| t.to).collect()
    }
}
