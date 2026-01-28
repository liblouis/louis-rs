pub mod multipass;
pub mod primary;

use crate::{
    parser::{AnchoredRule, CharacterClass, CharacterClasses, Rule, dot_to_unicode},
    translator::{CharacterDefinition, TranslationError, swap::SwapClasses},
};

#[derive(Debug, Default)]
pub struct TableContext {
    character_definitions: CharacterDefinition,
    character_classes: CharacterClasses,
    swap_classes: SwapClasses,
}

impl TableContext {
    // used for testing
    pub fn new(character_classes: CharacterClasses, swap_classes: SwapClasses) -> Self {
        Self {
            character_definitions: CharacterDefinition::default(),
            character_classes,
            swap_classes,
        }
    }

    pub fn character_definitions(&self) -> &CharacterDefinition {
        &self.character_definitions
    }

    pub fn character_classes(&self) -> &CharacterClasses {
        &self.character_classes
    }

    pub fn swap_classes(&self) -> &SwapClasses {
        &self.swap_classes
    }

    pub fn compile(rules: &[AnchoredRule]) -> Result<TableContext, TranslationError> {
        let mut character_definitions = CharacterDefinition::new();
        let mut character_classes = CharacterClasses::default();
        let mut swap_classes = SwapClasses::default();
        for rule in rules {
            match &rule.rule {
                Rule::Space {
                    character, dots, ..
                } => {
                    character_definitions.insert(*character, &dots.to_string());
                    character_classes.insert(CharacterClass::Space, *character);
                }
                Rule::Punctuation {
                    character, dots, ..
                } => {
                    character_definitions.insert(*character, &dots.to_string());
                    character_classes.insert(CharacterClass::Punctuation, *character);
                }
                Rule::Digit {
                    character, dots, ..
                } => {
                    character_definitions.insert(*character, &dots.to_string());
                    character_classes.insert(CharacterClass::Digit, *character);
                }
                Rule::Litdigit {
                    character, dots, ..
                } => {
                    character_definitions.insert(*character, &dots.to_string());
                    character_classes.insert(CharacterClass::Litdigit, *character);
                }
                Rule::Letter {
                    character, dots, ..
                } => {
                    character_definitions.insert(*character, &dots.to_string());
                    character_classes.insert(CharacterClass::Letter, *character);
                }
                Rule::Lowercase {
                    character, dots, ..
                } => {
                    character_definitions.insert(*character, &dots.to_string());
                    character_classes.insert(CharacterClass::Lowercase, *character);
                    character_classes.insert(CharacterClass::Letter, *character);
                }
                Rule::Uppercase {
                    character, dots, ..
                } => {
                    character_definitions.insert(*character, &dots.to_string());
                    character_classes.insert(CharacterClass::Uppercase, *character);
                    character_classes.insert(CharacterClass::Letter, *character);
                }
                Rule::Sign {
                    character, dots, ..
                } => {
                    character_definitions.insert(*character, &dots.to_string());
                    character_classes.insert(CharacterClass::Sign, *character);
                }
                Rule::Math {
                    character, dots, ..
                } => {
                    character_definitions.insert(*character, &dots.to_string());
                    character_classes.insert(CharacterClass::Math, *character);
                }
                Rule::Attribute { name, chars } => {
                    let class = CharacterClass::from(name.as_str());
                    for c in chars.chars() {
                        character_classes.insert(class.clone(), c);
                    }
                }
                Rule::Seqdelimiter { chars } => {
                    for c in chars.chars() {
                        character_classes.insert(CharacterClass::Seqdelimiter, c);
                    }
                }
                Rule::Seqbeforechars { chars } => {
                    for c in chars.chars() {
                        character_classes.insert(CharacterClass::Seqbeforechars, c);
                    }
                }
                Rule::Seqafterchars { chars } => {
                    for c in chars.chars() {
                        character_classes.insert(CharacterClass::Seqafterchars, c);
                    }
                }
                Rule::Swapcc {
                    name,
                    chars,
                    replacement,
                } => {
                    let class = CharacterClass::from(name.as_str());
                    for c in chars.chars() {
                        character_classes.insert(class.clone(), c);
                    }
                    let replacements: Vec<String> =
                        replacement.chars().map(|c| c.to_string()).collect();
                    let mapping: Vec<(char, &str)> = chars
                        .chars()
                        .zip(replacements.iter().map(|s| s.as_str()))
                        .collect();
                    swap_classes.insert(name, &mapping);
                }
                Rule::Swapcd { name, chars, dots } => {
                    let class = CharacterClass::from(name.as_str());
                    for c in chars.chars() {
                        character_classes.insert(class.clone(), c);
                    }
                    let replacements: Vec<String> = dots.iter().map(|b| b.to_string()).collect();
                    let mapping: Vec<(char, &str)> = chars
                        .chars()
                        .zip(replacements.iter().map(|s| s.as_str()))
                        .collect();
                    swap_classes.insert(name, &mapping);
                }
                Rule::Swapdd {
                    name,
                    dots,
                    replacement,
                } => {
                    let class = CharacterClass::from(name.as_str());
                    for c in dots.to_string().chars() {
                        character_classes.insert(class.clone(), c);
                    }
                    let replacements: Vec<String> =
                        replacement.iter().map(|b| b.to_string()).collect();
                    let mapping: Vec<(char, &str)> = dots
                        .iter()
                        .map(dot_to_unicode)
                        .zip(replacements.iter().map(|s| s.as_str()))
                        .collect();
                    swap_classes.insert(name, &mapping);
                }
                _ => (),
            }
        }
        for rule in rules {
            match &rule.rule {
                Rule::Base {
                    derived,
                    base,
                    name,
                } => {
                    if let Some(translation) = character_definitions.get(base).cloned() {
                        character_definitions.insert(*derived, &translation);
                        character_classes.insert(CharacterClass::from(name.as_str()), *derived);
                    } else {
                        // hm, there is no character definition for the base character.
                        // If we are backwards compatible ignore the problem, otherwise
                        // throw an error
                        if !cfg!(feature = "backwards_compatibility") {
                            return Err(TranslationError::BaseCharacterNotDefined {
                                base: *base,
                                derived: *derived,
                            });
                        }
                    }
                }
                _ => (),
            }
        }
        Ok(TableContext {
            character_definitions,
            character_classes,
            swap_classes,
        })
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use super::*;

    use crate::parser::RuleParser;

    fn parse_rule(source: &str) -> AnchoredRule {
        RuleParser::new(source).rule().unwrap().into()
    }

    #[test]
    fn character_classes() {
        let rules = [
            parse_rule("lowercase a 1"),
            parse_rule("lowercase b 12"),
            parse_rule("lowercase c 14"),
            parse_rule("base uppercase A a"),
            parse_rule("base uppercase B b"),
            parse_rule("space \\s 0"),
            parse_rule("litdigit 1 1"),
            parse_rule("attribute foo abc"),
            parse_rule("seqbeforechars xyz"),
        ];
        let context = TableContext::compile(&rules).unwrap();

        assert_eq!(
            context.character_classes.get(&CharacterClass::Letter),
            Some(HashSet::from(['a', 'b', 'c']))
        );
        assert_eq!(
            context.character_classes.get(&CharacterClass::Lowercase),
            Some(HashSet::from(['a', 'b', 'c']))
        );
        assert_eq!(
            context.character_classes.get(&CharacterClass::Uppercase),
            Some(HashSet::from(['A', 'B']))
        );
        assert_eq!(
            context.character_classes.get(&CharacterClass::Space),
            Some(HashSet::from([' ']))
        );
        assert_eq!(
            context.character_classes.get(&CharacterClass::Litdigit),
            Some(HashSet::from(['1']))
        );
        assert_eq!(
            context
                .character_classes
                .get(&CharacterClass::Seqbeforechars),
            Some(HashSet::from(['x', 'y', 'z']))
        );
        assert_eq!(
            context
                .character_classes
                .get(&CharacterClass::UserDefined("foo".to_string())),
            Some(HashSet::from(['a', 'b', 'c']))
        );
    }
}
