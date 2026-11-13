pub mod display;
pub mod multipass;
pub mod primary;

use crate::{
    parser::{AnchoredRule, CharacterClass, CharacterClasses, Rule},
    translator::{CharacterDefinition, CompileError, swap::SwapClasses},
};

#[derive(Debug, Default)]
pub struct TableContext {
    character_definitions: CharacterDefinition,
    character_classes: CharacterClasses,
    dots_classes: CharacterClasses,
    swap_classes: SwapClasses,
}

impl TableContext {
    #[cfg(test)]
    pub fn new(
        character_classes: CharacterClasses,
        dots_classes: CharacterClasses,
        swap_classes: SwapClasses,
    ) -> Self {
        Self {
            character_definitions: CharacterDefinition::default(),
            character_classes,
            dots_classes,
            swap_classes,
        }
    }

    pub fn character_definitions(&self) -> &CharacterDefinition {
        &self.character_definitions
    }

    pub fn character_classes(&self) -> &CharacterClasses {
        &self.character_classes
    }

    pub fn dots_classes(&self) -> &CharacterClasses {
        &self.dots_classes
    }

    pub fn swap_classes(&self) -> &SwapClasses {
        &self.swap_classes
    }

    pub fn compile(rules: &[AnchoredRule]) -> Result<TableContext, CompileError> {
        Self::compile_with_fallback(rules, None)
    }

    /// As [`Self::compile`], for `rules` filtered to one direction, with `fallback` holding the
    /// character definitions of all of the table's rules. A class member this direction leaves
    /// undefined still finds its cell through them, such as a braille pattern that
    /// `braille-patterns.cti` defines with `noback sign`: in liblouis, `noback` and `nofor` keep a
    /// rule from translating in one direction, not a character from being defined.
    pub fn compile_with_fallback(
        rules: &[AnchoredRule],
        fallback: Option<&CharacterDefinition>,
    ) -> Result<TableContext, CompileError> {
        let mut character_definitions = CharacterDefinition::new();
        let mut character_classes = CharacterClasses::default();
        let mut dots_classes = CharacterClasses::default();
        let mut swap_classes = SwapClasses::default();

        // First pass: everything that later rules need to *look up*. These include
        // character definitions (`space`, `letter`, `digit`, …), because `Rule::Base` and
        // the attribute opcodes in the second pass resolve a character to its dots
        // through them, and also the letter dots class, because a boundary check in
        // backward translation asks whether a neighbouring *cell* is a letter and a
        // contraction's cell only qualifies via the rule that produced it.
        for rule in rules {
            match &rule.rule {
                Rule::Space {
                    character, dots, ..
                } => {
                    character_definitions.insert(*character, &dots.to_string());
                    character_classes.insert(CharacterClass::Space, *character);
                    dots_classes.insert_dots(CharacterClass::Space, dots);
                }
                Rule::Punctuation {
                    character, dots, ..
                } => {
                    character_definitions.insert(*character, &dots.to_string());
                    character_classes.insert(CharacterClass::Punctuation, *character);
                    dots_classes.insert_dots(CharacterClass::Punctuation, dots);
                }
                Rule::Digit {
                    character, dots, ..
                } => {
                    character_definitions.insert(*character, &dots.to_string());
                    character_classes.insert(CharacterClass::Digit, *character);
                    dots_classes.insert_dots(CharacterClass::Digit, dots);
                }
                Rule::Litdigit {
                    character, dots, ..
                } => {
                    character_definitions.insert(*character, &dots.to_string());
                    character_classes.insert(CharacterClass::Litdigit, *character);
                    dots_classes.insert_dots(CharacterClass::Litdigit, dots);
                }
                Rule::Letter {
                    character, dots, ..
                } => {
                    character_definitions.insert(*character, &dots.to_string());
                    character_classes.insert(CharacterClass::Letter, *character);
                    dots_classes.insert_dots(CharacterClass::Letter, dots);
                }
                Rule::Lowercase {
                    character, dots, ..
                } => {
                    character_definitions.insert(*character, &dots.to_string());
                    character_classes.insert(CharacterClass::Lowercase, *character);
                    character_classes.insert(CharacterClass::Letter, *character);
                    dots_classes.insert_dots(CharacterClass::Lowercase, dots);
                    dots_classes.insert_dots(CharacterClass::Letter, dots);
                }
                Rule::Uppercase {
                    character, dots, ..
                } => {
                    character_definitions.insert(*character, &dots.to_string());
                    character_classes.insert(CharacterClass::Uppercase, *character);
                    character_classes.insert(CharacterClass::Letter, *character);
                    dots_classes.insert_dots(CharacterClass::Uppercase, dots);
                    dots_classes.insert_dots(CharacterClass::Letter, dots);
                }
                Rule::Sign {
                    character, dots, ..
                } => {
                    character_definitions.insert(*character, &dots.to_string());
                    character_classes.insert(CharacterClass::Sign, *character);
                    dots_classes.insert_dots(CharacterClass::Sign, dots);
                }
                Rule::Math {
                    character, dots, ..
                } => {
                    character_definitions.insert(*character, &dots.to_string());
                    character_classes.insert(CharacterClass::Math, *character);
                    dots_classes.insert_dots(CharacterClass::Math, dots);
                }
                Rule::Hyphen {
                    character, dots, ..
                } => {
                    character_definitions.insert(*character, &dots.to_string());
                    // FIXME: In what character class is the hyphen (if any)?
                    character_classes.insert(CharacterClass::Punctuation, *character);
                    dots_classes.insert_dots(CharacterClass::Punctuation, dots);
                }
                Rule::Decpoint { character, dots } => {
                    character_definitions.insert(*character, &dots.to_string());
                    // FIXME: In what character class is the decimal point (if any)?
                    character_classes.insert(CharacterClass::Punctuation, *character);
                    dots_classes.insert_dots(CharacterClass::Punctuation, dots);
                }
                _ => (),
            }
        }
        // go through the rules again with all the character_definitions sorted out
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
                        // liblouis also gives a single-cell base's dots the derived
                        // class, so with `base uppercase A a` a pass rule's `$U` matches
                        // the cell of "a".
                        dots_classes.insert_associated_dot(
                            CharacterClass::from(name.as_str()),
                            *derived,
                            &character_definitions,
                        );
                        // Base-derived uppercase/lowercase characters are letters too,
                        // matching what the `uppercase`/`lowercase` opcodes do directly.
                        if name == "uppercase" || name == "lowercase" {
                            character_classes.insert(CharacterClass::Letter, *derived);
                            dots_classes.insert_associated_dot(
                                CharacterClass::Letter,
                                *derived,
                                &character_definitions,
                            );
                        }
                    }
                    // no definition of the base character for this direction (`noback`, `nofor`), so
                    // nothing to derive; `TranslationPipeline::compile` rejects one with none at all
                }
                Rule::Attribute { name, chars } => {
                    let class = CharacterClass::from(name.as_str());
                    for c in chars.chars() {
                        character_classes.insert(class.clone(), c);
                        let definitions = match fallback {
                            Some(fallback) if character_definitions.get(&c).is_none() => fallback,
                            _ => &character_definitions,
                        };
                        dots_classes.insert_associated_dot(class.clone(), c, definitions);
                    }
                }
                Rule::Seqdelimiter { chars } => {
                    for c in chars.chars() {
                        character_classes.insert(CharacterClass::Seqdelimiter, c);
                        dots_classes.insert_associated_dot(
                            CharacterClass::Seqdelimiter,
                            c,
                            &character_definitions,
                        );
                    }
                }
                Rule::Seqbeforechars { chars } => {
                    for c in chars.chars() {
                        character_classes.insert(CharacterClass::Seqbeforechars, c);
                        dots_classes.insert_associated_dot(
                            CharacterClass::Seqbeforechars,
                            c,
                            &character_definitions,
                        );
                    }
                }
                Rule::Seqafterchars { chars } => {
                    for c in chars.chars() {
                        character_classes.insert(CharacterClass::Seqafterchars, c);
                        dots_classes.insert_associated_dot(
                            CharacterClass::Seqafterchars,
                            c,
                            &character_definitions,
                        );
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
                    // swapdd works on cells, and the passes match %name against the dots classes
                    for c in dots.to_string().chars() {
                        dots_classes.insert(class.clone(), c);
                    }
                    let replacements: Vec<String> =
                        replacement.iter().map(|b| b.to_string()).collect();
                    let mapping: Vec<(char, &str)> = dots
                        .iter()
                        .map(|b| b.to_unicode())
                        .zip(replacements.iter().map(|s| s.as_str()))
                        .collect();
                    swap_classes.insert(name, &mapping);
                }
                _ => (),
            }
        }
        Ok(TableContext {
            character_definitions,
            character_classes,
            dots_classes,
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
            Some(HashSet::from(['a', 'b', 'c', 'A', 'B']))
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

    #[test]
    fn base_gives_its_class_to_the_base_dots() {
        let rules = [
            parse_rule("lowercase a 1"),
            parse_rule("lowercase ύ 5-13456"),
            parse_rule("base uppercase A a"),
            parse_rule("base uppercase Ύ ύ"),
        ];
        let context = TableContext::compile(&rules).unwrap();
        // a two-cell base has no single cell to carry the class, as in liblouis
        assert_eq!(
            context.dots_classes.get(&CharacterClass::Uppercase),
            Some(HashSet::from(['⠁']))
        );
    }

    #[test]
    fn dots_classes() {
        let rules = [
            parse_rule("lowercase o 135"),
            parse_rule("lowercase ύ 5-13456"),
            parse_rule("sign ΄ 5"),
            parse_rule("attribute accent ΄"),
            parse_rule("pass2 @135[%accent]@13456 *@136"),
        ];
        let context = TableContext::compile(&rules).unwrap();

        assert_eq!(
            context.dots_classes.get(&CharacterClass::Letter),
            Some(HashSet::from(['⠕']))
        );
        assert_eq!(
            context.dots_classes.get(&CharacterClass::Lowercase),
            Some(HashSet::from(['⠕']))
        );
        assert_eq!(
            context.dots_classes.get(&CharacterClass::Sign),
            Some(HashSet::from(['⠐']))
        );
        assert_eq!(context.dots_classes.get(&CharacterClass::Uppercase), None);
        assert_eq!(context.dots_classes.get(&CharacterClass::Space), None);
        assert_eq!(context.dots_classes.get(&CharacterClass::Litdigit), None);
        assert_eq!(
            context.dots_classes.get(&CharacterClass::Seqbeforechars),
            None
        );
        assert_eq!(
            context
                .dots_classes
                .get(&CharacterClass::UserDefined("accent".to_string())),
            Some(HashSet::from(['⠐']))
        );
    }
}
