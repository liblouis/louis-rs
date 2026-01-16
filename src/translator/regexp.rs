//! Regular expression engine with virtual machine-based matching.
//!
//! This module implements a regular expression compiler and virtual machine
//! executor based on the approach described in [Regular Expression Matching:
//! the Virtual Machine Approach](https://swtch.com/~rsc/regexp/regexp2.html) by
//! Russ Cox.
//!
//! The engine compiles regular expression ASTs into a sequence of virtual
//! machine instructions that can be executed to match patterns against input
//! strings. This approach provides efficient matching with linear time
//! complexity and support for features like character classes, quantifiers, and
//! captures.

use std::collections::HashSet;

use crate::translator::{
    ResolvedTranslation, TranslationStage,
    translation::{Resolve, Translation},
};

/// Abstract syntax tree representation of regular expressions
#[derive(Debug)]
pub enum Regexp {
    Literal(char),
    Concat(Box<Regexp>, Box<Regexp>),
    Either(Box<Regexp>, Box<Regexp>),
    Optional(Box<Regexp>),
    ZeroOrMore(Box<Regexp>),
    OneOrMore(Box<Regexp>),
    Any,
    CharacterClass(HashSet<char>),
    RepeatExactly(u8, Box<Regexp>),
    RepeatAtLeast(u8, Box<Regexp>),
    RepeatAtLeastAtMost(u8, u8, Box<Regexp>),
    Capture(Box<Regexp>),
    /// Convenience that is unrolled into a sequence of [`Literal`](Regexp#variant.Literal)
    String(String),
    /// To support empty captures, we need an empty AST
    Empty,
    /// Stop-gap blanket "implementation" for things that might be needed but are not yet
    /// implemented
    NotImplemented,
}

impl Regexp {
    pub fn compile(&self) -> CompiledRegexp {
        let payload = Translation::Resolved(ResolvedTranslation::new(
            "",
            "",
            0,
            TranslationStage::Main,
            None,
        ));
        self.compile_with_payload(payload)
    }

    pub fn compile_with_payload(&self, payload: Translation) -> CompiledRegexp {
        let mut instructions = Vec::new();
        let mut character_classes = Vec::new();
        let translations = Vec::from([payload]);
        self.emit(&mut instructions, &mut character_classes);
        instructions.push(Instruction::Match(0));
        CompiledRegexp {
            instructions,
            character_classes,
            translations,
        }
    }

    fn compile_many_accepting_internal(
        pairs: &[(Regexp, Translation)],
        instructions: &mut Vec<Instruction>,
        character_classes: &mut Vec<HashSet<char>>,
        translations: &mut Vec<Translation>,
    ) {
        if pairs.is_empty() {
            // we're done
        } else if pairs.len() == 1 {
            let (regexp, payload) = &pairs[0];
            regexp.emit(instructions, character_classes);
            translations.push(payload.clone());
            instructions.push(Instruction::Match(translations.len() - 1));
        } else {
            let p1 = instructions.len();
            instructions.push(Instruction::Split(p1 + 1, 0));
            let (regexp, payload) = &pairs[0];
            regexp.emit(instructions, character_classes);
            translations.push(payload.clone());
            instructions.push(Instruction::Match(translations.len() - 1));
            let p2 = instructions.len();
            instructions.push(Instruction::Jump(0));
            instructions[p1] = Instruction::Split(p1 + 1, p2 + 1);
            Regexp::compile_many_accepting_internal(
                &pairs[1..],
                instructions,
                character_classes,
                translations,
            );
            instructions[p2] = Instruction::Jump(instructions.len());
        }
    }

    pub fn compile_many_accepting(pairs: &[(Regexp, Translation)]) -> CompiledRegexp {
        let mut instructions = Vec::new();
        let mut character_classes = Vec::new();
        let mut translations = Vec::new();
        Regexp::compile_many_accepting_internal(
            pairs,
            &mut instructions,
            &mut character_classes,
            &mut translations,
        );
        CompiledRegexp {
            instructions,
            character_classes,
            translations,
        }
    }

    fn emit(
        &self,
        instructions: &mut Vec<Instruction>,
        character_classes: &mut Vec<HashSet<char>>,
    ) {
        match self {
            Regexp::Literal(c) => instructions.push(Instruction::Char(*c)),
            Regexp::Concat(left, right) => {
                left.emit(instructions, character_classes);
                right.emit(instructions, character_classes);
            }
            Regexp::Either(left, right) => {
                let p1 = instructions.len();
                instructions.push(Instruction::Split(p1 + 1, 0));
                left.emit(instructions, character_classes);
                let p2 = instructions.len();
                instructions.push(Instruction::Jump(0));
                instructions[p1] = Instruction::Split(p1 + 1, p2 + 1);
                right.emit(instructions, character_classes);
                instructions[p2] = Instruction::Jump(instructions.len());
            }
            Regexp::Optional(regexp) => {
                let pos = instructions.len();
                instructions.push(Instruction::Split(pos + 1, 0));
                regexp.emit(instructions, character_classes);
                instructions[pos] = Instruction::Split(pos + 1, instructions.len());
            }
            Regexp::ZeroOrMore(regexp) => {
                let pos = instructions.len();
                instructions.push(Instruction::Split(pos + 1, 0));
                regexp.emit(instructions, character_classes);
                instructions.push(Instruction::Jump(pos));
                instructions[pos] = Instruction::Split(pos + 1, instructions.len());
            }
            Regexp::OneOrMore(regexp) => {
                let pos = instructions.len();
                regexp.emit(instructions, character_classes);
                instructions.push(Instruction::Split(pos, instructions.len() + 1));
            }
            Regexp::Any => instructions.push(Instruction::Any),
            Regexp::CharacterClass(characters) => {
                character_classes.push(characters.clone());
                instructions.push(Instruction::Class(character_classes.len() - 1));
            }
            Regexp::RepeatExactly(n, regexp) => {
                for _ in 0..*n {
                    regexp.emit(instructions, character_classes);
                }
            }
            Regexp::RepeatAtLeast(min, regexp) => {
                for _ in 0..*min {
                    regexp.emit(instructions, character_classes);
                }
                let pos = instructions.len();
                instructions.push(Instruction::Split(pos + 1, 0));
                regexp.emit(instructions, character_classes);
                instructions.push(Instruction::Jump(pos));
                instructions[pos] = Instruction::Split(pos + 1, instructions.len());
            }
            Regexp::RepeatAtLeastAtMost(min, max, regexp) => {
                for _ in 0..*min {
                    regexp.emit(instructions, character_classes);
                }
                let pos = instructions.len();
                instructions.push(Instruction::Split(pos + 1, 0));
                for _ in *min..*max {
                    regexp.emit(instructions, character_classes);
                }
                instructions.push(Instruction::Jump(pos));
                instructions[pos] = Instruction::Split(pos + 1, instructions.len());
            }
            Regexp::Capture(regexp) => {
                instructions.push(Instruction::CaptureStart);
                regexp.emit(instructions, character_classes);
                instructions.push(Instruction::CaptureEnd);
            }
            Regexp::String(s) => {
                for c in s.chars() {
                    instructions.push(Instruction::Char(c))
                }
            }
            Regexp::Empty => (),
            Regexp::NotImplemented => (),
        }
    }
}

type InstructionIndex = usize;
type CharacterClassIndex = usize;
type TranslationIndex = usize;

/// Virtual machine instruction set for pattern matching
#[derive(Debug, Clone)]
pub enum Instruction {
    /// Match a single char
    Char(char),
    /// Match a set of chars. Contains a reference to the character class.
    Class(CharacterClassIndex),
    /// Match any character
    Any,
    /// Mark a successful match of the regexp. Contains a reference to the
    /// payload, the [`Translation`]
    Match(TranslationIndex),
    /// Jump to the [`Instruction`] at given index
    Jump(InstructionIndex),
    /// Continue executing the virtual machine at both given indexes
    Split(InstructionIndex, InstructionIndex),
    /// Start a capture
    CaptureStart,
    /// End a capture
    CaptureEnd,
}

/// Compiled version of [`Regexp`]. Contains bytecode and associated data structures
#[derive(Debug, Clone)]
pub struct CompiledRegexp {
    /// The bytecode instructions that execute the pattern matching
    instructions: Vec<Instruction>,
    /// Character classes defined in this regexp. They are are stored separately
    /// from the instructions to improve cache locality
    character_classes: Vec<HashSet<char>>,
    /// Each match contains a [`Translation`] as a payload. Again, these are
    /// stored separately from the instructions to improve cache locality
    translations: Vec<Translation>,
}

impl CompiledRegexp {
    fn is_match_internal(&self, pc: usize, input: &str) -> bool {
        if pc >= self.instructions.len() {
            return false;
        }

        match self.instructions[pc] {
            Instruction::Char(expected) => {
                let mut chars = input.chars().peekable();
                if let Some(actual) = chars.peek()
                    && expected == *actual
                {
                    self.is_match_internal(pc + 1, &input[actual.len_utf8()..])
                } else {
                    false
                }
            }
            Instruction::Class(index) => {
                let mut chars = input.chars().peekable();
                if let Some(actual) = chars.peek()
                    && self.character_classes[index].contains(actual)
                {
                    self.is_match_internal(pc + 1, &input[actual.len_utf8()..])
                } else {
                    false
                }
            }
            Instruction::Any => {
                let mut chars = input.chars();
                if let Some(actual) = chars.next() {
                    self.is_match_internal(pc + 1, &input[actual.len_utf8()..])
                } else {
                    false
                }
            }
            Instruction::Match(_) => true,
            Instruction::Jump(index) => self.is_match_internal(index, input),
            Instruction::Split(index1, index2) => {
                self.is_match_internal(index1, input) || self.is_match_internal(index2, input)
            }
            Instruction::CaptureStart | Instruction::CaptureEnd => {
                self.is_match_internal(pc + 1, &input)
            } // Ignore
        }
    }

    pub fn is_match(&self, input: &str) -> bool {
        self.is_match_internal(0, input)
    }

    fn find_internal(
        &self,
        pc: usize,
        input: &str,
        sp: usize,
        length: usize,
        capture: (usize, usize),
        translations: &mut Vec<ResolvedTranslation>,
    ) {
        if pc >= self.instructions.len() {
            return;
        }

        match self.instructions[pc] {
            Instruction::Char(expected) => {
                let mut chars = input[sp..].chars().peekable();
                if let Some(actual) = chars.peek()
                    && expected == *actual
                {
                    self.find_internal(
                        pc + 1,
                        &input,
                        sp + actual.len_utf8(),
                        length + 1,
                        capture,
                        translations,
                    )
                }
            }
            Instruction::Class(index) => {
                let mut chars = input[sp..].chars().peekable();
                if let Some(actual) = chars.peek()
                    && self.character_classes[index].contains(actual)
                {
                    self.find_internal(
                        pc + 1,
                        &input,
                        sp + actual.len_utf8(),
                        length + 1,
                        capture,
                        translations,
                    )
                }
            }
            Instruction::Any => {
                let mut chars = input[sp..].chars();
                if let Some(actual) = chars.next() {
                    self.find_internal(
                        pc + 1,
                        &input,
                        sp + actual.len_utf8(),
                        length + 1,
                        capture,
                        translations,
                    )
                }
            }
            Instruction::Match(index) => {
                let (start, end) = capture;
                let capture = &input[start..end];
                translations.push(
                    self.translations[index]
                        .clone()
                        .resolve(capture, length, start),
                );
            }
            Instruction::Jump(index) => {
                self.find_internal(index, input, sp, length, capture, translations)
            }
            Instruction::Split(index1, index2) => {
                self.find_internal(index1, input, sp, length, capture, translations);
                self.find_internal(index2, input, sp, length, capture, translations);
            }
            Instruction::CaptureStart => {
                self.find_internal(pc + 1, &input, sp, length, (length, 0), translations)
            }
            Instruction::CaptureEnd => self.find_internal(
                pc + 1,
                &input,
                sp,
                length,
                (capture.0, length),
                translations,
            ),
        }
    }

    pub fn find(&self, input: &str) -> Vec<ResolvedTranslation> {
        let mut translations = Vec::new();
        self.find_internal(0, input, 0, 0, (0, 0), &mut translations);
        translations
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        parser::Precedence,
        translator::translation::{TranslationTarget, UnresolvedTranslation},
    };

    use super::*;

    #[test]
    fn character() {
        let re = Regexp::Literal('a').compile();
        assert!(re.is_match("a"));
        assert!(!re.is_match("b"));
    }

    #[test]
    fn alteration() {
        let re = Regexp::Either(
            Box::new(Regexp::Literal('a')),
            Box::new(Regexp::Literal('b')),
        )
        .compile();
        assert!(re.is_match("a"));
        assert!(re.is_match("b"));
        assert!(re.is_match("ab"));
        assert!(!re.is_match("c"));
    }

    #[test]
    fn multiple_alterations() {
        let re = Regexp::Either(
            Box::new(Regexp::Literal('a')),
            Box::new(Regexp::Either(
                Box::new(Regexp::Literal('b')),
                Box::new(Regexp::Literal('c')),
            )),
        )
        .compile();
        assert!(re.is_match("a"));
        assert!(re.is_match("b"));
        assert!(re.is_match("ab"));
        assert!(re.is_match("c"));
        assert!(!re.is_match("d"));
    }

    #[test]
    fn concatenation() {
        let re = Regexp::Concat(
            Box::new(Regexp::Literal('a')),
            Box::new(Regexp::Literal('b')),
        )
        .compile();
        assert!(re.is_match("ab"));
        assert!(re.is_match("abc"));
        assert!(!re.is_match("a"));
        assert!(!re.is_match("b"));
        assert!(!re.is_match("ba"));
        assert!(!re.is_match("c"));
    }

    #[test]
    fn kleene() {
        let re = Regexp::ZeroOrMore(Box::new(Regexp::Literal('a'))).compile();
        assert!(re.is_match(""));
        assert!(re.is_match("a"));
        assert!(re.is_match("aa"));
        assert!(re.is_match("aaaaa"));
        assert!(re.is_match("b"));
        assert!(re.is_match("ba"));
        assert!(re.is_match("ab"));
        assert!(re.is_match("c"));
        assert!(re.is_match("abc"));
    }

    #[test]
    fn one_or_more() {
        let re = Regexp::OneOrMore(Box::new(Regexp::Literal('a'))).compile();
        assert!(!re.is_match(""));
        assert!(re.is_match("a"));
        assert!(re.is_match("aa"));
        assert!(re.is_match("ab"));
        assert!(re.is_match("abc"));
        assert!(re.is_match("aaaaa"));
        assert!(!re.is_match("b"));
        assert!(!re.is_match("ba"));
        assert!(!re.is_match("c"));
    }

    #[test]
    fn any() {
        let re = Regexp::Any.compile();
        assert!(re.is_match("abb"));
    }

    #[test]
    fn optional() {
        let re = Regexp::Concat(
            Box::new(Regexp::Optional(Box::new(Regexp::Concat(
                Box::new(Regexp::Literal('a')),
                Box::new(Regexp::Any),
            )))),
            Box::new(Regexp::Literal('b')),
        )
        .compile();
        assert!(re.is_match("acb"));
        assert!(re.is_match("axb"));
        assert!(re.is_match("b"));
        assert!(re.is_match("bbb"));
        assert!(!re.is_match("c"));
    }

    #[test]
    fn character_class() {
        let re = Regexp::OneOrMore(Box::new(Regexp::CharacterClass(HashSet::from([
            'a', 'b', 'c',
        ]))))
        .compile();
        assert!(re.is_match("acb"));
        assert!(re.is_match("axb"));
        assert!(re.is_match("b"));
        assert!(re.is_match("bbb"));
        assert!(re.is_match("c"));
        assert!(!re.is_match("x"));
    }

    #[test]
    fn repeat_exactly() {
        // exactly one 'a'
        let re = Regexp::RepeatExactly(1, Box::new(Regexp::Literal('a'))).compile();
        assert!(re.is_match("a"));
        assert!(!re.is_match("c"));
        assert!(!re.is_match("bbb"));
        assert!(!re.is_match(""));
        assert!(re.is_match("aa"));
        assert!(re.is_match("aaaa"));

        // exactly two 'a'
        let re = Regexp::RepeatExactly(2, Box::new(Regexp::Literal('a'))).compile();
        assert!(re.is_match("aa"));
        assert!(!re.is_match("c"));
        assert!(!re.is_match("bbb"));
        assert!(!re.is_match(""));
        assert!(!re.is_match("a"));
        assert!(re.is_match("aaa"));
        assert!(re.is_match("aaaa"));

        // exactly three 'a'
        let re = Regexp::RepeatExactly(3, Box::new(Regexp::Literal('a'))).compile();
        assert!(re.is_match("aaa"));
        assert!(!re.is_match("c"));
        assert!(!re.is_match("bbb"));
        assert!(!re.is_match("a"));
        assert!(!re.is_match("aa"));
        assert!(re.is_match("aaaa"));
    }

    #[test]
    fn repeat_at_least() {
        // at least one 'a'
        let re = Regexp::RepeatAtLeast(1, Box::new(Regexp::Literal('a'))).compile();
        assert!(re.is_match("a"));
        assert!(re.is_match("aa"));
        assert!(re.is_match("aaa"));
        assert!(re.is_match("aaaaaaaaaaaaaaaaaaaaaa"));
        assert!(re.is_match("aaab"));
        assert!(!re.is_match("c"));
        assert!(!re.is_match("bbb"));
        assert!(!re.is_match(""));

        // at least two 'a'
        let re = Regexp::RepeatAtLeast(2, Box::new(Regexp::Literal('a'))).compile();
        assert!(re.is_match("aa"));
        assert!(re.is_match("aaa"));
        assert!(re.is_match("aaaa"));
        assert!(re.is_match("aaaaaaaaaaaaaaaaaaaaa"));
        assert!(re.is_match("aaab"));
        assert!(!re.is_match("c"));
        assert!(!re.is_match("bbb"));
        assert!(!re.is_match(""));
        assert!(!re.is_match("a"));

        // at least three 'a'
        let re = Regexp::RepeatAtLeast(3, Box::new(Regexp::Literal('a'))).compile();
        assert!(re.is_match("aaa"));
        assert!(re.is_match("aaaa"));
        assert!(re.is_match("aaaaa"));
        assert!(re.is_match("aaaaaaaaaaaaaaaaaaaaaa"));
        assert!(re.is_match("aaab"));
        assert!(!re.is_match("c"));
        assert!(!re.is_match("bbb"));
        assert!(!re.is_match("a"));
        assert!(!re.is_match("aa"));
    }

    #[test]
    fn repeat_at_least_at_most() {
        // at least three and at most five 'a's
        let re = Regexp::RepeatAtLeastAtMost(3, 5, Box::new(Regexp::Literal('a'))).compile();
        assert!(!re.is_match("a"));
        assert!(!re.is_match("aa"));
        assert!(re.is_match("aaa"));
        assert!(re.is_match("aaaa"));
        assert!(re.is_match("aaaaa"));
        assert!(re.is_match("aaaaaa"));
        assert!(re.is_match("aaaaaaaaaaaaaaaaaaaaaa"));
        assert!(re.is_match("aaab"));
        assert!(!re.is_match("c"));
        assert!(!re.is_match("bbb"));
    }

    #[test]
    fn capture() {
        let translation = Translation::Unresolved(UnresolvedTranslation::new(
            &[TranslationTarget::Capture],
            Precedence::Default,
            TranslationStage::Main,
            None,
        ));
        let re = Regexp::Concat(
            Box::new(Regexp::String("foo".to_string())),
            Box::new(Regexp::Concat(
                Box::new(Regexp::Capture(Box::new(Regexp::Concat(
                    Box::new(Regexp::Any),
                    Box::new(Regexp::String("ar".to_string())),
                )))),
                Box::new(Regexp::String("foo".to_string())),
            )),
        )
        .compile_with_payload(translation);

        assert_eq!(re.find("foo"), []);
        assert_eq!(re.find("foobar"), []);
        assert_eq!(
            re.find("foobarfoo"),
            [
                ResolvedTranslation::new("bar", "bar", 3, TranslationStage::Main, None)
                    .with_offset(3)
                    .with_weight(9)
            ]
        );
        assert_eq!(
            re.find("fooxarfoo"),
            [
                ResolvedTranslation::new("xar", "xar", 3, TranslationStage::Main, None)
                    .with_offset(3)
                    .with_weight(9)
            ]
        );
        assert_eq!(
            re.find("foobarfoobar"),
            [
                ResolvedTranslation::new("bar", "bar", 3, TranslationStage::Main, None)
                    .with_offset(3)
                    .with_weight(9)
            ]
        );
        assert_eq!(re.find("aaaaaa"), []);
        assert_eq!(re.find("bbb"), []);
    }

    #[test]
    fn capture_replace_with_literal() {
        let translation = Translation::Unresolved(UnresolvedTranslation::new(
            &[TranslationTarget::Literal("baz".to_string())],
            Precedence::Default,
            TranslationStage::Main,
            None,
        ));
        let re = Regexp::Concat(
            Box::new(Regexp::String("foo".to_string())),
            Box::new(Regexp::Concat(
                Box::new(Regexp::Capture(Box::new(Regexp::Concat(
                    Box::new(Regexp::Any),
                    Box::new(Regexp::String("ar".to_string())),
                )))),
                Box::new(Regexp::String("foo".to_string())),
            )),
        )
        .compile_with_payload(translation);

        assert_eq!(re.find("foo"), []);
        assert_eq!(re.find("foobar"), []);
        assert_eq!(
            re.find("foobarfoo"),
            [
                ResolvedTranslation::new("bar", "baz", 3, TranslationStage::Main, None)
                    .with_offset(3)
                    .with_weight(9)
            ]
        );
        assert_eq!(
            re.find("fooxarfoo"),
            [
                ResolvedTranslation::new("xar", "baz", 3, TranslationStage::Main, None)
                    .with_offset(3)
                    .with_weight(9)
            ]
        );
        assert_eq!(
            re.find("foobarfoobar"),
            [
                ResolvedTranslation::new("bar", "baz", 3, TranslationStage::Main, None)
                    .with_offset(3)
                    .with_weight(9)
            ]
        );
    }

    #[test]
    fn offset() {
        let translation = Translation::Resolved(ResolvedTranslation::new(
            "bar",
            "baz",
            9,
            TranslationStage::Main,
            None,
        ));
        let re = Regexp::Concat(
            Box::new(Regexp::Literal('f')),
            Box::new(Regexp::Concat(
                Box::new(Regexp::Literal('o')),
                Box::new(Regexp::Concat(
                    Box::new(Regexp::Literal('o')),
                    Box::new(Regexp::Concat(
                        Box::new(Regexp::Capture(Box::new(Regexp::Concat(
                            Box::new(Regexp::Literal('b')),
                            Box::new(Regexp::Concat(
                                Box::new(Regexp::Literal('a')),
                                Box::new(Regexp::Literal('r')),
                            )),
                        )))),
                        Box::new(Regexp::Concat(
                            Box::new(Regexp::Literal('f')),
                            Box::new(Regexp::Concat(
                                Box::new(Regexp::Literal('o')),
                                Box::new(Regexp::Literal('o')),
                            )),
                        )),
                    )),
                )),
            )),
        )
        .compile_with_payload(translation);

        assert_eq!(re.find("foo"), []);
        assert_eq!(re.find("foobar"), []);
        assert_eq!(
            re.find("foobarfoo"),
            [
                ResolvedTranslation::new("bar", "baz", 3, TranslationStage::Main, None)
                    .with_offset(3)
                    .with_weight(9)
            ]
        );
        assert_eq!(
            re.find("foobarfoobar"),
            [
                ResolvedTranslation::new("bar", "baz", 3, TranslationStage::Main, None)
                    .with_offset(3)
                    .with_weight(9)
            ]
        );
        assert_eq!(re.find("aaaaaa"), []);
        assert_eq!(re.find("bbb"), []);
    }

    #[test]
    fn compile_many() {
        let re = Regexp::compile_many_accepting(&[
            (
                Regexp::Literal('a'),
                Translation::Resolved(ResolvedTranslation::new(
                    "a",
                    "1",
                    1,
                    TranslationStage::Main,
                    None,
                )),
            ),
            (
                Regexp::Literal('b'),
                Translation::Resolved(ResolvedTranslation::new(
                    "b",
                    "2",
                    1,
                    TranslationStage::Main,
                    None,
                )),
            ),
            (
                Regexp::Literal('c'),
                Translation::Resolved(ResolvedTranslation::new(
                    "c",
                    "3",
                    1,
                    TranslationStage::Main,
                    None,
                )),
            ),
        ]);
        assert!(re.is_match("a"));
        assert!(re.is_match("b"));
        assert!(re.is_match("c"));
        assert!(!re.is_match("d"));
    }
}
