//! Regular expression engine implemented using a virtual machine.
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
    ResolvedTranslation,
    effect::Environment,
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
    NotCharacterClass(HashSet<char>),
    RepeatExactly(u8, Box<Regexp>),
    RepeatAtLeast(u8, Box<Regexp>),
    RepeatAtLeastAtMost(u8, u8, Box<Regexp>),
    Capture(Box<Regexp>),
    /// Convenience that is unrolled into a sequence of [`Char`](Instruction#variant.Char)
    String(String),
    /// Convenience that is unrolled into a sequence of [`NotChar`](Instruction#variant.NotChar)
    NotString(String),
    /// Check whether variable at index is equal to given value
    VariableEqual(VariableIndex, u8),
    /// Check whether variable at index is not equal to given value
    NotVariableEqual(VariableIndex, u8),
    Group(Box<Regexp>),
    /// To support empty captures, we need an empty AST
    Empty,
    /// Stop-gap blanket "implementation" for things that might be needed but are not yet
    /// implemented
    NotImplemented,
}

impl Regexp {
    pub fn compile(&self) -> CompiledRegexp {
        self.compile_with_payload(Translation::default())
    }

    pub fn negate(self) -> Regexp {
        match self {
            Regexp::Literal(_) => todo!(),
            Regexp::Concat(left, right) => {
                Regexp::Concat(Box::new(left.negate()), Box::new(right.negate()))
            }
            Regexp::Either(left, right) => {
                Regexp::Either(Box::new(left.negate()), Box::new(right.negate()))
            }
            Regexp::Optional(regexp) => Regexp::Optional(Box::new(regexp.negate())),
            Regexp::ZeroOrMore(regexp) => Regexp::ZeroOrMore(Box::new(regexp.negate())),
            Regexp::OneOrMore(regexp) => Regexp::OneOrMore(Box::new(regexp.negate())),
            Regexp::Any => todo!(), // how are you supposed to negate any?
            Regexp::CharacterClass(class) => Regexp::NotCharacterClass(class),
            Regexp::NotCharacterClass(_) => unreachable!(),
            Regexp::RepeatExactly(n, regexp) => Regexp::RepeatExactly(n, Box::new(regexp.negate())),
            Regexp::RepeatAtLeast(n, regexp) => Regexp::RepeatAtLeast(n, Box::new(regexp.negate())),
            Regexp::RepeatAtLeastAtMost(n, m, regexp) => {
                Regexp::RepeatAtLeastAtMost(n, m, Box::new(regexp.negate()))
            }
            Regexp::Capture(_) => unreachable!(), // negating a capture makes no sense
            Regexp::String(s) => Regexp::NotString(s),
            Regexp::NotString(_) => unreachable!(),
            Regexp::VariableEqual(slot, value) => Regexp::NotVariableEqual(slot, value),
            Regexp::NotVariableEqual(_, _) => unreachable!(),
            Regexp::Group(regexp) => Regexp::Group(Box::new(regexp.negate())),
            Regexp::Empty => unreachable!(), // sorry you cannot negate an empty regexp
            Regexp::NotImplemented => unreachable!(),
        }
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
            Regexp::NotCharacterClass(characters) => {
                character_classes.push(characters.clone());
                instructions.push(Instruction::NotClass(character_classes.len() - 1));
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
            Regexp::NotString(s) => {
                for c in s.chars() {
                    instructions.push(Instruction::NotChar(c))
                }
            }
            Regexp::VariableEqual(index, value) => {
                instructions.push(Instruction::VariableEqual(*index, *value))
            }
            Regexp::NotVariableEqual(index, value) => {
                instructions.push(Instruction::NotVariableEqual(*index, *value))
            }
            Regexp::Group(regexp) => {
                regexp.emit(instructions, character_classes);
            }
            Regexp::Empty => (),
            Regexp::NotImplemented => (),
        }
    }
}

type InstructionIndex = usize;
type CharacterClassIndex = usize;
type TranslationIndex = usize;
type VariableIndex = u8;

/// Virtual machine instruction set for pattern matching
#[derive(Debug, Clone)]
pub enum Instruction {
    /// Match a single char
    Char(char),
    NotChar(char),
    /// Match a set of chars. Contains a reference to the character class.
    Class(CharacterClassIndex),
    NotClass(CharacterClassIndex),
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
    /// Test whether a variable is equal to a value
    VariableEqual(VariableIndex, u8),
    NotVariableEqual(VariableIndex, u8),
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
    fn is_match_internal(&self, pc: usize, input: &str, env: &Environment) -> bool {
        if pc >= self.instructions.len() {
            return false;
        }

        match self.instructions[pc] {
            Instruction::Char(expected) => {
                let mut chars = input.chars().peekable();
                if let Some(actual) = chars.peek()
                    && expected == *actual
                {
                    self.is_match_internal(pc + 1, &input[actual.len_utf8()..], env)
                } else {
                    false
                }
            }
            Instruction::NotChar(expected) => {
                let mut chars = input.chars().peekable();
                if let Some(actual) = chars.peek()
                    && expected != *actual
                {
                    self.is_match_internal(pc + 1, &input[actual.len_utf8()..], env)
                } else {
                    false
                }
            }
            Instruction::Class(index) => {
                let mut chars = input.chars().peekable();
                if let Some(actual) = chars.peek()
                    && self.character_classes[index].contains(actual)
                {
                    self.is_match_internal(pc + 1, &input[actual.len_utf8()..], env)
                } else {
                    false
                }
            }
            Instruction::NotClass(index) => {
                let mut chars = input.chars().peekable();
                if let Some(actual) = chars.peek()
                    && !self.character_classes[index].contains(actual)
                {
                    self.is_match_internal(pc + 1, &input[actual.len_utf8()..], env)
                } else {
                    false
                }
            }
            Instruction::Any => {
                let mut chars = input.chars();
                if let Some(actual) = chars.next() {
                    self.is_match_internal(pc + 1, &input[actual.len_utf8()..], env)
                } else {
                    false
                }
            }
            Instruction::Match(_) => true,
            Instruction::Jump(index) => self.is_match_internal(index, input, env),
            Instruction::Split(index1, index2) => {
                self.is_match_internal(index1, input, env)
                    || self.is_match_internal(index2, input, env)
            }
            Instruction::CaptureStart | Instruction::CaptureEnd => {
                self.is_match_internal(pc + 1, input, env)
            }
            Instruction::VariableEqual(var, expected) => {
                if let Some(&actual) = env.get(var)
                    && actual == expected
                {
                    self.is_match_internal(pc + 1, input, env)
                } else {
                    false
                }
            }
            Instruction::NotVariableEqual(var, expected) => {
                if let Some(&actual) = env.get(var)
                    && actual != expected
                {
                    self.is_match_internal(pc + 1, input, env)
                } else {
                    false
                }
            }
        }
    }

    pub fn is_match(&self, input: &str, env: &Environment) -> bool {
        self.is_match_internal(0, input, env)
    }

    fn find_internal(
        &self,
        pc: usize,
        input: &str,
        sp: usize,
        length: usize,
        env: &Environment,
        capture: (usize, usize),
    ) -> Option<ResolvedTranslation> {
        if pc >= self.instructions.len() {
            return None;
        }

        match self.instructions[pc] {
            Instruction::Char(expected) => {
                let mut chars = input[sp..].chars().peekable();
                if let Some(actual) = chars.peek()
                    && expected == *actual
                {
                    self.find_internal(
                        pc + 1,
                        input,
                        sp + actual.len_utf8(),
                        length + 1,
                        env,
                        capture,
                    )
                } else {
                    None
                }
            }
            Instruction::NotChar(expected) => {
                let mut chars = input[sp..].chars().peekable();
                if let Some(actual) = chars.peek()
                    && expected != *actual
                {
                    self.find_internal(
                        pc + 1,
                        input,
                        sp + actual.len_utf8(),
                        length + 1,
                        env,
                        capture,
                    )
                } else {
                    None
                }
            }
            Instruction::Class(index) => {
                let mut chars = input[sp..].chars().peekable();
                if let Some(actual) = chars.peek()
                    && self.character_classes[index].contains(actual)
                {
                    self.find_internal(
                        pc + 1,
                        input,
                        sp + actual.len_utf8(),
                        length + 1,
                        env,
                        capture,
                    )
                } else {
                    None
                }
            }
            Instruction::NotClass(index) => {
                let mut chars = input[sp..].chars().peekable();
                if let Some(actual) = chars.peek()
                    && !self.character_classes[index].contains(actual)
                {
                    self.find_internal(
                        pc + 1,
                        input,
                        sp + actual.len_utf8(),
                        length + 1,
                        env,
                        capture,
                    )
                } else {
                    None
                }
            }
            Instruction::Any => {
                let mut chars = input[sp..].chars();
                if let Some(actual) = chars.next() {
                    self.find_internal(
                        pc + 1,
                        input,
                        sp + actual.len_utf8(),
                        length + 1,
                        env,
                        capture,
                    )
                } else {
                    None
                }
            }
            Instruction::Match(index) => {
                let (start, end) = capture;
                let capture = &input[start..end];
                // offset is in number of chars not a byte offset
                let offset = &input[..start].chars().count();
                Some(
                    self.translations[index]
                        .clone()
                        .resolve(capture, length, *offset),
                )
            }
            Instruction::Jump(index) => self.find_internal(index, input, sp, length, env, capture),
            Instruction::Split(index1, index2) => self
                .find_internal(index1, input, sp, length, env, capture)
                .or_else(|| self.find_internal(index2, input, sp, length, env, capture)),
            Instruction::CaptureStart => {
                self.find_internal(pc + 1, input, sp, length, env, (sp, 0))
            }
            Instruction::CaptureEnd => {
                self.find_internal(pc + 1, input, sp, length, env, (capture.0, sp))
            }
            Instruction::VariableEqual(var, expected) => {
                if let Some(&actual) = env.get(var)
                    && actual == expected
                {
                    self.find_internal(pc + 1, input, sp, length, env, capture)
                } else {
                    None
                }
            }
            Instruction::NotVariableEqual(var, expected) => {
                if let Some(&actual) = env.get(var)
                    && actual != expected
                {
                    self.find_internal(pc + 1, input, sp, length, env, capture)
                } else {
                    None
                }
            }
        }
    }

    pub fn find(&self, input: &str, env: &Environment) -> Option<ResolvedTranslation> {
        self.find_internal(0, input, 0, 0, env, (0, 0))
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        parser::Precedence,
        translator::{
            TranslationStage,
            effect::Effect,
            translation::{TranslationTarget, UnresolvedTranslation},
        },
    };

    use super::*;

    #[test]
    fn character() {
        let env = Environment::new();
        let re = Regexp::Literal('a').compile();
        assert!(re.is_match("a", &env));
        assert!(!re.is_match("b", &env));
    }

    #[test]
    fn alteration() {
        let env = Environment::new();
        let re = Regexp::Either(
            Box::new(Regexp::Literal('a')),
            Box::new(Regexp::Literal('b')),
        )
        .compile();
        assert!(re.is_match("a", &env));
        assert!(re.is_match("b", &env));
        assert!(re.is_match("ab", &env));
        assert!(!re.is_match("c", &env));
    }

    #[test]
    fn multiple_alterations() {
        let env = Environment::new();
        let re = Regexp::Either(
            Box::new(Regexp::Literal('a')),
            Box::new(Regexp::Either(
                Box::new(Regexp::Literal('b')),
                Box::new(Regexp::Literal('c')),
            )),
        )
        .compile();
        assert!(re.is_match("a", &env));
        assert!(re.is_match("b", &env));
        assert!(re.is_match("ab", &env));
        assert!(re.is_match("c", &env));
        assert!(!re.is_match("d", &env));
    }

    #[test]
    fn concatenation() {
        let env = Environment::new();
        let re = Regexp::Concat(
            Box::new(Regexp::Literal('a')),
            Box::new(Regexp::Literal('b')),
        )
        .compile();
        assert!(re.is_match("ab", &env));
        assert!(re.is_match("abc", &env));
        assert!(!re.is_match("a", &env));
        assert!(!re.is_match("b", &env));
        assert!(!re.is_match("ba", &env));
        assert!(!re.is_match("c", &env));
    }

    #[test]
    fn kleene() {
        let env = Environment::new();
        let re = Regexp::ZeroOrMore(Box::new(Regexp::Literal('a'))).compile();
        assert!(re.is_match("", &env));
        assert!(re.is_match("a", &env));
        assert!(re.is_match("aa", &env));
        assert!(re.is_match("aaaaa", &env));
        assert!(re.is_match("b", &env));
        assert!(re.is_match("ba", &env));
        assert!(re.is_match("ab", &env));
        assert!(re.is_match("c", &env));
        assert!(re.is_match("abc", &env));
    }

    #[test]
    fn one_or_more() {
        let env = Environment::new();
        let re = Regexp::OneOrMore(Box::new(Regexp::Literal('a'))).compile();
        assert!(!re.is_match("", &env));
        assert!(re.is_match("a", &env));
        assert!(re.is_match("aa", &env));
        assert!(re.is_match("ab", &env));
        assert!(re.is_match("abc", &env));
        assert!(re.is_match("aaaaa", &env));
        assert!(!re.is_match("b", &env));
        assert!(!re.is_match("ba", &env));
        assert!(!re.is_match("c", &env));
    }

    #[test]
    fn any() {
        let env = Environment::new();
        let re = Regexp::Any.compile();
        assert!(re.is_match("abb", &env));
    }

    #[test]
    fn optional() {
        let env = Environment::new();
        let re = Regexp::Concat(
            Box::new(Regexp::Optional(Box::new(Regexp::Concat(
                Box::new(Regexp::Literal('a')),
                Box::new(Regexp::Any),
            )))),
            Box::new(Regexp::Literal('b')),
        )
        .compile();
        assert!(re.is_match("acb", &env));
        assert!(re.is_match("axb", &env));
        assert!(re.is_match("b", &env));
        assert!(re.is_match("bbb", &env));
        assert!(!re.is_match("c", &env));
    }

    #[test]
    fn character_class() {
        let env = Environment::new();
        let re = Regexp::OneOrMore(Box::new(Regexp::CharacterClass(HashSet::from([
            'a', 'b', 'c',
        ]))))
        .compile();
        assert!(re.is_match("acb", &env));
        assert!(re.is_match("axb", &env));
        assert!(re.is_match("b", &env));
        assert!(re.is_match("bbb", &env));
        assert!(re.is_match("c", &env));
        assert!(!re.is_match("x", &env));
    }

    #[test]
    fn repeat_exactly() {
        let env = Environment::new();
        // exactly one 'a'
        let re = Regexp::RepeatExactly(1, Box::new(Regexp::Literal('a'))).compile();
        assert!(re.is_match("a", &env));
        assert!(!re.is_match("c", &env));
        assert!(!re.is_match("bbb", &env));
        assert!(!re.is_match("", &env));
        assert!(re.is_match("aa", &env));
        assert!(re.is_match("aaaa", &env));

        // exactly two 'a'
        let re = Regexp::RepeatExactly(2, Box::new(Regexp::Literal('a'))).compile();
        assert!(re.is_match("aa", &env));
        assert!(!re.is_match("c", &env));
        assert!(!re.is_match("bbb", &env));
        assert!(!re.is_match("", &env));
        assert!(!re.is_match("a", &env));
        assert!(re.is_match("aaa", &env));
        assert!(re.is_match("aaaa", &env));

        // exactly three 'a'
        let re = Regexp::RepeatExactly(3, Box::new(Regexp::Literal('a'))).compile();
        assert!(re.is_match("aaa", &env));
        assert!(!re.is_match("c", &env));
        assert!(!re.is_match("bbb", &env));
        assert!(!re.is_match("a", &env));
        assert!(!re.is_match("aa", &env));
        assert!(re.is_match("aaaa", &env));
    }

    #[test]
    fn repeat_at_least() {
        let env = Environment::new();
        // at least one 'a'
        let re = Regexp::RepeatAtLeast(1, Box::new(Regexp::Literal('a'))).compile();
        assert!(re.is_match("a", &env));
        assert!(re.is_match("aa", &env));
        assert!(re.is_match("aaa", &env));
        assert!(re.is_match("aaaaaaaaaaaaaaaaaaaaaa", &env));
        assert!(re.is_match("aaab", &env));
        assert!(!re.is_match("c", &env));
        assert!(!re.is_match("bbb", &env));
        assert!(!re.is_match("", &env));

        // at least two 'a'
        let re = Regexp::RepeatAtLeast(2, Box::new(Regexp::Literal('a'))).compile();
        assert!(re.is_match("aa", &env));
        assert!(re.is_match("aaa", &env));
        assert!(re.is_match("aaaa", &env));
        assert!(re.is_match("aaaaaaaaaaaaaaaaaaaaa", &env));
        assert!(re.is_match("aaab", &env));
        assert!(!re.is_match("c", &env));
        assert!(!re.is_match("bbb", &env));
        assert!(!re.is_match("", &env));
        assert!(!re.is_match("a", &env));

        // at least three 'a'
        let re = Regexp::RepeatAtLeast(3, Box::new(Regexp::Literal('a'))).compile();
        assert!(re.is_match("aaa", &env));
        assert!(re.is_match("aaaa", &env));
        assert!(re.is_match("aaaaa", &env));
        assert!(re.is_match("aaaaaaaaaaaaaaaaaaaaaa", &env));
        assert!(re.is_match("aaab", &env));
        assert!(!re.is_match("c", &env));
        assert!(!re.is_match("bbb", &env));
        assert!(!re.is_match("a", &env));
        assert!(!re.is_match("aa", &env));
    }

    #[test]
    fn repeat_at_least_at_most() {
        let env = Environment::new();
        // at least three and at most five 'a's
        let re = Regexp::RepeatAtLeastAtMost(3, 5, Box::new(Regexp::Literal('a'))).compile();
        assert!(!re.is_match("a", &env));
        assert!(!re.is_match("aa", &env));
        assert!(re.is_match("aaa", &env));
        assert!(re.is_match("aaaa", &env));
        assert!(re.is_match("aaaaa", &env));
        assert!(re.is_match("aaaaaa", &env));
        assert!(re.is_match("aaaaaaaaaaaaaaaaaaaaaa", &env));
        assert!(re.is_match("aaab", &env));
        assert!(!re.is_match("c", &env));
        assert!(!re.is_match("bbb", &env));
    }

    #[test]
    fn variable_equal() {
        let mut env = Environment::new();
        let re = Regexp::Concat(
            Box::new(Regexp::VariableEqual(1, 1)),
            Box::new(Regexp::Literal('a')),
        )
        .compile();
        // these should not match as the variable is not defined
        assert!(!re.is_match("a", &env));
        assert!(!re.is_match("aa", &env));
        assert!(!re.is_match("aaab", &env));
        assert!(!re.is_match("c", &env));
        assert!(!re.is_match("bbb", &env));
        let effect = Effect::new(1, 42);
        env.apply(&effect);
        // these should not match as the variable is 42 instead of 1
        assert!(!re.is_match("a", &env));
        assert!(!re.is_match("aa", &env));
        assert!(!re.is_match("aaab", &env));
        assert!(!re.is_match("c", &env));
        assert!(!re.is_match("bbb", &env));
        let effect = Effect::new(1, 1);
        env.apply(&effect);
        // these should match as the variable is finally equal to 1
        assert!(re.is_match("a", &env));
        assert!(re.is_match("aa", &env));
        assert!(re.is_match("aaab", &env));
        assert!(!re.is_match("c", &env));
        assert!(!re.is_match("bbb", &env));
    }

    #[test]
    fn capture() {
        let env = Environment::new();
        let translation = Translation::Unresolved(UnresolvedTranslation::new(
            &[TranslationTarget::Capture],
            Precedence::Default,
            TranslationStage::Main,
            &[],
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

        assert_eq!(re.find("foo", &env), None);
        assert_eq!(re.find("foobar", &env), None);
        assert_eq!(
            re.find("foobarfoo", &env).unwrap(),
            ResolvedTranslation::new("bar", "bar", 3, TranslationStage::Main, None)
                .with_offset(3)
                .with_weight(9)
        );
        assert_eq!(
            re.find("fooxarfoo", &env).unwrap(),
            ResolvedTranslation::new("xar", "xar", 3, TranslationStage::Main, None)
                .with_offset(3)
                .with_weight(9)
        );
        assert_eq!(
            re.find("foobarfoobar", &env).unwrap(),
            ResolvedTranslation::new("bar", "bar", 3, TranslationStage::Main, None)
                .with_offset(3)
                .with_weight(9)
        );
        assert_eq!(re.find("aaaaaa", &env), None);
        assert_eq!(re.find("bbb", &env), None);
    }

    #[test]
    fn capture_replace_with_literal() {
        let env = Environment::new();
        let translation = Translation::Unresolved(UnresolvedTranslation::new(
            &[TranslationTarget::Literal("baz".to_string())],
            Precedence::Default,
            TranslationStage::Main,
            &[],
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

        assert_eq!(re.find("foo", &env), None);
        assert_eq!(re.find("foobar", &env), None);
        assert_eq!(
            re.find("foobarfoo", &env).unwrap(),
            ResolvedTranslation::new("bar", "baz", 3, TranslationStage::Main, None)
                .with_offset(3)
                .with_weight(9)
        );
        assert_eq!(
            re.find("fooxarfoo", &env).unwrap(),
            ResolvedTranslation::new("xar", "baz", 3, TranslationStage::Main, None)
                .with_offset(3)
                .with_weight(9)
        );
        assert_eq!(
            re.find("foobarfoobar", &env).unwrap(),
            ResolvedTranslation::new("bar", "baz", 3, TranslationStage::Main, None)
                .with_offset(3)
                .with_weight(9)
        );
    }

    #[test]
    fn offset() {
        let env = Environment::new();
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

        assert_eq!(re.find("foo", &env), None);
        assert_eq!(re.find("foobar", &env), None);
        assert_eq!(
            re.find("foobarfoo", &env).unwrap(),
            ResolvedTranslation::new("bar", "baz", 3, TranslationStage::Main, None)
                .with_offset(3)
                .with_weight(9)
        );
        assert_eq!(
            re.find("foobarfoobar", &env).unwrap(),
            ResolvedTranslation::new("bar", "baz", 3, TranslationStage::Main, None)
                .with_offset(3)
                .with_weight(9)
        );
        assert_eq!(re.find("aaaaaa", &env), None);
        assert_eq!(re.find("bbb", &env), None);
    }
}
