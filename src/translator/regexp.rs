use std::collections::HashSet;

/// An abstract syntax tree
use crate::translator::{
    ResolvedTranslation,
    translation::{Resolve, Translation},
};

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
    /// To support empty captures, we need an empty AST
    Empty,
}

impl Regexp {
    pub fn compile(&self) -> CompiledRegexp {
        let mut instructions = Vec::new();
        let mut character_classes = Vec::new();
        let translations = Vec::new();
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
            Regexp::RepeatExactly(n, regexp) => {
                for i in 0..*n {
                    regexp.emit(instructions, character_classes);
                }
            }
            Regexp::RepeatAtLeast(min, regexp) => {
                for i in 0..*min {
                    regexp.emit(instructions, character_classes);
                }
                let pos = instructions.len();
                instructions.push(Instruction::Split(pos + 1, 0));
                regexp.emit(instructions, character_classes);
                instructions.push(Instruction::Jump(pos));
                instructions[pos] = Instruction::Split(pos + 1, instructions.len());
            }
            Regexp::RepeatAtLeastAtMost(min, max, regexp) => {
                for i in 0..*min {
                    regexp.emit(instructions, character_classes);
                }
                let pos = instructions.len();
                instructions.push(Instruction::Split(pos + 1, 0));
                for i in *min..*max {
                    regexp.emit(instructions, character_classes);
                }
                instructions.push(Instruction::Jump(pos));
                instructions[pos] = Instruction::Split(pos + 1, instructions.len());
            }
            Regexp::Capture(regexp) => todo!(),
            Regexp::Empty => (),
        }
    }
}

type InstructionIndex = usize;
type CharacterClassIndex = usize;
type TranslationIndex = usize;

#[derive(Debug, Clone)]
pub enum Instruction {
    Char(char),
    Class(CharacterClassIndex),
    Any,
    Match(TranslationIndex),
    Jump(InstructionIndex),
    Split(InstructionIndex, InstructionIndex),
}

#[derive(Debug, Clone)]
pub struct CompiledRegexp {
    instructions: Vec<Instruction>,
    // the HashSet for character classes are stored separately from the instructions to improve
    // cache locality
    character_classes: Vec<HashSet<char>>,
    translations: Vec<Translation>,
}

impl CompiledRegexp {
    fn is_match_from(&self, pc: usize, input: &str) -> bool {
        if pc >= self.instructions.len() {
            return false;
        }

        match self.instructions[pc] {
            Instruction::Char(expected) => {
                let mut chars = input.chars().peekable();
                if let Some(actual) = chars.peek()
                    && expected == *actual
                {
                    self.is_match_from(pc + 1, &input[actual.len_utf8()..])
                } else {
                    false
                }
            }
            Instruction::Class(index) => {
                let mut chars = input.chars().peekable();
                if let Some(actual) = chars.peek()
                    && self.character_classes[index].contains(actual)
                {
                    self.is_match_from(pc + 1, &input[actual.len_utf8()..])
                } else {
                    false
                }
            }
            Instruction::Any => {
                let mut chars = input.chars();
                if let Some(actual) = chars.next() {
                    self.is_match_from(pc + 1, &input[actual.len_utf8()..])
                } else {
                    false
                }
            }
            Instruction::Match(_) => true,
            Instruction::Jump(index) => self.is_match_from(index, input),
            Instruction::Split(index1, index2) => {
                self.is_match_from(index1, input) || self.is_match_from(index2, input)
            }
        }
    }

    pub fn is_match(&self, input: &str) -> bool {
        self.is_match_from(0, input)
    }

    fn find_from(
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
                    self.find_from(
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
                    self.find_from(
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
                    self.find_from(
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
                self.find_from(index, input, sp, length + 1, capture, translations)
            }
            Instruction::Split(index1, index2) => {
                self.find_from(index1, input, sp, length + 1, capture, translations);
                self.find_from(index2, input, sp, length + 1, capture, translations);
            }
        }
    }

    pub fn find(&self, input: &str) -> Vec<ResolvedTranslation> {
        let mut translations = Vec::new();
        self.find_from(0, input, 0, 0, (0, 0), &mut translations);
        translations
    }
}

#[cfg(test)]
mod tests {
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

    // #[test]
    // fn capture() {
    //     let re = Vm::new(&[]);
    //     let ast = AST::Concat(
    //         Box::new(AST::Concat(
    //             Box::new(AST::Character('a')),
    //             Box::new(AST::Capture(Box::new(AST::OneOrMore(Box::new(
    //                 AST::Character('b'),
    //             ))))),
    //         )),
    //         Box::new(AST::Character('c')),
    //     );
    //     let stage = TranslationStage::Main;
    //     let translation = UnresolvedTranslation::new(
    //         &[TranslationTarget::Literal("".to_string())],
    //         Precedence::Default,
    //         stage,
    //         None,
    //     );
    //     let re = re::from_with_translation(&ast, Translation::Unresolved(translation));
    //     assert_eq!(refind("a"), []);
    //     assert_eq!(re.find("ab"), []);
    //     assert_eq!(
    //         re.find("abc"),
    //         [ResolvedTranslation::new("b", "", 3, stage, None).with_offset(1)]
    //     );
    //     assert_eq!(
    //         re.find("abbc"),
    //         [ResolvedTranslation::new("bb", "", 4, stage, None).with_offset(1)]
    //     );
    //     assert_eq!(
    //         re.find("abbbbbc"),
    //         [ResolvedTranslation::new("bbbbb", "", 7, stage, None).with_offset(1)]
    //     );
    //     assert_eq!(re.find("aabbbbbc"), []);
    //     assert_eq!(re.find("abb"), []);
    //     assert_eq!(re.find("abbbb"), []);
    //     assert_eq!(re.find("abbbbbbbbbb"), []);
    //     assert_eq!(re.find("aaaaaa"), []);
    //     assert_eq!(re.find("aaaaaaaaaaaaaaaaaaaaaa"), []);
    //     assert_eq!(re.find("aaab"), []);
    //     assert_eq!(re.find("c"), []);
    //     assert_eq!(re.find("bbb"), []);
    // }
}
