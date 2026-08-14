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

/// Whether `actual` matches `expected` case-insensitively, lowercasing only `actual`.
/// A character without a single-character lowercase mapping never matches.
fn chars_match_case_insensitive(actual: char, expected: char) -> bool {
    actual.to_lowercase().count() == 1 && actual.to_lowercase().next() == Some(expected)
}

/// Abstract syntax tree representation of regular expressions
#[derive(Debug, Clone)]
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
    /// Like [`String`](Regexp::String), but each character is compared case-insensitively —
    /// mirroring [`trie::TrieNode::char_transition`](crate::translator::trie::TrieNode), which
    /// lowercases the *input* character before comparing against the (by table-author
    /// convention, already lowercase) stored one, rather than folding both sides. liblouis' own
    /// `match` opcode does the same for its literal `chars` field: it's found via the same
    /// case-insensitive `validMatch`/`toLowercase` used for every other opcode, and only the
    /// `pre`/`post` context patterns go through the strictly case-sensitive pattern matcher.
    CaseInsensitiveString(String),
    /// Check whether variable at index is equal to given value
    VariableEqual(VariableIndex, u8),
    /// Check whether variable at index is not equal to given value
    NotVariableEqual(VariableIndex, u8),
    Group(Box<Regexp>),
    /// To support empty captures, we need an empty AST
    Empty,
    /// Zero-width assertion that matches only when there is no input left, i.e. the match has
    /// reached the true end of the whole string being translated
    EndAnchor,
    /// Zero-width assertion that matches only when there is still input left. The negation of
    /// [`EndAnchor`](Regexp::EndAnchor).
    NotEndAnchor,
    /// Never matches, regardless of input. The negation of [`Empty`](Regexp::Empty).
    Never,
}

impl Regexp {
    pub fn compile(&self) -> CompiledRegexp {
        self.compile_with_payload(Translation::default())
    }

    // FIXME: the `Not*` arms below are unreachable only because parsers currently reject double
    // negation (`!!x`) before it gets here. A future caller that negates a `Regexp` without going
    // through that check would panic; consider returning a `Result` instead as a backstop.
    pub fn negate(self) -> Regexp {
        match self {
            Regexp::Literal(_) => todo!(),
            Regexp::Concat(left, right) => {
                // FIXME: Unfortunatelly this is wrong: !(e1e2) is not the same as (!e1!e2)
                Regexp::Concat(Box::new(left.negate()), Box::new(right.negate()))
            }
            Regexp::Either(left, right) => {
                // FIXME: Unfortunatelly this is wrong as well: !(e1|e2) is not the same as
                // (!e1|!e2). The following would be true: !(e1|e2) ≡ (!e1&!e2), but alas we do not
                // have &
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
            Regexp::CaseInsensitiveString(_) => unreachable!(), // only used for match's `chars`, never negated
            Regexp::VariableEqual(slot, value) => Regexp::NotVariableEqual(slot, value),
            Regexp::NotVariableEqual(_, _) => unreachable!(),
            Regexp::Group(regexp) => Regexp::Group(Box::new(regexp.negate())),
            Regexp::Empty => Regexp::Never,
            Regexp::Never => Regexp::Empty,
            Regexp::EndAnchor => Regexp::NotEndAnchor,
            Regexp::NotEndAnchor => Regexp::EndAnchor,
        }
    }

    /// Compile a regular expression and attach the given translation as a payload
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

    /// Whether *every* successful match of this regexp consumes no input at all.
    ///
    /// Used to guard `ZeroOrMore`/`OneOrMore`/`RepeatAtLeast` against infinite loops: their
    /// emitted bytecode repeats the body by jumping back to a `Split` that tries the body again
    /// before giving up, and the VM always explores the body's own preferred (leftmost) branch
    /// fully before backtracking — so if the body is always zero-width, every attempt resolves
    /// the same way, forever, without ever advancing `sp`. When that's the case, further
    /// repetitions can't add any matching power beyond the first attempt, so we cap the loop at
    /// one instead of building a self-referencing jump. A body that only *sometimes* matches
    /// zero-width (like the `EndAnchor` alternative above) is left with its normal unbounded
    /// loop — safe as long as it keeps making progress until it actually runs out of input,
    /// which is exactly when the zero-width alternative becomes the only option and the loop
    /// should stop; that per-iteration case isn't guarded here (see `TODO.org`'s "regexp
    /// infinite loop" entry), but no shipped liblouis table exercises it today.
    fn always_zero_width(&self) -> bool {
        match self {
            Regexp::Literal(_) | Regexp::Any | Regexp::CharacterClass(_) => false,
            Regexp::NotCharacterClass(_) => false,
            Regexp::Concat(left, right) => left.always_zero_width() && right.always_zero_width(),
            // both branches must be zero-width: the VM commits to whichever branch succeeds
            // first, and either one might be the one taken
            Regexp::Either(left, right) => left.always_zero_width() && right.always_zero_width(),
            Regexp::Optional(regexp)
            | Regexp::ZeroOrMore(regexp)
            | Regexp::OneOrMore(regexp)
            | Regexp::Group(regexp)
            | Regexp::Capture(regexp) => regexp.always_zero_width(),
            Regexp::RepeatExactly(0, _) => true,
            Regexp::RepeatExactly(_, regexp)
            | Regexp::RepeatAtLeast(_, regexp)
            | Regexp::RepeatAtLeastAtMost(_, _, regexp) => regexp.always_zero_width(),
            Regexp::String(s) | Regexp::NotString(s) | Regexp::CaseInsensitiveString(s) => {
                s.is_empty()
            }
            Regexp::VariableEqual(_, _) | Regexp::NotVariableEqual(_, _) => true,
            // genuine zero-width assertions
            Regexp::Empty | Regexp::EndAnchor | Regexp::NotEndAnchor => true,
            // `Never` doesn't succeed at all, so the VM's `Split` backs off immediately without
            // ever reaching the loop-back jump — it can't cause the infinite loop this guards
            // against, regardless of how it's classified here
            Regexp::Never => false,
        }
    }

    /// Emit byte code instructions for the RegExp AST and collect all character classes
    /// used in the RegExp.
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
            Regexp::ZeroOrMore(regexp) if regexp.always_zero_width() => {
                // see `always_zero_width`: repeating a zero-width-first body forever would never
                // make progress, so cap it at zero-or-one attempt instead of zero-or-more
                Regexp::Optional(regexp.clone()).emit(instructions, character_classes);
            }
            Regexp::ZeroOrMore(regexp) => {
                let pos = instructions.len();
                instructions.push(Instruction::Split(pos + 1, 0));
                regexp.emit(instructions, character_classes);
                instructions.push(Instruction::Jump(pos));
                instructions[pos] = Instruction::Split(pos + 1, instructions.len());
            }
            Regexp::OneOrMore(regexp) if regexp.always_zero_width() => {
                // see `always_zero_width`: cap at exactly one (mandatory) attempt instead of
                // looping forever on a zero-width-first body
                regexp.emit(instructions, character_classes);
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
                if regexp.always_zero_width() {
                    // see `always_zero_width`: the unbounded tail can't safely loop, so cap it
                    // at the mandatory `min` copies plus at most one more
                    Regexp::Optional(regexp.clone()).emit(instructions, character_classes);
                } else {
                    let pos = instructions.len();
                    instructions.push(Instruction::Split(pos + 1, 0));
                    regexp.emit(instructions, character_classes);
                    instructions.push(Instruction::Jump(pos));
                    instructions[pos] = Instruction::Split(pos + 1, instructions.len());
                }
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
            Regexp::CaseInsensitiveString(s) => {
                for c in s.chars() {
                    instructions.push(Instruction::CaseInsensitiveChar(c))
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
            Regexp::EndAnchor => instructions.push(Instruction::AssertEnd),
            Regexp::NotEndAnchor => instructions.push(Instruction::AssertMoreInput),
            Regexp::Never => instructions.push(Instruction::Fail),
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
    /// Match any character that is not char.
    NotChar(char),
    /// Match a single char case-insensitively — see [`Regexp::CaseInsensitiveString`]
    CaseInsensitiveChar(char),
    /// Match a set of chars. Contains a reference to the character class.
    Class(CharacterClassIndex),
    /// Match any character that is not in the set of chars.
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
    /// Test whether a variable is not equal to a value
    NotVariableEqual(VariableIndex, u8),
    /// Zero-width assertion: succeeds only if there is no input left
    AssertEnd,
    /// Zero-width assertion: succeeds only if there is still input left
    AssertMoreInput,
    /// Never matches, regardless of input
    Fail,
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

/// A single candidate execution path through the RegExp compiled program at the current
/// input position, carrying the capture span in progress along that path.
///
/// See the section "Pike's Implementation" at https://swtch.com/~rsc/regexp/regexp2.html
struct Thread {
    pc: InstructionIndex,
    capture: (usize, usize),
}

/// The threads alive at one input position, in priority order, i.e. the preference order between
/// alternatives that determines which one wins when both succeed (leftmost-first/Perl semantics, as
/// opposed to POSIX leftmost-longest which would pick differently).
struct ThreadList {
    threads: Vec<Thread>,
    /// Which `pc`s have already been added during the current step, to ensure a `pc` reachable by
    /// more than one epsilon path is only ever added once.
    seen: Vec<u32>,
    generation: u32,
}

impl ThreadList {
    fn new(program_len: usize) -> Self {
        Self {
            threads: Vec::new(),
            seen: vec![0; program_len],
            generation: 0,
        }
    }

    fn start_step(&mut self) {
        self.threads.clear();
        self.generation += 1;
    }
}

impl CompiledRegexp {
    /// Follow every epsilon transition reachable from `pc` without consuming input,
    /// adding each character-consuming or `Match` instruction reached to `list`, in
    /// priority order. A path whose assertion or variable check fails is simply dropped.
    fn add_thread(
        &self,
        list: &mut ThreadList,
        pc: InstructionIndex,
        capture: (usize, usize),
        sp: usize,
        input_len: usize,
        env: &Environment,
    ) {
        if list.seen[pc] == list.generation {
            return;
        }
        list.seen[pc] = list.generation;
        match self.instructions[pc] {
            Instruction::Jump(target) => self.add_thread(list, target, capture, sp, input_len, env),
            Instruction::Split(a, b) => {
                self.add_thread(list, a, capture, sp, input_len, env);
                self.add_thread(list, b, capture, sp, input_len, env);
            }
            Instruction::CaptureStart => self.add_thread(list, pc + 1, (sp, 0), sp, input_len, env),
            Instruction::CaptureEnd => {
                self.add_thread(list, pc + 1, (capture.0, sp), sp, input_len, env)
            }
            Instruction::VariableEqual(var, expected) => {
                if env.get(var) == Some(&expected) {
                    self.add_thread(list, pc + 1, capture, sp, input_len, env);
                }
            }
            Instruction::NotVariableEqual(var, expected) => {
                if env.get(var).is_some_and(|&actual| actual != expected) {
                    self.add_thread(list, pc + 1, capture, sp, input_len, env);
                }
            }
            Instruction::AssertEnd => {
                if sp == input_len {
                    self.add_thread(list, pc + 1, capture, sp, input_len, env);
                }
            }
            Instruction::AssertMoreInput => {
                if sp != input_len {
                    self.add_thread(list, pc + 1, capture, sp, input_len, env);
                }
            }
            Instruction::Fail => (),
            // Add a Thread for character consuming instructions. The epsilon closure ends here.
            _ => list.threads.push(Thread { pc, capture }),
        }
    }

    /// Does the input match the regular expression in the given environment?
    pub fn is_match(&self, input: &str, env: &Environment) -> bool {
        self.find(input, env).is_some()
    }

    /// If the input matches the regular expression in the given environment return the
    /// associated translation, otherwise return None.
    pub fn find(&self, input: &str, env: &Environment) -> Option<ResolvedTranslation> {
        let mut current = ThreadList::new(self.instructions.len());
        let mut next = ThreadList::new(self.instructions.len());
        let mut sp = 0;
        let mut length = 0;
        // The capture span, consumed-char count and translation of the best match found so far. A
        // lower-priority thread reaching `Match` doesn't end the search immediately: a still-alive
        // higher-priority thread might go on to match later
        let mut matched: Option<((usize, usize), usize, TranslationIndex)> = None;

        current.start_step();
        self.add_thread(&mut current, 0, (0, 0), sp, input.len(), env);

        while !current.threads.is_empty() {
            let next_char = input[sp..].chars().next();
            next.start_step();
            for thread in &current.threads {
                match self.instructions[thread.pc] {
                    Instruction::Match(index) => {
                        matched = Some((thread.capture, length, index));
                        // every remaining thread in this step is lower priority than the one
                        // that just matched, and so could never improve on it
                        break;
                    }
                    Instruction::Char(expected) => {
                        if next_char == Some(expected) {
                            self.add_thread(
                                &mut next,
                                thread.pc + 1,
                                thread.capture,
                                sp + expected.len_utf8(),
                                input.len(),
                                env,
                            );
                        }
                    }
                    Instruction::NotChar(expected) => {
                        if let Some(actual) = next_char
                            && actual != expected
                        {
                            self.add_thread(
                                &mut next,
                                thread.pc + 1,
                                thread.capture,
                                sp + actual.len_utf8(),
                                input.len(),
                                env,
                            );
                        }
                    }
                    Instruction::CaseInsensitiveChar(expected) => {
                        if let Some(actual) = next_char
                            && chars_match_case_insensitive(actual, expected)
                        {
                            self.add_thread(
                                &mut next,
                                thread.pc + 1,
                                thread.capture,
                                sp + actual.len_utf8(),
                                input.len(),
                                env,
                            );
                        }
                    }
                    Instruction::Class(index) => {
                        if let Some(actual) = next_char
                            && self.character_classes[index].contains(&actual)
                        {
                            self.add_thread(
                                &mut next,
                                thread.pc + 1,
                                thread.capture,
                                sp + actual.len_utf8(),
                                input.len(),
                                env,
                            );
                        }
                    }
                    Instruction::NotClass(index) => {
                        if let Some(actual) = next_char
                            && !self.character_classes[index].contains(&actual)
                        {
                            self.add_thread(
                                &mut next,
                                thread.pc + 1,
                                thread.capture,
                                sp + actual.len_utf8(),
                                input.len(),
                                env,
                            );
                        }
                    }
                    Instruction::Any => {
                        if let Some(actual) = next_char {
                            self.add_thread(
                                &mut next,
                                thread.pc + 1,
                                thread.capture,
                                sp + actual.len_utf8(),
                                input.len(),
                                env,
                            );
                        }
                    }
                    _ => unreachable!(
                        "add_thread only ever adds Char/NotChar/CaseInsensitiveChar/Class/NotClass/Any/Match instructions to a thread list"
                    ),
                }
            }
            let Some(c) = next_char else { break };
            std::mem::swap(&mut current, &mut next);
            sp += c.len_utf8();
            length += 1;
        }

        matched.map(|((start, end), length, index)| {
            let capture = &input[start..end];
            // offset is in number of chars not a byte offset
            let offset = input[..start].chars().count();
            self.translations[index]
                .clone()
                .resolve(capture, length, offset)
        })
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
    fn catastrophic_backtracking_pattern_is_bounded() {
        // Regression test for "Denial of Service via Regex Catastrophic Backtracking on Custom
        // Tables" (Shielder/OSTIF security audit, April 2026): `(a+)+b` matched against a long
        // run of `a`s with no trailing `b` took exponential time under the old recursive
        // backtracking matcher (confirmed to still be running after an 8s timeout on a 43-char
        // input); the thread-list VM this replaced it with bounds the whole match to
        // `instructions * input length`, regardless of how many ways the input could be
        // partitioned between the nested quantifiers.
        let env = Environment::new();
        let re = Regexp::Concat(
            Box::new(Regexp::OneOrMore(Box::new(Regexp::OneOrMore(Box::new(
                Regexp::Literal('a'),
            ))))),
            Box::new(Regexp::Literal('b')),
        )
        .compile();
        let input = "a".repeat(10_000);
        let start = std::time::Instant::now();
        assert!(!re.is_match(&input, &env));
        assert!(start.elapsed() < std::time::Duration::from_secs(1));
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
