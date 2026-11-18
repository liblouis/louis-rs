//! Which of a stage's compiled patterns are worth trying at an input position.
//!
//! `match` and `context` rules are tried at every position of the input, and a table can
//! hold hundreds of them. Almost every pattern can only begin with a known set of
//! characters, so indexing the patterns by those characters skips most of them without
//! running the VM at all. The index is a superset: it never drops a pattern that could
//! match, so it changes how fast a stage finds its candidates, not which it finds.

use std::collections::HashMap;

use super::{Bitset, CompiledRegexp, Instruction};

/// The patterns of one stage, by the first character of the input they could match.
#[derive(Debug, Default)]
pub struct FirstCharIndex {
    /// Per [`lowercase`] first character, the patterns that can start with it plus all of
    /// `unfiltered`, ascending.
    by_first_char: HashMap<char, Vec<usize>>,
    /// Patterns whose first character can't be narrowed down, ascending.
    unfiltered: Vec<usize>,
}

impl FirstCharIndex {
    pub fn new<'a>(regexps: impl IntoIterator<Item = &'a CompiledRegexp>) -> Self {
        let mut index = FirstCharIndex::default();
        for (i, regexp) in regexps.into_iter().enumerate() {
            match regexp.first_chars() {
                None => index.unfiltered.push(i),
                Some(chars) => {
                    for c in chars {
                        index.by_first_char.entry(lowercase(c)).or_default().push(i);
                    }
                }
            }
        }
        // Make each bucket the complete answer for its key, so `candidates` is one lookup:
        // - add the unfiltered patterns, which can start with any character
        // - sort back into pattern order, the order callers got before there was an
        //   index
        // - drop repeats, from a pattern with two first characters with the same
        //   lowercase, like a class `{a, A}`
        for bucket in index.by_first_char.values_mut() {
            bucket.extend(&index.unfiltered);
            bucket.sort_unstable();
            bucket.dedup();
        }
        index
    }

    /// The patterns that could match at the start of `input`, ascending.
    pub fn candidates(&self, input: &str) -> &[usize] {
        input
            .chars()
            .next()
            .and_then(|c| self.by_first_char.get(&lowercase(c)))
            .unwrap_or(&self.unfiltered)
    }
}

/// The key a character is indexed and looked up under. Lowercase, because
/// `CaseInsensitiveChar` matches an input character that lowercases to it; using it on
/// both sides keeps the index a superset. A multi-character lowercase (`İ` to `i̇`) keeps
/// only its first character, which is still the same key on both sides.
fn lowercase(c: char) -> char {
    c.to_lowercase().next().unwrap_or(c)
}

impl CompiledRegexp {
    /// Every character this program can begin with, or `None` when that can't be
    /// narrowed down. Walks the epsilon closure of `pc` 0 like `add_thread`, but follows
    /// assertions and variable tests rather than evaluating them, which can only widen the
    /// set. Reaching `Match` (the program matches the empty string) or `Any`, `NotChar`,
    /// `NotClass` (no finite set lists what they accept) gives `None`.
    fn first_chars(&self) -> Option<Vec<char>> {
        let mut chars = Vec::new();
        let mut seen = Bitset::new(self.instructions.len());
        let mut stack = vec![0];
        while let Some(pc) = stack.pop() {
            if seen.get(pc) {
                continue;
            }
            seen.insert(pc);
            match self.instructions[pc] {
                Instruction::Jump(target) => stack.push(target),
                Instruction::Split(a, b) => stack.extend([a, b]),
                Instruction::CaptureStart
                | Instruction::CaptureEnd
                | Instruction::ConsumeStart
                | Instruction::ConsumeEnd
                | Instruction::VariableCompare(..)
                | Instruction::AssertEnd
                | Instruction::AssertMoreInput
                | Instruction::AssertStart
                | Instruction::AssertNotStart => stack.push(pc + 1),
                Instruction::Char(c) | Instruction::CaseInsensitiveChar(c) => chars.push(c),
                Instruction::Class(index) => chars.extend(self.character_classes[index].members()),
                Instruction::Fail => (),
                Instruction::Match(_)
                | Instruction::Any
                | Instruction::NotChar(_)
                | Instruction::NotClass(_) => return None,
            }
        }
        Some(chars)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::translator::regexp::Regexp;
    use std::collections::HashSet;

    fn compile(r: Regexp) -> CompiledRegexp {
        r.compile_with_payload(Default::default())
    }

    #[test]
    fn candidates_keep_pattern_order() {
        let index = FirstCharIndex::new(&[
            compile(Regexp::String("a".into())),
            compile(Regexp::Any),
            compile(Regexp::CharacterClass(HashSet::from(['a', 'b']))),
            compile(Regexp::String("b".into())),
        ]);
        assert_eq!(index.candidates("abc"), &[0, 1, 2]);
        assert_eq!(index.candidates("Bc"), &[1, 2, 3]);
        assert_eq!(index.candidates("x"), &[1]);
        assert_eq!(index.candidates(""), &[1]);
    }

    fn sorted(chars: Option<Vec<char>>) -> Option<Vec<char>> {
        chars.map(|mut chars| {
            chars.sort_unstable();
            chars
        })
    }

    #[test]
    fn first_chars_walks_the_epsilon_closure() {
        let optional_a = || Box::new(Regexp::Optional(Box::new(Regexp::String("a".into()))));
        let then_b = Regexp::Concat(optional_a(), Box::new(Regexp::String("b".into())));
        assert_eq!(sorted(compile(then_b).first_chars()), Some(vec!['a', 'b']));
        // matches the empty string, so any input could start a match
        assert_eq!(compile(*optional_a()).first_chars(), None);
    }

    #[test]
    fn case_insensitive_pattern_is_found_by_uppercase_input() {
        let index = FirstCharIndex::new(&[compile(Regexp::CaseInsensitiveString("ab".into()))]);
        assert_eq!(index.candidates("AB"), &[0]);
        assert_eq!(index.candidates("ab"), &[0]);
        assert_eq!(index.candidates("b"), &[] as &[usize]);
    }
}
