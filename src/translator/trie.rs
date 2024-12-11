use std::collections::HashMap;

use crate::parser::{Pattern, Patterns};
use crate::translator::boundaries::{number_word, word_end, word_number, word_start};

use super::Translation;

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Boundary {
    Word,
    NotWord,
    Number,
    NumberWord,
    WordNumber,
    PrePattern,
    None,
}

#[derive(Debug, PartialEq, Eq, Hash)]
enum Transition {
    Character(char),
    Start(Boundary),
    End(Boundary),
    Any,
}

#[derive(Default, Debug)]
struct TrieNode {
    translation: Option<Translation>,
    transitions: HashMap<Transition, TrieNode>,
}

impl TrieNode {
    fn char_transition(&self, c: char) -> Option<&TrieNode> {
        self.transitions.get(&Transition::Character(c))
    }
    fn char_case_insensitive_transition(&self, c: char) -> Option<&TrieNode> {
        // if the character is already lowercase we can ignore it
        if c.is_lowercase() {
            return None;
        };
        // FIXME: we ignore characters that do not map to a single
        // character lowercase character
        if c.to_lowercase().count() != 1 {
            return None;
        };
        let lowercase = c.to_lowercase().next().unwrap();
        self.char_transition(lowercase)
    }
    fn any_transition(&self) -> Option<&TrieNode> {
        self.transitions.get(&Transition::Any)
    }
    fn word_start_transition(&self) -> Option<&TrieNode> {
        self.transitions.get(&Transition::Start(Boundary::Word))
    }
    fn not_word_start_transition(&self) -> Option<&TrieNode> {
        self.transitions.get(&Transition::Start(Boundary::NotWord))
    }
    fn word_end_transition(&self) -> Option<&TrieNode> {
        self.transitions.get(&Transition::End(Boundary::Word))
    }
    fn not_word_end_transition(&self) -> Option<&TrieNode> {
        self.transitions.get(&Transition::End(Boundary::NotWord))
    }
    fn word_num_transition(&self) -> Option<&TrieNode> {
        self.transitions.get(&Transition::End(Boundary::WordNumber))
    }
    fn num_word_transition(&self) -> Option<&TrieNode> {
        self.transitions
            .get(&Transition::Start(Boundary::NumberWord))
    }
    fn pre_end_transition(&self) -> Option<&TrieNode> {
        self.transitions.get(&Transition::End(Boundary::PrePattern))
    }
}

#[derive(Default, Debug)]
pub struct Trie {
    root: TrieNode,
}

impl Trie {
    pub fn new() -> Self {
        Trie {
            root: TrieNode::default(),
        }
    }

    pub fn insert(&mut self, from: String, to: String, before: Boundary, after: Boundary) {
        let mut current_node = &mut self.root;
        let mut length = from.chars().count();

        if before != Boundary::None {
            length += 1;
            current_node = current_node
                .transitions
                .entry(Transition::Start(before))
                .or_default();
        }

        for c in from.chars() {
            current_node = current_node
                .transitions
                .entry(Transition::Character(c))
                .or_default();
        }

        if after != Boundary::None {
            length += 1;
            current_node = current_node
                .transitions
                .entry(Transition::End(after))
                .or_default();
        }

        if cfg!(feature = "backwards_compatibility") {
            // first rule wins
            if current_node.translation.is_none() {
                current_node.translation = Some(Translation::new(from, to, length));
            }
        } else {
            // last rule wins
            current_node.translation = Some(Translation::new(from, to, length));
        }
    }

    pub fn insert_match(&mut self, from: String, to: String, pre: &Patterns, post: &Patterns) {
        let mut current_node = &mut self.root;
        let mut length = from.chars().count();

        for p in pre.into_iter() {
            match p {
                Pattern::Characters(s) => {
                    for c in s.chars() {
                        current_node = current_node
                            .transitions
                            .entry(Transition::Character(c))
                            .or_default();
                        length += 1;
                    }
                }
                Pattern::Any => {
                    current_node = current_node.transitions.entry(Transition::Any).or_default();
                    length += 1;
                }
                Pattern::Set(chars) => {
                    for c in chars {
                        current_node = current_node
                            .transitions
                            .entry(Transition::Character(*c))
                            .or_default();
                        length += 1;
                    }
                }
                _ => (),
            }
        }

        // add an epsilon transition to mark the end of the
        // pre-pattern. We need to know where the real match starts
        current_node = current_node
            .transitions
            .entry(Transition::End(Boundary::PrePattern))
            .or_default();

        for c in from.chars() {
            current_node = current_node
                .transitions
                .entry(Transition::Character(c))
                .or_default();
        }

        for p in post {
            match p {
                Pattern::Characters(s) => {
                    for c in s.chars() {
                        current_node = current_node
                            .transitions
                            .entry(Transition::Character(c))
                            .or_default();
                        length += 1;
                    }
                }
                Pattern::Any => {
                    current_node = current_node.transitions.entry(Transition::Any).or_default();
                    length += 1;
                }
                Pattern::Set(chars) => {
                    for c in chars {
                        current_node = current_node
                            .transitions
                            .entry(Transition::Character(*c))
                            .or_default();
                        length += 1;
                    }
                }
                _ => (),
            }
        }
        if cfg!(feature = "backwards_compatibility") {
            // first rule wins
            if current_node.translation.is_none() {
                current_node.translation = Some(Translation::new(from, to, length));
            }
        } else {
            // last rule wins
            current_node.translation = Some(Translation::new(from, to, length));
        }
    }

    fn find_translations_from_node(
        &self,
        input: &str,
        prev: Option<char>,
        node: &TrieNode,
        match_length: usize,
        offset: usize,
    ) -> Vec<Translation> {
        let mut matching_rules = Vec::new();
        let mut chars = input.chars();

        // if this node has a translation add it to the list of matching rules
        if let Some(ref translation) = node.translation {
            let translation = translation
                .clone()
                .with_offset(offset)
                // if there is an offset (typically in a match opcode), the weight needs
                // to be calculated at run-time. The weight is the actual length of match.
                .with_weight_if_offset(match_length, offset);
            matching_rules.push(translation)
        }
        let c = chars.next();
        if let Some(c) = c {
            let bytes = c.len_utf8();
            if let Some(node) = node.char_transition(c) {
                matching_rules.extend(self.find_translations_from_node(
                    &input[bytes..],
                    Some(c),
                    node,
                    match_length + 1,
                    offset,
                ));
            } else if let Some(node) = node.char_case_insensitive_transition(c) {
                matching_rules.extend(self.find_translations_from_node(
                    &input[bytes..],
                    Some(c),
                    node,
                    match_length + 1,
                    offset,
                ));
            } else if let Some(node) = node.any_transition() {
                matching_rules.extend(self.find_translations_from_node(
		    &input[bytes..],
		    Some(c),
		    node,
		    match_length + 1,
		    offset,
                ));
            }
        }
        if let Some(node) = node.word_start_transition() {
            if word_start(prev, c) {
                matching_rules.extend(self.find_translations_from_node(
                    &input[..],
                    prev,
                    node,
                    match_length,
                    offset,
                ));
            }
        }
        if let Some(node) = node.not_word_start_transition() {
            if !word_start(prev, c) {
                matching_rules.extend(self.find_translations_from_node(
                    &input[..],
                    prev,
                    node,
                    match_length,
                    offset,
                ));
            }
        }
        if let Some(node) = node.word_end_transition() {
            if word_end(prev, c) {
                matching_rules.extend(self.find_translations_from_node(
                    &input[..],
                    prev,
                    node,
                    match_length,
                    offset,
                ));
            }
        }
        if let Some(node) = node.not_word_end_transition() {
            if !word_end(prev, c) {
                matching_rules.extend(self.find_translations_from_node(
                    &input[..],
                    prev,
                    node,
                    match_length,
                    offset,
                ));
            }
        }
        if let Some(node) = node.word_num_transition() {
            if word_number(prev, c) {
                matching_rules.extend(self.find_translations_from_node(
                    &input[..],
                    prev,
                    node,
                    match_length,
                    offset,
                ));
            }
        }
        if let Some(node) = node.num_word_transition() {
            if number_word(prev, c) {
                matching_rules.extend(self.find_translations_from_node(
                    &input[..],
                    prev,
                    node,
                    match_length,
                    offset,
                ));
            }
        }
        if let Some(node) = node.pre_end_transition() {
            matching_rules.extend(self.find_translations_from_node(
                &input[..],
                prev,
                node,
                match_length,
                match_length,
            ));
        }
        matching_rules
    }

    pub fn find_translations(&self, input: &str, prev: Option<char>) -> Vec<Translation> {
        let mut matching_rules = Vec::new();

        matching_rules.extend(self.find_translations_from_node(input, prev, &self.root, 0, 0));
        //matching_rules.sort_by_key(|translation| translation.weight);
        matching_rules
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_trie_test() {
        let trie = Trie::new();
        assert_eq!(
            trie.find_translations("foo", None),
            Vec::<Translation>::new()
        );
    }

    #[test]
    fn find_translations_test() {
        let mut trie = Trie::new();
        let empty = Vec::<Translation>::new();
        let a = Translation::new("a".into(), "A".into(), 1);
        let f = Translation::new("f".into(), "F".into(), 1);
        let fo = Translation::new("fo".into(), "FO".into(), 2);
        let foo = Translation::new("foo".into(), "FOO".into(), 3);
        let foobar = Translation::new("foobar".into(), "FOOBAR".into(), 6);
        trie.insert("a".into(), "A".into(), Boundary::None, Boundary::None);
        trie.insert("f".into(), "F".into(), Boundary::None, Boundary::None);
        trie.insert("fo".into(), "FO".into(), Boundary::None, Boundary::None);
        trie.insert("foo".into(), "FOO".into(), Boundary::None, Boundary::None);
        trie.insert(
            "foobar".into(),
            "FOOBAR".into(),
            Boundary::None,
            Boundary::None,
        );
        assert_eq!(trie.find_translations("a", None), vec![a]);
        assert_eq!(trie.find_translations("f", None), vec![f.clone()]);
        assert_eq!(
            trie.find_translations("fo", None),
            vec![f.clone(), fo.clone()]
        );
        assert_eq!(
            trie.find_translations("foobar", None),
            vec![f.clone(), fo.clone(), foo.clone(), foobar.clone()]
        );
        assert_eq!(
            trie.find_translations("foobaz", None),
            vec![f.clone(), fo.clone(), foo.clone()]
        );
        assert_eq!(trie.find_translations("function", None), vec![f.clone()]);
        assert_eq!(
            trie.find_translations("folio", None),
            vec![f.clone(), fo.clone()]
        );
        assert_eq!(trie.find_translations("none", None), empty);
        assert_eq!(
            trie.find_translations("The start of a long long sentence", None),
            empty
        );
    }

    #[test]
    fn find_translations_with_boundaries_test() {
        let mut trie = Trie::new();
        let empty = Vec::<Translation>::new();
        let a = Translation::new("a".into(), "A".into(), 3);
        trie.insert("a".into(), "A".into(), Boundary::Word, Boundary::Word);
        assert_eq!(trie.find_translations("a", None), vec![a]);
        assert_eq!(trie.find_translations("aha", None), empty);
    }

    #[test]
    fn find_translations_with_negative_boundary_after_test() {
        let mut trie = Trie::new();
        let empty = Vec::<Translation>::new();
        let foo = Translation::new("foo".into(), "FOO".into(), 5);
        trie.insert(
            "foo".into(),
            "FOO".into(),
            Boundary::Word,
            Boundary::NotWord,
        );
        assert_eq!(trie.find_translations("foo", None), empty);
        assert_eq!(trie.find_translations("foo ", None), empty);
        assert_eq!(trie.find_translations("foobar", None), vec![foo]);
        assert_eq!(trie.find_translations("foo.", None), empty);
    }

    #[test]
    fn find_translations_with_negative_boundary_before_test() {
        let mut trie = Trie::new();
        let empty = Vec::<Translation>::new();
        let foo = Translation::new("foo".into(), "FOO".into(), 4);
        trie.insert(
            "foo".into(),
            "FOO".into(),
            Boundary::NotWord,
            Boundary::None,
        );
        assert_eq!(trie.find_translations("foo", None), empty);
        assert_eq!(trie.find_translations("foo", Some(' ')), empty);
        assert_eq!(trie.find_translations("foo", Some('.')), empty);
        assert_eq!(trie.find_translations("foo", Some('c')), vec![foo]);
    }

    #[test]
    fn find_translations_with_negative_boundaries_test() {
        let mut trie = Trie::new();
        let empty = Vec::<Translation>::new();
        let foo = Translation::new("foo".into(), "FOO".into(), 5);
        trie.insert(
            "foo".into(),
            "FOO".into(),
            Boundary::NotWord,
            Boundary::NotWord,
        );
        assert_eq!(trie.find_translations("foo", None), empty);
        assert_eq!(trie.find_translations("foo", Some(' ')), empty);
        assert_eq!(trie.find_translations("foo", Some('.')), empty);
        assert_eq!(trie.find_translations("foo", Some('c')), empty);
        assert_eq!(trie.find_translations("foobar", Some('c')), vec![foo]);
    }

    #[test]
    fn find_translations_with_word_num_boundary() {
        let mut trie = Trie::new();
        let empty = Vec::<Translation>::new();
        let foo = Translation::new("aaa".into(), "A".into(), 5);
        trie.insert(
            "aaa".into(),
            "A".into(),
            Boundary::Word,
            Boundary::WordNumber,
        );
        assert_eq!(trie.find_translations("aaa", None), empty);
        assert_eq!(trie.find_translations("aaa1", Some(' ')), vec![foo.clone()]);
        assert_eq!(trie.find_translations("aaa1", Some('.')), vec![foo.clone()]);
        assert_eq!(trie.find_translations("aaa1", Some('c')), empty);
    }

    #[test]
    fn find_translations_with_num_word_boundary() {
        let mut trie = Trie::new();
        let empty = Vec::<Translation>::new();
        let foo = Translation::new("st".into(), "S".into(), 4);
        trie.insert(
            "st".into(),
            "S".into(),
            Boundary::NumberWord,
            Boundary::Word,
        );
        assert_eq!(trie.find_translations("st", None), empty);
        assert_eq!(trie.find_translations("st", Some(' ')), empty);
        assert_eq!(trie.find_translations("st", Some('.')), empty);
        assert_eq!(trie.find_translations("st", Some('1')), vec![foo]);
        assert_eq!(trie.find_translations("sta", Some('1')), empty);
    }

    #[test]
    fn find_translations_case_insensitive_test() {
        let mut trie = Trie::new();
        let foo = Translation::new("foo".into(), "FOO".into(), 3);
        trie.insert("foo".into(), "FOO".into(), Boundary::None, Boundary::None);
        assert_eq!(trie.find_translations("foo", None), vec![foo.clone()]);
        assert_eq!(trie.find_translations("Foo", None), vec![foo.clone()]);
        assert_eq!(trie.find_translations("FOO", None), vec![foo.clone()]);
        assert_eq!(trie.find_translations("foO", None), vec![foo.clone()]);
    }
}
