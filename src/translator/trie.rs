use std::collections::HashMap;

use crate::translator::boundaries::{word_end, word_start};

use super::Translation;

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Boundary {
    Word,
    NotWord,
    Number,
    None,
}

#[derive(Debug, PartialEq, Eq, Hash)]
enum Transition {
    Character(char),
    Start(Boundary),
    End(Boundary),
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

        current_node.translation = Some(Translation { from, to, length });
    }

    fn find_translations_from_node<'a>(
        &'a self,
        input: &str,
        node: &'a TrieNode,
    ) -> Vec<&Translation> {
        let mut current_node = node;
        let mut matching_rules = Vec::new();
        let mut prev: Option<char> = None;
        let mut chars = input.chars();

        while let Some(c) = chars.next() {
            if let Some(node) = current_node.char_transition(c) {
                current_node = node;
                if let Some(ref translation) = node.translation {
                    matching_rules.push(translation)
                }
            } else if let Some(node) = current_node.word_end_transition() {
                current_node = node;
                if word_end(prev, Some(c)) {
                    if let Some(ref translation) = node.translation {
                        matching_rules.push(translation)
                    }
                }
            } else if let Some(node) = current_node.not_word_end_transition() {
                current_node = node;
                if !word_end(prev, Some(c)) {
                    if let Some(ref translation) = node.translation {
                        matching_rules.push(translation)
                    }
                }
            } else {
                prev = Some(c);
                break;
            }
            prev = Some(c);
        }
        if let Some(node) = current_node.word_end_transition() {
            if word_end(prev, chars.next()) {
                if let Some(ref translation) = node.translation {
                    matching_rules.push(translation)
                }
            }
        } else if let Some(node) = current_node.not_word_end_transition() {
            if !word_end(prev, chars.next()) {
                if let Some(ref translation) = node.translation {
                    matching_rules.push(translation)
                }
            }
        }
        matching_rules
    }

    pub fn find_translations(&self, input: &str, before: Option<char>) -> Vec<&Translation> {
        let mut matching_rules = Vec::new();

        if word_start(before, input.chars().next()) {
            if let Some(node) = self.root.word_start_transition() {
                matching_rules = self.find_translations_from_node(input, node);
            }
        } else {
            if let Some(node) = self.root.not_word_start_transition() {
                matching_rules = self.find_translations_from_node(input, node);
            }
        }

        matching_rules.extend(self.find_translations_from_node(input, &self.root));
        matching_rules.sort_by_key(|translation| translation.length);
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
            Vec::<&Translation>::new()
        );
    }

    #[test]
    fn find_translations_test() {
        let mut trie = Trie::new();
        let empty = Vec::<&Translation>::new();
        let a = Translation {
            from: "a".into(),
            to: "A".into(),
	    length: 1,
        };
        let f = Translation {
            from: "f".into(),
            to: "F".into(),
	    length: 1,
        };
        let fo = Translation {
            from: "fo".into(),
            to: "FO".into(),
	    length: 2,
        };
        let foo = Translation {
            from: "foo".into(),
            to: "FOO".into(),
	    length: 3,
        };
        let foobar = Translation {
            from: "foobar".into(),
            to: "FOOBAR".into(),
	    length: 6,
        };
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
        assert_eq!(trie.find_translations("a", None), vec![&a]);
        assert_eq!(trie.find_translations("f", None), vec![&f]);
        assert_eq!(trie.find_translations("fo", None), vec![&f, &fo]);
        assert_eq!(
            trie.find_translations("foobar", None),
            vec![&f, &fo, &foo, &foobar]
        );
        assert_eq!(trie.find_translations("foobaz", None), vec![&f, &fo, &foo]);
        assert_eq!(trie.find_translations("function", None), vec![&f]);
        assert_eq!(trie.find_translations("folio", None), vec![&f, &fo]);
        assert_eq!(trie.find_translations("none", None), empty);
        assert_eq!(
            trie.find_translations("The start of a long long sentence", None),
            empty
        );
    }

    #[test]
    fn find_translations_with_boundaries_test() {
        let mut trie = Trie::new();
        let empty = Vec::<&Translation>::new();
        let a = Translation {
            from: "a".into(),
            to: "A".into(),
	    length: 3,
        };
        trie.insert("a".into(), "A".into(), Boundary::Word, Boundary::Word);
        assert_eq!(trie.find_translations("a", None), vec![&a]);
        assert_eq!(trie.find_translations("aha", None), empty);
    }

    #[test]
    fn find_translations_with_negative_boundary_after_test() {
        let mut trie = Trie::new();
        let empty = Vec::<&Translation>::new();
        let foo = Translation {
            from: "foo".into(),
            to: "FOO".into(),
	    length: 5,
        };
        trie.insert(
            "foo".into(),
            "FOO".into(),
            Boundary::Word,
            Boundary::NotWord,
        );
        assert_eq!(trie.find_translations("foo", None), empty);
        assert_eq!(trie.find_translations("foo ", None), empty);
        assert_eq!(trie.find_translations("foobar", None), vec![&foo]);
        assert_eq!(trie.find_translations("foo.", None), empty);
    }

    #[test]
    fn find_translations_with_negative_boundary_before_test() {
        let mut trie = Trie::new();
        let empty = Vec::<&Translation>::new();
        let foo = Translation {
            from: "foo".into(),
            to: "FOO".into(),
	    length: 4,
        };
        trie.insert(
            "foo".into(),
            "FOO".into(),
            Boundary::NotWord,
            Boundary::None,
        );
        assert_eq!(trie.find_translations("foo", None), empty);
        assert_eq!(trie.find_translations("foo", Some(' ')), empty);
        assert_eq!(trie.find_translations("foo", Some('.')), empty);
        assert_eq!(trie.find_translations("foo", Some('c')), vec![&foo]);
    }

    #[test]
    fn find_translations_with_negative_boundaries_test() {
        let mut trie = Trie::new();
        let empty = Vec::<&Translation>::new();
        let foo = Translation {
            from: "foo".into(),
            to: "FOO".into(),
	    length: 5,
        };
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
        assert_eq!(trie.find_translations("foobar", Some('c')), vec![&foo]);
    }
}
