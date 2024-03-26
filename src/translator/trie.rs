use std::collections::HashMap;

use crate::translator::boundaries::{word_end, word_start};

use super::Translation;

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Boundary {
    Word,
    Number,
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

    pub fn insert(&mut self, word: &str, translation: Translation) {
        let mut current_node = &mut self.root;

        for c in word.chars() {
            current_node = current_node
                .transitions
                .entry(Transition::Character(c))
                .or_default();
        }
        current_node.translation = Some(translation);
    }

    pub fn insert_with_boundary(
        &mut self,
        word: &str,
        translation: Translation,
        before: Option<Boundary>,
        after: Option<Boundary>,
    ) {
        let mut current_node = &mut self.root;

        if let Some(boundary) = before {
            current_node = current_node
                .transitions
                .entry(Transition::Start(boundary))
                .or_default();
        }

        for c in word.chars() {
            current_node = current_node
                .transitions
                .entry(Transition::Character(c))
                .or_default();
        }

        if let Some(boundary) = after {
            current_node = current_node
                .transitions
                .entry(Transition::End(boundary))
                .or_default();
        }

        current_node.translation = Some(translation);
    }

    fn find_translations_from_node<'a>(
        &'a self,
        word: &str,
        node: &'a TrieNode,
        length: u32,
    ) -> Vec<(u32, &Translation)> {
        let mut current_node = node;
        let mut matching_rules = Vec::new();
        let mut prev: Option<char> = None;
        let mut chars = word.chars();
        let mut length = length;

        while let Some(c) = chars.next() {
            if let Some(node) = current_node.transitions.get(&Transition::Character(c)) {
                current_node = node;
                length += 1;
                if let Some(ref translation) = node.translation {
                    matching_rules.push((length, translation))
                }
            } else if let Some(node) = current_node
                .transitions
                .get(&Transition::End(Boundary::Word))
            {
                current_node = node;
                length += 1;
                if word_end(prev, Some(c)) {
                    if let Some(ref translation) = node.translation {
                        matching_rules.push((length, translation))
                    }
                }
            } else {
                prev = Some(c);
                break;
            }
            prev = Some(c);
        }
        if let Some(node) = current_node
            .transitions
            .get(&Transition::End(Boundary::Word))
        {
            length += 1;
            if word_end(prev, chars.next()) {
                if let Some(ref translation) = node.translation {
                    matching_rules.push((length, translation))
                }
            }
        }
        matching_rules
    }

    pub fn find_translations(&self, word: &str, before: Option<char>) -> Vec<&Translation> {
        let mut matching_rules = Vec::new();

        if word_start(before, word.chars().next()) {
            if let Some(node) = self
		.root
		.transitions
		.get(&Transition::Start(Boundary::Word))
            {
                matching_rules = self.find_translations_from_node(word, node, 1);
            }
        }

        matching_rules.extend(self.find_translations_from_node(word, &self.root, 0));
        matching_rules.sort_by_key(|(length, _translation)| *length);
        matching_rules
            .iter()
            .map(|(_, translation)| *translation)
            .collect()
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
        };
        let f = Translation {
            from: "f".into(),
            to: "F".into(),
        };
        let fo = Translation {
            from: "fo".into(),
            to: "FO".into(),
        };
        let foo = Translation {
            from: "foo".into(),
            to: "FOO".into(),
        };
        let foobar = Translation {
            from: "foobar".into(),
            to: "FOOBAR".into(),
        };
        trie.insert("a", a.clone());
        trie.insert("f", f.clone());
        trie.insert("fo", fo.clone());
        trie.insert("foo", foo.clone());
        trie.insert("foobar", foobar.clone());
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
        };
        trie.insert_with_boundary("a", a.clone(), Some(Boundary::Word), Some(Boundary::Word));
        assert_eq!(trie.find_translations("a", None), vec![&a]);
        assert_eq!(trie.find_translations("aha", None), empty);
    }
}
