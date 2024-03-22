use std::collections::HashMap;

use super::Translation;

#[derive(Debug, PartialEq, Eq, Hash)]
enum Transition {
    Character(char),
    WordStart,
    WordEnd,
    NumberStart,
    NumberEnd,
}

#[derive(Debug)]
enum Boundary {
    Word,
    Number,
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
        start_boundary: Option<Boundary>,
        end_boundary: Option<Boundary>,
    ) {
        let mut current_node = &mut self.root;

        let start_transition = match start_boundary {
            Some(Boundary::Word) => Some(Transition::WordStart),
            Some(Boundary::Number) => Some(Transition::NumberStart),
            None => None,
        };

        if let Some(transition) = start_transition {
            current_node = current_node.transitions.entry(transition).or_default();
        }

        for c in word.chars() {
            current_node = current_node
                .transitions
                .entry(Transition::Character(c))
                .or_default();
        }

        let end_transition = match end_boundary {
            Some(Boundary::Word) => Some(Transition::WordEnd),
            Some(Boundary::Number) => Some(Transition::NumberEnd),
            None => None,
        };

        if let Some(transition) = end_transition {
            current_node = current_node.transitions.entry(transition).or_default();
        }

        current_node.translation = Some(translation);
    }

    pub fn find_translations(&self, word: &str) -> Vec<&Translation> {
        let mut current_node = &self.root;
        let mut matching_rules = Vec::new();

        for c in word.chars() {
            match current_node.transitions.get(&Transition::Character(c)) {
                Some(node) => {
                    current_node = node;
                    if let Some(ref translation) = node.translation {
                        matching_rules.push(translation)
                    }
                }
                None => return matching_rules,
            }
        }
        matching_rules
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_trie_test() {
        let trie = Trie::new();
        assert_eq!(trie.find_translations("foo"), Vec::<&Translation>::new());
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
        assert_eq!(trie.find_translations("a"), vec![&a]);
        assert_eq!(trie.find_translations("f"), vec![&f]);
        assert_eq!(trie.find_translations("fo"), vec![&f, &fo]);
        assert_eq!(
            trie.find_translations("foobar"),
            vec![&f, &fo, &foo, &foobar]
        );
        assert_eq!(trie.find_translations("foobaz"), vec![&f, &fo, &foo]);
        assert_eq!(trie.find_translations("function"), vec![&f]);
        assert_eq!(trie.find_translations("folio"), vec![&f, &fo]);
        assert_eq!(trie.find_translations("none"), empty);
        assert_eq!(
            trie.find_translations("The start of a long long sentence"),
            empty
        );
    }
}
