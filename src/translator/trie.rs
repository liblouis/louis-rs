use std::collections::HashMap;

use super::Translation;

#[derive(Default, Debug)]
struct TrieNode<'a> {
    translation: Option<&'a Translation>,
    children: HashMap<char, TrieNode<'a>>,
}

#[derive(Default, Debug)]
pub struct Trie<'a> {
    root: TrieNode<'a>,
}

impl<'a> Trie<'a> {
    pub fn new() -> Self {
        Trie {
            root: TrieNode::default(),
        }
    }

    pub fn insert(&mut self, word: &str, translation: &'a Translation) {
        let mut current_node = &mut self.root;

        for c in word.chars() {
            current_node = current_node.children.entry(c).or_default();
        }
        current_node.translation = Some(translation);
    }

    pub fn find_translations(&self, word: &str) -> Vec<&Translation> {
        let mut current_node = &self.root;
        let mut matching_rules = Vec::new();

        for c in word.chars() {
            match current_node.children.get(&c) {
                Some(node) => {
                    current_node = node;
                    if let Some(translation) = node.translation {
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
        assert_eq!(
            trie.find_translations("foo"),
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
        trie.insert("a", &a);
        trie.insert("f", &f);
        trie.insert("fo", &fo);
        trie.insert("foo", &foo);
        trie.insert("foobar", &foobar);
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
