use std::collections::HashMap;

use crate::translator::boundaries::{word_start, word_end};

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
    ) -> Vec<&Translation> {
        let mut current_node = node;
        let mut matching_rules = Vec::new();
        let mut prev: Option<char> = None;
        let mut chars = word.chars();

        while let Some(c) = chars.next() {
            if let Some(node) = current_node.transitions.get(&Transition::Character(c)) {
                current_node = node;
                if let Some(ref translation) = node.translation {
                    matching_rules.push(translation)
                }
            } else if let Some(node) = current_node
                .transitions
                .get(&Transition::End(Boundary::Word))
            {
                current_node = node;
                if word_end(prev, Some(c)) {
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
        if let Some(node) = current_node
            .transitions
            .get(&Transition::End(Boundary::Word))
        {
            if word_end(prev, chars.next()) {
                if let Some(ref translation) = node.translation {
                    matching_rules.push(translation)
                }
            }
        }
        matching_rules
    }

    fn merge_translations<'a>(&self, xs: Vec<&'a Translation>, ys: Vec<&'a Translation>) -> Vec<&'a Translation> {
	let mut x_it = xs.iter().peekable();
	let mut y_it = ys.iter().peekable();
	let mut out = Vec::new();

	loop {
            match (x_it.peek(), y_it.peek()) {
		(None, None) => {
                    return out;
		},
		(Some(_), None) => {
                    while let Some(&x) = x_it.next() {
			out.push(x);
                    }
		},
		(None, Some(_)) => {
                    while let Some(&y) = y_it.next() {
			out.push(y);
                }
            },
		(Some(x), Some(y)) => {
		    let v = if x.from.len() > y.from.len() {
			y_it.next().unwrap()
		    } else {
			x_it.next().unwrap()
		    };
		    out.push(v);
		},
            }
	}
    }


    pub fn find_translations(&self, word: &str, before: Option<char>) -> Vec<&Translation> {
        let mut matching_rules = Vec::new();

        if let Some(node) = self
            .root
            .transitions
            .get(&Transition::Start(Boundary::Word))
        {
            if word_start(before, word.chars().next()) {
                matching_rules = self.find_translations_from_node(word, node);
            }
        }

	// there may be some candidates from the boundary rule. Those
	// have to be merged with the candidates from rules without
	// boundaries. Both lists are ordered by length of match.
	// Merge them accordingly
	self.merge_translations(matching_rules, self.find_translations_from_node(word, &self.root))
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
