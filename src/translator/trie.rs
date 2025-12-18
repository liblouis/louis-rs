//! Store and find simple translation rules using a prefix tree ([Trie](https://en.wikipedia.org/wiki/Trie))

use std::collections::HashMap;

use crate::{
    parser::{AnchoredRule, Direction, Precedence},
    translator::{
        TranslationStage,
        boundaries::{number_word, word_end, word_number, word_start},
    },
};

use super::Translation;

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Boundary {
    Word,
    NotWord,
    Number,
    NumberWord,
    WordNumber,
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

    pub fn insert_char(
        &mut self,
        from: char,
        to: &str,
        direction: Direction,
        precedence: Precedence,
        stage: TranslationStage,
        origin: &AnchoredRule,
    ) {
        self.insert(
            &from.to_string(),
            to,
            Boundary::None,
            Boundary::None,
            direction,
            precedence,
            stage,
            origin,
        );
    }

    pub fn insert(
        &mut self,
        from: &str,
        to: &str,
        before: Boundary,
        after: Boundary,
        direction: Direction,
        precedence: Precedence,
        stage: TranslationStage,
        origin: &AnchoredRule,
    ) {
        // swap `from` and `to` for backwards translation
        let (from, to) = match direction {
            Direction::Forward => (from, to),
            Direction::Backward => (to, from),
        };
        // FIXME: if `from` is an empty string (or `to` in case of Direction::Backward) there will
        // be an infinite loop in the translation as the returned Translation will not consume any
        // input. So for now just panic if that is the case. You could argue that it is a programmer
        // error to call `insert` with an empty string. OTOH a Result might be more adequate as a
        // user could indeed define a correct for example with an empty from. That should result in
        // an error.
        assert!(
            !from.is_empty(),
            "Cannot insert empty `from` string (or `to` string in case of backward translation) - this causes infinite loops when translating"
        );

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

        if let Some(translation) = &current_node.translation {
            // this node already contains a translation
            if precedence > translation.precedence() {
                current_node.translation = Some(Translation::new(
                    from,
                    to,
                    from.chars().count(),
                    stage,
                    origin.clone(),
                ));
            } else if cfg!(feature = "backwards_compatibility") {
                // first rule wins, so nothing to insert
            } else {
                // last rule wins
                current_node.translation =
                    Some(Translation::new(from, to, length, stage, origin.clone()));
            }
        } else {
            current_node.translation =
                Some(Translation::new(from, to, length, stage, origin.clone()));
        }
    }

    fn find_translations_from_node(
        &self,
        input: &str,
        prev: Option<char>,
        node: &TrieNode,
        match_length: usize,
    ) -> Vec<Translation> {
        let mut matching_rules = Vec::new();
        let mut chars = input.chars();

        // if this node has a translation add it to the list of matching rules
        if let Some(ref translation) = node.translation {
            let translation = translation.clone();
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
                ));
            } else if let Some(node) = node.char_case_insensitive_transition(c) {
                matching_rules.extend(self.find_translations_from_node(
                    &input[bytes..],
                    Some(c),
                    node,
                    match_length + 1,
                ));
            } else if let Some(node) = node.any_transition() {
                matching_rules.extend(self.find_translations_from_node(
                    &input[bytes..],
                    Some(c),
                    node,
                    match_length + 1,
                ));
            }
        }
        if let Some(node) = node.word_start_transition()
            && word_start(prev, c)
        {
            matching_rules.extend(self.find_translations_from_node(
                input,
                prev,
                node,
                match_length,
            ));
        }
        if let Some(node) = node.not_word_start_transition()
            && !word_start(prev, c)
        {
            matching_rules.extend(self.find_translations_from_node(
                input,
                prev,
                node,
                match_length,
            ));
        }
        if let Some(node) = node.word_end_transition()
            && word_end(prev, c)
        {
            matching_rules.extend(self.find_translations_from_node(
                input,
                prev,
                node,
                match_length,
            ));
        }
        if let Some(node) = node.not_word_end_transition()
            && !word_end(prev, c)
        {
            matching_rules.extend(self.find_translations_from_node(
                input,
                prev,
                node,
                match_length,
            ));
        }
        if let Some(node) = node.word_num_transition()
            && word_number(prev, c)
        {
            matching_rules.extend(self.find_translations_from_node(
                input,
                prev,
                node,
                match_length,
            ));
        }
        if let Some(node) = node.num_word_transition()
            && number_word(prev, c)
        {
            matching_rules.extend(self.find_translations_from_node(
                input,
                prev,
                node,
                match_length,
            ));
        }
        matching_rules
    }

    pub fn find_translations(&self, input: &str, prev: Option<char>) -> Vec<Translation> {
        let mut matching_rules = Vec::new();

        matching_rules.extend(self.find_translations_from_node(input, prev, &self.root, 0));
        //matching_rules.sort_by_key(|translation| translation.weight);
        matching_rules
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::RuleParser;

    use super::*;

    // just create some fake anchored rule for testing purposes
    fn fake_rule() -> AnchoredRule {
        let rule = RuleParser::new("always foo 1").rule().unwrap();
        AnchoredRule::new(rule, None, 0)
    }

    #[test]
    fn empty_trie() {
        let trie = Trie::new();
        assert_eq!(
            trie.find_translations("foo", None),
            Vec::<Translation>::new()
        );
    }

    #[test]
    fn find_translations() {
        let mut trie = Trie::new();
        let empty = Vec::<Translation>::new();
        let rule = fake_rule();
        let a = Translation::new(
            "a".into(),
            "A".into(),
            1,
            TranslationStage::Main,
            rule.clone(),
        );
        let f = Translation::new(
            "f".into(),
            "F".into(),
            1,
            TranslationStage::Main,
            rule.clone(),
        );
        let fo = Translation::new(
            "fo".into(),
            "FO".into(),
            2,
            TranslationStage::Main,
            rule.clone(),
        );
        let foo = Translation::new(
            "foo".into(),
            "FOO".into(),
            3,
            TranslationStage::Main,
            rule.clone(),
        );
        let foobar = Translation::new(
            "foobar".into(),
            "FOOBAR".into(),
            6,
            TranslationStage::Main,
            rule.clone(),
        );
        trie.insert(
            "a".into(),
            "A".into(),
            Boundary::None,
            Boundary::None,
            Direction::Forward,
            Precedence::Default,
            TranslationStage::Main,
            &rule,
        );
        trie.insert(
            "f".into(),
            "F".into(),
            Boundary::None,
            Boundary::None,
            Direction::Forward,
            Precedence::Default,
            TranslationStage::Main,
            &rule,
        );
        trie.insert(
            "fo".into(),
            "FO".into(),
            Boundary::None,
            Boundary::None,
            Direction::Forward,
            Precedence::Default,
            TranslationStage::Main,
            &rule,
        );
        trie.insert(
            "foo".into(),
            "FOO".into(),
            Boundary::None,
            Boundary::None,
            Direction::Forward,
            Precedence::Default,
            TranslationStage::Main,
            &rule,
        );
        trie.insert(
            "foobar".into(),
            "FOOBAR".into(),
            Boundary::None,
            Boundary::None,
            Direction::Forward,
            Precedence::Default,
            TranslationStage::Main,
            &rule,
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
    fn find_translations_with_boundaries() {
        let mut trie = Trie::new();
        let empty = Vec::<Translation>::new();
        let rule = fake_rule();
        let a = Translation::new(
            "a".into(),
            "A".into(),
            3,
            TranslationStage::Main,
            rule.clone(),
        );
        trie.insert(
            "a".into(),
            "A".into(),
            Boundary::Word,
            Boundary::Word,
            Direction::Forward,
            Precedence::Default,
            TranslationStage::Main,
            &rule,
        );
        assert_eq!(trie.find_translations("a", None), vec![a]);
        assert_eq!(trie.find_translations("aha", None), empty);
    }

    #[test]
    fn find_translations_with_negative_boundary_after() {
        let mut trie = Trie::new();
        let empty = Vec::<Translation>::new();
        let rule = fake_rule();
        let foo = Translation::new(
            "foo".into(),
            "FOO".into(),
            5,
            TranslationStage::Main,
            rule.clone(),
        );
        trie.insert(
            "foo".into(),
            "FOO".into(),
            Boundary::Word,
            Boundary::NotWord,
            Direction::Forward,
            Precedence::Default,
            TranslationStage::Main,
            &rule,
        );
        assert_eq!(trie.find_translations("foo", None), empty);
        assert_eq!(trie.find_translations("foo ", None), empty);
        assert_eq!(trie.find_translations("foobar", None), vec![foo]);
        assert_eq!(trie.find_translations("foo.", None), empty);
    }

    #[test]
    fn find_translations_with_negative_boundary_before() {
        let mut trie = Trie::new();
        let empty = Vec::<Translation>::new();
        let rule = fake_rule();
        let foo = Translation::new(
            "foo".into(),
            "FOO".into(),
            4,
            TranslationStage::Main,
            rule.clone(),
        );
        trie.insert(
            "foo".into(),
            "FOO".into(),
            Boundary::NotWord,
            Boundary::None,
            Direction::Forward,
            Precedence::Default,
            TranslationStage::Main,
            &rule,
        );
        assert_eq!(trie.find_translations("foo", None), empty);
        assert_eq!(trie.find_translations("foo", Some(' ')), empty);
        assert_eq!(trie.find_translations("foo", Some('.')), empty);
        assert_eq!(trie.find_translations("foo", Some('c')), vec![foo]);
    }

    #[test]
    fn find_translations_with_negative_boundaries() {
        let mut trie = Trie::new();
        let empty = Vec::<Translation>::new();
        let rule = fake_rule();
        let foo = Translation::new(
            "foo".into(),
            "FOO".into(),
            5,
            TranslationStage::Main,
            rule.clone(),
        );
        trie.insert(
            "foo".into(),
            "FOO".into(),
            Boundary::NotWord,
            Boundary::NotWord,
            Direction::Forward,
            Precedence::Default,
            TranslationStage::Main,
            &rule,
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
        let rule = fake_rule();
        let foo = Translation::new(
            "aaa".into(),
            "A".into(),
            5,
            TranslationStage::Main,
            rule.clone(),
        );
        trie.insert(
            "aaa".into(),
            "A".into(),
            Boundary::Word,
            Boundary::WordNumber,
            Direction::Forward,
            Precedence::Default,
            TranslationStage::Main,
            &rule,
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
        let rule = fake_rule();
        let foo = Translation::new(
            "st".into(),
            "S".into(),
            4,
            TranslationStage::Main,
            rule.clone(),
        );
        trie.insert(
            "st".into(),
            "S".into(),
            Boundary::NumberWord,
            Boundary::Word,
            Direction::Forward,
            Precedence::Default,
            TranslationStage::Main,
            &rule,
        );
        assert_eq!(trie.find_translations("st", None), empty);
        assert_eq!(trie.find_translations("st", Some(' ')), empty);
        assert_eq!(trie.find_translations("st", Some('.')), empty);
        assert_eq!(trie.find_translations("st", Some('1')), vec![foo]);
        assert_eq!(trie.find_translations("sta", Some('1')), empty);
    }

    #[test]
    fn find_translations_case_insensitive() {
        let mut trie = Trie::new();
        let rule = fake_rule();
        let foo = Translation::new(
            "foo".into(),
            "FOO".into(),
            3,
            TranslationStage::Main,
            rule.clone(),
        );
        trie.insert(
            "foo".into(),
            "FOO".into(),
            Boundary::None,
            Boundary::None,
            Direction::Forward,
            Precedence::Default,
            TranslationStage::Main,
            &rule,
        );
        assert_eq!(trie.find_translations("foo", None), vec![foo.clone()]);
        assert_eq!(trie.find_translations("Foo", None), vec![foo.clone()]);
        assert_eq!(trie.find_translations("FOO", None), vec![foo.clone()]);
        assert_eq!(trie.find_translations("foO", None), vec![foo.clone()]);
    }
}
