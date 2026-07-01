//! Store and find simple translation rules using a prefix tree ([Trie](https://en.wikipedia.org/wiki/Trie))

use std::collections::HashMap;

use crate::{
    parser::{AnchoredRule, CharacterClass, CharacterClasses, Direction, Precedence},
    translator::TranslationStage,
};

use super::ResolvedTranslation;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Boundary {
    Word,
    NotWord,
    Number,
    NumberWord,
    WordNumber,
    Punctuation,
    PunctuationWord,
    WordPunctuation,
    /// Checks only the preceding character (space, punctuation, or start of string).
    /// Unlike the other variants, this is a lookbehind-only condition and does not
    /// encode a two-class transition in the usual XY naming convention.
    AfterSpaceOrPunct,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Transition {
    Character(char),
    Start(Boundary),
    End(Boundary),
    /// Non-consuming lookbehind: the preceding character must be in this class.
    /// Inserted at the start of the path (before character transitions).
    StartClass(CharacterClass),
    /// Non-consuming lookahead: the character immediately after the match must be in this class.
    /// Inserted at the end of the path (after character transitions).
    EndClass(CharacterClass),
    Any,
}

/// A class-based constraint attached to a rule via the liblouis `before`/`after` keywords.
#[derive(Debug, Clone)]
pub enum ClassConstraint {
    /// `after CLASS` keyword: the character before the match must be in this class (lookbehind).
    Start(CharacterClass),
    /// `before CLASS` keyword: the character after the match must be in this class (lookahead).
    End(CharacterClass),
}

#[derive(Default, Debug, Clone)]
struct TrieNode {
    translation: Option<ResolvedTranslation>,
    transitions: HashMap<Transition, TrieNode>,
}

impl TrieNode {
    fn char_transition(&self, c: char) -> Option<&TrieNode> {
        // FIXME: we ignore characters that do not map to a single
        // character lowercase character
        if c.to_lowercase().count() != 1 {
            return None;
        };
        // character transitions are always case insensitive
        let lowercase = c.to_lowercase().next().unwrap();
        self.transitions.get(&Transition::Character(lowercase))
    }

    fn any_transition(&self) -> Option<&TrieNode> {
        self.transitions.get(&Transition::Any)
    }

    fn boundary_transition(&self, t: &Transition) -> Option<&TrieNode> {
        self.transitions.get(t)
    }
}

#[derive(Default, Debug, Clone)]
pub struct Trie {
    root: TrieNode,
    /// Character classes used for word/number/punctuation boundary checks and for
    /// `StartClass`/`EndClass` constraint checks. Set to text classes for forward translation,
    /// braille classes for backward.
    ctx: CharacterClasses,
}

impl Trie {
    pub fn new() -> Self {
        Trie {
            root: TrieNode::default(),
            ctx: CharacterClasses::default(),
        }
    }

    pub fn with_context(self, ctx: CharacterClasses) -> Self {
        Trie { ctx, ..self }
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
            None,
            None,
            direction,
            precedence,
            vec![], // char rules never carry class constraints
            stage,
            origin,
        );
    }

    pub fn insert(
        &mut self,
        from: &str,
        to: &str,
        before: Option<Transition>,
        after: Option<Transition>,
        direction: Direction,
        precedence: Precedence,
        class_constraints: Vec<ClassConstraint>,
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

        // StartClass constraints are lookbehind checks on `prev` — inserted before char transitions
        for constraint in &class_constraints {
            if let ClassConstraint::Start(class) = constraint {
                length += 1;
                current_node = current_node
                    .transitions
                    .entry(Transition::StartClass(class.clone()))
                    .or_default();
            }
        }

        if let Some(t) = before {
            length += 1;
            current_node = current_node.transitions.entry(t).or_default();
        }

        for c in from.chars() {
            current_node = current_node
                .transitions
                .entry(Transition::Character(c))
                .or_default();
        }

        if let Some(t) = after {
            length += 1;
            current_node = current_node.transitions.entry(t).or_default();
        }

        // EndClass constraints are lookahead checks on the next char — inserted after char transitions
        for constraint in &class_constraints {
            if let ClassConstraint::End(class) = constraint {
                length += 1;
                current_node = current_node
                    .transitions
                    .entry(Transition::EndClass(class.clone()))
                    .or_default();
            }
        }

        if let Some(translation) = &current_node.translation {
            // this node already contains a translation
            if precedence > translation.precedence() {
                current_node.translation = Some(ResolvedTranslation::new(
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
                current_node.translation = Some(ResolvedTranslation::new(
                    from,
                    to,
                    length,
                    stage,
                    origin.clone(),
                ));
            }
        } else {
            current_node.translation = Some(ResolvedTranslation::new(
                from,
                to,
                length,
                stage,
                origin.clone(),
            ));
        }
    }

    fn find_translations_from_node(
        &self,
        input: &str,
        prev: Option<char>,
        node: &TrieNode,
        match_length: usize,
    ) -> Vec<ResolvedTranslation> {
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
            } else if let Some(node) = node.any_transition() {
                matching_rules.extend(self.find_translations_from_node(
                    &input[bytes..],
                    Some(c),
                    node,
                    match_length + 1,
                ));
            }
        }
        // TODO: all 12 conditions are evaluated eagerly even when the node has no boundary
        // branches. Lazy evaluation (closures, check trie first) would be cheaper for the common
        // case, but needs benchmarks to justify the added complexity.
        #[rustfmt::skip]
        let boundary_checks = [
            (Transition::Start(Boundary::Word),            self.ctx.is_word_start(prev, c)),
            (Transition::Start(Boundary::NotWord),        !self.ctx.is_word_start(prev, c)),
            (Transition::End(Boundary::Word),              self.ctx.is_word_end(prev, c)),
            (Transition::End(Boundary::NotWord),          !self.ctx.is_word_end(prev, c)),
            (Transition::End(Boundary::WordNumber),        self.ctx.is_word_number(prev, c)),
            (Transition::Start(Boundary::NumberWord),      self.ctx.is_number_word(prev, c)),
            (Transition::Start(Boundary::Number),          self.ctx.is_number_start(prev, c)),
            (Transition::End(Boundary::Number),            self.ctx.is_number_end(prev, c)),
            (Transition::Start(Boundary::Punctuation),     self.ctx.is_punctuation_start(prev, c)),
            (Transition::End(Boundary::Punctuation),       self.ctx.is_punctuation_end(prev, c)),
            (Transition::Start(Boundary::WordPunctuation),  self.ctx.is_word_punctuation(prev, c)),
            (Transition::End(Boundary::PunctuationWord),    self.ctx.is_punctuation_word(prev, c)),
            (Transition::Start(Boundary::AfterSpaceOrPunct), self.ctx.is_after_space_or_punct(prev)),
        ];
        for (transition, matches) in &boundary_checks {
            if *matches {
                if let Some(node) = node.boundary_transition(transition) {
                    matching_rules.extend(self.find_translations_from_node(
                        input,
                        prev,
                        node,
                        match_length,
                    ));
                }
            }
        }
        // Class-based non-consuming checks. Iterate only transitions that are class variants to
        // avoid scanning all transitions on every node visit.
        // FIXME: `self.ctx.get(class)` is a HashMap lookup on every traversal. This could be
        // moved to build time by resolving each class to its character set once during `insert`
        // and storing it directly in the transition (e.g. as a HashSet in a separate Vec in
        // TrieNode, or as a sorted Vec<char> in the Transition key).
        for (transition, child_node) in &node.transitions {
            match transition {
                Transition::StartClass(class) => {
                    if let Some(set) = self.ctx.get(class) {
                        if prev.is_some_and(|p| set.contains(&p)) {
                            matching_rules.extend(self.find_translations_from_node(
                                input,
                                prev,
                                child_node,
                                match_length,
                            ));
                        }
                    }
                }
                Transition::EndClass(class) => {
                    if let Some(set) = self.ctx.get(class) {
                        if c.is_some_and(|ch| set.contains(&ch)) {
                            matching_rules.extend(self.find_translations_from_node(
                                input,
                                prev,
                                child_node,
                                match_length,
                            ));
                        }
                    }
                }
                _ => {}
            }
        }
        matching_rules
    }

    pub fn find_translations(&self, input: &str, prev: Option<char>) -> Vec<ResolvedTranslation> {
        self.find_translations_from_node(input, prev, &self.root, 0)
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::{CharacterClass, RuleParser};

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
            Vec::<ResolvedTranslation>::new()
        );
    }

    #[test]
    fn find_translations() {
        let mut trie = Trie::new();
        let empty = Vec::<ResolvedTranslation>::new();
        let rule = fake_rule();
        let a = ResolvedTranslation::new(
            "a".into(),
            "A".into(),
            1,
            TranslationStage::Main,
            rule.clone(),
        );
        let f = ResolvedTranslation::new(
            "f".into(),
            "F".into(),
            1,
            TranslationStage::Main,
            rule.clone(),
        );
        let fo = ResolvedTranslation::new(
            "fo".into(),
            "FO".into(),
            2,
            TranslationStage::Main,
            rule.clone(),
        );
        let foo = ResolvedTranslation::new(
            "foo".into(),
            "FOO".into(),
            3,
            TranslationStage::Main,
            rule.clone(),
        );
        let foobar = ResolvedTranslation::new(
            "foobar".into(),
            "FOOBAR".into(),
            6,
            TranslationStage::Main,
            rule.clone(),
        );
        trie.insert(
            "a".into(),
            "A".into(),
            None,
            None,
            Direction::Forward,
            Precedence::Default,
            vec![],
            TranslationStage::Main,
            &rule,
        );
        trie.insert(
            "f".into(),
            "F".into(),
            None,
            None,
            Direction::Forward,
            Precedence::Default,
            vec![],
            TranslationStage::Main,
            &rule,
        );
        trie.insert(
            "fo".into(),
            "FO".into(),
            None,
            None,
            Direction::Forward,
            Precedence::Default,
            vec![],
            TranslationStage::Main,
            &rule,
        );
        trie.insert(
            "foo".into(),
            "FOO".into(),
            None,
            None,
            Direction::Forward,
            Precedence::Default,
            vec![],
            TranslationStage::Main,
            &rule,
        );
        trie.insert(
            "foobar".into(),
            "FOOBAR".into(),
            None,
            None,
            Direction::Forward,
            Precedence::Default,
            vec![],
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
        let ctx = CharacterClasses::new(&[(CharacterClass::Letter, &['a', 'h'])]);
        let mut trie = Trie::new().with_context(ctx);
        let empty = Vec::<ResolvedTranslation>::new();
        let rule = fake_rule();
        let a = ResolvedTranslation::new(
            "a".into(),
            "A".into(),
            3,
            TranslationStage::Main,
            rule.clone(),
        );
        trie.insert(
            "a".into(),
            "A".into(),
            Some(Transition::Start(Boundary::Word)),
            Some(Transition::End(Boundary::Word)),
            Direction::Forward,
            Precedence::Default,
            vec![],
            TranslationStage::Main,
            &rule,
        );
        assert_eq!(trie.find_translations("a", None), vec![a]);
        assert_eq!(trie.find_translations("aha", None), empty);
    }

    #[test]
    fn find_translations_with_negative_boundary_after() {
        let ctx = CharacterClasses::new(&[
            (CharacterClass::Letter, &['f', 'o', 'b', 'a', 'r']),
            (CharacterClass::Space, &[' ']),
            (CharacterClass::Punctuation, &['.']),
        ]);
        let mut trie = Trie::new().with_context(ctx);
        let empty = Vec::<ResolvedTranslation>::new();
        let rule = fake_rule();
        let foo = ResolvedTranslation::new(
            "foo".into(),
            "FOO".into(),
            5,
            TranslationStage::Main,
            rule.clone(),
        );
        trie.insert(
            "foo".into(),
            "FOO".into(),
            Some(Transition::Start(Boundary::Word)),
            Some(Transition::End(Boundary::NotWord)),
            Direction::Forward,
            Precedence::Default,
            vec![],
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
        let ctx = CharacterClasses::new(&[
            (CharacterClass::Letter, &['f', 'o', 'c']),
            (CharacterClass::Space, &[' ']),
            (CharacterClass::Punctuation, &['.']),
        ]);
        let mut trie = Trie::new().with_context(ctx);
        let empty = Vec::<ResolvedTranslation>::new();
        let rule = fake_rule();
        let foo = ResolvedTranslation::new(
            "foo".into(),
            "FOO".into(),
            4,
            TranslationStage::Main,
            rule.clone(),
        );
        trie.insert(
            "foo".into(),
            "FOO".into(),
            Some(Transition::Start(Boundary::NotWord)),
            None,
            Direction::Forward,
            Precedence::Default,
            vec![],
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
        let ctx = CharacterClasses::new(&[
            (CharacterClass::Letter, &['f', 'o', 'b', 'a', 'r', 'c']),
            (CharacterClass::Space, &[' ']),
            (CharacterClass::Punctuation, &['.']),
        ]);
        let mut trie = Trie::new().with_context(ctx);
        let empty = Vec::<ResolvedTranslation>::new();
        let rule = fake_rule();
        let foo = ResolvedTranslation::new(
            "foo".into(),
            "FOO".into(),
            5,
            TranslationStage::Main,
            rule.clone(),
        );
        trie.insert(
            "foo".into(),
            "FOO".into(),
            Some(Transition::Start(Boundary::NotWord)),
            Some(Transition::End(Boundary::NotWord)),
            Direction::Forward,
            Precedence::Default,
            vec![],
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
        let ctx = CharacterClasses::new(&[
            (CharacterClass::Letter, &['a', 'c']),
            (CharacterClass::Litdigit, &['1']),
            (CharacterClass::Space, &[' ']),
            (CharacterClass::Punctuation, &['.']),
        ]);
        let mut trie = Trie::new().with_context(ctx);
        let empty = Vec::<ResolvedTranslation>::new();
        let rule = fake_rule();
        let foo = ResolvedTranslation::new(
            "aaa".into(),
            "A".into(),
            5,
            TranslationStage::Main,
            rule.clone(),
        );
        trie.insert(
            "aaa".into(),
            "A".into(),
            Some(Transition::Start(Boundary::Word)),
            Some(Transition::End(Boundary::WordNumber)),
            Direction::Forward,
            Precedence::Default,
            vec![],
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
        let ctx = CharacterClasses::new(&[
            (CharacterClass::Letter, &['s', 't', 'a']),
            (CharacterClass::Litdigit, &['1']),
            (CharacterClass::Space, &[' ']),
            (CharacterClass::Punctuation, &['.']),
        ]);
        let mut trie = Trie::new().with_context(ctx);
        let empty = Vec::<ResolvedTranslation>::new();
        let rule = fake_rule();
        let foo = ResolvedTranslation::new(
            "st".into(),
            "S".into(),
            4,
            TranslationStage::Main,
            rule.clone(),
        );
        trie.insert(
            "st".into(),
            "S".into(),
            Some(Transition::Start(Boundary::NumberWord)),
            Some(Transition::End(Boundary::Word)),
            Direction::Forward,
            Precedence::Default,
            vec![],
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
    fn find_translations_with_word_punc_boundary() {
        let ctx = CharacterClasses::new(&[
            (CharacterClass::Punctuation, &['(', ')', '.']),
            (CharacterClass::Space, &[' ']),
            (CharacterClass::Letter, &['a']),
        ]);
        let mut trie = Trie::new().with_context(ctx);
        let empty = Vec::<ResolvedTranslation>::new();
        let rule = fake_rule();
        let foo = ResolvedTranslation::new(
            "(".into(),
            "[".into(),
            2,
            TranslationStage::Main,
            rule.clone(),
        );
        trie.insert(
            "(".into(),
            "[".into(),
            Some(Transition::Start(Boundary::WordPunctuation)),
            None,
            Direction::Forward,
            Precedence::Default,
            vec![],
            TranslationStage::Main,
            &rule,
        );
        assert_eq!(trie.find_translations("(", None), empty);
        assert_eq!(trie.find_translations("(", Some(' ')), empty);
        assert_eq!(trie.find_translations("(", Some('.')), empty);
        assert_eq!(trie.find_translations("(", Some('a')), vec![foo]);
    }

    #[test]
    fn find_translations_with_punc_word_boundary() {
        let ctx = CharacterClasses::new(&[
            (CharacterClass::Space, &[' ']),
            (CharacterClass::Punctuation, &['(', ')', '.']),
            (CharacterClass::Letter, &['a']),
        ]);
        let mut trie = Trie::new().with_context(ctx);
        let empty = Vec::<ResolvedTranslation>::new();
        let rule = fake_rule();
        let foo = ResolvedTranslation::new(
            "(".into(),
            "[".into(),
            2,
            TranslationStage::Main,
            rule.clone(),
        );
        trie.insert(
            "(".into(),
            "[".into(),
            None,
            Some(Transition::End(Boundary::PunctuationWord)),
            Direction::Forward,
            Precedence::Default,
            vec![],
            TranslationStage::Main,
            &rule,
        );
        assert_eq!(trie.find_translations("(", None), empty);
        assert_eq!(trie.find_translations("(", Some(' ')), empty);
        assert_eq!(trie.find_translations("(", Some('.')), empty);
        assert_eq!(trie.find_translations("(", Some('a')), empty);
        assert_eq!(trie.find_translations("(a", None), vec![foo.clone()]);
        assert_eq!(trie.find_translations("(a", Some('(')), vec![foo.clone()]);
    }

    #[test]
    fn find_translations_case_insensitive() {
        let mut trie = Trie::new();
        let rule = fake_rule();
        let foo = ResolvedTranslation::new(
            "foo".into(),
            "FOO".into(),
            3,
            TranslationStage::Main,
            rule.clone(),
        );
        trie.insert(
            "foo".into(),
            "FOO".into(),
            None,
            None,
            Direction::Forward,
            Precedence::Default,
            vec![],
            TranslationStage::Main,
            &rule,
        );
        assert_eq!(trie.find_translations("foo", None), vec![foo.clone()]);
        assert_eq!(trie.find_translations("Foo", None), vec![foo.clone()]);
        assert_eq!(trie.find_translations("FOO", None), vec![foo.clone()]);
        assert_eq!(trie.find_translations("foO", None), vec![foo.clone()]);
    }

    #[test]
    fn find_translations_with_end_class() {
        let ctx = CharacterClasses::new(&[
            (CharacterClass::Letter, &['a', 'b', 'c']),
            (CharacterClass::Digit, &['1', '2']),
            (CharacterClass::Space, &[' ']),
        ]);
        let mut trie = Trie::new().with_context(ctx);
        let empty = Vec::<ResolvedTranslation>::new();
        let rule = fake_rule();
        let foo_before_letter = ResolvedTranslation::new(
            "foo".into(),
            "FL".into(),
            4,
            TranslationStage::Main,
            rule.clone(),
        );
        let foo_before_digit = ResolvedTranslation::new(
            "foo".into(),
            "FD".into(),
            4,
            TranslationStage::Main,
            rule.clone(),
        );
        // "before letter always foo FL" — next char after "foo" must be a letter
        trie.insert(
            "foo",
            "FL",
            None,
            None,
            Direction::Forward,
            Precedence::Default,
            vec![ClassConstraint::End(CharacterClass::Letter)],
            TranslationStage::Main,
            &rule,
        );
        // "before digit always foo FD" — next char after "foo" must be a digit
        trie.insert(
            "foo",
            "FD",
            None,
            None,
            Direction::Forward,
            Precedence::Default,
            vec![ClassConstraint::End(CharacterClass::Digit)],
            TranslationStage::Main,
            &rule,
        );
        // Both rules coexist on different paths — neither stomps the other
        assert_eq!(
            trie.find_translations("fooa", None),
            vec![foo_before_letter]
        );
        assert_eq!(trie.find_translations("foo1", None), vec![foo_before_digit]);
        assert_eq!(trie.find_translations("foo ", None), empty);
        assert_eq!(trie.find_translations("foo", None), empty);
    }

    #[test]
    fn find_translations_with_start_class() {
        let ctx = CharacterClasses::new(&[
            (CharacterClass::Letter, &['a', 'b', 'c']),
            (CharacterClass::Digit, &['1', '2']),
            (CharacterClass::Space, &[' ']),
        ]);
        let mut trie = Trie::new().with_context(ctx);
        let empty = Vec::<ResolvedTranslation>::new();
        let rule = fake_rule();
        let foo_after_letter = ResolvedTranslation::new(
            "foo".into(),
            "AL".into(),
            4,
            TranslationStage::Main,
            rule.clone(),
        );
        let foo_after_digit = ResolvedTranslation::new(
            "foo".into(),
            "AD".into(),
            4,
            TranslationStage::Main,
            rule.clone(),
        );
        // "after letter always foo AL" — prev char before "foo" must be a letter
        trie.insert(
            "foo",
            "AL",
            None,
            None,
            Direction::Forward,
            Precedence::Default,
            vec![ClassConstraint::Start(CharacterClass::Letter)],
            TranslationStage::Main,
            &rule,
        );
        // "after digit always foo AD" — prev char before "foo" must be a digit
        trie.insert(
            "foo",
            "AD",
            None,
            None,
            Direction::Forward,
            Precedence::Default,
            vec![ClassConstraint::Start(CharacterClass::Digit)],
            TranslationStage::Main,
            &rule,
        );
        assert_eq!(
            trie.find_translations("foo", Some('a')),
            vec![foo_after_letter]
        );
        assert_eq!(
            trie.find_translations("foo", Some('1')),
            vec![foo_after_digit]
        );
        assert_eq!(trie.find_translations("foo", Some(' ')), empty);
        assert_eq!(trie.find_translations("foo", None), empty);
    }
}
