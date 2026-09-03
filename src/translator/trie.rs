//! Store and find simple translation rules using a prefix tree ([Trie](https://en.wikipedia.org/wiki/Trie))

use std::collections::HashMap;

use crate::{
    parser::{AnchoredRule, CharacterClass, CharacterClasses, Direction, Precedence},
    translator::TranslationStage,
};

use super::ResolvedTranslation;

/// The union of one or more character classes, pre-resolved to their actual member
/// characters.
#[derive(Debug, PartialEq, Eq, Hash, Clone, Default)]
pub struct ResolvedClasses {
    /// The characters contained in this character class
    chars: Vec<char>,
    /// Whether `Space` was one of the requested classes. A missing neighbor (start/end
    /// of input) is treated as a literal space.
    includes_space: bool,
}

impl ResolvedClasses {
    pub fn resolve(ctx: &CharacterClasses, classes: &[CharacterClass]) -> Self {
        let mut chars: Vec<char> = classes
            .iter()
            .filter_map(|class| ctx.get(class))
            .flatten()
            .collect();
        chars.sort_unstable();
        chars.dedup();
        ResolvedClasses {
            chars,
            includes_space: classes.contains(&CharacterClass::Space),
        }
    }

    fn matches(&self, c: Option<char>) -> bool {
        match c {
            Some(c) => self.chars.binary_search(&c).is_ok(),
            None => self.includes_space,
        }
    }
}

/// A non-consuming boundary check to attach when inserting a rule, expressed as the character
/// classes a neighboring character must belong to. Resolved to concrete characters inside
/// `Trie::insert` against the trie's own context.
#[derive(Debug, Clone)]
pub enum Transition {
    /// Non-consuming lookbehind: the preceding character must be in one of these classes.
    /// Inserted at the start of the path (before character transitions).
    Start(Vec<CharacterClass>),
    /// Non-consuming lookahead: the character immediately after the match must be in one of
    /// these classes. Inserted at the end of the path (after character transitions).
    End(Vec<CharacterClass>),
}

/// The resolved, trie-internal counterpart of [`Transition`] plus the transitions that only ever
/// arise while inserting a plain character sequence.
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
enum ResolvedTransition {
    Character(char),
    Start(ResolvedClasses),
    End(ResolvedClasses),
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
    // FIXME: `HashMap` iteration order is randomized per process, so the order in which
    // `find_translations_from_node` collects candidates varies between runs. Candidates that
    // tie on [`ResolvedTranslation::rank`] are resolved by `max_by_key`, which returns the
    // last of them, so a tie is decided by chance: `fr-bfu-g2.ctb` back-translates `⠡` to
    // either `tout` (`word tout ⠡`) or `ation` (`endword ation ⠡`), both ranking (1, 3).
    // A `BTreeMap` makes this reproducible for 4 lines (derive `Ord` here and on
    // `ResolvedClasses`) at no cost -- translation is unchanged within noise and compiling
    // en-ueb-g2 got ~12% faster, since hashing a `ResolvedClasses` hashes its whole
    // `Vec<char>` while an ordered compare short-circuits on the discriminant. It was left
    // out because it only freezes an arbitrary order: liblouis breaks such a tie by
    // definition order, which neither map reproduces. Note it is the *first* rule defined
    // that wins there, not the last -- see the tie-break sub-case in the ADR "Match-rule
    // candidate selection", which also wants that same ordering key.
    transitions: HashMap<ResolvedTransition, TrieNode>,
}

impl TrieNode {
    fn char_transition(&self, c: char, case_sensitive: bool) -> Option<&TrieNode> {
        if case_sensitive {
            self.transitions.get(&ResolvedTransition::Character(c))
        } else if c.to_lowercase().count() != 1 {
            // FIXME: we ignore characters that do not map to a single
            // character lowercase character
            None
        } else {
            // case insensitive character transition
            let lowercase = c.to_lowercase().next().unwrap();
            self.transitions
                .get(&ResolvedTransition::Character(lowercase))
        }
    }

    fn any_transition(&self) -> Option<&TrieNode> {
        self.transitions.get(&ResolvedTransition::Any)
    }
}

#[derive(Default, Debug, Clone)]
pub struct Trie {
    root: TrieNode,
    /// Character classes used for word/number/punctuation boundary checks and for
    /// `StartClass`/`EndClass` constraint checks. Set to text classes for forward translation,
    /// braille classes for backward.
    ctx: CharacterClasses,
    /// When true, character transitions are matched on the exact character instead of
    /// being lowercased first. Needed for `comp6`, where `comp6 a 1` and `comp6 A 17`
    /// are independent rules.
    case_sensitive: bool,
}

impl Trie {
    pub fn new() -> Self {
        Trie {
            root: TrieNode::default(),
            ctx: CharacterClasses::default(),
            case_sensitive: false,
        }
    }

    pub fn with_context(self, ctx: CharacterClasses) -> Self {
        Trie { ctx, ..self }
    }

    pub fn case_sensitive(self) -> Self {
        Trie {
            case_sensitive: true,
            ..self
        }
    }

    fn resolve_transition(&self, transition: Transition) -> ResolvedTransition {
        match transition {
            Transition::Start(classes) => {
                ResolvedTransition::Start(ResolvedClasses::resolve(&self.ctx, &classes))
            }
            Transition::End(classes) => {
                ResolvedTransition::End(ResolvedClasses::resolve(&self.ctx, &classes))
            }
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

        let before = before.map(|t| self.resolve_transition(t));
        let after = after.map(|t| self.resolve_transition(t));

        let mut current_node = &mut self.root;
        let mut length = from.chars().count();

        // StartClass constraints are lookbehind checks on `prev` — inserted before char transitions
        for constraint in &class_constraints {
            if let ClassConstraint::Start(class) = constraint {
                length += 1;
                let resolved = ResolvedClasses::resolve(&self.ctx, &[class.clone()]);
                current_node = current_node
                    .transitions
                    .entry(ResolvedTransition::Start(resolved))
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
                .entry(ResolvedTransition::Character(c))
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
                let resolved = ResolvedClasses::resolve(&self.ctx, &[class.clone()]);
                current_node = current_node
                    .transitions
                    .entry(ResolvedTransition::End(resolved))
                    .or_default();
            }
        }

        if let Some(translation) = &current_node.translation {
            // this node already contains a translation
            if precedence > translation.precedence() {
                current_node.translation = Some(ResolvedTranslation::new(
                    from,
                    to,
                    length,
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
            if let Some(node) = node.char_transition(c, self.case_sensitive) {
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
        // Class-based non-consuming checks. Iterate only transitions that are class variants to
        // avoid scanning all transitions on every node visit. The character sets themselves are
        // already resolved at insert time (see `ResolvedClasses::resolve`), so this is a plain
        // binary search, not a `CharacterClasses` lookup.
        for (transition, child_node) in &node.transitions {
            match transition {
                ResolvedTransition::Start(resolved) => {
                    if resolved.matches(prev) {
                        matching_rules.extend(self.find_translations_from_node(
                            input,
                            prev,
                            child_node,
                            match_length,
                        ));
                    }
                }
                ResolvedTransition::End(resolved) => {
                    if resolved.matches(c) {
                        matching_rules.extend(self.find_translations_from_node(
                            input,
                            prev,
                            child_node,
                            match_length,
                        ));
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
        let a = ResolvedTranslation::new("a", "A", 1, TranslationStage::Main, rule.clone());
        let f = ResolvedTranslation::new("f", "F", 1, TranslationStage::Main, rule.clone());
        let fo = ResolvedTranslation::new("fo", "FO", 2, TranslationStage::Main, rule.clone());
        let foo = ResolvedTranslation::new("foo", "FOO", 3, TranslationStage::Main, rule.clone());
        let foobar =
            ResolvedTranslation::new("foobar", "FOOBAR", 6, TranslationStage::Main, rule.clone());
        trie.insert(
            "a",
            "A",
            None,
            None,
            Direction::Forward,
            Precedence::Default,
            vec![],
            TranslationStage::Main,
            &rule,
        );
        trie.insert(
            "f",
            "F",
            None,
            None,
            Direction::Forward,
            Precedence::Default,
            vec![],
            TranslationStage::Main,
            &rule,
        );
        trie.insert(
            "fo",
            "FO",
            None,
            None,
            Direction::Forward,
            Precedence::Default,
            vec![],
            TranslationStage::Main,
            &rule,
        );
        trie.insert(
            "foo",
            "FOO",
            None,
            None,
            Direction::Forward,
            Precedence::Default,
            vec![],
            TranslationStage::Main,
            &rule,
        );
        trie.insert(
            "foobar",
            "FOOBAR",
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
        let a = ResolvedTranslation::new("a", "A", 3, TranslationStage::Main, rule.clone());
        let boundary = vec![CharacterClass::Space, CharacterClass::Punctuation];
        trie.insert(
            "a",
            "A",
            Some(Transition::Start(boundary.clone())),
            Some(Transition::End(boundary)),
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
        let foo = ResolvedTranslation::new("foo", "FOO", 5, TranslationStage::Main, rule.clone());
        trie.insert(
            "foo",
            "FOO",
            Some(Transition::Start(vec![
                CharacterClass::Space,
                CharacterClass::Punctuation,
            ])),
            Some(Transition::End(vec![CharacterClass::Letter])),
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
        let foo = ResolvedTranslation::new("foo", "FOO", 4, TranslationStage::Main, rule.clone());
        trie.insert(
            "foo",
            "FOO",
            Some(Transition::Start(vec![CharacterClass::Letter])),
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
        let foo = ResolvedTranslation::new("foo", "FOO", 5, TranslationStage::Main, rule.clone());
        let boundary = vec![CharacterClass::Letter];
        trie.insert(
            "foo",
            "FOO",
            Some(Transition::Start(boundary.clone())),
            Some(Transition::End(boundary)),
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
        let foo = ResolvedTranslation::new("aaa", "A", 5, TranslationStage::Main, rule.clone());
        trie.insert(
            "aaa",
            "A",
            Some(Transition::Start(vec![
                CharacterClass::Space,
                CharacterClass::Punctuation,
            ])),
            Some(Transition::End(vec![CharacterClass::Litdigit])),
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
        let foo = ResolvedTranslation::new("st", "S", 4, TranslationStage::Main, rule.clone());
        trie.insert(
            "st",
            "S",
            Some(Transition::Start(vec![CharacterClass::Litdigit])),
            Some(Transition::End(vec![
                CharacterClass::Space,
                CharacterClass::Punctuation,
            ])),
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
        let foo = ResolvedTranslation::new("(", "[", 2, TranslationStage::Main, rule.clone());
        trie.insert(
            "(",
            "[",
            Some(Transition::Start(vec![CharacterClass::Letter])),
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
        let foo = ResolvedTranslation::new("(", "[", 2, TranslationStage::Main, rule.clone());
        trie.insert(
            "(",
            "[",
            None,
            Some(Transition::End(vec![CharacterClass::Letter])),
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
        let foo = ResolvedTranslation::new("foo", "FOO", 3, TranslationStage::Main, rule.clone());
        trie.insert(
            "foo",
            "FOO",
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
    fn find_translations_case_sensitive() {
        // a case sensitive trie keeps `a` and `A` apart, as comp6 requires
        let mut trie = Trie::new().case_sensitive();
        let rule = fake_rule();
        let lower = ResolvedTranslation::new("a", "1", 1, TranslationStage::Main, rule.clone());
        let upper = ResolvedTranslation::new("A", "17", 1, TranslationStage::Main, rule.clone());
        for (from, to) in [("a", "1"), ("A", "17")] {
            trie.insert(
                from,
                to,
                None,
                None,
                Direction::Forward,
                Precedence::Default,
                vec![],
                TranslationStage::Main,
                &rule,
            );
        }
        assert_eq!(trie.find_translations("a", None), vec![lower]);
        assert_eq!(trie.find_translations("A", None), vec![upper]);
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
        let foo_before_letter =
            ResolvedTranslation::new("foo", "FL", 4, TranslationStage::Main, rule.clone());
        let foo_before_digit =
            ResolvedTranslation::new("foo", "FD", 4, TranslationStage::Main, rule.clone());
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
        let foo_after_letter =
            ResolvedTranslation::new("foo", "AL", 4, TranslationStage::Main, rule.clone());
        let foo_after_digit =
            ResolvedTranslation::new("foo", "AD", 4, TranslationStage::Main, rule.clone());
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
