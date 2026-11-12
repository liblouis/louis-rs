//! Store and find simple translation rules using a prefix tree ([Trie](https://en.wikipedia.org/wiki/Trie))

use std::collections::BTreeMap;

use crate::{
    parser::{AnchoredRule, CharacterClass, CharacterClasses, Direction},
    translator::{
        TranslationStage, beg_word::BegWordScanner, end_word::EndWordScanner,
        standing_alone::StandingAloneScanner,
    },
};

use super::ResolvedTranslation;

/// The union of one or more character classes, pre-resolved to their actual member
/// characters.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Default)]
pub struct ResolvedClasses {
    /// The characters contained in this character class.
    ///
    /// A set, held as a sorted, deduped `Vec` rather than as a `BTreeSet`. This type is
    /// embedded in [`ResolvedTransition`], the key type of `TrieNode::transitions`, so
    /// `Trie::insert`'s `entry` call compares two of these on every rule inserted, and a flat
    /// scan beats walking two trees: `BTreeSet` was measured at +26% on
    /// `parse_and_compile/en-ueb-g2` (19.0ms to 23.9ms), +10% on `zh-tw`, and +2.7% on
    /// `translate`. A `HashSet` cannot be used at all -- it is neither `Ord` nor `Hash`.
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
    /// The negation of [`Transition::Start`]: the preceding character must be in none of
    /// these classes. The start of the input counts as a space.
    NotStart(Vec<CharacterClass>),
    /// Non-consuming lookahead: the character immediately after the match must be in one of
    /// these classes. Inserted at the end of the path (after character transitions).
    End(Vec<CharacterClass>),
    /// Non-consuming lookbehind for backward translation: a word begins before the match
    /// (see [`BegWordScanner`]).
    ///
    /// A transition rather than a candidate filter in `PrimaryTable::trace`, because
    /// `Trie::insert` counts transitions towards a rule's weight, which breaks ties between
    /// equally long matches. Like `Start`, it adds one, however far the scan reads.
    BegWord,
    /// Non-consuming lookahead for backward translation: the word ends after the match (see
    /// [`EndWordScanner`]). A transition for the same reason as [`Transition::BegWord`].
    EndWord,
    /// The negation of [`Transition::EndWord`]: the word must continue after the match.
    ContinuesWord,
    /// Forward, in a table with a `seqdelimiter`: the text before the match lets it stand
    /// alone (see [`StandingAloneScanner`]).
    StandsAloneBefore,
    /// Forward, in a table with a `seqdelimiter`: the text after the match lets it stand
    /// alone.
    StandsAloneAfter,
}

/// The resolved, trie-internal counterpart of [`Transition`] plus the character transitions
/// that only ever arise while inserting a plain character sequence.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
enum ResolvedTransition {
    Character(char),
    Start(ResolvedClasses),
    NotStart(ResolvedClasses),
    End(ResolvedClasses),
    BegWord,
    EndWord,
    ContinuesWord,
    StandsAloneBefore,
    StandsAloneAfter,
}

/// The class constraints a rule gets from the liblouis `before`/`after` keywords. A side with
/// several classes is satisfied by any one of them.
#[derive(Debug, Clone, Default)]
pub struct ClassConstraints {
    /// `after CLASS`: the character before the match must be in one of these (lookbehind).
    preceding: Vec<CharacterClass>,
    /// `before CLASS`: the character after the match must be in one of these (lookahead).
    following: Vec<CharacterClass>,
}

impl ClassConstraints {
    pub fn new(preceding: Vec<CharacterClass>, following: Vec<CharacterClass>) -> Self {
        Self {
            preceding,
            following,
        }
    }
}

#[derive(Default, Debug, Clone)]
struct TrieNode {
    translations: Vec<ResolvedTranslation>,
    // `BTreeMap` rather than `HashMap`: measured ~12% faster to compile en-ueb-g2, since an
    // ordered compare of two keys short-circuits on the discriminant where hashing one had to
    // hash every member of its `ResolvedClasses` (whose own layout is chosen for the same
    // comparison -- see `ResolvedClasses::chars`). Iteration order does not affect results:
    // `Rank` separates candidates from different rules.
    transitions: BTreeMap<ResolvedTransition, TrieNode>,
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
}

#[derive(Default, Debug, Clone)]
pub struct Trie {
    root: TrieNode,
    /// Character classes for the lookahead checks (`Transition::End`,
    /// `ClassConstraints::following`), which read the input after the match: text classes for
    /// forward translation, braille classes for backward.
    lookahead_classes: CharacterClasses,
    /// Character classes for the lookbehind checks (`Transition::Start`,
    /// `ClassConstraints::preceding`), which read the last character of `preceding`. The same as
    /// `lookahead_classes` unless set with [`Trie::with_lookbehind_classes`]: backward,
    /// `preceding` is text while the input is braille.
    lookbehind_classes: CharacterClasses,
    /// When true, character transitions are matched on the exact character instead of
    /// being lowercased first. Needed for `comp6`, where `comp6 a 1` and `comp6 A 17`
    /// are independent rules.
    case_sensitive: bool,
    /// Resolves [`Transition::BegWord`]. Empty for forward tries, which never insert it.
    beg_word: BegWordScanner,
    /// Resolves [`Transition::EndWord`] and [`Transition::ContinuesWord`]. Empty for forward
    /// tries, which never insert them.
    end_word: EndWordScanner,
    /// Resolves [`Transition::StandsAloneBefore`] and [`Transition::StandsAloneAfter`],
    /// which only tries for a table with a `seqdelimiter` insert.
    standing_alone: StandingAloneScanner,
}

impl Trie {
    pub fn new() -> Self {
        Trie {
            root: TrieNode::default(),
            lookahead_classes: CharacterClasses::default(),
            lookbehind_classes: CharacterClasses::default(),
            case_sensitive: false,
            beg_word: BegWordScanner::default(),
            end_word: EndWordScanner::default(),
            standing_alone: StandingAloneScanner::default(),
        }
    }

    /// Set the classes for both the lookahead and the lookbehind checks
    pub fn with_context(self, ctx: CharacterClasses) -> Self {
        Trie {
            lookbehind_classes: ctx.clone(),
            lookahead_classes: ctx,
            ..self
        }
    }

    /// Set the classes for the lookbehind checks only
    pub fn with_lookbehind_classes(self, lookbehind_classes: CharacterClasses) -> Self {
        Trie {
            lookbehind_classes,
            ..self
        }
    }

    pub fn with_beg_word_scanner(self, beg_word: BegWordScanner) -> Self {
        Trie { beg_word, ..self }
    }

    pub fn with_end_word_scanner(self, end_word: EndWordScanner) -> Self {
        Trie { end_word, ..self }
    }

    pub fn with_standing_alone_scanner(self, standing_alone: StandingAloneScanner) -> Self {
        Trie {
            standing_alone,
            ..self
        }
    }

    pub fn case_sensitive(self) -> Self {
        Trie {
            case_sensitive: true,
            ..self
        }
    }

    fn resolve_transition(&self, transition: Transition) -> ResolvedTransition {
        match transition {
            Transition::Start(classes) => ResolvedTransition::Start(ResolvedClasses::resolve(
                &self.lookbehind_classes,
                &classes,
            )),
            Transition::NotStart(classes) => ResolvedTransition::NotStart(
                ResolvedClasses::resolve(&self.lookbehind_classes, &classes),
            ),
            Transition::End(classes) => {
                ResolvedTransition::End(ResolvedClasses::resolve(&self.lookahead_classes, &classes))
            }
            Transition::BegWord => ResolvedTransition::BegWord,
            Transition::EndWord => ResolvedTransition::EndWord,
            Transition::ContinuesWord => ResolvedTransition::ContinuesWord,
            Transition::StandsAloneBefore => ResolvedTransition::StandsAloneBefore,
            Transition::StandsAloneAfter => ResolvedTransition::StandsAloneAfter,
        }
    }
    pub fn insert_char(
        &mut self,
        from: char,
        to: &str,
        direction: Direction,
        stage: TranslationStage,
        origin: &AnchoredRule,
    ) {
        self.insert(
            &from.to_string(),
            to,
            None,
            None,
            direction,
            ClassConstraints::default(), // char rules never carry class constraints
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
        class_constraints: ClassConstraints,
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

        // the lookbehind on `preceding` goes before the char transitions
        if !class_constraints.preceding.is_empty() {
            length += 1;
            let resolved =
                ResolvedClasses::resolve(&self.lookbehind_classes, &class_constraints.preceding);
            current_node = current_node
                .transitions
                .entry(ResolvedTransition::Start(resolved))
                .or_default();
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

        // the lookahead on the next char goes after them
        if !class_constraints.following.is_empty() {
            length += 1;
            let resolved =
                ResolvedClasses::resolve(&self.lookahead_classes, &class_constraints.following);
            current_node = current_node
                .transitions
                .entry(ResolvedTransition::End(resolved))
                .or_default();
        }

        // Rules with the same input and boundaries end up here together. They are all kept:
        // which one applies is decided by `Rank` among the candidates that pass `trace()`'s
        // checks, so a rule that fails one (e.g. a `prepunc` mid-word) leaves the others in
        // play, as when liblouis moves on to the next rule in its chain.
        current_node.translations.push(ResolvedTranslation::new(
            from,
            to,
            length,
            stage,
            origin.clone(),
        ));
    }

    fn find_translations_from_node(
        &self,
        input: &str,
        preceding: &str,
        node: &TrieNode,
        partial: bool,
    ) -> Vec<ResolvedTranslation> {
        let mut matching_rules = Vec::new();
        let mut chars = input.chars();

        // add this node's translations to the list of matching rules
        matching_rules.extend(node.translations.iter().cloned());
        let c = chars.next();
        if let Some(c) = c {
            let bytes = c.len_utf8();
            if let Some(node) = node.char_transition(c, self.case_sensitive) {
                // lookbehind checks sit before the characters of a rule, so `preceding`
                // needn't grow as they are consumed
                matching_rules.extend(self.find_translations_from_node(
                    &input[bytes..],
                    preceding,
                    node,
                    partial,
                ));
            }
        }
        // Non-consuming checks: they look at the context of the match without consuming input
        for (transition, child_node) in &node.transitions {
            let context_matches = match transition {
                ResolvedTransition::Start(resolved) => resolved.matches(preceding.chars().last()),
                ResolvedTransition::NotStart(resolved) => {
                    !resolved.matches(preceding.chars().last())
                }
                ResolvedTransition::End(resolved) => resolved.matches(c),
                ResolvedTransition::BegWord => self.beg_word.is_beg_word(preceding),
                // in a partial input no word ends: it may go on (`partialTrans`)
                ResolvedTransition::EndWord => !partial && self.end_word.is_end_word(input),
                ResolvedTransition::ContinuesWord => partial || !self.end_word.is_end_word(input),
                ResolvedTransition::StandsAloneBefore => {
                    self.standing_alone.stands_alone_before(preceding)
                }
                ResolvedTransition::StandsAloneAfter => {
                    self.standing_alone.stands_alone_after(input)
                }
                // Character transitions have been dealt with above
                ResolvedTransition::Character(_) => false,
            };
            if context_matches {
                matching_rules.extend(
                    self.find_translations_from_node(input, preceding, child_node, partial),
                );
            }
        }
        matching_rules
    }

    /// The rules that match at the start of `input`, given the `preceding` text: the input
    /// already consumed forward, the output already produced backward.
    pub fn find_translations(&self, input: &str, preceding: &str) -> Vec<ResolvedTranslation> {
        self.find_translations_from_node(input, preceding, &self.root, false)
    }

    /// Like [`Self::find_translations`], but when `partial`, the input may end in the
    /// middle of a word, so no backward word-end check holds anywhere in it, as with
    /// liblouis's `partialTrans`.
    pub fn find_translations_partial(
        &self,
        input: &str,
        preceding: &str,
        partial: bool,
    ) -> Vec<ResolvedTranslation> {
        self.find_translations_from_node(input, preceding, &self.root, partial)
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
    fn beg_word_transition_gates_a_rule() {
        let classes = CharacterClasses::new(&[
            (CharacterClass::Space, &[' ']),
            (CharacterClass::Letter, &['a']),
        ]);
        let mut trie = Trie::new().with_beg_word_scanner(BegWordScanner::new(&classes));
        trie.insert(
            "(",
            "⠶",
            Some(Transition::BegWord),
            None,
            Direction::Backward,
            ClassConstraints::default(),
            TranslationStage::Main,
            &fake_rule(),
        );

        assert!(!trie.find_translations("⠶", "").is_empty());
        assert!(!trie.find_translations("⠶", "a ").is_empty());
        assert!(trie.find_translations("⠶", "a").is_empty());
    }

    #[test]
    fn end_word_transitions_gate_a_rule() {
        let letter = AnchoredRule::new(RuleParser::new("letter a 1").rule().unwrap(), None, 0);
        let mut trie = Trie::new().with_end_word_scanner(EndWordScanner::compile(&[letter]));
        for (chars, dots, transition) in [
            ("ab", "⠁⠃", Transition::EndWord),
            ("cd", "⠉⠙", Transition::ContinuesWord),
        ] {
            trie.insert(
                chars,
                dots,
                None,
                Some(transition),
                Direction::Backward,
                ClassConstraints::default(),
                TranslationStage::Main,
                &fake_rule(),
            );
        }

        // nothing follows, so the word ends
        assert!(!trie.find_translations("⠁⠃", "").is_empty());
        assert!(trie.find_translations("⠉⠙", "").is_empty());
        // a letter follows, so the word continues
        assert!(trie.find_translations("⠁⠃⠁", "").is_empty());
        assert!(!trie.find_translations("⠉⠙⠁", "").is_empty());
    }

    #[test]
    fn empty_trie() {
        let trie = Trie::new();
        assert_eq!(
            trie.find_translations("foo", ""),
            Vec::<ResolvedTranslation>::new()
        );
    }

    #[test]
    fn find_translations_keeps_rules_on_the_same_node() {
        // both rules are candidates, so `trace()` can reject one (e.g. a `prepunc` mid-word)
        // and still apply the other
        let mut trie = Trie::new();
        let prepunc: AnchoredRule = RuleParser::new("prepunc ! 235").rule().unwrap().into();
        let always: AnchoredRule = RuleParser::new("always ! 2356").rule().unwrap().into();
        for (rule, dots) in [(&prepunc, "⠖"), (&always, "⠷")] {
            trie.insert(
                "!",
                dots,
                None,
                None,
                Direction::Forward,
                ClassConstraints::default(),
                TranslationStage::Main,
                rule,
            );
        }
        assert_eq!(
            trie.find_translations("!", ""),
            vec![
                ResolvedTranslation::new("!", "⠖", 1, TranslationStage::Main, prepunc),
                ResolvedTranslation::new("!", "⠷", 1, TranslationStage::Main, always),
            ]
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
            ClassConstraints::default(),
            TranslationStage::Main,
            &rule,
        );
        trie.insert(
            "f",
            "F",
            None,
            None,
            Direction::Forward,
            ClassConstraints::default(),
            TranslationStage::Main,
            &rule,
        );
        trie.insert(
            "fo",
            "FO",
            None,
            None,
            Direction::Forward,
            ClassConstraints::default(),
            TranslationStage::Main,
            &rule,
        );
        trie.insert(
            "foo",
            "FOO",
            None,
            None,
            Direction::Forward,
            ClassConstraints::default(),
            TranslationStage::Main,
            &rule,
        );
        trie.insert(
            "foobar",
            "FOOBAR",
            None,
            None,
            Direction::Forward,
            ClassConstraints::default(),
            TranslationStage::Main,
            &rule,
        );
        assert_eq!(trie.find_translations("a", ""), vec![a]);
        assert_eq!(trie.find_translations("f", ""), vec![f.clone()]);
        assert_eq!(
            trie.find_translations("fo", ""),
            vec![f.clone(), fo.clone()]
        );
        assert_eq!(
            trie.find_translations("foobar", ""),
            vec![f.clone(), fo.clone(), foo.clone(), foobar.clone()]
        );
        assert_eq!(
            trie.find_translations("foobaz", ""),
            vec![f.clone(), fo.clone(), foo.clone()]
        );
        assert_eq!(trie.find_translations("function", ""), vec![f.clone()]);
        assert_eq!(
            trie.find_translations("folio", ""),
            vec![f.clone(), fo.clone()]
        );
        assert_eq!(trie.find_translations("none", ""), empty);
        assert_eq!(
            trie.find_translations("The start of a long long sentence", ""),
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
            ClassConstraints::default(),
            TranslationStage::Main,
            &rule,
        );
        assert_eq!(trie.find_translations("a", ""), vec![a]);
        assert_eq!(trie.find_translations("aha", ""), empty);
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
            ClassConstraints::default(),
            TranslationStage::Main,
            &rule,
        );
        assert_eq!(trie.find_translations("foo", ""), empty);
        assert_eq!(trie.find_translations("foo ", ""), empty);
        assert_eq!(trie.find_translations("foobar", ""), vec![foo]);
        assert_eq!(trie.find_translations("foo.", ""), empty);
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
            ClassConstraints::default(),
            TranslationStage::Main,
            &rule,
        );
        assert_eq!(trie.find_translations("foo", ""), empty);
        assert_eq!(trie.find_translations("foo", " "), empty);
        assert_eq!(trie.find_translations("foo", "."), empty);
        assert_eq!(trie.find_translations("foo", "c"), vec![foo]);
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
            ClassConstraints::default(),
            TranslationStage::Main,
            &rule,
        );
        assert_eq!(trie.find_translations("foo", ""), empty);
        assert_eq!(trie.find_translations("foo", " "), empty);
        assert_eq!(trie.find_translations("foo", "."), empty);
        assert_eq!(trie.find_translations("foo", "c"), empty);
        assert_eq!(trie.find_translations("foobar", "c"), vec![foo]);
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
            ClassConstraints::default(),
            TranslationStage::Main,
            &rule,
        );
        assert_eq!(trie.find_translations("aaa", ""), empty);
        assert_eq!(trie.find_translations("aaa1", " "), vec![foo.clone()]);
        assert_eq!(trie.find_translations("aaa1", "."), vec![foo.clone()]);
        assert_eq!(trie.find_translations("aaa1", "c"), empty);
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
            ClassConstraints::default(),
            TranslationStage::Main,
            &rule,
        );
        assert_eq!(trie.find_translations("st", ""), empty);
        assert_eq!(trie.find_translations("st", " "), empty);
        assert_eq!(trie.find_translations("st", "."), empty);
        assert_eq!(trie.find_translations("st", "1"), vec![foo]);
        assert_eq!(trie.find_translations("sta", "1"), empty);
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
            ClassConstraints::default(),
            TranslationStage::Main,
            &rule,
        );
        assert_eq!(trie.find_translations("(", ""), empty);
        assert_eq!(trie.find_translations("(", " "), empty);
        assert_eq!(trie.find_translations("(", "."), empty);
        assert_eq!(trie.find_translations("(", "a"), vec![foo]);
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
            ClassConstraints::default(),
            TranslationStage::Main,
            &rule,
        );
        assert_eq!(trie.find_translations("(", ""), empty);
        assert_eq!(trie.find_translations("(", " "), empty);
        assert_eq!(trie.find_translations("(", "."), empty);
        assert_eq!(trie.find_translations("(", "a"), empty);
        assert_eq!(trie.find_translations("(a", ""), vec![foo.clone()]);
        assert_eq!(trie.find_translations("(a", "("), vec![foo.clone()]);
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
            ClassConstraints::default(),
            TranslationStage::Main,
            &rule,
        );
        assert_eq!(trie.find_translations("foo", ""), vec![foo.clone()]);
        assert_eq!(trie.find_translations("Foo", ""), vec![foo.clone()]);
        assert_eq!(trie.find_translations("FOO", ""), vec![foo.clone()]);
        assert_eq!(trie.find_translations("foO", ""), vec![foo.clone()]);
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
                ClassConstraints::default(),
                TranslationStage::Main,
                &rule,
            );
        }
        assert_eq!(trie.find_translations("a", ""), vec![lower]);
        assert_eq!(trie.find_translations("A", ""), vec![upper]);
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
            ClassConstraints {
                following: vec![CharacterClass::Letter],
                ..Default::default()
            },
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
            ClassConstraints {
                following: vec![CharacterClass::Digit],
                ..Default::default()
            },
            TranslationStage::Main,
            &rule,
        );
        // Both rules coexist on different paths — neither stomps the other
        assert_eq!(trie.find_translations("fooa", ""), vec![foo_before_letter]);
        assert_eq!(trie.find_translations("foo1", ""), vec![foo_before_digit]);
        assert_eq!(trie.find_translations("foo ", ""), empty);
        assert_eq!(trie.find_translations("foo", ""), empty);
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
            ClassConstraints {
                preceding: vec![CharacterClass::Letter],
                ..Default::default()
            },
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
            ClassConstraints {
                preceding: vec![CharacterClass::Digit],
                ..Default::default()
            },
            TranslationStage::Main,
            &rule,
        );
        assert_eq!(trie.find_translations("foo", "a"), vec![foo_after_letter]);
        assert_eq!(trie.find_translations("foo", "1"), vec![foo_after_digit]);
        assert_eq!(trie.find_translations("foo", " "), empty);
        assert_eq!(trie.find_translations("foo", ""), empty);
    }

    #[test]
    fn find_translations_with_not_start_class() {
        let ctx = CharacterClasses::new(&[
            (CharacterClass::Letter, &['a', 'b']),
            (CharacterClass::Space, &[' ']),
        ]);
        let mut trie = Trie::new().with_context(ctx);
        let rule = fake_rule();
        let foo = ResolvedTranslation::new("foo", "F", 4, TranslationStage::Main, rule.clone());
        // the preceding character must not be a letter
        trie.insert(
            "foo",
            "F",
            Some(Transition::NotStart(vec![CharacterClass::Letter])),
            None,
            Direction::Forward,
            ClassConstraints::default(),
            TranslationStage::Main,
            &rule,
        );
        assert_eq!(trie.find_translations("foo", "a"), vec![]);
        assert_eq!(trie.find_translations("foo", " "), vec![foo.clone()]);
        assert_eq!(trie.find_translations("foo", "-"), vec![foo.clone()]);
        // the start of the input counts as a space
        assert_eq!(trie.find_translations("foo", ""), vec![foo]);
    }
}
