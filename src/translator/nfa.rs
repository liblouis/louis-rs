//! Very simple regular expression engine along the lines of chapter 3
//! of the Dragon book
//!
//! A Nondeterministic Finite Automata (NFA) is constructed from an
//! abstract syntax tree (AST). To determine whether a given string
//! matches the regular expression we walk the NFA and check if any of
//! the reachable states is an accepting state
use std::collections::{HashMap, HashSet, VecDeque};

use super::Translation;

/// Reference to a state in the [NFA] states vector
type StateId = usize;

/// A state in the [NFA]
///
/// An accepting state contains a [Translation]
#[derive(Debug, Default)]
struct State {
    translation: Option<Translation>,
}

/// An transition between two [States](State) in the [NFA]
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
enum Transition {
    /// A transition that accepts a character
    Character(char),
    /// A transition that accepts any character
    Any,
    /// An Offset transition is essentially an epsilon transition that marks the end of a
    /// non-capturing group. It is used to mark the end of the pre pattern in match rules
    Offset,
    CaptureStart,
    CaptureEnd,
}

/// An NFA consisting of a set of states and transitions between them
///
/// Basically an implementation of the data structure laid out in
/// chapter 3 of the dragon book. `transitions` can only contain one
/// transition starting from a state for a type of transition, whereas
/// there can be any number of epsilon transitions originating from a
/// state. That is why `epsilon_transitions` contains a set of states
/// for a given start state
#[derive(Default, Debug)]
pub struct NFA {
    states: Vec<State>,
    start: StateId,
    end: StateId,
    transitions: HashMap<(StateId, Transition), StateId>,
    epsilon_transitions: HashMap<StateId, HashSet<StateId>>,
}

/// A fragment of an [AST] with a `start` and `end` state
///
/// Used to construct an NFA from smaller parts
#[derive(Debug, Default)]
pub struct Fragment {
    start: StateId,
    end: StateId,
}

/// An abstract syntax tree
#[derive(Debug)]
pub enum AST {
    Character(char),
    String(String),
    Set(HashSet<char>),
    Any,
    Concat(Box<AST>, Box<AST>),
    ZeroOrMore(Box<AST>),
    Optional(Box<AST>),
    OneOrMore(Box<AST>),
    RepeatExactly(u8, Box<AST>),
    RepeatAtLeast(u8, Box<AST>),
    RepeatAtLeastAtMost(u8, u8, Box<AST>),
    Either(Box<AST>, Box<AST>),
    Offset,
    Capture(Box<AST>),
    /// Stop-gap blanket "implementation" for things that might be needed but are not yet
    /// implemented
    NotImplemented,
}

impl NFA {
    fn new() -> NFA {
        NFA {
            start: 0,
            end: 0,
            states: vec![],
            transitions: HashMap::new(),
            epsilon_transitions: HashMap::new(),
        }
    }

    fn is_empty(&self) -> bool {
        self.states.is_empty()
    }

    fn add_state(&mut self, state: State) -> StateId {
        let idx = self.states.len();
        self.states.push(state);
        idx
    }

    fn set_accepting(&mut self, end: StateId, translation: Translation) {
        self.states[end].translation = Some(translation);
    }

    /// Add a character transition to the NFA
    fn add_char(&mut self, c: char) -> Fragment {
        let start = self.add_state(State::default());
        let end = self.add_state(State::default());
        self.transitions
            .insert((start, Transition::Character(c)), end);
        Fragment { start, end }
    }

    /// Add a sequence of character transitions to the NFA
    fn add_string(&mut self, s: &str) -> Fragment {
        let start = self.add_state(State::default());
        let mut prev = start;
        let mut end = start;
        for c in s.chars() {
            end = self.add_state(State::default());
            self.transitions
                .insert((prev, Transition::Character(c)), end);
            prev = end;
        }
        Fragment { start, end }
    }

    /// Add an epsilon transition to the NFA
    fn add_epsilon(&mut self, start: StateId, end: StateId) -> Fragment {
        self.epsilon_transitions
            .entry(start)
            .or_default()
            .insert(end);
        Fragment { start, end }
    }

    /// Add an "any" transition to the NFA
    fn add_any(&mut self) -> Fragment {
        let start = self.add_state(State::default());
        let end = self.add_state(State::default());
        self.transitions.insert((start, Transition::Any), end);
        Fragment { start, end }
    }

    /// Add a Offset transition to the NFA
    fn add_offset(&mut self) -> Fragment {
        let start = self.add_state(State::default());
        let end = self.add_state(State::default());
        self.transitions.insert((start, Transition::Offset), end);
        Fragment { start, end }
    }

    /// Add a Capture transitions around a fragment to the NFA
    fn add_capture(&mut self, fragment: &Fragment) -> Fragment {
        let start = self.add_state(State::default());
        let end = self.add_state(State::default());
        self.transitions
            .insert((start, Transition::CaptureStart), fragment.start);
        self.transitions
            .insert((fragment.end, Transition::CaptureEnd), end);
        Fragment { start, end }
    }

    /// Combine two NFAs into the union of both
    pub fn add_union(&mut self, r1: &Fragment, r2: &Fragment) -> Fragment {
        let start = self.add_state(State::default());
        let end = self.add_state(State::default());
        self.add_epsilon(start, r1.start);
        self.add_epsilon(start, r2.start);
        self.add_epsilon(r1.end, end);
        self.add_epsilon(r2.end, end);
        Fragment { start, end }
    }

    /// Combine two NFAs into the concatenation of both
    fn add_concatenation(&mut self, r1: &Fragment, r2: &Fragment) -> Fragment {
        // FIXME: instead of adding an epsilon transition between the
        // end node of r1 and the start node of r2 we should *merge* the
        // two nodes as outlined in the dragon book. But for that we
        // need a way to find all transitions going of a certain node.
        // This doesn't feel very easy with the current data structure
        self.add_epsilon(r1.end, r2.start);
        Fragment {
            start: r1.start,
            end: r2.end,
        }
    }

    /// Wrap an NFA so that it can be repeated zero or many times
    fn add_kleene(&mut self, r: &Fragment) -> Fragment {
        let start = self.add_state(State::default());
        let end = self.add_state(State::default());
        self.add_epsilon(start, r.start);
        self.add_epsilon(start, end);
        self.add_epsilon(r.end, r.start);
        self.add_epsilon(r.end, end);
        Fragment { start, end }
    }

    /// Wrap an NFA so that it is optional
    fn add_optional(&mut self, r: &Fragment) -> Fragment {
        self.add_epsilon(r.start, r.end);
        Fragment {
            start: r.start,
            end: r.end,
        }
    }

    fn add_character_class(&mut self, chars: &HashSet<char>) -> Fragment {
        let start = self.add_state(State::default());
        let end = self.add_state(State::default());
        for c in chars {
            self.transitions
                .insert((start, Transition::Character(*c)), end);
        }
        Fragment { start, end }
    }

    fn add_noop(&mut self) -> Fragment {
        let start = self.add_state(State::default());
        let end = self.add_state(State::default());
        self.add_epsilon(start, end);
        Fragment { start, end }
    }

    fn add_fragment(&mut self, ast: &AST) -> Fragment {
        match ast {
            AST::Character(c) => self.add_char(*c),
            AST::String(s) => self.add_string(s),
            AST::Set(chars) => self.add_character_class(chars),
            AST::Any => self.add_any(),
            AST::Concat(ast1, ast2) => {
                let r1 = self.add_fragment(ast1);
                let r2 = self.add_fragment(ast2);
                self.add_concatenation(&r1, &r2)
            }
            AST::ZeroOrMore(ast) => {
                let fragment = self.add_fragment(ast);
                self.add_kleene(&fragment)
            }
            AST::Optional(ast) => {
                let fragment = self.add_fragment(ast);
                self.add_optional(&fragment)
            }
            AST::OneOrMore(ast) => {
                let one = self.add_fragment(ast);
                let fragment = self.add_fragment(ast);
                let kleene = self.add_kleene(&fragment);
                self.add_concatenation(&one, &kleene)
            }
            AST::Either(ast1, ast2) => {
                let r1 = self.add_fragment(ast1);
                let r2 = self.add_fragment(ast2);
                self.add_union(&r1, &r2)
            }
            AST::RepeatExactly(n, ast) => {
                let mut concatenation = self.add_fragment(ast);
                for _ in 1..*n {
                    let repeat = self.add_fragment(ast);
                    concatenation = self.add_concatenation(&concatenation, &repeat);
                }
                concatenation
            }
            AST::RepeatAtLeast(n, ast) => {
                let mut concatenation = self.add_fragment(ast);
                for _ in 1..*n {
                    let repeat = self.add_fragment(ast);
                    concatenation = self.add_concatenation(&concatenation, &repeat);
                }
                let fragment = self.add_fragment(ast);
                let kleene = self.add_kleene(&fragment);
                self.add_concatenation(&concatenation, &kleene)
            }
            AST::RepeatAtLeastAtMost(min, max, ast) => {
                let mut concatenation = self.add_fragment(ast);
                for _ in 1..*min {
                    let repeat = self.add_fragment(ast);
                    concatenation = self.add_concatenation(&concatenation, &repeat);
                }
                for _ in *min..*max {
                    let fragment = self.add_fragment(ast);
                    let optional = self.add_optional(&fragment);
                    concatenation = self.add_concatenation(&concatenation, &optional);
                }
                concatenation
            }
            AST::Capture(ast) => {
                let fragment = self.add_fragment(ast);
                self.add_capture(&fragment)
            }
            AST::Offset => self.add_offset(),
            AST::NotImplemented => self.add_noop(),
        }
    }

    pub fn add_accepting_fragment(&mut self, ast: &AST, translation: Translation) -> Fragment {
        let fragment = self.add_fragment(ast);
        self.set_accepting(fragment.end, translation);
        fragment
    }

    pub fn merge_accepting_fragment(&mut self, ast: &AST, translation: Translation) {
        let fragment = self.add_accepting_fragment(ast, translation);
        // if we haven't added any fragments just add the fragment without a union
        if self.start == 0 && self.end == 0 {
            self.start = fragment.start;
            self.end = fragment.end;
        } else {
            let existing = Fragment {
                start: self.start,
                end: self.end,
            };
            let union = self.add_union(&existing, &fragment);
            self.start = union.start;
            self.end = union.end;
        }
    }

    /// Return all states that are reachable from a set of `states` via epsilon transitions
    fn epsilon_closure(&self, states: &HashSet<StateId>) -> HashSet<StateId> {
        let mut closure: HashSet<StateId> = states.clone();
        let mut queue = VecDeque::from(states.iter().cloned().collect::<Vec<_>>());

        while let Some(state) = queue.pop_front() {
            if let Some(next_states) = self.epsilon_transitions.get(&state) {
                for state in next_states.difference(&closure) {
                    queue.push_back(*state);
                }
                closure.extend(next_states);
            }
        }
        closure
    }

    /// Return all states that are directly reachable from the a set of `states` via the
    /// `transition`.
    fn move_state(&self, states: &HashSet<StateId>, transition: Transition) -> HashSet<StateId> {
        let mut next_states = HashSet::new();
        for from in states {
            let key = (*from, transition.clone());
            if let Some(to) = self.transitions.get(&key) {
                next_states.insert(*to);
            }
        }
        next_states
    }

    /// Simulate the NFA
    ///
    /// Given an input string, simulate the NFA to determine if the input is accepted by the input
    /// string.
    pub fn accepts(&self, input: &str) -> bool {
        let mut next_states = self.epsilon_closure(&HashSet::from([self.start]));
        for c in input.chars() {
            let reachable_via_character = self.move_state(&next_states, Transition::Character(c));
            let reachable_via_any = self.move_state(&next_states, Transition::Any);
            next_states = reachable_via_character
                .union(&reachable_via_any)
                .cloned()
                .collect();
            next_states = self.epsilon_closure(&next_states);
        }
        next_states
            .iter()
            .any(|s| self.states[*s].translation.is_some())
    }

    fn find_translations_from_state(
        &self,
        state: StateId,
        input: &str,
        match_length: usize,
        offset: usize,
        capturing: bool,
        capture: &str,
    ) -> Vec<Translation> {
        let mut matching_rules = Vec::new();

        // return early if the nfa is empty
        if self.is_empty() {
            return matching_rules;
        };

        let next_states = self.epsilon_closure(&HashSet::from([state]));

        // if any of the states in the epsilon closure (reachable via epsilon transition)
        // has a translation add it to the list of matching rules
        matching_rules.extend(
            next_states
                .iter()
                .flat_map(|state| &self.states[*state].translation)
                .map(|translation| {
                    translation
                        .clone()
                        .with_offset(offset)
                        // when dealing with matches that can vary in size we need to calculate the
                        // weight at run-time and set it accordingly
                        .with_weight_maybe(match_length)
                        .with_capture(&capture)
                }),
        );

        // traverse all states that are reachable via an offset transition (essentially an
        // epsilon transition that marks the end of a non-capture group)
        let reachable_via_offset = self.move_state(&next_states, Transition::Offset);
        let next_states_with_offset = self.epsilon_closure(&reachable_via_offset);
        for state in next_states_with_offset {
            matching_rules.extend(self.find_translations_from_state(
                state,
                input,
                match_length,
                match_length,
                capturing,
                capture,
            ));
        }

        // traverse all states that are reachable via a CaptureStart transition (an epsilon
        // transition that marks the start of a capture group)
        let reachable_via_capture_start = self.move_state(&next_states, Transition::CaptureStart);
        let next_states_with_capture_start = self.epsilon_closure(&reachable_via_capture_start);
        for state in next_states_with_capture_start {
            matching_rules.extend(self.find_translations_from_state(
                state,
                input,
                match_length,
                match_length,
                true,
                capture,
            ));
        }

        // traverse all states that are reachable via a CaptureEnd transition (an epsilon transition
        // that marks the end of a capture group)
        let reachable_via_capture_end = self.move_state(&next_states, Transition::CaptureEnd);
        let next_states_with_capture_end = self.epsilon_closure(&reachable_via_capture_end);
        for state in next_states_with_capture_end {
            matching_rules.extend(self.find_translations_from_state(
                state,
                input,
                match_length,
                offset,
                false,
                capture,
            ));
        }

        if let Some(c) = input.chars().next() {
            let reachable_via_character = self.move_state(&next_states, Transition::Character(c));
            let reachable_via_any = self.move_state(&next_states, Transition::Any);
            let mut next_states = reachable_via_character
                .union(&reachable_via_any)
                .cloned()
                .collect();
            next_states = self.epsilon_closure(&next_states);
            let mut capture = capture.to_string();
            if capturing {
                capture.push(c);
            };
            for state in next_states {
                let bytes = c.len_utf8();
                matching_rules.extend(self.find_translations_from_state(
                    state,
                    &input[bytes..],
                    match_length + 1,
                    offset,
                    capturing,
                    capture.as_str(),
                ));
            }
        }

        matching_rules
    }

    pub fn find_translations(&self, input: &str) -> Vec<Translation> {
        let mut translations =
            self.find_translations_from_state(self.start, input, 0, 0, false, "");
        translations.sort_by(|a, b| b.weight().cmp(&a.weight()));
        // FIXME: It feels a bit smelly to have to dedup the list of translations. Maybe
        // there is something wrong how we traverse the NFA or even how we build it?
        translations.dedup();
        translations
    }
}

/**
 * Generate a DOT structured string.
 */
pub fn nfa_dot(nfa: &NFA) -> String {
    let mut dot = String::from("digraph nfa {\n\trankdir=\"LR\";\n\tnode [shape = circle];\n");
    dot.push_str(&format!(
        "\tstart [shape=\"none\"]\n\tstart -> {}\n",
        nfa.start
    ));
    for (idx, state) in nfa.states.iter().enumerate() {
        if state.translation.is_some() {
            dot.push_str(&format!("\t{} [shape=\"doublecircle\"]\n", idx));
        }
    }
    for ((from, transition), to) in nfa.transitions.iter() {
        match transition {
            Transition::Character(c) => {
                dot.push_str(&format!("\t{} -> {} [label=\"{}\"]\n", from, to, c))
            }
            Transition::Any => {
                dot.push_str(&format!("\t{} -> {} [label=\"{}\"]\n", from, to, "Any"))
            }
            Transition::Offset => {
                dot.push_str(&format!("\t{} -> {} [label=\"{}\"]\n", from, to, "Offset"))
            }
            Transition::CaptureStart => dot.push_str(&format!(
                "\t{} -> {} [label=\"{}\"]\n",
                from, to, "Capture start"
            )),
            Transition::CaptureEnd => dot.push_str(&format!(
                "\t{} -> {} [label=\"{}\"]\n",
                from, to, "Capture end"
            )),
        }
    }
    for (from, tos) in nfa.epsilon_transitions.iter() {
        for to in tos.iter() {
            dot.push_str(&format!("\t{} -> {} [label=\"{}\"]\n", from, to, "ε"));
        }
    }
    dot += "}";
    dot
}

#[cfg(test)]
mod tests {
    use crate::translator::TranslationStage;

    use super::*;

    /// Create an NFA from an abstract syntax tree `ast`
    impl From<&AST> for NFA {
        fn from(ast: &AST) -> Self {
            let mut nfa = NFA::new();
            let body = nfa.add_accepting_fragment(ast, Translation::default());
            nfa.start = body.start;
            nfa
        }
    }

    #[test]
    fn character() {
        let ast = AST::Character('a');
        let nfa = NFA::from(&ast);
        assert!(nfa.accepts("a"));
        assert!(!nfa.accepts("b"));
    }

    #[test]
    fn find_character() {
        let ast = AST::Character('a');
        let nfa = NFA::from(&ast);
        assert!(!nfa.find_translations("a").is_empty());
        assert!(nfa.find_translations("b").is_empty());
    }

    #[test]
    fn alteration() {
        let ast = AST::Either(Box::new(AST::Character('a')), Box::new(AST::Character('b')));
        let nfa = NFA::from(&ast);
        assert!(nfa.accepts("a"));
        assert!(nfa.accepts("b"));
        assert!(!nfa.accepts("ab"));
        assert!(!nfa.accepts("c"));
    }

    #[test]
    fn find_alteration() {
        let ast = AST::Either(Box::new(AST::Character('a')), Box::new(AST::Character('b')));
        let nfa = NFA::from(&ast);
        assert!(!nfa.find_translations("a").is_empty());
        assert!(!nfa.find_translations("b").is_empty());
        assert!(!nfa.find_translations("ab").is_empty());
        assert!(nfa.find_translations("c").is_empty());
    }

    #[test]
    fn concatenation() {
        let ast = AST::Concat(Box::new(AST::Character('a')), Box::new(AST::Character('b')));
        let nfa = NFA::from(&ast);
        assert!(nfa.accepts("ab"));
        assert!(!nfa.accepts("a"));
        assert!(!nfa.accepts("b"));
        assert!(!nfa.accepts("ba"));
        assert!(!nfa.accepts("c"));
        assert!(!nfa.accepts("abc"));
    }

    #[test]
    fn find_concatenation() {
        let ast = AST::Concat(Box::new(AST::Character('a')), Box::new(AST::Character('b')));
        let nfa = NFA::from(&ast);
        assert!(!nfa.find_translations("ab").is_empty());
        assert!(!nfa.find_translations("abc").is_empty());
        assert!(nfa.find_translations("a").is_empty());
        assert!(nfa.find_translations("b").is_empty());
        assert!(nfa.find_translations("ba").is_empty());
        assert!(nfa.find_translations("c").is_empty());
    }

    #[test]
    fn kleene() {
        let ast = AST::ZeroOrMore(Box::new(AST::Character('a')));
        let nfa = NFA::from(&ast);
        assert!(nfa.accepts(""));
        assert!(nfa.accepts("a"));
        assert!(nfa.accepts("aa"));
        assert!(nfa.accepts("aaaaa"));
        assert!(!nfa.accepts("b"));
        assert!(!nfa.accepts("ba"));
        assert!(!nfa.accepts("ab"));
        assert!(!nfa.accepts("c"));
        assert!(!nfa.accepts("abc"));
    }

    #[test]
    fn find_kleene() {
        let ast = AST::ZeroOrMore(Box::new(AST::Character('a')));
        let nfa = NFA::from(&ast);
        assert!(!nfa.find_translations("").is_empty());
        assert!(!nfa.find_translations("a").is_empty());
        assert!(!nfa.find_translations("aa").is_empty());
        assert!(!nfa.find_translations("aaaaa").is_empty());
        assert!(!nfa.find_translations("ab").is_empty());
        assert!(!nfa.find_translations("abc").is_empty());
        assert!(!nfa.find_translations("b").is_empty());
        assert!(!nfa.find_translations("ba").is_empty());
        assert!(!nfa.find_translations("c").is_empty());

        let ast = AST::Concat(
            Box::new(AST::Character('a')),
            Box::new(AST::ZeroOrMore(Box::new(AST::Character('b')))),
        );
        let nfa = NFA::from(&ast);
        assert!(!nfa.find_translations("a").is_empty());
        assert!(!nfa.find_translations("aa").is_empty());
        assert!(!nfa.find_translations("ab").is_empty());
        assert!(!nfa.find_translations("abbbb").is_empty());
        assert!(nfa.find_translations("").is_empty());
        assert!(nfa.find_translations("ccccc").is_empty());
        assert!(nfa.find_translations("cb").is_empty());
        assert!(nfa.find_translations("cba").is_empty());
        assert!(nfa.find_translations("b").is_empty());
        assert!(nfa.find_translations("ba").is_empty());
        assert!(nfa.find_translations("c").is_empty());
    }

    #[test]
    fn one_or_more() {
        let ast = AST::OneOrMore(Box::new(AST::Character('a')));
        let nfa = NFA::from(&ast);
        assert!(!nfa.accepts(""));
        assert!(nfa.accepts("a"));
        assert!(nfa.accepts("aa"));
        assert!(nfa.accepts("aaaaa"));
        assert!(!nfa.accepts("b"));
        assert!(!nfa.accepts("ba"));
        assert!(!nfa.accepts("ab"));
        assert!(!nfa.accepts("c"));
        assert!(!nfa.accepts("abc"));
    }

    #[test]
    fn find_one_or_more() {
        let ast = AST::OneOrMore(Box::new(AST::Character('a')));
        let nfa = NFA::from(&ast);
        assert!(nfa.find_translations("").is_empty());
        assert!(!nfa.find_translations("a").is_empty());
        assert!(!nfa.find_translations("aa").is_empty());
        assert!(!nfa.find_translations("aaaaa").is_empty());
        assert!(nfa.find_translations("b").is_empty());
        assert!(nfa.find_translations("ba").is_empty());
        assert!(!nfa.find_translations("ab").is_empty());
        assert!(nfa.find_translations("c").is_empty());
        assert!(!nfa.find_translations("abc").is_empty());
    }

    #[test]
    fn any() {
        let ast = AST::Concat(
            Box::new(AST::Concat(
                Box::new(AST::Character('a')),
                Box::new(AST::Any),
            )),
            Box::new(AST::Character('b')),
        );
        let nfa = NFA::from(&ast);
        assert!(nfa.accepts("abb"));
    }

    #[test]
    fn find_any() {
        let ast = AST::Concat(
            Box::new(AST::Concat(
                Box::new(AST::Character('a')),
                Box::new(AST::Any),
            )),
            Box::new(AST::Character('b')),
        );
        let nfa = NFA::from(&ast);
        assert!(!nfa.find_translations("abb").is_empty());
    }

    #[test]
    fn optional() {
        let ast = AST::Concat(
            Box::new(AST::Optional(Box::new(AST::Concat(
                Box::new(AST::Character('a')),
                Box::new(AST::Any),
            )))),
            Box::new(AST::Character('b')),
        );
        let nfa = NFA::from(&ast);
        assert!(nfa.accepts("acb"));
        assert!(nfa.accepts("axb"));
        assert!(nfa.accepts("b"));
        assert!(!nfa.accepts("c"));
        assert!(!nfa.accepts("bbb"));
    }

    #[test]
    fn find_optional() {
        let ast = AST::Concat(
            Box::new(AST::Optional(Box::new(AST::Concat(
                Box::new(AST::Character('a')),
                Box::new(AST::Any),
            )))),
            Box::new(AST::Character('b')),
        );
        let nfa = NFA::from(&ast);
        assert!(!nfa.find_translations("acb").is_empty());
        assert!(!nfa.find_translations("axb").is_empty());
        assert!(!nfa.find_translations("b").is_empty());
        assert!(!nfa.find_translations("bbb").is_empty());
        assert!(nfa.find_translations("c").is_empty());
        assert!(nfa.find_translations("").is_empty());
    }

    #[test]
    fn string() {
        let ast = AST::Concat(
            Box::new(AST::Concat(
                Box::new(AST::OneOrMore(Box::new(AST::Character('(')))),
                Box::new(AST::String("hello".to_string())),
            )),
            Box::new(AST::OneOrMore(Box::new(AST::Character(')')))),
        );
        let nfa = NFA::from(&ast);
        assert!(nfa.accepts("(hello)"));
        assert!(nfa.accepts("(((((hello)))"));
        assert!(!nfa.accepts("hello"));
        assert!(!nfa.accepts("(hello"));
        assert!(!nfa.accepts("hello)"));
        assert!(!nfa.accepts("()"));
        assert!(!nfa.accepts("(helo)"));
    }

    #[test]
    fn find_string() {
        let ast = AST::Concat(
            Box::new(AST::Concat(
                Box::new(AST::OneOrMore(Box::new(AST::Character('(')))),
                Box::new(AST::String("hello".to_string())),
            )),
            Box::new(AST::OneOrMore(Box::new(AST::Character(')')))),
        );
        let nfa = NFA::from(&ast);
        assert!(!nfa.find_translations("(hello)").is_empty());
        assert!(!nfa.find_translations("(((((hello)))").is_empty());
        assert!(nfa.find_translations("hello").is_empty());
        assert!(nfa.find_translations("(hello").is_empty());
        assert!(nfa.find_translations("hello)").is_empty());
        assert!(nfa.find_translations("()").is_empty());
        assert!(nfa.find_translations("(helo)").is_empty());
    }

    #[test]
    fn find_character_class() {
        let ast = AST::Set(HashSet::from(['a', 'b']));
        let nfa = NFA::from(&ast);
        assert!(nfa.accepts("a"));
        assert!(nfa.accepts("b"));
        assert!(!nfa.accepts(""));
        assert!(!nfa.accepts("c"));
    }

    #[test]
    fn find_character_class_one_or_more() {
        let ast = AST::OneOrMore(Box::new(AST::Set(HashSet::from(['a', 'b']))));
        let nfa = NFA::from(&ast);
        assert!(nfa.accepts("a"));
        assert!(nfa.accepts("b"));
        assert!(nfa.accepts("ab"));
        assert!(nfa.accepts("abbbbbbaaaa"));
        assert!(!nfa.accepts(""));
        assert!(!nfa.accepts("c"));
    }

    #[test]
    fn find_with_offset() {
        let translation =
            Translation::new("".into(), "".into(), 7, TranslationStage::Main, None).with_offset(4);
        let ast = AST::Concat(
            Box::new(AST::Concat(
                Box::new(AST::OneOrMore(Box::new(AST::Any))),
                Box::new(AST::Offset),
            )),
            Box::new(AST::String("foo".into())),
        );
        let nfa = NFA::from(&ast);
        assert_eq!(nfa.find_translations("aaaafoo"), vec![translation.clone()]);
        assert_eq!(nfa.find_translations("____foo"), vec![translation.clone()]);
        assert_eq!(nfa.find_translations("1111foo"), vec![translation.clone()]);
        assert_eq!(nfa.find_translations("fooofoo"), vec![translation.clone()]);
        assert_ne!(nfa.find_translations("foo"), vec![translation.clone()]);
        assert_ne!(nfa.find_translations("foofoo"), vec![translation.clone()]);
    }

    #[test]
    fn repeat_exactly() {
        let ast = AST::RepeatExactly(1, Box::new(AST::Character('a')));
        let nfa = NFA::from(&ast);
        assert!(nfa.accepts("a"));
        assert!(!nfa.accepts("c"));
        assert!(!nfa.accepts("bbb"));
        assert!(!nfa.accepts(""));
        assert!(!nfa.accepts("aa"));
        assert!(!nfa.accepts("aaaa"));

        let ast = AST::RepeatExactly(2, Box::new(AST::Character('a')));
        let nfa = NFA::from(&ast);
        assert!(nfa.accepts("aa"));
        assert!(!nfa.accepts("c"));
        assert!(!nfa.accepts("bbb"));
        assert!(!nfa.accepts(""));
        assert!(!nfa.accepts("a"));
        assert!(!nfa.accepts("aaa"));
        assert!(!nfa.accepts("aaaa"));

        let ast = AST::RepeatExactly(3, Box::new(AST::Character('a')));
        let nfa = NFA::from(&ast);
        assert!(nfa.accepts("aaa"));
        assert!(!nfa.accepts("c"));
        assert!(!nfa.accepts("bbb"));
        assert!(!nfa.accepts("a"));
        assert!(!nfa.accepts("aa"));
        assert!(!nfa.accepts("aaaa"));
    }

    #[test]
    fn repeat_at_least() {
        let ast = AST::RepeatAtLeast(1, Box::new(AST::Character('a')));
        let nfa = NFA::from(&ast);
        assert!(nfa.accepts("a"));
        assert!(nfa.accepts("aa"));
        assert!(nfa.accepts("aaa"));
        assert!(nfa.accepts("aaaaaaaaaaaaaaaaaaaaaa"));
        assert!(!nfa.accepts("aaab"));
        assert!(!nfa.accepts("c"));
        assert!(!nfa.accepts("bbb"));
        assert!(!nfa.accepts(""));

        let ast = AST::RepeatAtLeast(2, Box::new(AST::Character('a')));
        let nfa = NFA::from(&ast);
        assert!(nfa.accepts("aa"));
        assert!(nfa.accepts("aaa"));
        assert!(nfa.accepts("aaaa"));
        assert!(nfa.accepts("aaaaaaaaaaaaaaaaaaaaa"));
        assert!(!nfa.accepts("aaab"));
        assert!(!nfa.accepts("c"));
        assert!(!nfa.accepts("bbb"));
        assert!(!nfa.accepts(""));
        assert!(!nfa.accepts("a"));

        let ast = AST::RepeatAtLeast(3, Box::new(AST::Character('a')));
        let nfa = NFA::from(&ast);
        assert!(nfa.accepts("aaa"));
        assert!(nfa.accepts("aaaa"));
        assert!(nfa.accepts("aaaaa"));
        assert!(nfa.accepts("aaaaaaaaaaaaaaaaaaaaaa"));
        assert!(!nfa.accepts("aaab"));
        assert!(!nfa.accepts("c"));
        assert!(!nfa.accepts("bbb"));
        assert!(!nfa.accepts("a"));
        assert!(!nfa.accepts("aa"));
    }

    #[test]
    fn repeat_at_least_at_most() {
        let ast = AST::RepeatAtLeastAtMost(3, 5, Box::new(AST::Character('a')));
        let nfa = NFA::from(&ast);
        assert!(!nfa.accepts("a"));
        assert!(!nfa.accepts("aa"));
        assert!(nfa.accepts("aaa"));
        assert!(nfa.accepts("aaaa"));
        assert!(nfa.accepts("aaaaa"));
        assert!(!nfa.accepts("aaaaaa"));
        assert!(!nfa.accepts("aaaaaaaaaaaaaaaaaaaaaa"));
        assert!(!nfa.accepts("aaab"));
        assert!(!nfa.accepts("c"));
        assert!(!nfa.accepts("bbb"));
    }

    #[test]
    fn capture() {
        let ast = AST::Concat(
            Box::new(AST::Concat(
                Box::new(AST::Character('a')),
                Box::new(AST::Capture(Box::new(AST::OneOrMore(Box::new(
                    AST::Character('b'),
                ))))),
            )),
            Box::new(AST::Character('c')),
        );
        let nfa = NFA::from(&ast);
        let stage = TranslationStage::Main;
        assert_eq!(nfa.find_translations("a"), []);
        assert_eq!(nfa.find_translations("ab"), []);
        assert_eq!(
            nfa.find_translations("abc"),
            [Translation::new("b", "", 3, stage, None).with_offset(1)]
        );
        assert_eq!(
            nfa.find_translations("abbc"),
            [Translation::new("bb", "", 4, stage, None).with_offset(1)]
        );
        assert_eq!(
            nfa.find_translations("abbbbbc"),
            [Translation::new("bbbbb", "", 7, stage, None).with_offset(1)]
        );
        assert_eq!(nfa.find_translations("aabbbbbc"), []);
        assert_eq!(nfa.find_translations("abb"), []);
        assert_eq!(nfa.find_translations("abbbb"), []);
        assert_eq!(nfa.find_translations("abbbbbbbbbb"), []);
        assert_eq!(nfa.find_translations("aaaaaa"), []);
        assert_eq!(nfa.find_translations("aaaaaaaaaaaaaaaaaaaaaa"), []);
        assert_eq!(nfa.find_translations("aaab"), []);
        assert_eq!(nfa.find_translations("c"), []);
        assert_eq!(nfa.find_translations("bbb"), []);
    }
}
