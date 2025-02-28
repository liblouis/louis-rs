use std::collections::HashMap;

use crate::parser::{Pattern, Patterns};

use super::{
    Translation,
    boundaries::{number_word, word_end, word_number, word_start},
};

#[derive(Debug, Default)]
pub struct Node {
    translation: Option<Translation>,
}

type NodeId = usize;

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Boundary {
    Word,
    NotWord,
    Number,
    NumberWord,
    WordNumber,
    PrePattern,
    None,
}

#[derive(Debug, PartialEq, Eq, Hash)]
enum Transition {
    Character(char),
    Start(Boundary),
    End(Boundary),
    Any,
}

#[derive(Debug)]
pub struct Graph {
    nodes: Vec<Node>,
    edges: HashMap<(NodeId, Transition), NodeId>,
}

impl Graph {
    pub fn new() -> Graph {
        let nodes = vec![Node::default()];
        let edges = HashMap::new();
        Graph { nodes, edges }
    }

    fn add_node(&mut self) -> NodeId {
        self.nodes.push(Node::default());
        self.nodes.len() - 1 // because we added an empty root node
    }

    fn add_edge(&mut self, from: NodeId, transition: Transition, to: NodeId) {
        self.edges.insert((from, transition), to);
    }

    fn char_case_insensitive_transition(&self, from: NodeId, c: char) -> Option<&NodeId> {
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
        self.edges.get(&(from, Transition::Character(lowercase)))
    }

    pub fn insert(&mut self, from: String, to: String, before: Boundary, after: Boundary) {
        let mut current_node = 0;
        let mut length = from.chars().count();

        if before != Boundary::None {
            length += 1;
            let next_node = self.add_node();
            self.add_edge(current_node, Transition::Start(before), next_node);
            current_node = next_node;
        }

        for c in from.chars() {
            match self.edges.get(&(current_node, Transition::Character(c))) {
                Some(node) => {
                    current_node = *node;
                }
                None => {
                    let next_node = self.add_node();
                    self.add_edge(current_node, Transition::Character(c), next_node);
                    current_node = next_node;
                }
            }
        }

        if after != Boundary::None {
            length += 1;
            let next_node = self.add_node();
            self.add_edge(current_node, Transition::End(after), next_node);
            current_node = next_node;
        }

        let translation = Some(Translation::new(from, to, length));
        if cfg!(feature = "backwards_compatibility") {
            // first rule wins
            if self.nodes[current_node].translation.is_none() {
                self.nodes[current_node].translation = translation;
            }
        } else {
            // last rule wins
            self.nodes[current_node].translation = translation;
        }
    }

    pub fn insert_match(&mut self, from: String, to: String, pre: &Patterns, post: &Patterns) {
        let mut current_node = 0;
        let mut length = from.chars().count();

        for p in pre {
            match p {
                Pattern::Characters(s) => {
                    for c in s.chars() {
                        match self.edges.get(&(current_node, Transition::Character(c))) {
                            Some(node) => {
                                current_node = *node;
                            }
                            None => {
                                let next_node = self.add_node();
                                self.add_edge(current_node, Transition::Character(c), next_node);
                                current_node = next_node;
                            }
                        }
                        length += 1;
                    }
                }
                Pattern::Any => {
                    let next_node = self.add_node();
                    self.add_edge(current_node, Transition::Any, next_node);
                    current_node = next_node;
                    length += 1;
                }
                Pattern::Set(chars) => {
                    let next_node = self.add_node();
                    for c in chars {
                        match self.edges.get(&(current_node, Transition::Character(*c))) {
                            Some(_) => (),
                            None => {
                                self.add_edge(current_node, Transition::Character(*c), next_node);
                            }
                        }
                        length += 1;
                    }
                    current_node = next_node;
                }
                _ => (),
            }
        }

        // add an epsilon transition to mark the end of the
        // pre-pattern. We need to know where the real match starts
        let next_node = self.add_node();
        self.add_edge(
            current_node,
            Transition::End(Boundary::PrePattern),
            next_node,
        );
        current_node = next_node;

        for c in from.chars() {
            match self.edges.get(&(current_node, Transition::Character(c))) {
                Some(node) => {
                    current_node = *node;
                }
                None => {
                    let next_node = self.add_node();
                    self.add_edge(current_node, Transition::Character(c), next_node);
                    current_node = next_node;
                }
            }
            length += 1;
        }

        for p in post {
            match p {
                Pattern::Characters(s) => {
                    for c in s.chars() {
                        match self.edges.get(&(current_node, Transition::Character(c))) {
                            Some(node) => {
                                current_node = *node;
                            }
                            None => {
                                let next_node = self.add_node();
                                self.add_edge(current_node, Transition::Character(c), next_node);
                                current_node = next_node;
                            }
                        }
                        length += 1;
                    }
                }
                Pattern::Any => {
                    let next_node = self.add_node();
                    self.add_edge(current_node, Transition::Any, next_node);
                    current_node = next_node;
                    length += 1;
                }
                Pattern::Set(chars) => {
                    let next_node = self.add_node();
                    for c in chars {
                        match self.edges.get(&(current_node, Transition::Character(*c))) {
                            Some(_) => (),
                            None => {
                                self.add_edge(current_node, Transition::Character(*c), next_node);
                            }
                        }
                        length += 1;
                    }
                    current_node = next_node;
                }
                _ => (),
            }
        }

        let translation = Some(Translation::new(from, to, length));
        if cfg!(feature = "backwards_compatibility") {
            // first rule wins
            if self.nodes[current_node].translation.is_none() {
                self.nodes[current_node].translation = translation;
            }
        } else {
            // last rule wins
            self.nodes[current_node].translation = translation;
        }
    }

    fn find_translations_from_node(
        &self,
        input: &str,
        prev: Option<char>,
        node_id: NodeId,
        match_length: usize,
        offset: usize,
    ) -> Vec<Translation> {
        let mut matching_rules = Vec::new();
        let mut chars = input.chars();

        // if this node has a translation add it to the list of matching rules
        if let Some(ref translation) = self.nodes[node_id].translation {
            let translation = translation
                .clone()
                .with_offset(offset)
                // if there is an offset (typically in a match opcode), the weight needs
                // to be calculated at run-time. The weight is the actual length of match.
                .with_weight_if_offset(match_length, offset);
            matching_rules.push(translation)
        }
        let c = chars.next();
        if let Some(c) = c {
            let bytes = c.len_utf8();
            if let Some(node_id) = self.edges.get(&(node_id, Transition::Character(c))) {
                matching_rules.extend(self.find_translations_from_node(
                    &input[bytes..],
                    Some(c),
                    *node_id,
                    match_length + 1,
                    offset,
                ));
            } else if let Some(node_id) = self.char_case_insensitive_transition(node_id, c) {
                matching_rules.extend(self.find_translations_from_node(
                    &input[bytes..],
                    Some(c),
                    *node_id,
                    match_length + 1,
                    offset,
                ));
            } else if let Some(node_id) = self.edges.get(&(node_id, Transition::Any)) {
                matching_rules.extend(self.find_translations_from_node(
                    &input[bytes..],
                    Some(c),
                    *node_id,
                    match_length + 1,
                    offset,
                ));
            }
        }
        if let Some(node_id) = self
            .edges
            .get(&(node_id, Transition::Start(Boundary::Word)))
        {
            if word_start(prev, c) {
                matching_rules.extend(self.find_translations_from_node(
                    input,
                    prev,
                    *node_id,
                    match_length,
                    offset,
                ));
            }
        }
        if let Some(node_id) = self
            .edges
            .get(&(node_id, Transition::Start(Boundary::NotWord)))
        {
            if !word_start(prev, c) {
                matching_rules.extend(self.find_translations_from_node(
                    input,
                    prev,
                    *node_id,
                    match_length,
                    offset,
                ));
            }
        }
        if let Some(node_id) = self.edges.get(&(node_id, Transition::End(Boundary::Word))) {
            if word_end(prev, c) {
                matching_rules.extend(self.find_translations_from_node(
                    input,
                    prev,
                    *node_id,
                    match_length,
                    offset,
                ));
            }
        }
        if let Some(node_id) = self
            .edges
            .get(&(node_id, Transition::End(Boundary::NotWord)))
        {
            if !word_end(prev, c) {
                matching_rules.extend(self.find_translations_from_node(
                    input,
                    prev,
                    *node_id,
                    match_length,
                    offset,
                ));
            }
        }
        if let Some(node_id) = self
            .edges
            .get(&(node_id, Transition::End(Boundary::WordNumber)))
        {
            if word_number(prev, c) {
                matching_rules.extend(self.find_translations_from_node(
                    input,
                    prev,
                    *node_id,
                    match_length,
                    offset,
                ));
            }
        }
        if let Some(node_id) = self
            .edges
            .get(&(node_id, Transition::Start(Boundary::NumberWord)))
        {
            if number_word(prev, c) {
                matching_rules.extend(self.find_translations_from_node(
                    input,
                    prev,
                    *node_id,
                    match_length,
                    offset,
                ));
            }
        }
        if let Some(node_id) = self
            .edges
            .get(&(node_id, Transition::End(Boundary::PrePattern)))
        {
            matching_rules.extend(self.find_translations_from_node(
                input,
                prev,
                *node_id,
                match_length,
                match_length,
            ));
        }
        matching_rules
    }

    pub fn find_translations(&self, input: &str, prev: Option<char>) -> Vec<Translation> {
        let mut matching_rules = Vec::new();

        matching_rules.extend(self.find_translations_from_node(input, prev, 0, 0, 0));
        matching_rules
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_graph_test() {
        let graph = Graph::new();
        assert_eq!(
            graph.find_translations("foo", None),
            Vec::<Translation>::new()
        );
    }

    #[test]
    fn find_translations_test() {
        let mut graph = Graph::new();
        let empty = Vec::<Translation>::new();
        let a = Translation::new("a".into(), "A".into(), 1);
        let f = Translation::new("f".into(), "F".into(), 1);
        let fo = Translation::new("fo".into(), "FO".into(), 2);
        let foo = Translation::new("foo".into(), "FOO".into(), 3);
        let foobar = Translation::new("foobar".into(), "FOOBAR".into(), 6);
        graph.insert("a".into(), "A".into(), Boundary::None, Boundary::None);
        graph.insert("f".into(), "F".into(), Boundary::None, Boundary::None);
        graph.insert("fo".into(), "FO".into(), Boundary::None, Boundary::None);
        graph.insert("foo".into(), "FOO".into(), Boundary::None, Boundary::None);
        graph.insert(
            "foobar".into(),
            "FOOBAR".into(),
            Boundary::None,
            Boundary::None,
        );
        assert_eq!(graph.find_translations("a", None), vec![a]);
        assert_eq!(graph.find_translations("f", None), vec![f.clone()]);
        assert_eq!(
            graph.find_translations("fo", None),
            vec![f.clone(), fo.clone()]
        );
        assert_eq!(
            graph.find_translations("foobar", None),
            vec![f.clone(), fo.clone(), foo.clone(), foobar.clone()]
        );
        assert_eq!(
            graph.find_translations("foobaz", None),
            vec![f.clone(), fo.clone(), foo.clone()]
        );
        assert_eq!(graph.find_translations("function", None), vec![f.clone()]);
        assert_eq!(
            graph.find_translations("folio", None),
            vec![f.clone(), fo.clone()]
        );
        assert_eq!(graph.find_translations("none", None), empty);
        assert_eq!(
            graph.find_translations("The start of a long long sentence", None),
            empty
        );
    }

    #[test]
    fn find_translations_with_boundaries_test() {
        let mut graph = Graph::new();
        let empty = Vec::<Translation>::new();
        let a = Translation::new("a".into(), "A".into(), 3);
        graph.insert("a".into(), "A".into(), Boundary::Word, Boundary::Word);
        assert_eq!(graph.find_translations("a", None), vec![a]);
        assert_eq!(graph.find_translations("aha", None), empty);
    }

    #[test]
    fn find_translations_with_negative_boundary_after_test() {
        let mut graph = Graph::new();
        let empty = Vec::<Translation>::new();
        let foo = Translation::new("foo".into(), "FOO".into(), 5);
        graph.insert(
            "foo".into(),
            "FOO".into(),
            Boundary::Word,
            Boundary::NotWord,
        );
        assert_eq!(graph.find_translations("foo", None), empty);
        assert_eq!(graph.find_translations("foo ", None), empty);
        assert_eq!(graph.find_translations("foobar", None), vec![foo]);
        assert_eq!(graph.find_translations("foo.", None), empty);
    }

    #[test]
    fn find_translations_with_negative_boundary_before_test() {
        let mut graph = Graph::new();
        let empty = Vec::<Translation>::new();
        let foo = Translation::new("foo".into(), "FOO".into(), 4);
        graph.insert(
            "foo".into(),
            "FOO".into(),
            Boundary::NotWord,
            Boundary::None,
        );
        assert_eq!(graph.find_translations("foo", None), empty);
        assert_eq!(graph.find_translations("foo", Some(' ')), empty);
        assert_eq!(graph.find_translations("foo", Some('.')), empty);
        assert_eq!(graph.find_translations("foo", Some('c')), vec![foo]);
    }

    #[test]
    fn find_translations_with_negative_boundaries_test() {
        let mut graph = Graph::new();
        let empty = Vec::<Translation>::new();
        let foo = Translation::new("foo".into(), "FOO".into(), 5);
        graph.insert(
            "foo".into(),
            "FOO".into(),
            Boundary::NotWord,
            Boundary::NotWord,
        );
        assert_eq!(graph.find_translations("foo", None), empty);
        assert_eq!(graph.find_translations("foo", Some(' ')), empty);
        assert_eq!(graph.find_translations("foo", Some('.')), empty);
        assert_eq!(graph.find_translations("foo", Some('c')), empty);
        assert_eq!(graph.find_translations("foobar", Some('c')), vec![foo]);
    }

    #[test]
    fn find_translations_with_word_num_boundary() {
        let mut graph = Graph::new();
        let empty = Vec::<Translation>::new();
        let foo = Translation::new("aaa".into(), "A".into(), 5);
        graph.insert(
            "aaa".into(),
            "A".into(),
            Boundary::Word,
            Boundary::WordNumber,
        );
        assert_eq!(graph.find_translations("aaa", None), empty);
        assert_eq!(
            graph.find_translations("aaa1", Some(' ')),
            vec![foo.clone()]
        );
        assert_eq!(
            graph.find_translations("aaa1", Some('.')),
            vec![foo.clone()]
        );
        assert_eq!(graph.find_translations("aaa1", Some('c')), empty);
    }

    #[test]
    fn find_translations_with_num_word_boundary() {
        let mut graph = Graph::new();
        let empty = Vec::<Translation>::new();
        let foo = Translation::new("st".into(), "S".into(), 4);
        graph.insert(
            "st".into(),
            "S".into(),
            Boundary::NumberWord,
            Boundary::Word,
        );
        assert_eq!(graph.find_translations("st", None), empty);
        assert_eq!(graph.find_translations("st", Some(' ')), empty);
        assert_eq!(graph.find_translations("st", Some('.')), empty);
        assert_eq!(graph.find_translations("st", Some('1')), vec![foo]);
        assert_eq!(graph.find_translations("sta", Some('1')), empty);
    }

    #[test]
    fn find_translations_case_insensitive_test() {
        let mut graph = Graph::new();
        let foo = Translation::new("foo".into(), "FOO".into(), 3);
        graph.insert("foo".into(), "FOO".into(), Boundary::None, Boundary::None);
        assert_eq!(graph.find_translations("foo", None), vec![foo.clone()]);
        assert_eq!(graph.find_translations("Foo", None), vec![foo.clone()]);
        assert_eq!(graph.find_translations("FOO", None), vec![foo.clone()]);
        assert_eq!(graph.find_translations("foO", None), vec![foo.clone()]);
    }
}
