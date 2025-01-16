use std::collections::{HashMap, HashSet, VecDeque};

type StateId = usize;

#[derive(Debug, PartialEq, Eq, Hash)]
struct State {
    id: StateId,
    is_final: bool,
}

#[derive(Debug, PartialEq, Eq, Hash)]
enum Transition {
    Char(char),
    Epsilon,
}

struct NFA {
    states: Vec<State>,
    edges: HashMap<(StateId, Transition), StateId>,
    start_state: usize,
}

impl NFA {
    fn new() -> Self {
        NFA {
            states: Vec::new(),
            edges: HashMap::new(),
            start_state: 0,
        }
    }

    fn add_state(&mut self, is_final: bool) -> usize {
        let id = self.states.len();
        self.states.push(State { id, is_final });
        id
    }

    fn add_transition(&mut self, from: StateId, to: StateId, transition: Transition) {
        self.edges.insert((from, transition), to);
    }

    fn set_start_state(&mut self, start_state: usize) {
        self.start_state = start_state;
    }

    fn simulate(&self, input: &str) -> bool {
        let mut current_states = VecDeque::new();
        current_states.push_back(self.start_state);
	
        for c in input.chars() {
            let mut next_states = VecDeque::new();
	    
            for state in current_states.iter() {
		if let Some(to) = self.edges.get(&(*state, Transition::Char(c))) {
                    next_states.push_back(*to);
		}
		if let Some(to) = self.edges.get(&(*state, Transition::Epsilon)) {
                    next_states.push_back(*to);
		}
            }

            current_states = next_states;
        }
	
        for state in current_states {
            if self.states[state].is_final {
                return true;
            }
        }
	
        false
    }

    fn epsilon_closure(&self, states: HashSet<StateId>) -> HashSet<StateId> {
        let mut closure = states.clone();
        let mut queue: VecDeque<StateId> = states.into_iter().collect();
	
        while let Some(state) = queue.pop_front() {
	    if let Some(to) = self.edges.get(&(state, Transition::Epsilon)) {
		closure.insert(*to);
                queue.push_back(*to);
	    }
        }
	
        closure
    }

    fn move_state(&self, states: HashSet<StateId>, symbol: char) -> HashSet<StateId> {
        let mut next_states = HashSet::new();

        for from in states {
	    if let Some(to) = self.edges.get(&(from, Transition::Char(symbol))) {
		next_states.insert(*to);
	    }
        }

        next_states
    }

}

struct DFA {
    states: Vec<StateId>,
    edges: HashMap<(StateId, char), StateId>,
    start_state: StateId,
    final_state_ids: HashSet<StateId>,
}
