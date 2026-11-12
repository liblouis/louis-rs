use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum EffectKind {
    Increment,
    Decrement,
    Set(u8),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Effect {
    slot: u8,
    kind: EffectKind,
}

impl Effect {
    pub fn new(slot: u8, value: u8) -> Self {
        Effect {
            slot,
            kind: EffectKind::Set(value),
        }
    }

    pub fn increment(slot: u8) -> Self {
        Effect {
            slot,
            kind: EffectKind::Increment,
        }
    }

    pub fn decrement(slot: u8) -> Self {
        Effect {
            slot,
            kind: EffectKind::Decrement,
        }
    }
}

pub struct Environment(HashMap<u8, u8>);

impl Environment {
    pub fn new() -> Self {
        Environment(HashMap::new())
    }

    pub fn get(&self, slot: u8) -> Option<&u8> {
        self.0.get(&slot)
    }

    pub fn apply(&mut self, effect: &Effect) {
        let slot = effect.slot;
        match effect.kind {
            EffectKind::Increment => {
                self.0
                    .entry(slot)
                    .and_modify(|v| *v = v.saturating_add(1))
                    .or_insert(1);
            }
            EffectKind::Decrement => {
                self.0
                    .entry(slot)
                    .and_modify(|v| *v = v.saturating_sub(1))
                    .or_insert(0);
            }
            EffectKind::Set(value) => {
                self.0
                    .entry(slot)
                    .and_modify(|v| *v = value)
                    .or_insert(value);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn increment_and_decrement() {
        let mut env = Environment::new();
        // as in liblouis, an unset variable counts as 0, and decrementing stops at 0
        env.apply(&Effect::decrement(1));
        assert_eq!(env.get(1), Some(&0));
        env.apply(&Effect::increment(1));
        env.apply(&Effect::increment(1));
        assert_eq!(env.get(1), Some(&2));
        env.apply(&Effect::decrement(1));
        assert_eq!(env.get(1), Some(&1));
        env.apply(&Effect::new(1, 7));
        assert_eq!(env.get(1), Some(&7));
    }
}
