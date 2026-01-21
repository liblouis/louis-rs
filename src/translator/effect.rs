use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum EffectKind {
    Increment,
    Decrement,
    Set(u8),
}

#[derive(Debug, Clone)]
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
                self.0.entry(slot).and_modify(|v| *v += 1).or_insert(1);
            }
            EffectKind::Decrement => {
                self.0
                    .entry(slot)
                    .and_modify(|v| if *v > 0 { *v -= 1 } else { *v = 0 })
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
