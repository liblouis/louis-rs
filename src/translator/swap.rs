//! Define char to string mappings, a _swap_ in liblouis terminolgy
//!
//! These are used in the action part of the `context`, `correct` and multipass opcodes.
use std::collections::HashMap;

/// A mapping between characters and strings.
///
/// These mappings are defined in the `swap*` opcodes and used in the action part of the `context`,
/// `correct` and multipass opcodes
#[derive(Debug, Default, Clone, PartialEq)]
pub struct Swapper(HashMap<char, String>);

impl Swapper {
    pub fn new(mappings: &[(char, &str)]) -> Self {
        Self(HashMap::from_iter(
            mappings.iter().cloned().map(|(c, s)| (c, s.to_string())),
        ))
    }

    /// Swap the characters in the `input` according to the mapping defined in this Swapper
    pub fn swap(&self, input: &str) -> String {
        input
            .chars()
            .map(|c| self.0.get(&c).cloned().unwrap_or(c.to_string()))
            .collect()
    }
}

/// A mapping between a name and the associated [`Swapper`]
#[derive(Debug, Default)]
pub struct SwapClasses(HashMap<String, Swapper>);

impl SwapClasses {
    /// Insert a list of `mappings` under the given `name`
    pub fn insert(&mut self, name: &str, mappings: &[(char, &str)]) {
        self.0.insert(name.to_string(), Swapper::new(mappings));
    }

    pub fn get(&self, name: &str) -> Option<Swapper> {
        self.0.get(name).cloned()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn swapper_swap() {
        let swapper = Swapper::new(&[('a', "⠁"), ('b', "⠉"), ('c', "⠙⠇")]);

        assert_eq!(swapper.swap("abc"), "⠁⠉⠙⠇");
        assert_eq!(swapper.swap("xyz"), "xyz"); // unmapped chars unchanged
    }

    #[test]
    fn swapper_empty() {
        let swapper = Swapper::default();
        assert_eq!(swapper.swap("hello"), "hello");
    }

    #[test]
    fn swapper_unicode() {
        let swapper = Swapper::new(&[('α', "alpha"), ('🚀', "rocket")]);

        assert_eq!(swapper.swap("α🚀"), "alpharocket");
    }

    #[test]
    fn swap_classes_nonexistent() {
        let classes = SwapClasses::default();
        assert!(classes.get("nonexistent").is_none());
    }
}
