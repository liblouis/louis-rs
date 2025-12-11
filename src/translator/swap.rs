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
    /// Insert a mapping between a char `from` and a string `to`
    pub fn insert(&mut self, from: char, to: &str) {
        self.0.insert(from, to.to_string());
    }

    /// Swap the characters in the `input` according to the mapping defined in this Swapper
    pub fn swap(&self, input: &str) -> String {
        input
            .chars()
            .map(|c| self.0.get(&c).cloned().unwrap_or(c.to_string()))
            .collect()
    }
}

/// A mapping between a name and the associated Swapper
#[derive(Debug, Default)]
pub struct SwapClasses(HashMap<String, Swapper>);

impl SwapClasses {
    /// Insert a list of `mappings` under the given `name`
    pub fn insert(&mut self, name: &str, mappings: &[(char, &str)]) {
        for (from, to) in mappings {
            self.0
                .entry(name.to_string())
                .or_default()
                .insert(*from, to)
        }
    }

    pub fn get(&self, name: &str) -> Option<Swapper> {
        self.0.get(name).cloned()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn swapper_insert_and_swap() {
        let mut swapper = Swapper::default();
        swapper.insert('a', "⠁");
        swapper.insert('b', "⠉");
        swapper.insert('c', "⠙⠇");

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
        let mut swapper = Swapper::default();
        swapper.insert('α', "alpha");
        swapper.insert('🚀', "rocket");

        assert_eq!(swapper.swap("α🚀"), "alpharocket");
    }

    #[test]
    fn swap_classes_insert_and_get() {
        let mut classes = SwapClasses::default();
        classes.insert("test", &[('a', "⠁"), ('b', "⠉")]);

        assert_eq!(classes.get("test").unwrap().swap("ab"), "⠁⠉");
    }

    #[test]
    fn swap_classes_multiple_inserts() {
        let mut classes = SwapClasses::default();
        classes.insert("test", &[('a', "⠁")]);
        classes.insert("test", &[('b', "⠉")]);

        assert_eq!(classes.get("test").unwrap().swap("ab"), "⠁⠉");
    }

    #[test]
    fn swap_classes_nonexistent() {
        let classes = SwapClasses::default();
        assert!(classes.get("nonexistent").is_none());
    }

}
