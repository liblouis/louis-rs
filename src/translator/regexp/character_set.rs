//! An efficient bitmap-based character set geared towards fast lookup
//! for mostly ASCII characters and read-only use.

use std::collections::HashSet;

/// A character set, optimised for ascii using a bitmap with a fallback for any other
/// Unicode character.
#[derive(Debug, Clone)]
pub struct CharacterSet {
    /// Bitmap for the ascii range, 128 bit.
    ascii: [u64; 2],
    /// Sorted array of non-ascii members of the set used for fallback.
    chars: Vec<char>,
}

impl From<&HashSet<char>> for CharacterSet {
    fn from(characters: &HashSet<char>) -> Self {
        let mut chars = Vec::new();
        let mut ascii = [0u64; 2];
        for &c in characters {
            let cp = c as u32;
            if cp < 128 {
                ascii[(cp >> 6) as usize] |= 1 << (cp & 63);
            } else {
                chars.push(c);
            }
        }
        chars.sort_unstable();
        CharacterSet { ascii, chars }
    }
}

impl CharacterSet {
    pub fn contains(&self, c: char) -> bool {
        let cp = c as u32;
        if cp < 128 {
            self.ascii[(cp >> 6) as usize] & (1 << (cp & 63)) != 0
        } else {
            self.chars.binary_search(&c).is_ok()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn contains_ascii_and_fallback() {
        let members = ['a', 'Z', '0', '\0', '\u{7f}', 'ä', '€', '\u{10ffff}'];
        let set = CharacterSet::from(&HashSet::from(members));
        for c in members {
            assert!(set.contains(c), "{c:?} should be a member");
        }
        for c in ['b', 'A', '1', 'ö', '\u{80}', '\u{10fffe}'] {
            assert!(!set.contains(c), "{c:?} should not be a member");
        }
    }
}
