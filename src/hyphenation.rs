//! Knuth-Liang hyphenation, parsed from liblouis's `.dic`
//! pattern-dictionary format.
//!
//! A table answers one question -- where in this word may a break occur?
//! -- and is identified purely by whichever `.dic` file it was parsed
//! from, exactly like every other liblouis table type. Implemented
//! natively rather than via the `hyphenation` crate, since we only need
//! its matching algorithm, not its bundled pattern corpus or its
//! print-text case-folding.

use std::collections::HashMap;

/// A parsed hyphenation table: patterns plus the boundary minima
/// liblouis's `LEFTHYPHENMIN`/`RIGHTHYPHENMIN` header declares.
#[derive(Debug, Default, Clone)]
pub struct HyphenationTable {
    // Key: a pattern's letters, e.g. "ab" for the source pattern ".ab3a"
    // (the leading '.' is a literal word-boundary marker character, kept
    // as part of the key).
    // Value: the score at each gap around those letters -- one more
    // entry than there are characters in the key.
    patterns: HashMap<String, Vec<u8>>,
    left_min: usize,
    right_min: usize,
}

/// A bad `LEFTHYPHENMIN`/`RIGHTHYPHENMIN` value is a configuration
/// mistake, not a pattern, so it fails loudly rather than falling back
/// to a default.
#[derive(thiserror::Error, Debug)]
pub enum ParseError {
    #[error("line {line}: expected a number after {directive}, found {found:?}")]
    InvalidHyphenMin {
        line: usize,
        directive: &'static str,
        found: String,
    },
    #[error("line {line}: pattern {pattern:?} has no hyphenation value")]
    PatternWithoutValue { line: usize, pattern: String },
    #[error("unsupported encoding {encoding:?}: only UTF-8 .dic files are supported")]
    UnsupportedEncoding { encoding: String },
}

impl HyphenationTable {
    /// Parse a `.dic` hyphenation-pattern file.
    ///
    /// `LEFTHYPHENMIN`/`RIGHTHYPHENMIN` set the boundary minima when
    /// present; otherwise they default to `(1, 1)`, the least
    /// restrictive bound.
    pub fn parse(source: &str) -> Result<Self, ParseError> {
        let mut table = HyphenationTable {
            left_min: 1,
            right_min: 1,
            ..Default::default()
        };

        for (lineno, raw_line) in source.lines().enumerate() {
            let line = raw_line.trim();
            if line.is_empty() {
                continue;
            }
            // The first line is an encoding marker; we only support UTF-8.
            if lineno == 0 {
                if line.eq_ignore_ascii_case("UTF-8") {
                    continue;
                }
                if line.eq_ignore_ascii_case("ISO8859-1") {
                    return Err(ParseError::UnsupportedEncoding {
                        encoding: line.to_string(),
                    });
                }
            }
            // Comment lines.
            if line.starts_with('#') || line.starts_with('%') {
                continue;
            }
            if let Some(rest) = line.strip_prefix("LEFTHYPHENMIN") {
                table.left_min = parse_hyphenmin(rest, lineno, "LEFTHYPHENMIN")?;
                continue;
            }
            if let Some(rest) = line.strip_prefix("RIGHTHYPHENMIN") {
                table.right_min = parse_hyphenmin(rest, lineno, "RIGHTHYPHENMIN")?;
                continue;
            }
            let (letters, tally) = parse_pattern(line, lineno)?;
            table.patterns.insert(letters, tally);
        }

        Ok(table)
    }

    /// For each internal gap of `word`, whether it's a valid hyphenation
    /// break (odd score, staying `left_min`/`right_min` characters away
    /// from either edge). `result[i]` is the gap between word-char `i`
    /// and word-char `i + 1`; a word of fewer than two characters has no
    /// internal gaps and returns an empty vector.
    pub fn break_points(&self, word: &str) -> Vec<bool> {
        let char_count = word.chars().count();
        if char_count < 2 {
            return Vec::new();
        }
        if char_count < self.left_min + self.right_min {
            return vec![false; char_count - 1];
        }

        let wrapped = format!(".{word}.");
        // Byte offset of each char boundary in `wrapped`, plus a sentinel
        // at the end, so `&wrapped[a..b]` slices land on char boundaries.
        let bounds: Vec<usize> = wrapped
            .char_indices()
            .map(|(b, _)| b)
            .chain(std::iter::once(wrapped.len()))
            .collect();
        let wrapped_chars = bounds.len() - 1;

        // scores[g] = the highest value any matching pattern assigns to
        // the gap immediately before the g-th character of `wrapped`. A
        // gap's final score can only be known once every pattern has been
        // tried, since a later match may raise it past an earlier one --
        // so this pass can't stop early even though each gap only needs a
        // yes/no answer in the end.
        let mut scores = vec![0u8; wrapped_chars + 1];
        for start in 0..wrapped_chars {
            for end in (start + 1)..=wrapped_chars {
                if let Some(tally) = self.patterns.get(&wrapped[bounds[start]..bounds[end]]) {
                    for (k, &v) in tally.iter().enumerate() {
                        let gap = start + k;
                        if v > scores[gap] {
                            scores[gap] = v;
                        }
                    }
                }
            }
        }

        // Gap `g` sits right before wrapped-char `g`; wrapped-char `i` is
        // word-char `i - 1` (wrapped-char 0 is the leading '.'). So `g` is
        // a genuine internal word break -- between word chars (g-2) and
        // (g-1) -- for g in [2, char_count], landing on word char-index
        // (g - 1). `result[i]` is that break for g = i + 2.
        (0..char_count - 1)
            .map(|i| {
                let g = i + 2;
                let char_idx = g - 1;
                !scores[g].is_multiple_of(2)
                    && char_idx >= self.left_min
                    && char_idx <= char_count - self.right_min
            })
            .collect()
    }
}

/// Parse the value after a `LEFTHYPHENMIN`/`RIGHTHYPHENMIN` directive.
fn parse_hyphenmin(
    rest: &str,
    lineno: usize,
    directive: &'static str,
) -> Result<usize, ParseError> {
    rest.trim()
        .parse()
        .map_err(|_| ParseError::InvalidHyphenMin {
            line: lineno + 1,
            directive,
            found: rest.trim().to_string(),
        })
}

/// ".ab3a" -> ("aba", [0,0,3,0]) -- letters plus one score per gap
/// (len(letters) + 1 of them). Fails if no digit was found, since such a
/// pattern could never produce a break anywhere.
fn parse_pattern(pattern: &str, lineno: usize) -> Result<(String, Vec<u8>), ParseError> {
    let mut letters = String::new();
    let mut tally = vec![0u8];
    for ch in pattern.chars() {
        if let Some(d) = ch.to_digit(10) {
            // Scores the gap the last letter already opened, not a new one.
            *tally.last_mut().unwrap() = d as u8;
        } else {
            letters.push(ch);
            tally.push(0);
        }
    }
    if tally.iter().all(|&v| v == 0) {
        return Err(ParseError::PatternWithoutValue {
            line: lineno + 1,
            pattern: pattern.to_string(),
        });
    }
    Ok((letters, tally))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn reads_hyphenmin_header() {
        let t =
            HyphenationTable::parse("UTF-8\nLEFTHYPHENMIN 2\nRIGHTHYPHENMIN 3\n.ab3a\n").unwrap();
        assert_eq!((t.left_min, t.right_min), (2, 3));
    }

    #[test]
    fn defaults_when_header_absent() {
        let t = HyphenationTable::parse("UTF-8\n.ab3a\n").unwrap();
        assert_eq!((t.left_min, t.right_min), (1, 1));
    }

    #[test]
    fn rejects_malformed_header_value() {
        assert!(HyphenationTable::parse("UTF-8\nLEFTHYPHENMIN two\n.ab3a\n").is_err());
    }

    #[test]
    fn rejects_iso8859_1_encoding() {
        assert!(HyphenationTable::parse("ISO8859-1\n.ab3a\n").is_err());
    }

    #[test]
    fn finds_a_break() {
        // ".ab3a" -- the '3' sits between 'b' and the second 'a', so for
        // the word "aba" that's a break after 2 characters: "ab|a".
        let t =
            HyphenationTable::parse("UTF-8\nLEFTHYPHENMIN 1\nRIGHTHYPHENMIN 1\n.ab3a\n").unwrap();
        assert_eq!(t.break_points("aba"), vec![false, true]);
    }

    #[test]
    fn respects_minima() {
        let t =
            HyphenationTable::parse("UTF-8\nLEFTHYPHENMIN 2\nRIGHTHYPHENMIN 2\n.ab3a\n").unwrap();
        // break at char-index 2 is not <= char_count(3) - right_min(2) = 1
        assert_eq!(t.break_points("aba"), vec![false, false]);
    }

    #[test]
    fn no_match_means_no_breaks() {
        let t = HyphenationTable::parse("UTF-8\ns1b\n").unwrap();
        assert_eq!(t.break_points("fff"), vec![false, false]);
    }

    #[test]
    fn skips_comments_and_blank_lines() {
        let t = HyphenationTable::parse("UTF-8\n# a comment\n\n% also a comment\n.ab3a\n").unwrap();
        assert_eq!(t.break_points("aba"), vec![false, true]);
    }

    #[test]
    fn short_word_has_no_gaps() {
        let t = HyphenationTable::parse("UTF-8\n.ab3a\n").unwrap();
        assert_eq!(t.break_points(""), Vec::<bool>::new());
        assert_eq!(t.break_points("a"), Vec::<bool>::new());
    }
}
