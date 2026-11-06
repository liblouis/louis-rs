//! Per-position translation constraints
//!
//! A [`Constrainers`] collection analyses the input text before translation begins and
//! produces a [`PositionConstraints`] value. The translation loop consults this
//! to decide which rules are eligible at each character position.
//!
//! The constrainer types are:
//! * [`Constrainer::Numeric`]: detects digit runs and sets [`Constraint::DontContract`]
//! * [`Constrainer::ComputerBraille`]: maps computer-braille spans to [`Constraint::UseComp6`]

use enumset::{EnumSet, EnumSetType};
use std::collections::HashSet;

use crate::emphasis::EmphasisSpan;
use crate::hyphenation::HyphenationTable;

/// A rule-selection constraint that can be set at a character position.
#[derive(EnumSetType, Debug)]
pub enum Constraint {
    /// Suppress multi-character (contraction) rules at this position.
    ///
    /// Set for every character inside a numeric run so that contractions are
    /// not applied to digit sequences.
    DontContract,
    /// Activate comp6 rules and suppress normal translation rules at this position.
    ///
    /// Set for every character inside a computer-braille span so that comp6
    /// mappings are used instead of the regular translation rules.
    UseComp6,
    /// Uppercase every character produced at this position, in backward translation.
    ///
    /// Set for every letter cell of a `begcapsword`/`endcapsword` span. A single cell can
    /// resolve to more than one character (e.g. a digraph rule), and inside a whole-word
    /// caps span every character of it is capitalized — unlike [`Constraint::TitleCase`].
    Uppercase,
    /// Uppercase only the first character produced at this position, in backward
    /// translation.
    ///
    /// Set for the cell following a `capsletter` indicator. `capsletter` marks a single
    /// capitalized *letter*, not a whole word — if that cell resolves to more than one
    /// character (e.g. the Hungarian `sz` digraph), only the first is capitalized
    /// (`Sz`, not `SZ`), matching liblouis.
    TitleCase,
    /// Prefer a digit reading over a letter reading at this position, in backward
    /// translation.
    ///
    /// Set for every digit cell inside a `numsign`/`nonumsign` span, disambiguating
    /// dots shared between a digit and a letter (common in six-dot literary codes).
    PreferDigit,
    /// There's a valid hyphenation break between this character and the next, in
    /// forward translation.
    ///
    /// Set for every internal gap of a word (as delimited by the table's `letter`
    /// class) that the word's hyphenation dictionary marks as a valid break point.
    /// Consulted to decide whether a `nocross` rule may cross this gap.
    HyphenationBreak,
    /// Suppress the word-family rules at this position, in backward translation.
    ///
    /// Set for the run of letter cells following a `letsign`/`nocontractsign`/
    /// `nonumsign` indicator. Those indicators exist precisely to say "read the
    /// following cells as letters, not as a contraction", so the `word`, `sufword`,
    /// `prfword` and `begword` rules must not fire inside their scope.
    NoContract,
    /// A `prepunc` rule may fire on the punctuation character at this position, in
    /// forward translation.
    PrepuncOk,
    /// A `postpunc` rule may fire on the punctuation character at this position, in
    /// forward translation.
    PostpuncOk,
    /// This position is in a run of whitespace that a digit follows, so a `joinnum`
    /// rule just before it may fire and swallow the run, in forward translation.
    SpaceBeforeDigit,
    /// Like [`Constraint::SpaceBeforeDigit`], but a letter follows, for `joinword`.
    SpaceBeforeLetter,
    /// Contractions are off at this position, in forward translation: it is in a word
    /// with a `nocont` string, or, with `capsnocont`, at or after a run of capitals.
    Uncontracted,
    /// This position is in a word that the `repword` separator after it repeats, in
    /// forward translation.
    RepeatedWord,
    /// Like [`Constraint::RepeatedWord`], but the word is the end of a longer one, so
    /// `rependword` applies instead.
    RepeatedWordEnd,
    /// This position is in a further repetition, the separator and the word again, after
    /// the first one that a `repword`/`rependword` separator repeats, in forward
    /// translation.
    Repetition,
}

pub type Constraints = EnumSet<Constraint>;

/// The result of pre-computing per-position constraints for a given input.
pub struct PositionConstraints(Vec<Constraints>);

impl PositionConstraints {
    pub fn empty() -> Self {
        Self(Vec::new())
    }

    pub fn dont_contract_at(&self, pos: usize) -> bool {
        self.0
            .get(pos)
            .is_some_and(|f| f.contains(Constraint::DontContract))
    }

    pub fn no_contract_at(&self, pos: usize) -> bool {
        self.0
            .get(pos)
            .is_some_and(|f| f.contains(Constraint::NoContract))
    }

    pub fn use_comp6_at(&self, pos: usize) -> bool {
        self.0
            .get(pos)
            .is_some_and(|f| f.contains(Constraint::UseComp6))
    }

    pub fn uppercase_at(&self, pos: usize) -> bool {
        self.0
            .get(pos)
            .is_some_and(|f| f.contains(Constraint::Uppercase))
    }

    pub fn titlecase_at(&self, pos: usize) -> bool {
        self.0
            .get(pos)
            .is_some_and(|f| f.contains(Constraint::TitleCase))
    }

    pub fn prefer_digit_at(&self, pos: usize) -> bool {
        self.0
            .get(pos)
            .is_some_and(|f| f.contains(Constraint::PreferDigit))
    }

    pub fn hyphenation_break_at(&self, pos: usize) -> bool {
        self.0
            .get(pos)
            .is_some_and(|f| f.contains(Constraint::HyphenationBreak))
    }

    pub fn prepunc_ok_at(&self, pos: usize) -> bool {
        self.0
            .get(pos)
            .is_some_and(|f| f.contains(Constraint::PrepuncOk))
    }

    pub fn postpunc_ok_at(&self, pos: usize) -> bool {
        self.0
            .get(pos)
            .is_some_and(|f| f.contains(Constraint::PostpuncOk))
    }

    pub fn uncontracted_at(&self, pos: usize) -> bool {
        self.0
            .get(pos)
            .is_some_and(|f| f.contains(Constraint::Uncontracted))
    }

    pub fn space_before_digit_at(&self, pos: usize) -> bool {
        self.0
            .get(pos)
            .is_some_and(|f| f.contains(Constraint::SpaceBeforeDigit))
    }

    pub fn space_before_letter_at(&self, pos: usize) -> bool {
        self.0
            .get(pos)
            .is_some_and(|f| f.contains(Constraint::SpaceBeforeLetter))
    }

    pub fn repetition_at(&self, pos: usize) -> bool {
        self.0
            .get(pos)
            .is_some_and(|f| f.contains(Constraint::Repetition))
    }

    /// How many positions right before `pos` have `constraint`
    pub fn run_before(&self, pos: usize, constraint: Constraint) -> usize {
        self.0[..pos]
            .iter()
            .rev()
            .take_while(|c| c.contains(constraint))
            .count()
    }
}

/// Whether two characters are the same letter, ignoring case
fn same_letter(a: char, b: char) -> bool {
    a.to_lowercase().eq(b.to_lowercase())
}

/// Detects numeric runs in the input and sets [`Constraint::DontContract`] at
/// every character position that falls inside one.
#[derive(Debug, Clone)]
pub struct NumericConstrainer {
    numeric_chars: HashSet<char>,
    extra_numeric_chars: HashSet<char>,
    mid_numeric_chars: HashSet<char>,
}

impl NumericConstrainer {
    fn compute(&self, input: &str) -> Vec<Constraints> {
        let chars: Vec<char> = input.chars().collect();
        let mut constraints: Vec<Constraints> = vec![Constraints::empty(); chars.len()];
        let mut in_numeric = false;

        for (pos, &c) in chars.iter().enumerate() {
            let is_numeric = self.numeric_chars.contains(&c);
            match (is_numeric, in_numeric) {
                (false, false) => {}
                (true, false) => {
                    in_numeric = true;
                    constraints[pos].insert(Constraint::DontContract);
                }
                (true, true) => {
                    constraints[pos].insert(Constraint::DontContract);
                }
                (false, true) if self.extra_numeric_chars.contains(&c) => {
                    constraints[pos].insert(Constraint::DontContract);
                }
                (false, true)
                    if self.extra_numeric_chars.is_empty()
                        && self.mid_numeric_chars.contains(&c)
                        && chars
                            .get(pos + 1)
                            .is_some_and(|nc| self.numeric_chars.contains(nc)) =>
                {
                    constraints[pos].insert(Constraint::DontContract);
                }
                (false, true) => {
                    in_numeric = false;
                }
            }
        }
        constraints
    }
}

/// Detects the table's `letter`-delimited words in the input and sets
/// [`Constraint::HyphenationBreak`] at every internal gap the hyphenation
/// dictionary marks as a valid break, mirroring liblouis's `syllableBreak`
/// (forward translation only -- back-translation never consults it).
#[derive(Debug, Clone)]
pub struct HyphenationConstrainer {
    hyphenator: HyphenationTable,
    letter_chars: HashSet<char>,
}

impl HyphenationConstrainer {
    fn compute(&self, input: &str) -> Vec<Constraints> {
        let chars: Vec<char> = input.chars().collect();
        let mut constraints: Vec<Constraints> = vec![Constraints::empty(); chars.len()];
        let mut word_start = None;
        for (pos, &c) in chars.iter().enumerate() {
            if self.letter_chars.contains(&c) {
                word_start.get_or_insert(pos);
            } else if let Some(start) = word_start.take() {
                self.mark_word(&mut constraints, &chars, start, pos);
            }
        }
        if let Some(start) = word_start {
            self.mark_word(&mut constraints, &chars, start, chars.len());
        }
        constraints
    }

    /// Marks the breaks of the word spanning `chars[start..end]` at their
    /// absolute positions in `constraints`.
    fn mark_word(&self, constraints: &mut [Constraints], chars: &[char], start: usize, end: usize) {
        let word: String = chars[start..end].iter().collect();
        for (i, is_break) in self.hyphenator.break_points(&word).into_iter().enumerate() {
            if is_break {
                constraints[start + i].insert(Constraint::HyphenationBreak);
            }
        }
    }
}

/// Detects, at every punctuation character position, whether a `prepunc`/`postpunc`
/// rule may fire there, mirroring liblouis's `CTO_PrePunc`/`CTO_PostPunc` (forward
/// translation only; back-translation never consults these opcodes).
///
/// Unlike every other boundary check (`begword`, `midword`, ...), this isn't a
/// fixed-position lookaround: `prepunc` requires scanning forward past any further
/// punctuation for a letter or digit before hitting whitespace (and the mirror image,
/// backward, for `postpunc`), which the trie's single-neighbor
/// `Transition::Start`/`End(Vec<CharacterClass>)` can't express. This is computed once
/// over the whole input, like `HyphenationConstrainer`, rather than as a trie
/// transition.
#[derive(Debug, Clone)]
pub struct PunctuationConstrainer {
    letter_chars: HashSet<char>,
    letter_or_digit_chars: HashSet<char>,
    space_chars: HashSet<char>,
}

impl PunctuationConstrainer {
    fn compute(&self, input: &str) -> Vec<Constraints> {
        let chars: Vec<char> = input.chars().collect();
        let n = chars.len();
        let mut constraints: Vec<Constraints> = vec![Constraints::empty(); n];
        for pos in 0..n {
            // prepunc: not immediately preceded by a letter, then scan forward (past
            // any further punctuation) for a letter/digit before hitting whitespace.
            if pos == 0 || !self.letter_chars.contains(&chars[pos - 1]) {
                for k in pos + 1..n {
                    if self.letter_or_digit_chars.contains(&chars[k]) {
                        constraints[pos].insert(Constraint::PrepuncOk);
                        break;
                    }
                    if self.space_chars.contains(&chars[k]) {
                        break;
                    }
                }
            }
            // postpunc: the mirror image, scanning backward from just before this
            // position.
            if pos == n - 1 || !self.letter_chars.contains(&chars[pos + 1]) {
                for k in (0..pos).rev() {
                    if self.letter_or_digit_chars.contains(&chars[k]) {
                        constraints[pos].insert(Constraint::PostpuncOk);
                        break;
                    }
                    if self.space_chars.contains(&chars[k]) {
                        break;
                    }
                }
            }
        }
        constraints
    }
}

/// Marks every word, delimited by whitespace, that contains one of the `nocont`
/// strings, mirroring liblouis's `doNocont`: such a word is translated
/// uncontracted (forward translation only).
#[derive(Debug, Clone)]
pub struct NoContConstrainer {
    strings: Vec<String>,
    space_chars: HashSet<char>,
}

impl NoContConstrainer {
    fn compute(&self, input: &str) -> Vec<Constraints> {
        let chars: Vec<char> = input.chars().collect();
        let mut constraints = vec![Constraints::empty(); chars.len()];
        let mut start = 0;
        while start < chars.len() {
            let end = (start..chars.len())
                .find(|&i| self.space_chars.contains(&chars[i]))
                .unwrap_or(chars.len());
            // case-insensitive, like every rule's characters in liblouis
            let word = String::from_iter(&chars[start..end]).to_lowercase();
            if self.strings.iter().any(|s| word.contains(s.as_str())) {
                for c in &mut constraints[start..end] {
                    c.insert(Constraint::Uncontracted);
                }
            }
            start = end + 1;
        }
        constraints
    }
}

/// Marks a word, delimited by whitespace, from the first of two or more consecutive
/// capitals to its end, mirroring liblouis's `capsnocont` handling in `markEmphases`:
/// that part is translated uncontracted (forward translation only).
#[derive(Debug, Clone)]
pub struct CapsNoContConstrainer {
    uppercase_chars: HashSet<char>,
    space_chars: HashSet<char>,
}

impl CapsNoContConstrainer {
    fn compute(&self, input: &str) -> Vec<Constraints> {
        let chars: Vec<char> = input.chars().collect();
        let mut constraints = vec![Constraints::empty(); chars.len()];
        let mut start = 0;
        while start < chars.len() {
            let end = (start..chars.len())
                .find(|&i| self.space_chars.contains(&chars[i]))
                .unwrap_or(chars.len());
            // find the first pair of capitals
            if let Some(i) = chars[start..end]
                .windows(2)
                .position(|w| w.iter().all(|c| self.uppercase_chars.contains(c)))
            {
                // mark dontContract up to the next space
                for c in &mut constraints[start + i..end] {
                    c.insert(Constraint::Uncontracted);
                }
            }
            start = end + 1;
        }
        constraints
    }
}

/// Marks the whitespace runs that a digit or a letter follows, so a `joinnum` or a
/// `joinword` rule just before one may fire and swallow it, mirroring liblouis's
/// `CTO_JoinNum` and `CTO_JoinableWord` (forward translation only). Like `prepunc`, a
/// scan of any length that the trie's single-neighbor transitions can't express.
#[derive(Debug, Clone)]
pub struct JoinConstrainer {
    digit_chars: HashSet<char>,
    letter_chars: HashSet<char>,
    space_chars: HashSet<char>,
}

impl JoinConstrainer {
    fn compute(&self, input: &str) -> Vec<Constraints> {
        let chars: Vec<char> = input.chars().collect();
        let mut constraints = vec![Constraints::empty(); chars.len()];
        // scanning backward, remember what the next non-space is
        let mut follows = None;
        for pos in (0..chars.len()).rev() {
            if self.space_chars.contains(&chars[pos]) {
                if let Some(constraint) = follows {
                    constraints[pos].insert(constraint);
                }
            } else if self.digit_chars.contains(&chars[pos]) {
                follows = Some(Constraint::SpaceBeforeDigit);
            } else if self.letter_chars.contains(&chars[pos]) {
                follows = Some(Constraint::SpaceBeforeLetter);
            } else {
                follows = None;
            }
        }
        constraints
    }
}

/// Marks the word before a `repword`/`rependword` separator that the letters after it
/// repeat, mirroring liblouis's `isRepeatedWord` (forward translation only): the
/// longest such run, ignoring case. This is the `(\w+)-\1` of the two opcodes.
///
/// The opcode spans three places: this constrainer finds the repetitions, the trie
/// finds the separator, and `trace()` checks what depends on the rules chosen before
/// (`repeated_word`, `repetitions_end`) and swallows the repetitions. See
/// `doc/adr/0020-repword.org` for how they fit together.
#[derive(Debug, Clone)]
pub struct RepwordConstrainer {
    separators: HashSet<char>,
    letter_chars: HashSet<char>,
}

impl RepwordConstrainer {
    /// At each separator, find the longest run of letters that comes both right before
    /// and right after it, and flag that run before the separator, and any further
    /// repetitions after the first one
    fn compute(&self, input: &str) -> Vec<Constraints> {
        let chars: Vec<char> = input.chars().collect();
        let mut constraints = vec![Constraints::empty(); chars.len()];
        let is_letter = |p: usize| self.letter_chars.contains(&chars[p]);
        for pos in 0..chars.len() {
            if !self.separators.contains(&chars[pos]) {
                continue;
            }
            let after = pos + 1;
            // as long as there are letters on both sides
            let max_len = (1..=pos)
                .take_while(|&len| {
                    after + len <= chars.len() && is_letter(pos - len) && is_letter(after + len - 1)
                })
                .count();
            // whether the `len` letters before the separator come again at `start`
            let repeats = |len: usize, start: usize| {
                start + len <= chars.len()
                    && (0..len).all(|i| same_letter(chars[pos - len + i], chars[start + i]))
            };
            let repeated_len = (1..=max_len).rev().find(|&len| repeats(len, after));
            if let Some(len) = repeated_len {
                // a letter before the run makes it the end of a longer word: rependword
                let constraint = if len < pos && is_letter(pos - len - 1) {
                    Constraint::RepeatedWordEnd
                } else {
                    Constraint::RepeatedWord
                };
                for c in &mut constraints[pos - len..pos] {
                    c.insert(constraint);
                }
                // the separator and the word again
                let mut end = after + len;
                while chars.get(end) == Some(&chars[pos]) && repeats(len, end + 1) {
                    for c in &mut constraints[end..end + 1 + len] {
                        c.insert(Constraint::Repetition);
                    }
                    end += 1 + len;
                }
            }
        }
        constraints
    }
}

#[derive(Debug, Clone)]
pub struct ComputerBrailleConstrainer {}

impl ComputerBrailleConstrainer {
    fn compute(&self, input: &str, spans: &[EmphasisSpan]) -> Vec<Constraints> {
        let n = input.chars().count();
        let mut constraints: Vec<Constraints> = vec![Constraints::empty(); n];
        for span in spans {
            if span.class == "computer_braille" {
                let end = span.range.end.min(n);
                for pos in span.range.start..end {
                    constraints[pos].insert(Constraint::UseComp6);
                }
            }
        }
        constraints
    }
}

/// Returns true if `chars[pos..]` starts with `pattern`.
fn matches_at(chars: &[char], pos: usize, pattern: &str) -> bool {
    let pattern: Vec<char> = pattern.chars().collect();
    chars[pos..].starts_with(&pattern)
}

/// Length in cells of the longest of `patterns` matching at `pos`.
fn longest_match_at(chars: &[char], pos: usize, patterns: &[String]) -> Option<usize> {
    patterns
        .iter()
        .filter(|p| matches_at(chars, pos, p))
        .map(|p| p.chars().count())
        .max()
}

/// Detects `capsletter`/`begcapsword`/`endcapsword` indicator dots in backward
/// (braille → text) input and sets [`Constraint::TitleCase`]/[`Constraint::Uppercase`]
/// at every position whose decoded character(s) should be capitalized.
///
/// `capsletter` title-cases just the following cell (a cell can resolve to more than one
/// character, e.g. a digraph rule — only the first is capitalized). `begcapsword`
/// uppercases every character of every subsequent letter cell until `endcapsword` (if
/// defined) or the first non-letter cell, mirroring the forward word-boundary
/// auto-termination rule.
#[derive(Debug, Clone)]
pub struct BackwardCapsConstrainer {
    /// The indicator's own dots plus any `multind` sequences naming it (e.g.
    /// `56-6 letsign capsletter`), which act like it, just spelled with more cells.
    capsletter_dots: Vec<String>,
    begcapsword_dots: Vec<String>,
    endcapsword_dots: Option<String>,
    letter_cells: HashSet<char>,
}

impl BackwardCapsConstrainer {
    fn compute(&self, input: &str) -> Vec<Constraints> {
        let chars: Vec<char> = input.chars().collect();
        let n = chars.len();
        let mut constraints: Vec<Constraints> = vec![Constraints::empty(); n];
        let mut pos = 0;
        while pos < n {
            // Longest match wins, begcapsword on a tie: capsletter's dots are commonly
            // a prefix of begcapsword's (e.g. `46` vs `46-46`), and a `multind`
            // (e.g. `56-6 letsign capsletter`) is longer than the bare indicator.
            let begcapsword = longest_match_at(&chars, pos, &self.begcapsword_dots);
            let capsletter = longest_match_at(&chars, pos, &self.capsletter_dots);
            if let Some(len) = begcapsword
                && len >= capsletter.unwrap_or(0)
            {
                pos += len;
                loop {
                    if let Some(end_dots) = &self.endcapsword_dots
                        && matches_at(&chars, pos, end_dots)
                    {
                        pos += end_dots.chars().count();
                        break;
                    }
                    if pos < n && self.letter_cells.contains(&chars[pos]) {
                        constraints[pos].insert(Constraint::Uppercase);
                        pos += 1;
                    } else {
                        break;
                    }
                }
                continue;
            }
            if let Some(len) = capsletter {
                pos += len;
                if pos < n {
                    constraints[pos].insert(Constraint::TitleCase);
                }
                continue;
            }
            pos += 1;
        }
        constraints
    }
}

/// Detects `numsign`/`nonumsign` indicator dots in backward input and sets
/// [`Constraint::PreferDigit`] at every digit cell of the numeric run, disambiguating
/// dots shared between a digit and a letter.
///
/// `numsign`'s dots are not necessarily exclusive to the indicator: some tables (e.g.
/// Punjabi, which shares the common `3456` numsign convention with the unrelated
/// Gurmukhi letter `ਣ`, disambiguated in liblouis via a `midendword` position
/// constraint) also use them for an ordinary, frequently-occurring letter. A genuine
/// numsign is never preceded by a letter cell — mid-word occurrences are excluded to
/// avoid false positives on such tables.
#[derive(Debug, Clone)]
pub struct BackwardNumericConstrainer {
    /// The `numsign` dots plus any `multind` sequences naming it, e.g.
    /// `6-3456 letsign numsign`.
    numsign_dots: Vec<String>,
    nonumsign_dots: Option<String>,
    digit_cells: HashSet<char>,
    letter_cells: HashSet<char>,
}

impl BackwardNumericConstrainer {
    fn compute(&self, input: &str) -> Vec<Constraints> {
        let chars: Vec<char> = input.chars().collect();
        let n = chars.len();
        let mut constraints: Vec<Constraints> = vec![Constraints::empty(); n];
        let mut pos = 0;
        let mut in_number = false;
        while pos < n {
            if let Some(len) = longest_match_at(&chars, pos, &self.numsign_dots)
                && (pos == 0 || !self.letter_cells.contains(&chars[pos - 1]))
            {
                pos += len;
                in_number = true;
                continue;
            }
            if in_number {
                if let Some(dots) = &self.nonumsign_dots
                    && matches_at(&chars, pos, dots)
                {
                    pos += dots.chars().count();
                    in_number = false;
                    continue;
                }
                if self.digit_cells.contains(&chars[pos]) {
                    constraints[pos].insert(Constraint::PreferDigit);
                    pos += 1;
                    continue;
                }
                in_number = false;
            }
            pos += 1;
        }
        constraints
    }
}

/// Detects `letsign`/`nocontractsign`/`nonumsign` indicator dots in backward
/// (braille -> text) input and sets [`Constraint::NoContract`] on the run of letter
/// cells that follows, so the word-family rules cannot claim cells the indicator has
/// declared to be plain letters.
#[derive(Debug, Clone)]
pub struct BackwardNoContractConstrainer {
    indicator_dots: Vec<String>,
    letter_cells: HashSet<char>,
}

impl BackwardNoContractConstrainer {
    fn compute(&self, input: &str) -> Vec<Constraints> {
        let chars: Vec<char> = input.chars().collect();
        let n = chars.len();
        let mut constraints: Vec<Constraints> = vec![Constraints::empty(); n];
        let mut pos = 0;
        while pos < n {
            // Longest first: one indicator's dots are often a prefix of another's.
            if let Some(len) = longest_match_at(&chars, pos, &self.indicator_dots) {
                pos += len;
                while pos < n && self.letter_cells.contains(&chars[pos]) {
                    constraints[pos].insert(Constraint::NoContract);
                    pos += 1;
                }
                continue;
            }
            pos += 1;
        }
        constraints
    }
}

/// A builder for [`Constrainer::BackwardNoContract`].
#[derive(Debug, Default)]
pub struct BackwardNoContractConstrainerBuilder {
    indicator_dots: Vec<String>,
    letter_cells: HashSet<char>,
}

impl BackwardNoContractConstrainerBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn indicator(&mut self, dots: &str) {
        self.indicator_dots.push(dots.to_string());
    }

    pub fn letter_cells(&mut self, cells: HashSet<char>) {
        self.letter_cells = cells;
    }

    pub fn build(self) -> Option<BackwardNoContractConstrainer> {
        if self.indicator_dots.is_empty() {
            None
        } else {
            Some(BackwardNoContractConstrainer {
                indicator_dots: self.indicator_dots,
                letter_cells: self.letter_cells,
            })
        }
    }
}

/// A single analysis pass that contributes to [`PositionConstraints`].
#[derive(Debug, Clone)]
pub enum Constrainer {
    /// Detects numeric runs and sets [`Constraint::DontContract`].
    Numeric(NumericConstrainer),
    /// Maps `computer_braille` emphasis spans to [`Constraint::UseComp6`].
    ComputerBraille(ComputerBrailleConstrainer),
    /// Detects backward `capsletter`/`begcapsword`/`endcapsword` dots and sets
    /// [`Constraint::Uppercase`].
    BackwardCaps(BackwardCapsConstrainer),
    /// Detects backward `numsign`/`nonumsign` dots and sets [`Constraint::PreferDigit`].
    BackwardNumeric(BackwardNumericConstrainer),
    /// Detects backward `letsign`/`nocontractsign`/`nonumsign` dots and sets
    /// [`Constraint::NoContract`].
    BackwardNoContract(BackwardNoContractConstrainer),
    /// Detects hyphenation-dictionary breaks and sets [`Constraint::HyphenationBreak`].
    Hyphenation(HyphenationConstrainer),
    /// Detects `prepunc`/`postpunc` scan eligibility and sets
    /// [`Constraint::PrepuncOk`]/[`Constraint::PostpuncOk`].
    Punctuation(PunctuationConstrainer),
    /// Detects whitespace runs that a digit or a letter follows and sets
    /// [`Constraint::SpaceBeforeDigit`]/[`Constraint::SpaceBeforeLetter`].
    Join(JoinConstrainer),
    /// Detects words containing a `nocont` string and sets
    /// [`Constraint::Uncontracted`].
    NoCont(NoContConstrainer),
    /// Detects runs of capitals and sets [`Constraint::Uncontracted`].
    CapsNoCont(CapsNoContConstrainer),
    /// Detects words that a `repword`/`rependword` separator repeats and sets
    /// [`Constraint::RepeatedWord`]/[`Constraint::RepeatedWordEnd`].
    Repword(RepwordConstrainer),
}

impl Constrainer {
    fn compute(&self, input: &str, spans: &[EmphasisSpan]) -> Vec<Constraints> {
        match self {
            Constrainer::Numeric(c) => c.compute(input),
            Constrainer::ComputerBraille(c) => c.compute(input, spans),
            Constrainer::BackwardCaps(c) => c.compute(input),
            Constrainer::BackwardNoContract(c) => c.compute(input),
            Constrainer::BackwardNumeric(c) => c.compute(input),
            Constrainer::Hyphenation(c) => c.compute(input),
            Constrainer::Punctuation(c) => c.compute(input),
            Constrainer::Join(c) => c.compute(input),
            Constrainer::NoCont(c) => c.compute(input),
            Constrainer::CapsNoCont(c) => c.compute(input),
            Constrainer::Repword(c) => c.compute(input),
        }
    }
}

/// A collection of [`Constrainer`]s that are applied together.
#[derive(Debug, Clone)]
pub struct Constrainers(Vec<Constrainer>);

impl Constrainers {
    pub fn new(constrainers: Vec<Constrainer>) -> Self {
        Constrainers(constrainers)
    }

    pub fn precompute(&self, input: &str, spans: &[EmphasisSpan]) -> PositionConstraints {
        let n = input.chars().count();
        let mut constraints: Vec<Constraints> = vec![Constraints::empty(); n];
        for constrainer in &self.0 {
            for (accum, computed) in constraints
                .iter_mut()
                .zip(constrainer.compute(input, spans))
            {
                *accum |= computed;
            }
        }
        PositionConstraints(constraints)
    }
}

/// A builder for [`Constrainer::Numeric`].
#[derive(Debug, Default)]
pub struct NumericConstrainerBuilder {
    numeric_chars: HashSet<char>,
    extra_numeric_chars: HashSet<char>,
    mid_numeric_chars: HashSet<char>,
}

impl NumericConstrainerBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn numeric_characters(&mut self, chars: HashSet<char>) {
        self.numeric_chars = chars;
    }

    pub fn numericmodechars(&mut self, s: &str) {
        self.extra_numeric_chars.extend(s.chars());
    }

    pub fn midnum(&mut self, s: &str) {
        self.mid_numeric_chars.extend(s.chars());
    }

    pub fn build(self) -> Option<NumericConstrainer> {
        if self.numeric_chars.is_empty() {
            None
        } else {
            Some(NumericConstrainer {
                numeric_chars: self.numeric_chars,
                extra_numeric_chars: self.extra_numeric_chars,
                mid_numeric_chars: self.mid_numeric_chars,
            })
        }
    }
}

/// A builder for [`Constrainer::BackwardCaps`].
#[derive(Debug, Default)]
pub struct BackwardCapsConstrainerBuilder {
    capsletter_dots: Option<String>,
    begcapsword_dots: Option<String>,
    endcapsword_dots: Option<String>,
    /// `multind` sequences that end in (or contain) a `capsletter`/`begcapsword`:
    /// they act like that indicator, just spelled with more cells.
    multind_capsletter_dots: Vec<String>,
    multind_begcapsword_dots: Vec<String>,
    letter_cells: HashSet<char>,
}

impl BackwardCapsConstrainerBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn capsletter(&mut self, dots: &str) {
        self.capsletter_dots = Some(dots.to_string());
    }

    pub fn begcapsword(&mut self, dots: &str) {
        self.begcapsword_dots = Some(dots.to_string());
    }

    pub fn endcapsword(&mut self, dots: &str) {
        self.endcapsword_dots = Some(dots.to_string());
    }

    pub fn multind_capsletter(&mut self, dots: &str) {
        self.multind_capsletter_dots.push(dots.to_string());
    }

    pub fn multind_begcapsword(&mut self, dots: &str) {
        self.multind_begcapsword_dots.push(dots.to_string());
    }

    pub fn letter_cells(&mut self, cells: HashSet<char>) {
        self.letter_cells = cells;
    }

    pub fn build(self) -> Option<BackwardCapsConstrainer> {
        let capsletter_dots: Vec<String> = self
            .capsletter_dots
            .into_iter()
            .chain(self.multind_capsletter_dots)
            .collect();
        let begcapsword_dots: Vec<String> = self
            .begcapsword_dots
            .into_iter()
            .chain(self.multind_begcapsword_dots)
            .collect();
        if capsletter_dots.is_empty() && begcapsword_dots.is_empty() {
            None
        } else {
            Some(BackwardCapsConstrainer {
                capsletter_dots,
                begcapsword_dots,
                endcapsword_dots: self.endcapsword_dots,
                letter_cells: self.letter_cells,
            })
        }
    }
}

/// A builder for [`Constrainer::BackwardNumeric`].
#[derive(Debug, Default)]
pub struct BackwardNumericConstrainerBuilder {
    numsign_dots: Option<String>,
    /// `multind` sequences containing a `numsign`, e.g. `6-3456 letsign numsign`.
    multind_numsign_dots: Vec<String>,
    nonumsign_dots: Option<String>,
    digit_cells: HashSet<char>,
    letter_cells: HashSet<char>,
}

impl BackwardNumericConstrainerBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn numsign(&mut self, dots: &str) {
        self.numsign_dots = Some(dots.to_string());
    }

    pub fn nonumsign(&mut self, dots: &str) {
        self.nonumsign_dots = Some(dots.to_string());
    }

    pub fn multind_numsign(&mut self, dots: &str) {
        self.multind_numsign_dots.push(dots.to_string());
    }

    pub fn digit_cells(&mut self, cells: HashSet<char>) {
        self.digit_cells = cells;
    }

    pub fn letter_cells(&mut self, cells: HashSet<char>) {
        self.letter_cells = cells;
    }

    pub fn build(self) -> Option<BackwardNumericConstrainer> {
        let numsign_dots: Vec<String> = self
            .numsign_dots
            .into_iter()
            .chain(self.multind_numsign_dots)
            .collect();
        if numsign_dots.is_empty() {
            None
        } else {
            Some(BackwardNumericConstrainer {
                numsign_dots,
                nonumsign_dots: self.nonumsign_dots,
                digit_cells: self.digit_cells,
                letter_cells: self.letter_cells,
            })
        }
    }
}

/// A builder for [`Constrainer::Hyphenation`].
#[derive(Debug, Default)]
pub struct HyphenationConstrainerBuilder {
    hyphenator: Option<HyphenationTable>,
    letter_chars: HashSet<char>,
}

impl HyphenationConstrainerBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn hyphenator(&mut self, hyphenator: HyphenationTable) {
        self.hyphenator = Some(hyphenator);
    }

    pub fn letter_characters(&mut self, chars: HashSet<char>) {
        self.letter_chars = chars;
    }

    pub fn build(self) -> Option<HyphenationConstrainer> {
        Some(HyphenationConstrainer {
            hyphenator: self.hyphenator?,
            letter_chars: self.letter_chars,
        })
    }
}

/// A builder for [`Constrainer::Punctuation`].
#[derive(Debug, Default)]
pub struct PunctuationConstrainerBuilder {
    has_prepunc_or_postpunc: bool,
    letter_chars: HashSet<char>,
    letter_or_digit_chars: HashSet<char>,
    space_chars: HashSet<char>,
}

impl PunctuationConstrainerBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    /// Marks that the table declares at least one `prepunc`/`postpunc` rule, so
    /// `build()` produces a constrainer. Tables without either opcode skip this
    /// pass entirely.
    pub fn mark_present(&mut self) {
        self.has_prepunc_or_postpunc = true;
    }

    pub fn letter_characters(&mut self, chars: HashSet<char>) {
        self.letter_chars = chars;
    }

    pub fn letter_or_digit_characters(&mut self, chars: HashSet<char>) {
        self.letter_or_digit_chars = chars;
    }

    pub fn space_characters(&mut self, chars: HashSet<char>) {
        self.space_chars = chars;
    }

    pub fn build(self) -> Option<PunctuationConstrainer> {
        if self.has_prepunc_or_postpunc {
            Some(PunctuationConstrainer {
                letter_chars: self.letter_chars,
                letter_or_digit_chars: self.letter_or_digit_chars,
                space_chars: self.space_chars,
            })
        } else {
            None
        }
    }
}

/// A builder for [`Constrainer::NoCont`].
#[derive(Debug, Default)]
pub struct NoContConstrainerBuilder {
    strings: Vec<String>,
    space_chars: HashSet<char>,
}

impl NoContConstrainerBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn nocont(&mut self, s: &str) {
        self.strings.push(s.to_lowercase());
    }

    pub fn space_characters(&mut self, chars: HashSet<char>) {
        self.space_chars = chars;
    }

    pub fn build(self) -> Option<NoContConstrainer> {
        (!self.strings.is_empty()).then_some(NoContConstrainer {
            strings: self.strings,
            space_chars: self.space_chars,
        })
    }
}

/// A builder for [`Constrainer::CapsNoCont`].
#[derive(Debug, Default)]
pub struct CapsNoContConstrainerBuilder {
    present: bool,
    uppercase_chars: HashSet<char>,
    space_chars: HashSet<char>,
}

impl CapsNoContConstrainerBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn mark_present(&mut self) {
        self.present = true;
    }

    pub fn uppercase_characters(&mut self, chars: HashSet<char>) {
        self.uppercase_chars = chars;
    }

    pub fn space_characters(&mut self, chars: HashSet<char>) {
        self.space_chars = chars;
    }

    pub fn build(self) -> Option<CapsNoContConstrainer> {
        self.present.then_some(CapsNoContConstrainer {
            uppercase_chars: self.uppercase_chars,
            space_chars: self.space_chars,
        })
    }
}

/// A builder for [`Constrainer::Join`].
#[derive(Debug, Default)]
pub struct JoinConstrainerBuilder {
    present: bool,
    digit_chars: HashSet<char>,
    letter_chars: HashSet<char>,
    space_chars: HashSet<char>,
}

impl JoinConstrainerBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    /// Marks that the table declares at least one `joinnum` or `joinword` rule, so `build()`
    /// produces a constrainer.
    pub fn mark_present(&mut self) {
        self.present = true;
    }

    pub fn digit_characters(&mut self, chars: HashSet<char>) {
        self.digit_chars = chars;
    }

    pub fn letter_characters(&mut self, chars: HashSet<char>) {
        self.letter_chars = chars;
    }

    pub fn space_characters(&mut self, chars: HashSet<char>) {
        self.space_chars = chars;
    }

    pub fn build(self) -> Option<JoinConstrainer> {
        self.present.then_some(JoinConstrainer {
            digit_chars: self.digit_chars,
            letter_chars: self.letter_chars,
            space_chars: self.space_chars,
        })
    }
}

/// A builder for [`Constrainer::Repword`].
#[derive(Debug, Default)]
pub struct RepwordConstrainerBuilder {
    separators: HashSet<char>,
    letter_chars: HashSet<char>,
}

impl RepwordConstrainerBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn separator(&mut self, c: char) {
        self.separators.insert(c);
    }

    pub fn letter_characters(&mut self, chars: HashSet<char>) {
        self.letter_chars = chars;
    }

    pub fn build(self) -> Option<RepwordConstrainer> {
        (!self.separators.is_empty()).then_some(RepwordConstrainer {
            separators: self.separators,
            letter_chars: self.letter_chars,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::emphasis::EmphasisSpan;

    fn dont_contract(c: &PositionConstraints) -> Vec<bool> {
        c.0.iter()
            .map(|f| f.contains(Constraint::DontContract))
            .collect()
    }

    fn use_comp6(c: &PositionConstraints) -> Vec<bool> {
        c.0.iter()
            .map(|f| f.contains(Constraint::UseComp6))
            .collect()
    }

    fn numeric_constrainers(builder: NumericConstrainerBuilder) -> Constrainers {
        Constrainers::new(vec![Constrainer::Numeric(builder.build().unwrap())])
    }

    #[test]
    fn numeric_run() {
        let mut builder = NumericConstrainerBuilder::new();
        builder.numeric_characters(HashSet::from(['1', '2', '3']));
        let c = numeric_constrainers(builder).precompute("ab12 a", &[]);
        assert_eq!(
            dont_contract(&c),
            vec![false, false, true, true, false, false]
        );
    }

    #[test]
    fn midnum_extends_run() {
        let mut builder = NumericConstrainerBuilder::new();
        builder.numeric_characters(HashSet::from(['1', '2', '3']));
        builder.midnum(".");
        let c = numeric_constrainers(builder).precompute("1.2", &[]);
        assert_eq!(dont_contract(&c), vec![true, true, true]);
    }

    #[test]
    fn empty_returns_false() {
        let c = PositionConstraints::empty();
        assert!(!c.dont_contract_at(0));
        assert!(!c.dont_contract_at(99));
    }

    #[test]
    fn computer_braille_span_sets_use_comp6() {
        let spans = vec![EmphasisSpan::new("computer_braille", 1..4)];
        let constrainers = Constrainers::new(vec![Constrainer::ComputerBraille(
            ComputerBrailleConstrainer {},
        )]);
        let c = constrainers.precompute("hello", &spans);
        assert_eq!(use_comp6(&c), vec![false, true, true, true, false]);
        assert_eq!(dont_contract(&c), vec![false, false, false, false, false]);
    }

    fn titlecase(c: &PositionConstraints) -> Vec<bool> {
        c.0.iter()
            .map(|f| f.contains(Constraint::TitleCase))
            .collect()
    }

    fn uppercase(c: &PositionConstraints) -> Vec<bool> {
        c.0.iter()
            .map(|f| f.contains(Constraint::Uppercase))
            .collect()
    }

    fn prefer_digit(c: &PositionConstraints) -> Vec<bool> {
        c.0.iter()
            .map(|f| f.contains(Constraint::PreferDigit))
            .collect()
    }

    fn backward_caps_constrainers(builder: BackwardCapsConstrainerBuilder) -> Constrainers {
        Constrainers::new(vec![Constrainer::BackwardCaps(builder.build().unwrap())])
    }

    fn backward_numeric_constrainers(builder: BackwardNumericConstrainerBuilder) -> Constrainers {
        Constrainers::new(vec![Constrainer::BackwardNumeric(builder.build().unwrap())])
    }

    fn no_contract(c: &PositionConstraints) -> Vec<bool> {
        c.0.iter()
            .map(|f| f.contains(Constraint::NoContract))
            .collect()
    }

    fn backward_nocontract_constrainers(
        builder: BackwardNoContractConstrainerBuilder,
    ) -> Constrainers {
        Constrainers::new(vec![Constrainer::BackwardNoContract(
            builder.build().unwrap(),
        )])
    }

    #[test]
    fn letsign_marks_letters_until_non_letter_cell() {
        let mut builder = BackwardNoContractConstrainerBuilder::new();
        builder.indicator("L");
        builder.letter_cells(HashSet::from(['a', 'b', 'c']));
        let c = backward_nocontract_constrainers(builder).precompute("Lab cLc", &[]);
        assert_eq!(
            no_contract(&c),
            vec![false, true, true, false, false, false, true]
        );
    }

    #[test]
    fn longest_indicator_wins() {
        // `nonumsign 56` and `nocontractsign 56-56` in one table: the two-cell
        // indicator must not be read as the one-cell one followed by a letter cell.
        let mut builder = BackwardNoContractConstrainerBuilder::new();
        builder.indicator("N");
        builder.indicator("NN");
        builder.letter_cells(HashSet::from(['a', 'N']));
        let c = backward_nocontract_constrainers(builder).precompute("NNa", &[]);
        assert_eq!(no_contract(&c), vec![false, false, true]);
    }

    #[test]
    fn capsletter_marks_next_cell_titlecase() {
        let mut builder = BackwardCapsConstrainerBuilder::new();
        builder.capsletter("C");
        let c = backward_caps_constrainers(builder).precompute("aCbc", &[]);
        assert_eq!(titlecase(&c), vec![false, false, true, false]);
    }

    #[test]
    fn begcapsword_marks_letters_until_non_letter_cell() {
        let mut builder = BackwardCapsConstrainerBuilder::new();
        builder.begcapsword("W");
        builder.letter_cells(HashSet::from(['a', 'b', 'c']));
        let c = backward_caps_constrainers(builder).precompute("Wabc ", &[]);
        assert_eq!(uppercase(&c), vec![false, true, true, true, false]);
    }

    #[test]
    fn begcapsword_stops_at_endcapsword() {
        let mut builder = BackwardCapsConstrainerBuilder::new();
        builder.begcapsword("W");
        builder.endcapsword("E");
        builder.letter_cells(HashSet::from(['a', 'b', 'c']));
        let c = backward_caps_constrainers(builder).precompute("WabEc", &[]);
        assert_eq!(uppercase(&c), vec![false, true, true, false, false]);
    }

    #[test]
    fn begcapsword_checked_before_capsletter_when_dots_share_a_prefix() {
        // Regression: capsletter's dots ("C") are a prefix of begcapsword's ("CC").
        // Checking capsletter first would match it twice instead of begcapsword once.
        let mut builder = BackwardCapsConstrainerBuilder::new();
        builder.capsletter("C");
        builder.begcapsword("CC");
        builder.letter_cells(HashSet::from(['a', 'b']));
        let c = backward_caps_constrainers(builder).precompute("CCab ", &[]);
        assert_eq!(uppercase(&c), vec![false, false, true, true, false]);
        assert_eq!(titlecase(&c), vec![false, false, false, false, false]);
    }

    #[test]
    fn multind_capsletter_after_letsign_titlecases_the_letter() {
        // `multind LC letsign capsletter`: the bare capsletter `C` would also match
        // one cell in, but the whole sequence is consumed first.
        let mut builder = BackwardCapsConstrainerBuilder::new();
        builder.capsletter("C");
        builder.multind_capsletter("CL");
        let c = backward_caps_constrainers(builder).precompute("CLab", &[]);
        assert_eq!(titlecase(&c), vec![false, false, true, false]);
    }

    #[test]
    fn nocont_marks_the_whole_word() {
        let mut builder = NoContConstrainerBuilder::new();
        builder.nocont("c@");
        builder.space_characters(HashSet::from([' ']));
        let c = Constrainers::new(vec![Constrainer::NoCont(builder.build().unwrap())])
            .precompute("ab C@D e", &[]);
        let marked: Vec<bool> =
            c.0.iter()
                .map(|f| f.contains(Constraint::Uncontracted))
                .collect();
        assert_eq!(
            marked,
            vec![false, false, false, true, true, true, false, false]
        );
    }

    #[test]
    fn capsnocont_marks_from_the_capitals_to_the_end_of_the_word() {
        let mut builder = CapsNoContConstrainerBuilder::new();
        builder.mark_present();
        builder.uppercase_characters(HashSet::from(['A', 'B']));
        builder.space_characters(HashSet::from([' ']));
        let c = Constrainers::new(vec![Constrainer::CapsNoCont(builder.build().unwrap())])
            .precompute("aABa Aa", &[]);
        let marked: Vec<bool> =
            c.0.iter()
                .map(|f| f.contains(Constraint::Uncontracted))
                .collect();
        // a single capital is not enough
        assert_eq!(marked, vec![false, true, true, true, false, false, false]);
    }

    /// The length of the run flagged right before the `-` in `input`, for `repword` and
    /// for `rependword`
    fn repeated_lengths(input: &str) -> (usize, usize) {
        let mut builder = RepwordConstrainerBuilder::new();
        builder.separator('-');
        builder.letter_characters("abcxABC".chars().collect());
        let c = Constrainers::new(vec![Constrainer::Repword(builder.build().unwrap())])
            .precompute(input, &[]);
        let at = input.chars().position(|c| c == '-').unwrap();
        (
            c.run_before(at, Constraint::RepeatedWord),
            c.run_before(at, Constraint::RepeatedWordEnd),
        )
    }

    #[test]
    fn repword_marks_further_repetitions() {
        let mut builder = RepwordConstrainerBuilder::new();
        builder.separator('-');
        builder.letter_characters("abcx".chars().collect());
        let c = Constrainers::new(vec![Constrainer::Repword(builder.build().unwrap())])
            .precompute("abc-abc-abc-abx", &[]);
        // the second `-abc`, but not `-abx`
        let marked: Vec<usize> = (0..15).filter(|&p| c.repetition_at(p)).collect();
        assert_eq!(marked, (7..11).collect::<Vec<_>>());
    }

    #[test]
    fn repword_marks_the_longest_repeated_word() {
        assert_eq!(repeated_lengths("abc-ABCxx"), (3, 0));
        // the end of a longer word
        assert_eq!(repeated_lengths("xabc-abc"), (0, 3));
        assert_eq!(repeated_lengths("abc-xab"), (0, 0));
    }

    #[test]
    fn joinword_marks_spaces_followed_by_a_letter() {
        let mut builder = JoinConstrainerBuilder::new();
        builder.mark_present();
        builder.letter_characters(HashSet::from(['a']));
        builder.space_characters(HashSet::from([' ']));
        let c = Constrainers::new(vec![Constrainer::Join(builder.build().unwrap())])
            .precompute("a  a . a", &[]);
        let marked: Vec<usize> = (0..8).filter(|&p| c.space_before_letter_at(p)).collect();
        // not the space before the `.`
        assert_eq!(marked, vec![1, 2, 6]);
    }

    #[test]
    fn joinnum_marks_spaces_followed_by_a_digit() {
        let mut builder = JoinConstrainerBuilder::new();
        builder.mark_present();
        builder.digit_characters(HashSet::from(['1']));
        builder.space_characters(HashSet::from([' ']));
        let c = Constrainers::new(vec![Constrainer::Join(builder.build().unwrap())])
            .precompute("$  1 a $ 1", &[]);
        let marked: Vec<bool> =
            c.0.iter()
                .map(|f| f.contains(Constraint::SpaceBeforeDigit))
                .collect();
        assert_eq!(
            marked,
            vec![
                false, true, true, false, false, false, false, false, true, false
            ]
        );
    }

    #[test]
    fn numsign_marks_digit_cells_until_non_digit() {
        let mut builder = BackwardNumericConstrainerBuilder::new();
        builder.numsign("N");
        builder.digit_cells(HashSet::from(['1', '2']));
        let c = backward_numeric_constrainers(builder).precompute("N12a1", &[]);
        assert_eq!(prefer_digit(&c), vec![false, true, true, false, false]);
    }

    #[test]
    fn nonumsign_ends_the_run_explicitly() {
        let mut builder = BackwardNumericConstrainerBuilder::new();
        builder.numsign("N");
        builder.nonumsign("X");
        builder.digit_cells(HashSet::from(['1', '2']));
        let c = backward_numeric_constrainers(builder).precompute("N1X2", &[]);
        assert_eq!(prefer_digit(&c), vec![false, true, false, false]);
    }

    #[test]
    fn numsign_ignored_when_preceded_by_a_letter_cell() {
        // Regression: Punjabi shares the common numsign dots ("N" here) with the
        // unrelated, frequently-occurring Gurmukhi letter "ਣ" (disambiguated in
        // liblouis by word position). A mid-word occurrence must not start a numeric
        // run and corrupt the digit-cell-colliding letters that follow it.
        let mut builder = BackwardNumericConstrainerBuilder::new();
        builder.numsign("N");
        builder.digit_cells(HashSet::from(['1', '2']));
        builder.letter_cells(HashSet::from(['x']));
        let c = backward_numeric_constrainers(builder).precompute("xN12", &[]);
        assert_eq!(prefer_digit(&c), vec![false, false, false, false]);
    }
}
