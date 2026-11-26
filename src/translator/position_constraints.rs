//! Per-position translation constraints
//!
//! A [`Constrainers`] collection analyses the input text, the emphasis spans and the
//! translation options before translation begins and produces a [`PositionConstraints`]
//! value. The translation loop consults this
//! to decide which rules are eligible at each character position.
//!
//! The constrainer types are:
//! * [`Constrainer::Numeric`]: detects digit runs and sets [`Constraint::DontContract`]
//! * [`Constrainer::ComputerBraille`]: maps computer-braille spans to [`Constraint::UseComp6`]

use enumset::{EnumSet, EnumSetType};
use std::collections::HashSet;

use std::ops::Range;

use crate::emphasis::EmphasisSpan;
use crate::hyphenation::HyphenationTable;
use crate::parser::CharacterClasses;
use crate::translator::{TranslationMode, TranslationOptions};

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
    /// A rule that starts before this character can't take it, in forward translation.
    ///
    /// Set where a syllable of a `syllable` rule, an emphasis span or a run of capitals
    /// starts, so that "GitHub" doesn't contract "tH" and the "H" keeps its capital
    /// indicator. A rule starting on a [`Constraint::Capital`] is exempt.
    TypeStart,
    /// A capital, in forward translation with a table that has a `capsletter`.
    ///
    /// Exempts a rule starting on it from [`Constraint::TypeStart`]: it may run into a
    /// syllable, an emphasis span or another run of capitals.
    Capital,
    /// A letter in the other case than the letter before it, in forward translation. A rule
    /// can't take it after its second character, so that "NorthEast" isn't contracted as
    /// "northeast" and the "E" keeps its capital indicator; "The" still contracts.
    CaseChange,
    /// Suppress the word-family rules at this position, in backward translation.
    ///
    /// Set for the run of letter cells following a `letsign`/`nocontractsign`/
    /// `nonumsign` indicator. Those indicators exist precisely to say "read the
    /// following cells as letters, not as a contraction", so the `word`, `sufword`,
    /// `prfword` and `begword` rules must not fire inside their scope.
    NoContract,
    /// Suppress the whole-word rules at this position, in backward translation.
    ///
    /// Set from where a [`Constraint::NoContract`] run of letters ends to the next
    /// space: liblouis lets a `letsign`/`nocontractsign`/`nonumsign` lapse rather than
    /// end at a cell that is not a letter, and until the word ends it keeps `word`,
    /// `sufword` and `prfword` off, though not `begword`. So in UEB the ⠆ of ⠰⠉⠲⠆ is
    /// the semicolon of "c.;", not the wordsign "be".
    NoWholeWord,
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
    /// This position is in the word at the cursor, which `compbrlAtCursor` or
    /// `compbrlLeftCursor` shows in computer braille, in forward translation: no rule
    /// reaches into it, and no indicator goes in it.
    CursorWord,
    /// This position is in the run of whitespace right before the word at the cursor, so a
    /// `joinword` or `joinnum` just before it would join onto that word.
    SpaceBeforeCursorWord,
    /// The end of a capitals passage indicator starts here, in backward translation.
    ///
    /// An `endcapsphrase` is read only here, and wins over a capital indicator with the
    /// same cells, such as UEB's `endcapsword` ⠠⠄ or Dutch `begcapsword` ⠘.
    CapsPassageEnd,
}

pub type Constraints = EnumSet<Constraint>;

/// The result of pre-computing per-position constraints for a given input.
pub struct PositionConstraints(Vec<Constraints>);

impl PositionConstraints {
    #[cfg(test)]
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

    pub fn no_whole_word_at(&self, pos: usize) -> bool {
        self.0
            .get(pos)
            .is_some_and(|f| f.contains(Constraint::NoWholeWord))
    }

    pub fn use_comp6_at(&self, pos: usize) -> bool {
        self.0
            .get(pos)
            .is_some_and(|f| f.contains(Constraint::UseComp6))
    }

    pub fn prefer_digit_at(&self, pos: usize) -> bool {
        self.0
            .get(pos)
            .is_some_and(|f| f.contains(Constraint::PreferDigit))
    }

    pub fn type_start_at(&self, pos: usize) -> bool {
        self.0
            .get(pos)
            .is_some_and(|f| f.contains(Constraint::TypeStart))
    }

    pub fn case_change_at(&self, pos: usize) -> bool {
        self.0
            .get(pos)
            .is_some_and(|f| f.contains(Constraint::CaseChange))
    }

    pub fn capital_at(&self, pos: usize) -> bool {
        self.0
            .get(pos)
            .is_some_and(|f| f.contains(Constraint::Capital))
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

    pub fn caps_passage_end_at(&self, pos: usize) -> bool {
        self.0
            .get(pos)
            .is_some_and(|f| f.contains(Constraint::CapsPassageEnd))
    }

    pub fn in_cursor_word_at(&self, pos: usize) -> bool {
        self.0
            .get(pos)
            .is_some_and(|f| f.contains(Constraint::CursorWord))
    }

    /// Whether a translation of `length` characters at `pos` reaches into the word at the
    /// cursor from outside it
    pub fn reaches_into_cursor_word(&self, pos: usize, length: usize) -> bool {
        !self.in_cursor_word_at(pos) && (pos..pos + length).any(|p| self.in_cursor_word_at(p))
    }

    /// Whether only whitespace, or nothing, lies between `pos` and the word at the cursor
    pub fn joins_cursor_word_at(&self, pos: usize) -> bool {
        self.0.get(pos).is_some_and(|f| {
            f.contains(Constraint::CursorWord) || f.contains(Constraint::SpaceBeforeCursorWord)
        })
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

/// Sets [`Constraint::DontContract`] from the first digit of a number to the next space
/// or `seqdelimiter`, the letters after the number included. liblouis does this only in a
/// table with numeric mode whose `nocontractsign` is the `nonumsign` (`checkNumericMode`),
/// see [`NumericConstrainerBuilder::build`].
#[derive(Debug, Clone)]
pub struct NumericConstrainer {
    numeric_chars: HashSet<char>,
    word_ends: HashSet<char>,
}

impl NumericConstrainer {
    fn compute(&self, input: &str) -> Vec<Constraints> {
        let mut active = false;
        input
            .chars()
            .map(|c| {
                if self.word_ends.contains(&c) {
                    active = false;
                } else if self.numeric_chars.contains(&c) {
                    active = true;
                }
                if active {
                    Constraints::from(Constraint::DontContract)
                } else {
                    Constraints::empty()
                }
            })
            .collect()
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

/// Keeps a rule from running into a syllable, an emphasis span or a run of capitals that
/// starts after its first character, by setting [`Constraint::TypeStart`] where each of
/// them starts, and [`Constraint::Capital`] on the capitals, whose rules are exempt.
///
/// - The syllables of the `syllable` rules: going through the text, the longest syllable
///   at each position, then on after it. Two syllables next to each other are two.
/// - The emphasis spans.
/// - The runs of capitals, in a table with a `capsletter`, so that "GitHub" doesn't
///   contract "tH" and the "H" keeps its capital indicator.
///
/// This is how liblouis's `validMatch` checks a rule against the types it keeps per
/// character.
#[derive(Debug, Clone)]
pub struct TypeConstrainer {
    /// The strings of the `syllable` rules, lowercased, one `char` per character, so that
    /// they compare with the input at character positions, which the constraints use. A
    /// linear scan at each position, as only `en-us-g2` and `en-us-interline` have them,
    /// 38 at most; a trie if that changes.
    syllable_rules: Vec<Vec<char>>,
    capitals: bool,
}

impl TypeConstrainer {
    fn compute(&self, input: &str, spans: &[EmphasisSpan]) -> Vec<Constraints> {
        let chars: Vec<char> = input.chars().collect();
        let mut constraints: Vec<Constraints> = vec![Constraints::empty(); chars.len()];
        let lowercased: Vec<char> = chars.iter().copied().map(lowercase).collect();
        let mut pos = 0;
        while pos < chars.len() {
            match self
                .syllable_rules
                .iter()
                .filter(|s| lowercased[pos..].starts_with(s))
                .map(Vec::len)
                .max()
            {
                Some(len) => {
                    constraints[pos].insert(Constraint::TypeStart);
                    pos += len;
                }
                None => pos += 1,
            }
        }
        for span in spans {
            if let Some(c) = constraints.get_mut(span.range().start) {
                c.insert(Constraint::TypeStart);
            }
        }
        for (pos, pair) in chars.windows(2).enumerate() {
            let cased = |c: &char| c.is_uppercase() || c.is_lowercase();
            if cased(&pair[0])
                && cased(&pair[1])
                && pair[0].is_uppercase() != pair[1].is_uppercase()
            {
                constraints[pos + 1].insert(Constraint::CaseChange);
            }
        }
        if self.capitals {
            for (pos, c) in chars.iter().enumerate() {
                if c.is_uppercase() {
                    constraints[pos].insert(Constraint::Capital);
                    if pos == 0 || !chars[pos - 1].is_uppercase() {
                        constraints[pos].insert(Constraint::TypeStart);
                    }
                }
            }
        }
        constraints
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
                for c in &chars[pos + 1..] {
                    if self.letter_or_digit_chars.contains(c) {
                        constraints[pos].insert(Constraint::PrepuncOk);
                        break;
                    }
                    if self.space_chars.contains(c) {
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
            if span.class() == EmphasisSpan::COMPUTER_BRAILLE {
                let end = span.range().end.min(n);
                for c in constraints.iter_mut().take(end).skip(span.range().start) {
                    c.insert(Constraint::UseComp6);
                }
            }
        }
        constraints
    }
}

/// Switches contractions off everywhere with the `noContractions` mode.
#[derive(Debug, Clone)]
pub struct NoContractionsConstrainer;

impl NoContractionsConstrainer {
    fn compute(&self, input: &str, options: &TranslationOptions) -> Vec<Constraints> {
        let constraints = if options.modes().contains(TranslationMode::NoContractions) {
            Constraints::only(Constraint::Uncontracted)
        } else {
            Constraints::empty()
        };
        vec![constraints; input.chars().count()]
    }
}

/// Finds the word at the cursor to show uncontracted, in computer braille, with
/// `compbrlAtCursor` or `compbrlLeftCursor`, as liblouis does: the word the cursor is in, up
/// to the next space, or only up to the cursor with `compbrlLeftCursor`; the space itself if
/// the cursor is on one.
///
/// Unlike liblouis, `repeated` rules next to the word are left alone: liblouis's limits
/// there are off by one, so a `repeated \s 0` expands spaces inconsistently (its own
/// `present_progressive.yaml` marks this `xfail`), and a `repword` can't reach into the word.
#[derive(Debug, Clone)]
pub struct CursorWordConstrainer {
    pub character_classes: CharacterClasses,
}

impl CursorWordConstrainer {
    fn compute(&self, input: &str, options: &TranslationOptions) -> Vec<Constraints> {
        let chars: Vec<char> = input.chars().collect();
        let mut constraints = vec![Constraints::empty(); chars.len()];
        let Some(word) = self.word(&chars, options) else {
            return constraints;
        };
        for c in &mut constraints[word.clone()] {
            *c |= Constraint::UseComp6 | Constraint::CursorWord;
        }
        let is_space = |c: &char| self.character_classes.is_whitespace(*c);
        let spaces = chars[..word.start]
            .iter()
            .rev()
            .take_while(|c| is_space(c))
            .count();
        for c in &mut constraints[word.start - spaces..word.start] {
            c.insert(Constraint::SpaceBeforeCursorWord);
        }
        constraints
    }

    fn word(&self, chars: &[char], options: &TranslationOptions) -> Option<Range<usize>> {
        let modes = options.modes();
        let left = modes.contains(TranslationMode::CompbrlLeftCursor);
        if !(left || modes.contains(TranslationMode::CompbrlAtCursor)) {
            return None;
        }
        let cursor = options.cursor_position()?;
        if cursor > chars.len() {
            return None;
        }
        let is_space = |c: &char| self.character_classes.is_whitespace(*c);
        if chars.get(cursor).is_some_and(is_space) {
            return Some(cursor..cursor + 1);
        }
        let start = chars[..cursor]
            .iter()
            .rposition(is_space)
            .map_or(0, |p| p + 1);
        let end = if left {
            cursor
        } else {
            chars[cursor..]
                .iter()
                .position(is_space)
                .map_or(chars.len(), |p| cursor + p)
        };
        Some(start..end)
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

/// Detects `numsign`/`nonumsign` indicator dots in backward input and sets
/// [`Constraint::PreferDigit`] at every digit cell of the numeric run, disambiguating
/// dots shared between a digit and a letter.
///
/// `numsign`'s dots are not necessarily exclusive to the indicator: some tables (e.g.
/// Punjabi, which shares the common `3456` numsign convention with the unrelated
/// Gurmukhi letter `ਣ`, disambiguated in liblouis via a `midendword` position
/// constraint) also use them for an ordinary, frequently-occurring letter. A rule
/// that only applies after a letter (`midword`, `midendword`, `endword`) takes such
/// dots right after a letter cell, so they don't start a number there. Otherwise a
/// number can follow a letter, as in Hungarian "A1". Neither does a numsign that is
/// part of a longer rule starting before it, such as sk-g1's `context [@5-46-3456]
/// "#"`, which takes the numsign's cell as the end of "#".
///
/// It decides from the cells alone, before any rule is chosen, where liblouis decides from
/// the rules it applied and the text it wrote: a number starts when the numsign rule is
/// applied, and whether a letter comes before depends on how the cells before were read.
/// The cells can't tell that exactly, as one cell can be a letter in one rule and part of
/// a sign in another, so this is a guess, refined case by case. Keeping track of number
/// mode in the translation loop, from the rules actually applied, would make it exact
/// (#95).
#[derive(Debug, Clone)]
pub struct BackwardNumericConstrainer {
    /// The `numsign` dots plus any `multind` sequences naming it, e.g.
    /// `6-3456 letsign numsign`.
    numsign_dots: Vec<String>,
    nonumsign_dots: Option<String>,
    digit_cells: HashSet<char>,
    /// The cells of the `numericmodechars` and `midendnumericmodechars` characters,
    /// such as a decimal comma, which a number continues across.
    numeric_mode_cells: HashSet<char>,
    /// The dots of the `endnum` rules, which end a number unless a digit follows.
    endnum_dots: Vec<String>,
    letter_cells: HashSet<char>,
    /// The cells of one-cell rules that can apply inside a word and write text ending in a
    /// letter, such as en-us-g2's `midword gg 2356`
    inner_letter_cells: HashSet<char>,
    /// The dots of the rules that only apply after a letter, such as Punjabi's
    /// `midendword ਣ 3456`.
    after_letter_dots: HashSet<String>,
    /// The dots of the backward rules that contain a numsign after their first cell.
    covering_dots: Vec<String>,
    /// The dots of characters other than letters that are a numsign and more, such as
    /// Portuguese `sign # 3456-13`. The longer match wins over the numsign, and the text
    /// then ends in the character, whatever its last cell is.
    sign_dots: Vec<String>,
}

impl BackwardNumericConstrainer {
    /// Whether `cells` come back as text ending in a letter, as far as the cells tell: they
    /// end in a letter cell, or in cells of one-cell rules that apply inside a word after
    /// one. So in en-us-g2 ⠶ is "gg" after ⠁, but "(" at the start of a word.
    fn ends_in_letter(&self, cells: &[char]) -> bool {
        cells
            .iter()
            .rev()
            .find(|c| self.letter_cells.contains(c) || !self.inner_letter_cells.contains(c))
            .is_some_and(|c| self.letter_cells.contains(c))
    }

    /// Whether a rule starting before `pos` takes the numsign there as part of its dots.
    fn covered(&self, chars: &[char], pos: usize) -> bool {
        self.covering_dots.iter().any(|dots| {
            (1..dots.chars().count().min(pos + 1))
                .any(|offset| matches_at(chars, pos - offset, dots))
        })
    }

    fn compute(&self, input: &str) -> Vec<Constraints> {
        let chars: Vec<char> = input.chars().collect();
        let n = chars.len();
        let mut constraints: Vec<Constraints> = vec![Constraints::empty(); n];
        let mut pos = 0;
        let mut in_number = false;
        let mut sign_end = None;
        while pos < n {
            // not on a digit inside a number, where only the digit is tried
            if !(in_number && self.digit_cells.contains(&chars[pos]))
                && let Some(len) = longest_match_at(&chars, pos, &self.sign_dots)
            {
                pos += len;
                in_number = false;
                sign_end = Some(pos);
                continue;
            }
            if let Some(len) = longest_match_at(&chars, pos, &self.numsign_dots)
                && !(pos > 0
                    && sign_end != Some(pos)
                    && self.ends_in_letter(&chars[..pos])
                    && self
                        .after_letter_dots
                        .contains(&chars[pos..pos + len].iter().collect::<String>()))
                && !self.covered(&chars, pos)
            {
                pos += len;
                in_number = true;
                continue;
            }
            if in_number {
                // an `endnum` sign ends the number, unless a digit follows it, in which
                // case it is a sign within the number, as in "1+2"
                if let Some(len) = longest_match_at(&chars, pos, &self.endnum_dots)
                    && !chars
                        .get(pos + len)
                        .is_some_and(|c| self.digit_cells.contains(c))
                {
                    pos += len;
                    in_number = false;
                    continue;
                }
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
                // liblouis ends a number only after a character that is neither a digit
                // nor a numeric mode character, so in "10,5" the cell after the decimal
                // comma is still a digit.
                if self.numeric_mode_cells.contains(&chars[pos]) {
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
///
/// Capital indicators between the letters don't end the run: liblouis consumes them
/// without leaving its no-contract mode, so in UEB ⠰⠠⠠⠠⠅ is the capital letter "K",
/// not the shortform "KNOWLEDGE".
#[derive(Debug, Clone)]
pub struct BackwardNoContractConstrainer {
    indicator_dots: Vec<String>,
    caps_dots: Vec<String>,
    letter_cells: HashSet<char>,
    space_cells: HashSet<char>,
    /// The dots of the backward `always`, `prepunc` and `postpunc` rules and character
    /// definitions that start with an indicator's dots and go on, such as Danish
    /// `always j 56-245`, which take the cells as their own.
    ///
    /// This is a heuristic. Whether the indicator's dots are an indicator or the start
    /// of a longer rule is guessed here, before translation, from the rules' dots alone,
    /// ignoring their boundary and class checks. The real solution is to decide from
    /// which candidate translation picks at that position.
    claiming_dots: Vec<String>,
    /// Like `claiming_dots`, but of the rules that only apply after a letter, such as
    /// UEB's ⠰⠎ for "ness", which take the cells only there.
    inword_claiming_dots: Vec<String>,
}

impl BackwardNoContractConstrainer {
    fn compute(&self, input: &str) -> Vec<Constraints> {
        let chars: Vec<char> = input.chars().collect();
        let n = chars.len();
        let mut constraints: Vec<Constraints> = vec![Constraints::empty(); n];
        let mut pos = 0;
        while pos < n {
            // Longest first: one indicator's dots are often a prefix of another's. A
            // longer rule spelling the indicator's dots and the cells after it wins
            // over the indicator, as in liblouis, which tries the longest rules first.
            let after_letter = pos > 0 && self.letter_cells.contains(&chars[pos - 1]);
            if let Some(len) = longest_match_at(&chars, pos, &self.indicator_dots)
                && longest_match_at(&chars, pos, &self.claiming_dots)
                    .is_none_or(|claim| claim <= len)
                && !(after_letter
                    && longest_match_at(&chars, pos, &self.inword_claiming_dots)
                        .is_some_and(|claim| claim > len))
            {
                pos += len;
                while pos < n {
                    if let Some(len) = longest_match_at(&chars, pos, &self.caps_dots) {
                        pos += len;
                    } else if self.letter_cells.contains(&chars[pos]) {
                        constraints[pos].insert(Constraint::NoContract);
                        pos += 1;
                    } else {
                        break;
                    }
                }
                while pos < n
                    && !self.space_cells.contains(&chars[pos])
                    && longest_match_at(&chars, pos, &self.indicator_dots).is_none()
                {
                    constraints[pos].insert(Constraint::NoWholeWord);
                    pos += 1;
                }
                continue;
            }
            pos += 1;
        }
        constraints
    }
}

/// Finds the capitals passages in backward input and sets [`Constraint::CapsPassageEnd`]
/// where each one's end indicator starts: the first `endcapsphrase` or `endcaps` after a
/// `begcapsphrase`, a `begcaps` or a `multind` with a `begcapsphrase`. Outside a passage,
/// the end indicator's cells are something else, as `endcapsphrase after 6` is also
/// `capsletter 6` in no-no-g0.utb.
#[derive(Debug, Clone, Default)]
pub struct BackwardCapsPassageConstrainer {
    begin_dots: Vec<String>,
    end_dots: Vec<String>,
}

impl BackwardCapsPassageConstrainer {
    pub fn begin(&mut self, dots: &str) {
        self.begin_dots.push(dots.to_string());
    }

    pub fn end(&mut self, dots: &str) {
        self.end_dots.push(dots.to_string());
    }

    pub fn build(self) -> Option<Self> {
        (!self.begin_dots.is_empty() && !self.end_dots.is_empty()).then_some(self)
    }

    fn compute(&self, input: &str) -> Vec<Constraints> {
        let chars: Vec<char> = input.chars().collect();
        let mut constraints = vec![Constraints::empty(); chars.len()];
        let mut in_passage = false;
        let mut pos = 0;
        while pos < chars.len() {
            let dots = if in_passage {
                &self.end_dots
            } else {
                &self.begin_dots
            };
            if let Some(len) = longest_match_at(&chars, pos, dots) {
                if in_passage {
                    constraints[pos].insert(Constraint::CapsPassageEnd);
                }
                in_passage = !in_passage;
                pos += len;
            } else {
                pos += 1;
            }
        }
        constraints
    }
}

/// A builder for [`Constrainer::BackwardNoContract`].
#[derive(Debug, Default)]
pub struct BackwardNoContractConstrainerBuilder {
    indicator_dots: Vec<String>,
    caps_dots: Vec<String>,
    letter_cells: HashSet<char>,
    space_cells: HashSet<char>,
    rule_dots: Vec<String>,
    inword_rule_dots: Vec<String>,
}

impl BackwardNoContractConstrainerBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn indicator(&mut self, dots: &str) {
        self.indicator_dots.push(dots.to_string());
    }

    /// The dots of a capital indicator, which a run of plain letters continues across.
    pub fn caps_indicator(&mut self, dots: &str) {
        self.caps_dots.push(dots.to_string());
    }

    pub fn letter_cells(&mut self, cells: HashSet<char>) {
        self.letter_cells = cells;
    }

    /// The cells that end a word, and with it a lapsed indicator's scope.
    pub fn space_cells(&mut self, cells: HashSet<char>) {
        self.space_cells = cells;
    }

    /// Registers the dots of a backward rule that takes its cells whatever surrounds them, see
    /// [`BackwardNoContractConstrainer::claiming_dots`].
    pub fn rule_dots(&mut self, dots: &str) {
        self.rule_dots.push(dots.to_string());
    }

    /// Registers the dots of a backward rule that applies only after a letter, see
    /// [`BackwardNoContractConstrainer::inword_claiming_dots`].
    pub fn inword_rule_dots(&mut self, dots: &str) {
        self.inword_rule_dots.push(dots.to_string());
    }

    pub fn build(self) -> Option<BackwardNoContractConstrainer> {
        if self.indicator_dots.is_empty() {
            None
        } else {
            Some(BackwardNoContractConstrainer {
                claiming_dots: claiming(self.rule_dots, &self.indicator_dots),
                inword_claiming_dots: claiming(self.inword_rule_dots, &self.indicator_dots),
                indicator_dots: self.indicator_dots,
                caps_dots: self.caps_dots,
                letter_cells: self.letter_cells,
                space_cells: self.space_cells,
            })
        }
    }
}

/// The rule dots that start with one of the indicators' dots and go on.
fn claiming(rule_dots: Vec<String>, indicator_dots: &[String]) -> Vec<String> {
    rule_dots
        .into_iter()
        .filter(|dots| {
            indicator_dots.iter().any(|indicator| {
                dots.starts_with(indicator.as_str()) && dots.len() > indicator.len()
            })
        })
        .collect()
}

/// A single analysis pass that contributes to [`PositionConstraints`].
#[derive(Debug, Clone)]
pub enum Constrainer {
    /// Detects numeric runs and sets [`Constraint::DontContract`].
    Numeric(NumericConstrainer),
    /// Maps `computer_braille` emphasis spans to [`Constraint::UseComp6`].
    ComputerBraille(ComputerBrailleConstrainer),
    /// Detects backward `numsign`/`nonumsign` dots and sets [`Constraint::PreferDigit`].
    BackwardNumeric(BackwardNumericConstrainer),
    /// Detects backward `letsign`/`nocontractsign`/`nonumsign` dots and sets
    /// [`Constraint::NoContract`].
    BackwardNoContract(BackwardNoContractConstrainer),
    /// Detects backward capitals passages and sets [`Constraint::CapsPassageEnd`].
    BackwardCapsPassage(BackwardCapsPassageConstrainer),
    /// Detects hyphenation-dictionary breaks and sets [`Constraint::HyphenationBreak`].
    Hyphenation(HyphenationConstrainer),
    /// Detects where a syllable, an emphasis span or a run of capitals starts and sets
    /// [`Constraint::TypeStart`], and [`Constraint::Capital`] on the capitals.
    Type(TypeConstrainer),
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
    /// Sets [`Constraint::Uncontracted`] everywhere with the `noContractions` mode.
    NoContractions(NoContractionsConstrainer),
    /// Finds the word at the cursor with `compbrlAtCursor` or `compbrlLeftCursor` and sets
    /// [`Constraint::UseComp6`] and [`Constraint::CursorWord`] in it, and
    /// [`Constraint::SpaceBeforeCursorWord`] before it.
    CursorWord(CursorWordConstrainer),
}

impl Constrainer {
    fn compute(
        &self,
        input: &str,
        spans: &[EmphasisSpan],
        options: &TranslationOptions,
    ) -> Vec<Constraints> {
        match self {
            Constrainer::Numeric(c) => c.compute(input),
            Constrainer::ComputerBraille(c) => c.compute(input, spans),
            Constrainer::BackwardNoContract(c) => c.compute(input),
            Constrainer::BackwardNumeric(c) => c.compute(input),
            Constrainer::BackwardCapsPassage(c) => c.compute(input),
            Constrainer::Hyphenation(c) => c.compute(input),
            Constrainer::Type(c) => c.compute(input, spans),
            Constrainer::Punctuation(c) => c.compute(input),
            Constrainer::Join(c) => c.compute(input),
            Constrainer::NoCont(c) => c.compute(input),
            Constrainer::CapsNoCont(c) => c.compute(input),
            Constrainer::Repword(c) => c.compute(input),
            Constrainer::NoContractions(c) => c.compute(input, options),
            Constrainer::CursorWord(c) => c.compute(input, options),
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

    pub fn precompute(
        &self,
        input: &str,
        spans: &[EmphasisSpan],
        options: &TranslationOptions,
    ) -> PositionConstraints {
        let n = input.chars().count();
        let mut constraints: Vec<Constraints> = vec![Constraints::empty(); n];
        for constrainer in &self.0 {
            for (accum, computed) in constraints
                .iter_mut()
                .zip(constrainer.compute(input, spans, options))
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
    word_ends: HashSet<char>,
    numeric_mode: bool,
    nocontractsign: Option<String>,
    nonumsign: Option<String>,
}

impl NumericConstrainerBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn numeric_characters(&mut self, chars: HashSet<char>) {
        self.numeric_chars = chars;
    }

    /// The space and `seqdelimiter` characters, which end the uncontracted stretch
    pub fn word_ends(&mut self, chars: HashSet<char>) {
        self.word_ends = chars;
    }

    /// The table has `numericmodechars`, `midendnumericmodechars` or
    /// `numericnocontchars` (liblouis's `usesNumericMode`)
    pub fn numeric_mode(&mut self) {
        self.numeric_mode = true;
    }

    pub fn nocontractsign(&mut self, dots: &str) {
        self.nocontractsign = Some(dots.to_string());
    }

    pub fn nonumsign(&mut self, dots: &str) {
        self.nonumsign = Some(dots.to_string());
    }

    pub fn build(self) -> Option<NumericConstrainer> {
        let same_signs = self.nocontractsign.is_some() && self.nocontractsign == self.nonumsign;
        (self.numeric_mode && same_signs && !self.numeric_chars.is_empty()).then_some(
            NumericConstrainer {
                numeric_chars: self.numeric_chars,
                word_ends: self.word_ends,
            },
        )
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
    numeric_mode_cells: HashSet<char>,
    endnum_dots: Vec<String>,
    letter_cells: HashSet<char>,
    inner_letter_cells: HashSet<char>,
    after_letter_dots: HashSet<String>,
    rule_dots: Vec<String>,
    sign_dots: Vec<String>,
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

    pub fn numeric_mode_cells(&mut self, cells: HashSet<char>) {
        self.numeric_mode_cells = cells;
    }

    pub fn endnum(&mut self, dots: &str) {
        self.endnum_dots.push(dots.to_string());
    }

    pub fn letter_cells(&mut self, cells: HashSet<char>) {
        self.letter_cells = cells;
    }

    /// Registers the cell of a one-cell rule that can apply inside a word and writes text
    /// ending in a letter.
    pub fn inner_letter(&mut self, cell: char) {
        self.inner_letter_cells.insert(cell);
    }

    /// Registers the dots of a rule that only applies after a letter.
    pub fn after_letter(&mut self, dots: &str) {
        self.after_letter_dots.insert(dots.to_string());
    }

    /// Registers the dots of a backward rule, see [`BackwardNumericConstrainer::covered`].
    pub fn rule_dots(&mut self, dots: &str) {
        self.rule_dots.push(dots.to_string());
    }

    /// Registers the dots of a character other than a letter, of more than one cell, see
    /// [`BackwardNumericConstrainer::sign_dots`].
    pub fn sign_dots(&mut self, dots: &str) {
        self.sign_dots.push(dots.to_string());
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
            let covering_dots = self
                .rule_dots
                .into_iter()
                .filter(|dots| {
                    let cells: Vec<char> = dots.chars().collect();
                    (1..cells.len())
                        .any(|offset| numsign_dots.iter().any(|n| matches_at(&cells, offset, n)))
                })
                .collect();
            let sign_dots = self
                .sign_dots
                .into_iter()
                .filter(|dots| numsign_dots.iter().any(|n| dots.starts_with(n.as_str())))
                .collect();
            Some(BackwardNumericConstrainer {
                numsign_dots,
                nonumsign_dots: self.nonumsign_dots,
                digit_cells: self.digit_cells,
                numeric_mode_cells: self.numeric_mode_cells,
                endnum_dots: self.endnum_dots,
                letter_cells: self.letter_cells,
                inner_letter_cells: self.inner_letter_cells,
                after_letter_dots: self.after_letter_dots,
                covering_dots,
                sign_dots,
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

/// `c` lowercased, keeping one character per character so positions stay those of the
/// input.
fn lowercase(c: char) -> char {
    c.to_lowercase().next().unwrap_or(c)
}

/// A builder for [`Constrainer::Type`].
#[derive(Debug, Default)]
pub struct TypeConstrainerBuilder {
    syllable_rules: Vec<Vec<char>>,
    capitals: bool,
}

impl TypeConstrainerBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn syllable(&mut self, chars: &str) {
        self.syllable_rules
            .push(chars.chars().map(lowercase).collect());
    }

    /// The table has a `capsletter`.
    pub fn capsletter(&mut self) {
        self.capitals = true;
    }

    pub fn build(self) -> TypeConstrainer {
        TypeConstrainer {
            syllable_rules: self.syllable_rules,
            capitals: self.capitals,
        }
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
    use crate::translator::TranslationModes;

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

    fn numeric_mode_builder() -> NumericConstrainerBuilder {
        let mut builder = NumericConstrainerBuilder::new();
        builder.numeric_characters(HashSet::from(['1', '2', '3']));
        builder.word_ends(HashSet::from([' ']));
        builder.numeric_mode();
        builder.nocontractsign("⠰");
        builder.nonumsign("⠰");
        builder
    }

    #[test]
    fn numeric_run() {
        // from the first digit to the next space, the letters after the number included
        let c = numeric_constrainers(numeric_mode_builder()).precompute(
            "ab12a a",
            &[],
            &TranslationOptions::default(),
        );
        assert_eq!(
            dont_contract(&c),
            vec![false, false, true, true, true, false, false]
        );
    }

    #[test]
    fn numeric_run_needs_numeric_mode_and_equal_signs() {
        let mut builder = numeric_mode_builder();
        builder.nonumsign("⠠");
        assert!(builder.build().is_none());
        let mut builder = numeric_mode_builder();
        builder.numeric_mode = false;
        assert!(builder.build().is_none());
    }

    #[test]
    fn empty_returns_false() {
        let c = PositionConstraints::empty();
        assert!(!c.dont_contract_at(0));
        assert!(!c.dont_contract_at(99));
    }

    #[test]
    fn computer_braille_span_sets_use_comp6() {
        let spans = vec![EmphasisSpan::new(EmphasisSpan::COMPUTER_BRAILLE, 1..4)];
        let constrainers = Constrainers::new(vec![Constrainer::ComputerBraille(
            ComputerBrailleConstrainer {},
        )]);
        let c = constrainers.precompute("hello", &spans, &TranslationOptions::default());
        assert_eq!(use_comp6(&c), vec![false, true, true, true, false]);
        assert_eq!(dont_contract(&c), vec![false, false, false, false, false]);
    }

    fn prefer_digit(c: &PositionConstraints) -> Vec<bool> {
        c.0.iter()
            .map(|f| f.contains(Constraint::PreferDigit))
            .collect()
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
        let c = backward_nocontract_constrainers(builder).precompute(
            "Lab cLc",
            &[],
            &TranslationOptions::default(),
        );
        assert_eq!(
            no_contract(&c),
            vec![false, true, true, false, false, false, true]
        );
    }

    fn no_whole_word(c: &PositionConstraints) -> Vec<bool> {
        c.0.iter()
            .map(|f| f.contains(Constraint::NoWholeWord))
            .collect()
    }

    #[test]
    fn letsign_lapses_until_the_next_space() {
        // UEB "c.;" is ⠰⠉⠲⠆: past the full stop the letter sign has lapsed, which still
        // keeps the wordsign "be" off the ⠆ until the word ends
        let mut builder = BackwardNoContractConstrainerBuilder::new();
        builder.indicator("L");
        builder.letter_cells(HashSet::from(['a', 'b']));
        builder.space_cells(HashSet::from([' ']));
        let c = backward_nocontract_constrainers(builder).precompute(
            "La.b b",
            &[],
            &TranslationOptions::default(),
        );
        assert_eq!(
            no_contract(&c),
            vec![false, true, false, false, false, false]
        );
        assert_eq!(
            no_whole_word(&c),
            vec![false, false, true, true, false, false]
        );
    }

    #[test]
    fn a_longer_rule_takes_the_indicator_dots() {
        // Danish `always j 56-245`: ⠰⠚ is the letter, not a letter sign and "jeg"
        let mut builder = BackwardNoContractConstrainerBuilder::new();
        builder.indicator("L");
        builder.rule_dots("Lb");
        builder.letter_cells(HashSet::from(['a', 'b']));
        builder.space_cells(HashSet::from([' ']));
        let c = backward_nocontract_constrainers(builder).precompute(
            "Lb-b La-b",
            &[],
            &TranslationOptions::default(),
        );
        assert_eq!(
            no_contract(&c),
            vec![false, false, false, false, false, false, true, false, false]
        );
        assert_eq!(
            no_whole_word(&c),
            vec![false, false, false, false, false, false, false, true, true]
        );
    }

    #[test]
    fn a_rule_inside_a_word_takes_the_indicator_dots_only_after_a_letter() {
        // UEB "ness" is ⠰⠎ after a letter, but ⠰⠎ on its own is the letter s
        let mut builder = BackwardNoContractConstrainerBuilder::new();
        builder.indicator("L");
        builder.inword_rule_dots("Lb");
        builder.letter_cells(HashSet::from(['a', 'b']));
        builder.space_cells(HashSet::from([' ']));
        let c = backward_nocontract_constrainers(builder).precompute(
            "aLb-a Lb",
            &[],
            &TranslationOptions::default(),
        );
        assert_eq!(
            no_contract(&c),
            vec![false, false, false, false, false, false, false, true]
        );
        assert_eq!(no_whole_word(&c), vec![false; 8]);
    }

    #[test]
    fn letsign_carries_across_capital_indicators() {
        // UEB writes a capital letter standing alone in capitals as ⠰⠠⠠⠠⠅ ("K."), with
        // the grade 1 sign before the capitals passage indicator
        let mut builder = BackwardNoContractConstrainerBuilder::new();
        builder.indicator("L");
        builder.caps_indicator("C");
        builder.caps_indicator("CC");
        builder.letter_cells(HashSet::from(['a', 'b']));
        let c = backward_nocontract_constrainers(builder).precompute(
            "LCCCa b",
            &[],
            &TranslationOptions::default(),
        );
        assert_eq!(
            no_contract(&c),
            vec![false, false, false, false, true, false, false]
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
        let c = backward_nocontract_constrainers(builder).precompute(
            "NNa",
            &[],
            &TranslationOptions::default(),
        );
        assert_eq!(no_contract(&c), vec![false, false, true]);
    }

    #[test]
    fn syllables_are_marked_where_they_start() {
        let mut builder = TypeConstrainerBuilder::new();
        builder.syllable("hood");
        builder.syllable("ho");
        let c = Constrainers::new(vec![Constrainer::Type(builder.build())]).precompute(
            "tHoodho",
            &[],
            &TranslationOptions::default(),
        );
        let marked: Vec<bool> =
            c.0.iter()
                .map(|f| f.contains(Constraint::TypeStart))
                .collect();
        // the longest syllable, case-insensitively, and on after it
        assert_eq!(marked, vec![false, true, false, false, false, true, false]);
    }

    #[test]
    fn emphasis_and_capitals_are_marked_where_they_start() {
        let mut builder = TypeConstrainerBuilder::new();
        builder.capsletter();
        let spans = [EmphasisSpan::new("italic", 4..6)];
        let c = Constrainers::new(vec![Constrainer::Type(builder.build())]).precompute(
            "aBCdef",
            &spans,
            &TranslationOptions::default(),
        );
        let starts: Vec<bool> =
            c.0.iter()
                .map(|f| f.contains(Constraint::TypeStart))
                .collect();
        let capitals: Vec<bool> =
            c.0.iter()
                .map(|f| f.contains(Constraint::Capital))
                .collect();
        // the run of capitals "BC" starts at 1, the emphasis at 4
        assert_eq!(starts, vec![false, true, false, false, true, false]);
        assert_eq!(capitals, vec![false, true, true, false, false, false]);
    }

    #[test]
    fn nocont_marks_the_whole_word() {
        let mut builder = NoContConstrainerBuilder::new();
        builder.nocont("c@");
        builder.space_characters(HashSet::from([' ']));
        let c = Constrainers::new(vec![Constrainer::NoCont(builder.build().unwrap())]).precompute(
            "ab C@D e",
            &[],
            &TranslationOptions::default(),
        );
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
            .precompute("aABa Aa", &[], &TranslationOptions::default());
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
        let c = Constrainers::new(vec![Constrainer::Repword(builder.build().unwrap())]).precompute(
            input,
            &[],
            &TranslationOptions::default(),
        );
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
        let c = Constrainers::new(vec![Constrainer::Repword(builder.build().unwrap())]).precompute(
            "abc-abc-abc-abx",
            &[],
            &TranslationOptions::default(),
        );
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
        let c = Constrainers::new(vec![Constrainer::Join(builder.build().unwrap())]).precompute(
            "a  a . a",
            &[],
            &TranslationOptions::default(),
        );
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
        let c = Constrainers::new(vec![Constrainer::Join(builder.build().unwrap())]).precompute(
            "$  1 a $ 1",
            &[],
            &TranslationOptions::default(),
        );
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
        let c = backward_numeric_constrainers(builder).precompute(
            "N12a1",
            &[],
            &TranslationOptions::default(),
        );
        assert_eq!(prefer_digit(&c), vec![false, true, true, false, false]);
    }

    #[test]
    fn nonumsign_ends_the_run_explicitly() {
        let mut builder = BackwardNumericConstrainerBuilder::new();
        builder.numsign("N");
        builder.nonumsign("X");
        builder.digit_cells(HashSet::from(['1', '2']));
        let c = backward_numeric_constrainers(builder).precompute(
            "N1X2",
            &[],
            &TranslationOptions::default(),
        );
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
        builder.letter_cells(HashSet::from(['x', 'N']));
        builder.after_letter("N");
        let constrainers = backward_numeric_constrainers(builder);
        let c = constrainers.precompute("xN12", &[], &TranslationOptions::default());
        assert_eq!(prefer_digit(&c), vec![false, false, false, false]);
        // at the start of a word the numsign still starts a number
        let c = constrainers.precompute(" N12", &[], &TranslationOptions::default());
        assert_eq!(prefer_digit(&c), vec![false, false, true, true]);
    }

    #[test]
    fn numsign_inside_a_longer_rule_starts_no_number() {
        // as in sk-g1.ctb, where `context [@5-46-3456] "#"` takes the numsign
        let mut builder = BackwardNumericConstrainerBuilder::new();
        builder.numsign("N");
        builder.digit_cells(HashSet::from(['1', '2']));
        builder.letter_cells(HashSet::from(['x']));
        builder.rule_dots("xN");
        let c = backward_numeric_constrainers(builder).precompute(
            "xN12",
            &[],
            &TranslationOptions::default(),
        );
        assert_eq!(prefer_digit(&c), vec![false, false, false, false]);
    }

    #[test]
    fn numsign_after_a_letter_cell_starts_a_number_without_an_after_letter_rule() {
        // Hungarian "A1-ben": no rule claims the numsign's dots after a letter, so it
        // starts a number even right after one.
        let mut builder = BackwardNumericConstrainerBuilder::new();
        builder.numsign("N");
        builder.digit_cells(HashSet::from(['1', '2']));
        builder.letter_cells(HashSet::from(['x']));
        let c = backward_numeric_constrainers(builder).precompute(
            "xN12",
            &[],
            &TranslationOptions::default(),
        );
        assert_eq!(prefer_digit(&c), vec![false, false, true, true]);
    }

    #[test]
    fn cursor_word() {
        let mut classes = CharacterClasses::default();
        classes.insert(crate::parser::CharacterClass::Space, ' ');
        let constrainers =
            Constrainers::new(vec![Constrainer::CursorWord(CursorWordConstrainer {
                character_classes: classes,
            })]);
        let options = TranslationOptions::default()
            .with_modes([TranslationMode::CompbrlAtCursor].into_iter().collect())
            .with_cursor_position(5);
        let c = constrainers.precompute("ab  cde f", &[], &options);
        let flags = |pos| c.0[pos];
        assert!(flags(4).is_superset(Constraint::CursorWord | Constraint::UseComp6));
        assert!(flags(6).contains(Constraint::CursorWord));
        assert!(!flags(7).contains(Constraint::CursorWord));
        assert!(flags(2).contains(Constraint::SpaceBeforeCursorWord));
        assert!(flags(3).contains(Constraint::SpaceBeforeCursorWord));
        assert!(c.reaches_into_cursor_word(1, 4));
        assert!(!c.reaches_into_cursor_word(1, 3));
        assert!(c.joins_cursor_word_at(2));
        assert!(!c.joins_cursor_word_at(1));
        // without the mode, there is no word at the cursor
        let c = constrainers.precompute(
            "ab  cde f",
            &[],
            &options.with_modes(TranslationModes::empty()),
        );
        assert!(!(0..9).any(|pos| c.in_cursor_word_at(pos)));
    }

    #[test]
    fn caps_passage_end_only_inside_a_passage() {
        // no-no-g0.utb: `endcapsphrase after 6` has the cells of `capsletter 6`
        let mut c = BackwardCapsPassageConstrainer::default();
        c.begin("⠠⠠⠠");
        c.end("⠠");
        let ends: Vec<bool> = c
            .build()
            .unwrap()
            .compute("⠠⠁ ⠠⠠⠠⠁ ⠃⠠ ⠠⠉")
            .iter()
            .map(|f| f.contains(Constraint::CapsPassageEnd))
            .collect();
        let at: Vec<usize> = (0..ends.len()).filter(|&i| ends[i]).collect();
        assert_eq!(at, vec![9]);
    }
}
