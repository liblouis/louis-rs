//! Mapping between input positions and output positions of a translation

use crate::{parser::Direction, translator::ResolvedTranslation};

/// The (input length, output length) of every translation step of one stage -- all the position
/// algorithms need.
fn stage_lengths(steps: &[ResolvedTranslation], direction: Direction) -> Vec<(usize, usize)> {
    steps
        .iter()
        .map(|step| {
            let (input, output) = (step.input().chars().count(), step.output().chars().count());
            match direction {
                Direction::Forward => (input, output),
                Direction::Backward => (output, input),
            }
        })
        .collect()
}

pub(crate) fn compute_output_positions(translations: &[(usize, usize)]) -> Vec<usize> {
    let mut output_positions: Vec<usize> = Vec::new();
    let mut output_offset: usize = 0;
    // An inserted, indicator-only unit (empty input) has no input char of its own to record a
    // position for. liblouis attributes its output to the next real input char instead of
    // skipping past it, so remember where such a run started until that char shows up.
    let mut pending_insertion_start: Option<usize> = None;

    for &(input_len, output_len) in translations {
        if input_len == 0 {
            pending_insertion_start.get_or_insert(output_offset);
        } else if output_len == 0 {
            // A deleted unit produces no output of its own, so liblouis leaves its input chars
            // pointing at the last cell already emitted -- the *preceding* character's -- rather
            // than at the next one. At the very start of the output there is no preceding cell,
            // and everything clamps to 0.
            let position = pending_insertion_start
                .take()
                .unwrap_or_else(|| output_offset.saturating_sub(1));
            output_positions.extend(std::iter::repeat_n(position, input_len));
        } else {
            let first = pending_insertion_start.take().unwrap_or(output_offset);
            output_positions.push(first);
            for i in 1..input_len {
                output_positions.push(output_offset + i.min(output_len - 1));
            }
        }
        output_offset += output_len;
    }
    output_positions
}

pub(crate) fn compute_input_positions(translations: &[(usize, usize)]) -> Vec<usize> {
    let mut input_positions: Vec<usize> = Vec::new();
    let mut input_offset: usize = 0;

    for &(input_len, output_len) in translations {
        for i in 0..output_len {
            input_positions.push(input_offset + i.min(input_len.saturating_sub(1)));
        }
        input_offset += input_len;
    }
    input_positions
}

/// Where every input char ends up in the output and where every output char comes from,
/// composed over all translation stages.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct PositionMap {
    /// The output position of every input char
    output_positions: Vec<usize>,
    /// The input position of every output char
    input_positions: Vec<usize>,
    input_len: usize,
    output_len: usize,
}

impl PositionMap {
    /// Compose the positions of `stages`, the translation steps of every stage in execution
    /// order, into a map between the original input of `input_len` chars and the final output.
    ///
    /// liblouis keeps one position mapping between the braille and the text, and only ever fills
    /// its gaps -- a character the translation consumed without producing anything -- on the
    /// *text* side. Forward that is the pipeline's input, so [`compute_output_positions`] (which
    /// fills gaps) computes the input-indexed array and [`compute_input_positions`] the
    /// output-indexed one. Backward the text is the pipeline's *output*, so the two swap roles:
    /// feeding either one a stage whose lengths are exchanged makes it produce the other array.
    pub fn from_trace(
        input_len: usize,
        stages: &[Vec<ResolvedTranslation>],
        direction: Direction,
    ) -> Self {
        // an index equal to the length of the text it points into means "past the end"
        let mut output_positions: Vec<usize> = (0..input_len).collect();
        let mut input_positions: Vec<usize> = (0..input_len).collect();
        let mut output_len = input_len;
        for steps in stages {
            let lengths = stage_lengths(steps, direction);
            let (stage_output_positions, stage_input_positions) = match direction {
                Direction::Forward => (
                    compute_output_positions(&lengths),
                    compute_input_positions(&lengths),
                ),
                Direction::Backward => (
                    compute_input_positions(&lengths),
                    compute_output_positions(&lengths),
                ),
            };
            let stage_output_len = stage_input_positions.len();
            debug_assert_eq!(stage_output_positions.len(), output_len);
            output_positions = output_positions
                .iter()
                .map(|&p| {
                    stage_output_positions
                        .get(p)
                        .copied()
                        .unwrap_or(stage_output_len)
                })
                .collect();
            input_positions = stage_input_positions
                .iter()
                .map(|&p| input_positions.get(p).copied().unwrap_or(input_len))
                .collect();
            output_len = stage_output_len;
        }
        clamp_to_last(&mut output_positions, output_len);
        clamp_to_last(&mut input_positions, input_len);
        Self {
            output_positions,
            input_positions,
            input_len,
            output_len,
        }
    }

    pub fn output_positions(&self) -> &[usize] {
        &self.output_positions
    }

    pub fn input_positions(&self) -> &[usize] {
        &self.input_positions
    }

    /// The output position of a cursor at input position `cursor`. A cursor past the end of the
    /// input ends up past the end of the output.
    pub fn cursor(&self, cursor: usize) -> usize {
        self.output_positions
            .get(cursor)
            .copied()
            .unwrap_or(self.output_len)
    }

    /// Split into `(output_positions, input_positions)`
    pub fn into_parts(self) -> (Vec<usize>, Vec<usize>) {
        (self.output_positions, self.input_positions)
    }
}

fn clamp_to_last(positions: &mut [usize], len: usize) {
    let last = len.saturating_sub(1);
    for position in positions {
        *position = (*position).min(last);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::translator::TranslationStage;

    fn translation(input: &str, output: &str) -> ResolvedTranslation {
        ResolvedTranslation::new(input, output, 0, TranslationStage::Main, None)
    }

    /// The (input length, output length) pair the position algorithms actually consume.
    fn align(input: &str, output: &str) -> (usize, usize) {
        (input.chars().count(), output.chars().count())
    }

    fn forward_map(input_len: usize, stages: &[Vec<ResolvedTranslation>]) -> PositionMap {
        PositionMap::from_trace(input_len, stages, Direction::Forward)
    }

    fn backward_map(input_len: usize, stages: &[Vec<ResolvedTranslation>]) -> PositionMap {
        PositionMap::from_trace(input_len, stages, Direction::Backward)
    }

    #[test]
    fn output_positions() {
        assert_eq!(compute_output_positions(&[align("abc", "⠁⠃⠇")]), [0, 1, 2]);
        assert_eq!(compute_output_positions(&[align("foo", "⠁")]), [0, 0, 0]);
        assert_eq!(compute_output_positions(&[align("foo", "⠁⠃")]), [0, 1, 1]);
        assert_eq!(compute_output_positions(&[align("a", "⠁⠃⠇")]), [0]);
        assert_eq!(
            compute_output_positions(&[align("abc", "⠁⠃⠇"), align("abc", "⠁⠃⠇")]),
            [0, 1, 2, 3, 4, 5]
        );
        assert_eq!(
            compute_output_positions(&[align("foo", "⠁"), align("abc", "⠁⠃⠇")]),
            [0, 0, 0, 1, 2, 3]
        );
        assert_eq!(
            compute_output_positions(&[align("a", "⠁⠃⠇"), align("abc", "⠁⠃⠇")]),
            [0, 3, 4, 5]
        );
    }

    #[test]
    fn input_positions() {
        assert_eq!(compute_input_positions(&[align("abc", "⠁⠃⠇")]), [0, 1, 2]);
        assert_eq!(compute_input_positions(&[align("foo", "⠁")]), [0]);
        assert_eq!(compute_input_positions(&[align("foo", "⠁⠃")]), [0, 1]);
        assert_eq!(compute_input_positions(&[align("a", "⠁⠃⠇")]), [0, 0, 0]);
        assert_eq!(
            compute_input_positions(&[align("abc", "⠁⠃⠇"), align("abc", "⠁⠃⠇")]),
            [0, 1, 2, 3, 4, 5]
        );
        assert_eq!(
            compute_input_positions(&[align("foo", "⠁"), align("abc", "⠁⠃⠇")]),
            [0, 3, 4, 5]
        );
        assert_eq!(
            compute_input_positions(&[align("a", "⠁⠃⠇"), align("abc", "⠁⠃⠇")]),
            [0, 0, 0, 1, 2, 3]
        );
    }

    fn stage(steps: &[(&str, &str)]) -> Vec<ResolvedTranslation> {
        steps
            .iter()
            .map(|(input, output)| translation(input, output))
            .collect()
    }

    #[test]
    fn map_identity() {
        let map = forward_map(2, &[stage(&[("a", "⠁"), ("b", "⠃")])]);
        assert_eq!(map.output_positions(), [0, 1]);
        assert_eq!(map.input_positions(), [0, 1]);
        assert_eq!(map.cursor(0), 0);
        assert_eq!(map.cursor(1), 1);
    }

    #[test]
    fn map_one_to_many() {
        let map = forward_map(1, &[stage(&[("a", "⠁⠃")])]);
        assert_eq!(map.output_positions(), [0]);
        assert_eq!(map.input_positions(), [0, 0]);
    }

    #[test]
    fn map_many_to_one() {
        let map = forward_map(3, &[stage(&[("foo", "⠁")])]);
        assert_eq!(map.output_positions(), [0, 0, 0]);
        assert_eq!(map.input_positions(), [0]);
    }

    #[test]
    fn map_leading_indicator() {
        let map = forward_map(1, &[stage(&[("", "⠠"), ("h", "⠓")])]);
        assert_eq!(map.output_positions(), [0]);
        assert_eq!(map.input_positions(), [0, 0]);
        assert_eq!(map.cursor(0), 0);
    }

    #[test]
    fn map_trailing_indicator_clamps_to_last_input() {
        let map = forward_map(1, &[stage(&[("h", "⠓"), ("", "⠠")])]);
        assert_eq!(map.output_positions(), [0]);
        assert_eq!(map.input_positions(), [0, 0]);
    }

    #[test]
    fn map_deletion_in_the_middle() {
        let map = forward_map(
            4,
            &[stage(&[("f", "⠋"), (",", "⠠"), (".", ""), ("o", "⠕")])],
        );
        // the deleted "." shares the preceding ","'s cell, as liblouis does
        assert_eq!(map.output_positions(), [0, 1, 1, 2]);
        assert_eq!(map.input_positions(), [0, 1, 3]);
    }

    #[test]
    fn map_deletion_at_the_end_clamps_to_last_output() {
        let map = forward_map(2, &[stage(&[("f", "⠋"), ("o", "")])]);
        assert_eq!(map.output_positions(), [0, 0]);
        assert_eq!(map.input_positions(), [0]);
    }

    /// liblouis fills the gaps of its position mapping on the text side, which is the pipeline's
    /// input going forward but its output going backward. A consumed character is therefore
    /// "appended to the previous" one forward and "prepended to the next" one backward -- see the
    /// two same-named cases in liblouis' `inpos_outpos.yaml`.
    #[test]
    fn map_a_consumed_character_leans_the_other_way_backward() {
        let consumed_then_kept = [stage(&[("⠠", ""), ("⠓", "H")])];

        let forward = forward_map(2, &consumed_then_kept);
        assert_eq!(forward.output_positions(), [0, 0]);
        assert_eq!(forward.input_positions(), [1]);

        let backward = backward_map(2, &consumed_then_kept);
        assert_eq!(backward.output_positions(), [0, 0]);
        // the "H" covers the consumed cell as well, rather than only its own
        assert_eq!(backward.input_positions(), [0]);
    }

    #[test]
    fn map_everything_deleted() {
        let map = forward_map(1, &[stage(&[("f", "")])]);
        assert_eq!(map.output_positions(), [0]);
        assert_eq!(map.input_positions(), []);
        assert_eq!(map.cursor(0), 0);
        assert_eq!(map.cursor(1), 0);
    }

    #[test]
    fn map_empty_input() {
        let map = forward_map(0, &[stage(&[])]);
        assert_eq!(map.output_positions(), []);
        assert_eq!(map.input_positions(), []);
        assert_eq!(map.cursor(0), 0);
    }

    #[test]
    fn map_without_stages_is_the_identity() {
        let map = forward_map(2, &[]);
        assert_eq!(map.output_positions(), [0, 1]);
        assert_eq!(map.input_positions(), [0, 1]);
        assert_eq!(map.cursor(2), 2);
    }

    #[test]
    fn map_cursor_past_the_end_maps_to_output_length() {
        let map = forward_map(1, &[stage(&[("", "⠠"), ("h", "⠓")])]);
        assert_eq!(map.cursor(1), 2);
        assert_eq!(map.cursor(7), 2);
    }

    #[test]
    fn map_composes_a_deletion_and_an_insertion_across_stages() {
        let map = forward_map(
            2,
            &[
                stage(&[("a", "⠁"), ("b", "")]),
                stage(&[("⠁", "x"), ("", "!")]),
            ],
        );
        // "b" is deleted, so it collapses onto the preceding "a"'s cell
        assert_eq!(map.output_positions(), [0, 0]);
        assert_eq!(map.input_positions(), [0, 1]);
        // a cursor past the end of the input is still carried past the end of the output
        assert_eq!(map.cursor(2), 2);
    }

    #[test]
    fn map_composes_two_stages() {
        // input "aaabcdefg", after the correction stage "abcddefg", after the main stage "xcddwg"
        let map = forward_map(
            9,
            &[
                stage(&[
                    ("aaa", "a"),
                    ("b", "b"),
                    ("c", "c"),
                    ("d", "dd"),
                    ("e", "e"),
                    ("f", "f"),
                    ("g", "g"),
                ]),
                stage(&[
                    ("ab", "x"),
                    ("c", "c"),
                    ("d", "d"),
                    ("d", "d"),
                    ("ef", "w"),
                    ("g", "g"),
                ]),
            ],
        );
        assert_eq!(map.input_positions(), [0, 4, 5, 5, 6, 8]);
        assert_eq!(map.output_positions(), [0, 0, 0, 0, 1, 2, 4, 4, 5]);
    }

    #[test]
    fn map_insertion_prepends_to_the_next_char_across_stages() {
        // "f,oobar" -> correct-stage inserts "-" before the first "o" -> "f,-oobar" ->
        // main-stage translation. liblouis attributes the inserted "-" cell to the "o"
        // that triggered the insertion rather than to the "," before it.
        let map = forward_map(
            4,
            &[
                stage(&[("f", "f"), (",", ","), ("", "-"), ("o", "o"), ("o", "o")]),
                stage(&[("f", "⠋"), (",", "⠠"), ("-", "⠤"), ("o", "⠕"), ("o", "⠕")]),
            ],
        );
        assert_eq!(map.output_positions(), [0, 1, 2, 4]);
    }

    #[test]
    fn map_consumed_indicator() {
        let map = forward_map(2, &[stage(&[("⠠", ""), ("⠓", "H")])]);
        assert_eq!(map.output_positions(), [0, 0]);
        assert_eq!(map.input_positions(), [1]);
    }

    #[test]
    fn map_into_parts() {
        let map = forward_map(1, &[stage(&[("a", "⠁⠃")])]);
        assert_eq!(map.into_parts(), (vec![0], vec![0, 0]));
    }
}
