//! Mapping between input positions and output positions of a translation

use crate::translator::ResolvedTranslation;

pub(crate) fn compute_output_positions(translations: &[ResolvedTranslation]) -> Vec<usize> {
    let mut output_positions: Vec<usize> = Vec::new();
    let mut output_offset: usize = 0;
    // An inserted, indicator-only unit (empty input) has no input char of its own to record a
    // position for. liblouis attributes its output to the next real input char instead of
    // skipping past it, so remember where such a run started until that char shows up.
    let mut pending_insertion_start: Option<usize> = None;

    for translation in translations {
        let input_len = translation.input().chars().count();
        let output_len = translation.output().chars().count();

        if input_len == 0 {
            pending_insertion_start.get_or_insert(output_offset);
        } else {
            let first = pending_insertion_start.take().unwrap_or(output_offset);
            output_positions.push(first);
            for i in 1..input_len {
                output_positions.push(output_offset + i.min(output_len.saturating_sub(1)));
            }
        }
        output_offset += output_len;
    }
    output_positions
}

pub(crate) fn compute_input_positions(translations: &[ResolvedTranslation]) -> Vec<usize> {
    let mut input_positions: Vec<usize> = Vec::new();
    let mut input_offset: usize = 0;

    for translation in translations {
        let input_len = translation.input().chars().count();
        let output_len = translation.output().chars().count();

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
    pub fn from_trace(input_len: usize, stages: &[Vec<ResolvedTranslation>]) -> Self {
        // an index equal to the length of the text it points into means "past the end"
        let mut output_positions: Vec<usize> = (0..input_len).collect();
        let mut input_positions: Vec<usize> = (0..input_len).collect();
        let mut output_len = input_len;
        for steps in stages {
            let stage_output_positions = compute_output_positions(steps);
            let stage_input_positions = compute_input_positions(steps);
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

    #[test]
    fn output_positions() {
        assert_eq!(
            compute_output_positions(&[translation("abc", "⠁⠃⠇")]),
            [0, 1, 2]
        );
        assert_eq!(
            compute_output_positions(&[translation("foo", "⠁")]),
            [0, 0, 0]
        );
        assert_eq!(
            compute_output_positions(&[translation("foo", "⠁⠃")]),
            [0, 1, 1]
        );
        assert_eq!(compute_output_positions(&[translation("a", "⠁⠃⠇")]), [0]);
        assert_eq!(
            compute_output_positions(&[translation("abc", "⠁⠃⠇"), translation("abc", "⠁⠃⠇")]),
            [0, 1, 2, 3, 4, 5]
        );
        assert_eq!(
            compute_output_positions(&[translation("foo", "⠁"), translation("abc", "⠁⠃⠇")]),
            [0, 0, 0, 1, 2, 3]
        );
        assert_eq!(
            compute_output_positions(&[translation("a", "⠁⠃⠇"), translation("abc", "⠁⠃⠇")]),
            [0, 3, 4, 5]
        );
    }

    #[test]
    fn input_positions() {
        assert_eq!(
            compute_input_positions(&[translation("abc", "⠁⠃⠇")]),
            [0, 1, 2]
        );
        assert_eq!(compute_input_positions(&[translation("foo", "⠁")]), [0]);
        assert_eq!(compute_input_positions(&[translation("foo", "⠁⠃")]), [0, 1]);
        assert_eq!(
            compute_input_positions(&[translation("a", "⠁⠃⠇")]),
            [0, 0, 0]
        );
        assert_eq!(
            compute_input_positions(&[translation("abc", "⠁⠃⠇"), translation("abc", "⠁⠃⠇")]),
            [0, 1, 2, 3, 4, 5]
        );
        assert_eq!(
            compute_input_positions(&[translation("foo", "⠁"), translation("abc", "⠁⠃⠇")]),
            [0, 3, 4, 5]
        );
        assert_eq!(
            compute_input_positions(&[translation("a", "⠁⠃⠇"), translation("abc", "⠁⠃⠇")]),
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
        let map = PositionMap::from_trace(2, &[stage(&[("a", "⠁"), ("b", "⠃")])]);
        assert_eq!(map.output_positions(), [0, 1]);
        assert_eq!(map.input_positions(), [0, 1]);
        assert_eq!(map.cursor(0), 0);
        assert_eq!(map.cursor(1), 1);
    }

    #[test]
    fn map_one_to_many() {
        let map = PositionMap::from_trace(1, &[stage(&[("a", "⠁⠃")])]);
        assert_eq!(map.output_positions(), [0]);
        assert_eq!(map.input_positions(), [0, 0]);
    }

    #[test]
    fn map_many_to_one() {
        let map = PositionMap::from_trace(3, &[stage(&[("foo", "⠁")])]);
        assert_eq!(map.output_positions(), [0, 0, 0]);
        assert_eq!(map.input_positions(), [0]);
    }

    #[test]
    fn map_leading_indicator() {
        let map = PositionMap::from_trace(1, &[stage(&[("", "⠠"), ("h", "⠓")])]);
        assert_eq!(map.output_positions(), [0]);
        assert_eq!(map.input_positions(), [0, 0]);
        assert_eq!(map.cursor(0), 0);
    }

    #[test]
    fn map_trailing_indicator_clamps_to_last_input() {
        let map = PositionMap::from_trace(1, &[stage(&[("h", "⠓"), ("", "⠠")])]);
        assert_eq!(map.output_positions(), [0]);
        assert_eq!(map.input_positions(), [0, 0]);
    }

    #[test]
    fn map_deletion_in_the_middle() {
        let map = PositionMap::from_trace(
            4,
            &[stage(&[("f", "⠋"), (",", "⠠"), (".", ""), ("o", "⠕")])],
        );
        assert_eq!(map.output_positions(), [0, 1, 2, 2]);
        assert_eq!(map.input_positions(), [0, 1, 3]);
    }

    #[test]
    fn map_deletion_at_the_end_clamps_to_last_output() {
        let map = PositionMap::from_trace(2, &[stage(&[("f", "⠋"), ("o", "")])]);
        assert_eq!(map.output_positions(), [0, 0]);
        assert_eq!(map.input_positions(), [0]);
    }

    #[test]
    fn map_everything_deleted() {
        let map = PositionMap::from_trace(1, &[stage(&[("f", "")])]);
        assert_eq!(map.output_positions(), [0]);
        assert_eq!(map.input_positions(), []);
        assert_eq!(map.cursor(0), 0);
        assert_eq!(map.cursor(1), 0);
    }

    #[test]
    fn map_empty_input() {
        let map = PositionMap::from_trace(0, &[stage(&[])]);
        assert_eq!(map.output_positions(), []);
        assert_eq!(map.input_positions(), []);
        assert_eq!(map.cursor(0), 0);
    }

    #[test]
    fn map_without_stages_is_the_identity() {
        let map = PositionMap::from_trace(2, &[]);
        assert_eq!(map.output_positions(), [0, 1]);
        assert_eq!(map.input_positions(), [0, 1]);
        assert_eq!(map.cursor(2), 2);
    }

    #[test]
    fn map_cursor_past_the_end_maps_to_output_length() {
        let map = PositionMap::from_trace(1, &[stage(&[("", "⠠"), ("h", "⠓")])]);
        assert_eq!(map.cursor(1), 2);
        assert_eq!(map.cursor(7), 2);
    }

    #[test]
    fn map_carries_past_the_end_across_stages() {
        let map = PositionMap::from_trace(
            2,
            &[
                stage(&[("a", "⠁"), ("b", "")]),
                stage(&[("⠁", "x"), ("", "!")]),
            ],
        );
        assert_eq!(map.output_positions(), [0, 1]);
        assert_eq!(map.input_positions(), [0, 1]);
        assert_eq!(map.cursor(2), 2);
    }

    #[test]
    fn map_composes_two_stages() {
        // input "aaabcdefg", after the correction stage "abcddefg", after the main stage "xcddwg"
        let map = PositionMap::from_trace(
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
        let map = PositionMap::from_trace(
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
        let map = PositionMap::from_trace(2, &[stage(&[("⠠", ""), ("⠓", "H")])]);
        assert_eq!(map.output_positions(), [0, 0]);
        assert_eq!(map.input_positions(), [1]);
    }

    #[test]
    fn map_into_parts() {
        let map = PositionMap::from_trace(1, &[stage(&[("a", "⠁⠃")])]);
        assert_eq!(map.into_parts(), (vec![0], vec![0, 0]));
    }
}
