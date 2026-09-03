use crate::{
    Direction,
    parser::{AnchoredRule, HasDirection, Rule},
    translator::{
        DisplayTable, PositionMap, ResolvedTranslation, TranslationError, TranslationOptions,
        TranslationStage,
        table::{TableContext, multipass::MultipassTable, primary::PrimaryTable},
    },
};

#[derive(Debug)]
pub enum Transformation {
    Pre(MultipassTable),
    Primary(PrimaryTable),
    Post(MultipassTable),
    Display(DisplayTable),
}

impl Transformation {
    pub fn trace(&self, input: &str, options: &TranslationOptions) -> Vec<ResolvedTranslation> {
        match self {
            Transformation::Pre(t) => t.trace(input),
            Transformation::Primary(t) => t.trace(input, options),
            Transformation::Post(t) => t.trace(input),
            Transformation::Display(t) => t.trace(input),
        }
    }

    fn translate(&self, input: &str) -> String {
        match self {
            Transformation::Pre(t) => t.translate(input),
            Transformation::Primary(t) => t.translate(input),
            Transformation::Post(t) => t.translate(input),
            Transformation::Display(t) => t.translate(input),
        }
    }

    fn translate_with_options(&self, input: &str, options: &TranslationOptions) -> String {
        match self {
            Transformation::Pre(t) => t.translate(input),
            Transformation::Primary(t) => t.translate_with_options(input, options),
            Transformation::Post(t) => t.translate(input),
            Transformation::Display(t) => t.translate(input),
        }
    }
}

#[derive(Debug)]
pub struct TranslationPipeline {
    steps: Vec<Transformation>,
    direction: Direction,
}

impl TranslationPipeline {
    pub fn compile(rules: &[AnchoredRule], direction: Direction) -> Result<Self, TranslationError> {
        let ctx = TableContext::compile(rules)?;
        let mut steps = Vec::new();

        // ignore rules that aren't meant for the given direction
        let rules: Vec<_> = rules
            .iter()
            .filter(|r| r.is_direction(direction))
            .cloned()
            .collect();

        let correct_rules: Vec<AnchoredRule> = rules
            .iter()
            .filter(|r| matches!(r.rule, Rule::Correct { .. }))
            .cloned()
            .collect();
        if !correct_rules.is_empty() {
            let transform =
                MultipassTable::compile(&correct_rules, direction, TranslationStage::Pre, &ctx)?;
            steps.push(Transformation::Pre(transform));
        }
        let context = TableContext::compile(rules.as_slice())?;
        let transform = PrimaryTable::compile(
            rules.as_slice(),
            direction,
            TranslationStage::Main,
            &context,
        )?;
        steps.push(Transformation::Primary(transform));
        let pass2_rules: Vec<AnchoredRule> = rules
            .iter()
            .filter(|r| matches!(r.rule, Rule::Pass2 { .. }))
            .cloned()
            .collect();
        if !pass2_rules.is_empty() {
            let transform =
                MultipassTable::compile(&pass2_rules, direction, TranslationStage::Post1, &ctx)?;
            steps.push(Transformation::Post(transform));
        }
        let pass3_rules: Vec<AnchoredRule> = rules
            .iter()
            .filter(|r| matches!(r.rule, Rule::Pass3 { .. }))
            .cloned()
            .collect();
        if !pass3_rules.is_empty() {
            let transform =
                MultipassTable::compile(&pass3_rules, direction, TranslationStage::Post2, &ctx)?;
            steps.push(Transformation::Post(transform));
        }
        let pass4_rules: Vec<AnchoredRule> = rules
            .iter()
            .filter(|r| matches!(r.rule, Rule::Pass4 { .. }))
            .cloned()
            .collect();
        if !pass4_rules.is_empty() {
            let transform =
                MultipassTable::compile(&pass4_rules, direction, TranslationStage::Post3, &ctx)?;
            steps.push(Transformation::Post(transform));
        }
        match direction {
            Direction::Forward => Ok(Self { steps, direction }),
            Direction::Backward => Ok(Self {
                steps: steps.into_iter().rev().collect(),
                direction,
            }),
        }
    }

    /// Add `display` as the outermost stage, mapping the braille side of the pipeline.
    ///
    /// The display table is supplied rather than compiled from the same rules because the
    /// caller chooses it independently of the translation table: the YAML harness takes it
    /// from a test's `display:` key, `louis translate` from `--display`.
    pub fn with_display(mut self, display: DisplayTable) -> Self {
        let step = Transformation::Display(display);
        match self.direction {
            // translating to braille, so the display table maps what we produced: it runs last
            Direction::Forward => self.steps.push(step),
            // translating from braille, so it maps what we are about to read: it runs first.
            // `compile` has already reversed `steps`, so pushing would put it at the wrong end.
            Direction::Backward => self.steps.insert(0, step),
        }
        self
    }

    pub fn trace(&self, input: &str) -> Vec<Vec<ResolvedTranslation>> {
        self.trace_with_options(input, &TranslationOptions::default())
    }

    pub fn trace_with_options(
        &self,
        input: &str,
        options: &TranslationOptions,
    ) -> Vec<Vec<ResolvedTranslation>> {
        let mut input = input.to_string();
        let mut result: Vec<Vec<ResolvedTranslation>> = Vec::new();
        for step in &self.steps {
            let translations = step.trace(&input, options);
            input = translations.iter().map(|t| t.output()).collect();
            result.push(translations);
        }
        result
    }

    pub fn translate(&self, input: &str) -> String {
        let mut result = input.to_string();
        for step in &self.steps {
            result = step.translate(&result);
        }
        result
    }

    pub fn translate_with_options(&self, input: &str, options: &TranslationOptions) -> String {
        let mut result = input.to_string();
        for step in &self.steps {
            result = step.translate_with_options(&result, options);
        }
        result
    }

    /// Translate `input` and map its positions to the positions of the output
    pub fn translate_with_positions(
        &self,
        input: &str,
        options: &TranslationOptions,
    ) -> (String, PositionMap) {
        let stages = self.trace_with_options(input, options);
        let output = stages.last().map_or_else(
            || input.to_string(),
            |steps| steps.iter().map(|t| t.output()).collect(),
        );
        let positions = PositionMap::from_trace(input.chars().count(), &stages, self.direction);
        (output, positions)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::parser::RuleParser;

    fn parse_rule(source: &str) -> AnchoredRule {
        RuleParser::new(source).rule().unwrap().into()
    }

    #[test]
    fn correct() {
        let rules = [
            parse_rule("always foo 123"),
            parse_rule("always bar 456"),
            parse_rule("noback correct \"baz\" \"bar\""),
            parse_rule("space \\s 0"),
        ];
        let pipeline = TranslationPipeline::compile(&rules, Direction::Forward).unwrap();
        assert_eq!(pipeline.translate("baz"), "⠸");
        assert_eq!(pipeline.translate("foobaz"), "⠇⠸");
        assert_eq!(pipeline.translate("foobar"), "⠇⠸");
        assert_eq!(pipeline.translate("  "), "⠀⠀");
        assert_eq!(pipeline.translate("🐂"), "⠳⠭⠂⠋⠲⠴⠆");
    }

    #[test]
    fn pass2() {
        let rules = [
            parse_rule("always foo 123"),
            parse_rule("always bar 456"),
            parse_rule("noback pass2 @123 @1"),
            parse_rule("space \\s 0"),
        ];
        let pipeline = TranslationPipeline::compile(&rules, Direction::Forward).unwrap();
        assert_eq!(pipeline.translate("foo"), "⠁");
        assert_eq!(pipeline.translate("foobar"), "⠁⠸");
        assert_eq!(pipeline.translate("  "), "⠀⠀");
        assert_eq!(pipeline.translate("🐂"), "⠳⠭⠂⠋⠲⠴⠆");
    }

    #[test]
    fn pass2_with_capture() {
        let rules = [
            parse_rule("lowercase o 135"),
            parse_rule("lowercase ύ 5-13456"),
            parse_rule("sign ΄ 5"),
            parse_rule("attribute accent ΄"),
            parse_rule("noback pass2 @135[%accent]@13456 *@136"),
        ];
        let pipeline = TranslationPipeline::compile(&rules, Direction::Forward).unwrap();
        assert_eq!(pipeline.translate("o"), "⠕");
        assert_eq!(pipeline.translate("oύ"), "⠕⠐⠥⠽");
    }

    #[test]
    fn translate_with_positions_matches_translate() {
        let rules = [
            parse_rule("always foo 123"),
            parse_rule("always bar 456"),
            parse_rule("noback correct \"baz\" \"bar\""),
            parse_rule("noback pass2 @456 @1"),
            parse_rule("space \\s 0"),
        ];
        let pipeline = TranslationPipeline::compile(&rules, Direction::Forward).unwrap();
        let options = TranslationOptions::default();
        for input in ["foobaz", "foobar", "  ", "🐂", ""] {
            assert_eq!(
                pipeline.translate_with_positions(input, &options).0,
                pipeline.translate_with_options(input, &options)
            );
        }
    }

    #[test]
    fn translate_with_positions_composes_stages() {
        let rules = [
            parse_rule("always foo 123"),
            parse_rule("always bar 456"),
            parse_rule("noback correct \"baz\" \"bar\""),
            parse_rule("space \\s 0"),
        ];
        let pipeline = TranslationPipeline::compile(&rules, Direction::Forward).unwrap();
        let (output, positions) =
            pipeline.translate_with_positions("foobaz", &TranslationOptions::default());
        assert_eq!(output, "⠇⠸");
        assert_eq!(positions.output_positions(), [0, 0, 0, 1, 1, 1]);
        assert_eq!(positions.input_positions(), [0, 3]);
        assert_eq!(positions.cursor(4), 1);
        assert_eq!(positions.cursor(6), 2);
    }

    #[test]
    fn pass3() {
        let rules = [
            parse_rule("always foo 123"),
            parse_rule("always bar 456"),
            parse_rule("noback pass2 @123 @78"),
            parse_rule("noback pass3 @78 @1"),
            parse_rule("space \\s 0"),
        ];
        let pipeline = TranslationPipeline::compile(&rules, Direction::Forward).unwrap();
        assert_eq!(pipeline.translate("foo"), "⠁");
        assert_eq!(pipeline.translate("foobar"), "⠁⠸");
        assert_eq!(pipeline.translate("  "), "⠀⠀");
        assert_eq!(pipeline.translate("🐂"), "⠳⠭⠂⠋⠲⠴⠆");
    }

    #[test]
    fn pass4() {
        let rules = [
            parse_rule("always foo 123"),
            parse_rule("always bar 456"),
            parse_rule("noback pass2 @123 @67"),
            parse_rule("noback pass3 @67 @78"),
            parse_rule("noback pass4 @78 @1"),
            parse_rule("space \\s 0"),
        ];
        let pipeline = TranslationPipeline::compile(&rules, Direction::Forward).unwrap();
        assert_eq!(pipeline.translate("foo"), "⠁");
        assert_eq!(pipeline.translate("foobar"), "⠁⠸");
        assert_eq!(pipeline.translate("  "), "⠀⠀");
        assert_eq!(pipeline.translate("🐂"), "⠳⠭⠂⠋⠲⠴⠆");
    }

    #[test]
    fn with_display_puts_the_display_stage_on_the_braille_side() {
        let rules = [parse_rule("letter a 1"), parse_rule("space \\s 0")];
        // maps the cell for dot 1 to `A` when displaying, and back when reading
        let display = [parse_rule("display A 1")];

        // forward: translate to braille, then show the cell as `A`
        let forward = TranslationPipeline::compile(&rules, Direction::Forward)
            .unwrap()
            .with_display(DisplayTable::compile(&display, Direction::Forward));
        assert_eq!(forward.translate("a"), "A");

        // backward: read `A` as the cell, then translate it back to text. `compile` has
        // already reversed the steps here, so this only works if `with_display` inserts at
        // the front rather than pushing.
        let backward = TranslationPipeline::compile(&rules, Direction::Backward)
            .unwrap()
            .with_display(DisplayTable::compile(&display, Direction::Backward));
        assert_eq!(backward.translate("A"), "a");

        // a one-to-one stage must leave the positions alone
        let options = TranslationOptions::default();
        let (output, _) = forward.translate_with_positions("a", &options);
        assert_eq!(output, "A");
    }
}
