use crate::{
    Direction, Emphasis,
    parser::{AnchoredRule, HasDirection, Rule},
    translator::{
        CompileError, CompileErrorKind, PositionMap, ResolvedTranslation, TranslationOptions,
        TranslationStage,
        indication::emphasis::backward_emphasis,
        table::{
            TableContext, display::DisplayTable, multipass::MultipassTable, primary::PrimaryTable,
        },
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
    /// The `emphclass` names, in the order the rules declare them
    emphasis_classes: Vec<String>,
}

impl TranslationPipeline {
    pub fn compile(rules: &[AnchoredRule], direction: Direction) -> Result<Self, CompileError> {
        let ctx = TableContext::compile(rules)?;
        // A base character has to be defined, but a definition for the other direction
        // (`noback`, `nofor`) is enough. So check against all rules, before they are
        // filtered by direction.
        for rule in rules {
            if let Rule::Base { derived, base, .. } = rule.rule
                && ctx.character_definitions().get(&base).is_none()
            {
                return Err(CompileErrorKind::BaseCharacterNotDefined { base, derived }.at(rule));
            }
        }
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
        let mut emphasis_classes: Vec<String> = Vec::new();
        for rule in rules {
            if let Rule::Emphclass { name } = &rule.rule
                && !emphasis_classes.contains(name)
            {
                emphasis_classes.push(name.clone());
            }
        }
        let steps = match direction {
            Direction::Forward => steps,
            Direction::Backward => steps.into_iter().rev().collect(),
        };
        Ok(Self {
            steps,
            direction,
            emphasis_classes,
        })
    }

    pub fn emphasis_classes(&self) -> &[String] {
        &self.emphasis_classes
    }

    /// Add `display` as the outermost stage, mapping the braille side of the pipeline.
    ///
    /// The display table is supplied rather than compiled from the same rules because the
    /// caller chooses it independently of the translation table: the YAML harness takes it
    /// from a test's `display:` key, `louis translate` from `--display`.
    pub fn direction(&self) -> Direction {
        self.direction
    }

    /// Show the braille as `display` says, replacing any display stage there already was
    pub fn with_display(mut self, display: DisplayTable) -> Self {
        self.steps
            .retain(|step| !matches!(step, Transformation::Display(_)));
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

    /// Translate `input`, map its positions to the positions of the output and, backward,
    /// find the emphasis of the output
    pub fn translate_with_details(
        &self,
        input: &str,
        options: &TranslationOptions,
    ) -> (String, PositionMap, Option<Emphasis>) {
        let stages = self.trace_with_options(input, options);
        let output = stages.last().map_or_else(
            || input.to_string(),
            |steps| steps.iter().map(|t| t.output()).collect(),
        );
        let positions = PositionMap::from_trace(input.chars().count(), &stages, self.direction);
        let emphasis = self.emphasis(&stages);
        (output, positions, emphasis)
    }

    /// Backward, the emphasis that the indicators chosen in the main stage give the output.
    /// The stages after the main stage can change the text, so the emphasis of each
    /// character of the main stage's output is carried over to the characters it becomes.
    fn emphasis(&self, stages: &[Vec<ResolvedTranslation>]) -> Option<Emphasis> {
        if self.direction != Direction::Backward {
            return None;
        }
        let main = self
            .steps
            .iter()
            .position(|step| matches!(step, Transformation::Primary(_)))?;
        let emphasis = backward_emphasis(&stages[main]);
        let main_output_len = stages[main]
            .iter()
            .map(|t| t.output().chars().count())
            .sum();
        let later = PositionMap::from_trace(main_output_len, &stages[main + 1..], self.direction);
        Some(emphasis.remap(later.input_positions()))
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
    fn correct_control_characters() {
        for (escape, control) in [
            (r"\e", '\u{1b}'),
            (r"\f", '\u{c}'),
            (r"\n", '\n'),
            (r"\r", '\r'),
            (r"\t", '\t'),
            (r"\v", '\u{b}'),
        ] {
            let rules = [
                parse_rule("letter a 1"),
                parse_rule(&format!("sign {escape} 12")),
                parse_rule(&format!(r#"noback correct "{escape}" "a""#)),
                parse_rule(&format!(r#"noback correct "a" "{escape}""#)),
            ];
            let pipeline = TranslationPipeline::compile(&rules, Direction::Forward).unwrap();
            assert_eq!(pipeline.translate(&control.to_string()), "⠁");
            assert_eq!(pipeline.translate("a"), "⠃");
        }
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
        // `*@136` copies the accent out of the bracket and consumes the whole match,
        // so the @135 and @13456 cells around it are replaced, not kept
        assert_eq!(pipeline.translate("oύ"), "⠐⠥");
    }

    #[test]
    fn translate_with_details_matches_translate() {
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
                pipeline.translate_with_details(input, &options).0,
                pipeline.translate_with_options(input, &options)
            );
        }
    }

    #[test]
    fn translate_with_details_composes_stages() {
        let rules = [
            parse_rule("always foo 123"),
            parse_rule("always bar 456"),
            parse_rule("noback correct \"baz\" \"bar\""),
            parse_rule("space \\s 0"),
        ];
        let pipeline = TranslationPipeline::compile(&rules, Direction::Forward).unwrap();
        let (output, positions, _) =
            pipeline.translate_with_details("foobaz", &TranslationOptions::default());
        assert_eq!(output, "⠇⠸");
        assert_eq!(positions.output_positions(), [0, 0, 0, 1, 1, 1]);
        assert_eq!(positions.input_positions(), [0, 3]);
        assert_eq!(positions.cursor(4), 1);
        assert_eq!(positions.cursor(6), 2);
    }

    #[test]
    fn translate_with_details_counts_the_context_a_star_action_consumes() {
        let rules = [
            parse_rule("letter a 1"),
            parse_rule("letter b 12"),
            parse_rule("letter c 14"),
            // `*` copies only the bracketed "a" but consumes the "b" after it as well
            parse_rule("noback correct [\"a\"]\"b\" *"),
        ];
        let pipeline = TranslationPipeline::compile(&rules, Direction::Forward).unwrap();
        let (output, positions, _) =
            pipeline.translate_with_details("abc", &TranslationOptions::default());
        assert_eq!(output, "⠁⠉");
        // the dropped "b" shares the "a"'s cell, and the "c" keeps its own
        assert_eq!(positions.output_positions(), [0, 0, 1]);
        assert_eq!(positions.input_positions(), [0, 2]);
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
        let (output, _, _) = forward.translate_with_details("a", &options);
        assert_eq!(output, "A");
    }
}
