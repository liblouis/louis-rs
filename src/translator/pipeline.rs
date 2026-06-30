use crate::{
    Direction,
    parser::{AnchoredRule, HasDirection, Rule},
    translator::{
        ResolvedTranslation, TranslationError, TranslationOptions, TranslationStage,
        table::{TableContext, multipass::MultipassTable, primary::PrimaryTable},
    },
};

#[derive(Debug)]
pub enum Transformation {
    Pre(MultipassTable),
    Primary(PrimaryTable),
    Post(MultipassTable),
}

impl Transformation {
    pub fn trace(&self, input: &str, options: &TranslationOptions) -> Vec<ResolvedTranslation> {
        match self {
            Transformation::Pre(t) => t.trace(input),
            Transformation::Primary(t) => t.trace(input, options),
            Transformation::Post(t) => t.trace(input),
        }
    }

    fn translate(&self, input: &str) -> String {
        match self {
            Transformation::Pre(t) => t.translate(input),
            Transformation::Primary(t) => t.translate(input),
            Transformation::Post(t) => t.translate(input),
        }
    }

    fn translate_with_options(&self, input: &str, options: &TranslationOptions) -> String {
        match self {
            Transformation::Pre(t) => t.translate(input),
            Transformation::Primary(t) => t.translate_with_options(input, options),
            Transformation::Post(t) => t.translate(input),
        }
    }
}

#[derive(Debug)]
pub struct TranslationPipeline {
    steps: Vec<Transformation>,
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
            Direction::Forward => Ok(Self { steps }),
            Direction::Backward => Ok(Self {
                steps: steps.into_iter().rev().collect(),
            }),
        }
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
}
