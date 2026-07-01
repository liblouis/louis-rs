#![no_main]

use libfuzzer_sys::fuzz_target;
use louis::{Direction, Translator};

const FORWARD_SAMPLES: &[&str] = &[
    "",
    " ",
    "  ",
    "\t",
    "\n",
    "a",
    "aaaa",
    "A",
    "ABC",
    "AaA",
    "1",
    "123",
    "a1b2",
    "1.2",
    ".,;:!?",
    "()[]{}",
    "foo",
    "foobar",
    "foo bar",
    "foo-bar",
    "é",
    "ß",
    "΄ύ",
    "🐂",
];
const BACKWARD_SAMPLES: &[&str] = &[
    "",
    "⠀",
    "⠀⠀",
    "⠁",
    "⠁⠁⠁⠁",
    "⠁⠃⠉",
    "⠠⠁",
    "⠠⠁⠠⠃",
    "⠼⠁",
    "⠼⠁⠃",
    "⠲",
    "⠶⠶",
    "⠓⠑⠇⠇⠕",
    "⠋⠕⠕⠀⠃⠁⠗",
];

fuzz_target!(|data: &[u8]| {
    let Ok(table) = std::str::from_utf8(data) else {
        return;
    };

    for (direction, samples) in [
        (Direction::Forward, FORWARD_SAMPLES),
        (Direction::Backward, BACKWARD_SAMPLES),
    ] {
        if let Ok(translator) = Translator::from_table_source(table, direction) {
            for sample in samples {
                let _ = translator.translate(sample);
            }
        }
    }
});
