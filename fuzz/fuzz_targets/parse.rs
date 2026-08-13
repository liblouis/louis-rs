#![no_main]

// Parser-focused fuzz target.
//
// The existing `table` target already feeds arbitrary bytes to
// `Translator::from_table_source`, but it also spins up a full translator
// and runs a fixed set of translation samples in both directions for every
// input. On valid tables that burns most of the exec budget on the runtime
// instead of the parser.
//
// This target calls `from_table_source` and discards the result, so the
// whole exec goes to the table parser and pipeline builder — which is the
// code actually changing on this branch (`src/parser.rs`).

use libfuzzer_sys::fuzz_target;
use louis::{Direction, Translator};

fuzz_target!(|source: &str| {
    let _ = Translator::from_table_source(source, Direction::Forward);
});
