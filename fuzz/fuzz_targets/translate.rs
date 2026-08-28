#![no_main]

use std::path::PathBuf;
use std::sync::OnceLock;

use libfuzzer_sys::fuzz_target;
use louis::{Direction, Translator};

fn translator() -> &'static Translator {
    static TRANSLATOR: OnceLock<Translator> = OnceLock::new();

    TRANSLATOR.get_or_init(|| {
        let path =
            PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("data/translate_tables/fuzz_maximal.ctb");
        Translator::new(&[path], Direction::Forward)
            .unwrap_or_else(|error| panic!("failed to load fuzz_maximal.ctb: {error}"))
    })
}

fuzz_target!(|text: &str| {
    let _ = translator().translate(text);
});
