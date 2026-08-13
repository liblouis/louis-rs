#![no_main]

use std::path::PathBuf;
use std::sync::{Once, OnceLock};

use libfuzzer_sys::fuzz_target;
use louis::{Direction, Translator};

fn translator() -> &'static Translator {
    static TRANSLATOR: OnceLock<Translator> = OnceLock::new();

    TRANSLATOR.get_or_init(|| {
        let path = tables_dir().join("fuzz_maximal.ctb");
        Translator::new(&[path], Direction::Forward)
            .unwrap_or_else(|error| panic!("failed to load fuzz_maximal.ctb: {error}"))
    })
}

fn tables_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("data/translate_tables")
}

fn ensure_table_search_path() {
    static INIT: Once = Once::new();

    INIT.call_once(|| unsafe {
        std::env::set_var("LOUIS_TABLE_PATH", tables_dir());
    });
}

fuzz_target!(|text: &str| {
    ensure_table_search_path();
    let _ = translator().translate(text);
});
