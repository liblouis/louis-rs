//! Benchmarks forward translation through en-ueb-g2.ctb, the liblouis table with by far the
//! most `match`/`context` rules (964), to track the cost of the regexp VM in
//! `src/translator/regexp.rs` that those opcodes compile to.
//!
//! Requires `LOUIS_TABLE_PATH` to point at a liblouis checkout's `tables` directory, e.g.:
//!   export LOUIS_TABLE_PATH=~/src/liblouis/tables
//!   cargo bench

use criterion::{Criterion, black_box, criterion_group, criterion_main};
use louis::{Direction, Translator};

const WORD: &str = "understanding";

const SENTENCE: &str =
    "The quick brown fox jumps over the lazy dog while thinking about tomorrow's weather.";

const PARAGRAPH: &str = "\
On 25 December 2024, the committee received 1,234 applications for the position, and \
after careful consideration, they decided that the best candidate would be someone with \
both technical understanding and practical experience. It's often said that patience and \
perseverance are the keys to success; nevertheless, without proper knowledge, even the \
most dedicated person will struggle. The organization's mission is to encourage learning, \
foster collaboration, and support everyone who wants to make a difference in their community.";

fn translate_benchmark(c: &mut Criterion) {
    let translator = Translator::new(&["en-ueb-g2.ctb"], Direction::Forward).expect(
        "failed to load en-ueb-g2.ctb -- set LOUIS_TABLE_PATH, e.g. \
         `export LOUIS_TABLE_PATH=~/src/liblouis/tables`",
    );

    let mut group = c.benchmark_group("translate/en-ueb-g2");
    for (name, input) in [
        ("word", WORD),
        ("sentence", SENTENCE),
        ("paragraph", PARAGRAPH),
    ] {
        group.throughput(criterion::Throughput::Bytes(input.len() as u64));
        group.bench_function(name, |b| {
            b.iter(|| translator.translate(black_box(input)).unwrap())
        });
    }
    group.finish();
}

criterion_group!(benches, translate_benchmark);
criterion_main!(benches);
