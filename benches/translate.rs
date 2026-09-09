//! Benchmarks translation through en-ueb-g2.ctb, the liblouis table with by far the
//! most `match`/`context` rules -- its main stage compiles 444 `match` and 57
//! `context` patterns -- to track the cost of the regexp VM in
//! `src/translator/regexp.rs` that those opcodes compile to.
//!
//! Three groups, because the cost of a rule depends on what the input looks like and
//! forward English is the least representative case there is: the tables are mostly
//! non-ASCII on the text side, and backward translation reads dot patterns, which are
//! non-ASCII by construction.
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

/// A Greek paragraph: every letter is outside ASCII, so it exercises the non-ASCII path
/// of every character-class membership test.
const GREEK: &str = "\
Στις 25 Δεκεμβρίου 2024, η επιτροπή έλαβε 1234 \
αιτήσεις για τη θέση, και μετά από προσεκτική εξέταση \
αποφάσισαν ότι ο καλύτερος υποψήφιος θα ήταν \
κάποιος με τεχνική κατανόηση και πρακτική εμπειρία. \
Λέγεται συχνά ότι η υπομονή και η επιμονή είναι \
τα κλειδιά της επιτυχίας, αλλά χωρίς κατάλληλες γνώσεις, \
ακόμη και ο πιο αφοσιωμένος άνθρωπος θα δυσκολευτεί.";

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

    // Backward translation reads Unicode braille, so *every* input character is in
    // U+2800..U+28FF and no input is ever ASCII.
    let braille = translator.translate(PARAGRAPH).unwrap();
    let backward = Translator::new(&["en-ueb-g2.ctb"], Direction::Backward)
        .expect("failed to load en-ueb-g2.ctb for backward translation");
    let mut group = c.benchmark_group("translate/en-ueb-g2-backward");
    group.throughput(criterion::Throughput::Bytes(braille.len() as u64));
    group.bench_function("paragraph", |b| {
        b.iter(|| backward.translate(black_box(&braille)).unwrap())
    });
    group.finish();

    // A non-Latin table, where the text side is non-ASCII too. el.ctb carries 152
    // match/context rules -- enough of them to be worth measuring, unlike most of the
    // other non-Latin tables.
    let greek = Translator::new(&["el.ctb"], Direction::Forward).expect("failed to load el.ctb");
    let mut group = c.benchmark_group("translate/el");
    group.throughput(criterion::Throughput::Bytes(GREEK.len() as u64));
    group.bench_function("paragraph", |b| {
        b.iter(|| greek.translate(black_box(GREEK)).unwrap())
    });
    group.finish();
}

criterion_group!(benches, translate_benchmark);
criterion_main!(benches);
