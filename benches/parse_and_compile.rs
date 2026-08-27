//! Benchmarks both halves of getting a table ready to translate with, since
//! [`louis::Translator::new`] does them together and that total is what a caller pays:
//!
//! 1. parsing -- reading the table source and everything it pulls in via `include`, turning
//!    the lines into rules, and running the whole-table consistency checks that happen once
//!    the include tree is flattened;
//! 2. compiling -- turning those rules into the tries, regexps and indicator tables the
//!    translator runs against.
//!
//! Parsing alone is roughly 15-25% of the total. Timing it separately would mean exposing
//! the private `parser` module, which isn't worth it: per-rule parse costs sit well below
//! this benchmark's run-to-run variance anyway.
//!
//! Covers a spread of table sizes: `de-g2.ctb` is small but include-heavy, `en-ueb-g2.ctb`
//! is the rule-complexity case, and `zh-tw.ctb` is by far the largest table liblouis ships
//! (~54k lines), so it dominates any per-rule cost.
//!
//! Requires `LOUIS_TABLE_PATH` to point at a liblouis checkout's `tables` directory, e.g.:
//!   export LOUIS_TABLE_PATH=~/src/liblouis/tables
//!   cargo bench --bench parse_and_compile

use criterion::{Criterion, black_box, criterion_group, criterion_main};
use louis::{Direction, Translator};

const TABLES: [&str; 3] = ["de-g2.ctb", "en-ueb-g2.ctb", "zh-tw.ctb"];

fn parse_and_compile_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("parse_and_compile");
    for table in TABLES {
        // fail fast with a useful message rather than inside the timing loop
        Translator::new(&[table], Direction::Forward).unwrap_or_else(|e| {
            panic!(
                "failed to load {table} ({e}) -- set LOUIS_TABLE_PATH, e.g. \
                 `export LOUIS_TABLE_PATH=~/src/liblouis/tables`"
            )
        });
        group.bench_function(table, |b| {
            b.iter(|| Translator::new(black_box(&[table]), Direction::Forward).unwrap())
        });
    }
    group.finish();
}

criterion_group!(benches, parse_and_compile_benchmark);
criterion_main!(benches);
