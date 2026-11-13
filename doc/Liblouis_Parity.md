# louis-rs vs. liblouis

louis-rs is a from-scratch Rust reimplementation of [liblouis](https://liblouis.io/),
not a port: the data structures and algorithms are different, the tables and the test
suite are the same. It translates in both directions with every table liblouis ships.

This document lists what we know louis-rs does differently from liblouis, so you can
tell whether that affects you before you switch. Anything not listed here should
behave the same as liblouis, and if it doesn't, that's a bug:
[report it](https://github.com/liblouis/louis-rs/issues).

## How close

Measured at louis-rs `b2134ba` against liblouis `011cfb3b` plus the open pull requests
[#2176](https://github.com/liblouis/liblouis/pull/2176),
[#2190](https://github.com/liblouis/liblouis/pull/2190),
[#2194](https://github.com/liblouis/liblouis/pull/2194),
[#2195](https://github.com/liblouis/liblouis/pull/2195),
[#2196](https://github.com/liblouis/liblouis/pull/2196) and
[#2197](https://github.com/liblouis/liblouis/pull/2197). Without #2196 and #2197,
`it-it-comp6.utb` and `th-g2.ctb` don't load (see `_N` in
[Differences_From_Liblouis.md](Differences_From_Liblouis.md)).

|                                |                |
|--------------------------------|----------------|
| Assertions run                 | 2 241 030      |
| Pass                           | **99.4%**      |
| Fail                           | 1 021 (0.05%)  |
| Expected failure               | 0.5%           |
| Tables that compile            | all            |

Forward and backward combined. "Expected failure" means tests liblouis itself marks as
failing (`xfail`), and louis-rs fails them too. Going the other way, 42 tests that
liblouis marks as failing pass in louis-rs, spread over 16 files.

Reproduce the assertion figures with:

```sh
export LOUIS_TABLE_PATH=~/src/liblouis/tables:~/src/liblouis
cargo run --release -- check \
    ~/src/liblouis/tests/braille-specs/*.yaml \
    ~/src/liblouis/tests/yaml/*.yaml --summary
```

and the table figure, which compiles each table on its own in both directions as
liblouis's `lou_checktable` does, with:

```sh
cargo build --release
export LOUIS_TABLE_PATH=~/src/liblouis/tables
for f in ~/src/liblouis/tables/*.{ctb,utb,uti,tbl,cti,dis}; do
    ./target/release/louis check-table -q "$f" || echo "$f"
done
```

The few files it lists are fragments that only work when included in another table;
liblouis can't compile them on their own either.

## Deliberate differences

Some differences are decisions rather than gaps: table lookup through
`LOUIS_TABLE_PATH`, display tables that must be named explicitly, `$w`–`$z`
attributes, `_N` only before or after the tests it steps over, backward word emphasis
that stops before the space, and the six unimplemented opcodes. They're collected in
[Differences_From_Liblouis.md](Differences_From_Liblouis.md), each with a migration path.

## The liblouis API in louis-rs

There is **no C ABI** yet. Python bindings live in
[louis-py](https://github.com/liblouis/louis-py).

| liblouis                                              | louis-rs                                                                                      |
|-------------------------------------------------------|-----------------------------------------------------------------------------------------------|
| `lou_translate`, `lou_translateString`                | `Translator::translate_with_options`, or `translate` for just the output                      |
| `lou_backTranslate`, `lou_backTranslateString`        | The same, with a `Direction::Backward` translator                                              |
| `inputPos`, `outputPos`, `cursorPos`                  | `TranslationResult::input_positions`, `output_positions`, `cursor_position`; three known mismatches across the whole suite |
| `typeform`, forward and backward                      | `Emphasis`, as spans of named classes: `TranslationOptions::with_emphasis` going in, `TranslationResult::emphasis` coming out |
| `lou_getEmphClasses`, `lou_getTypeformForEmphClass`   | `Translator::emphasis_classes`; spans name their class, so no typeform bits are needed           |
| `lou_compileString`                                   | `Translator::from_table_source` compiles a whole table from a string; no adding rules to a table already loaded |
| `lou_checkTable`                                      | `Translator::new` returns the error; `louis check-table` on the command line                    |
| `lou_setDataPath`, `lou_registerTableResolver`        | `Translator::with_search_path`; there's no resolver callback                                   |
| `lou_findTable`, `lou_findTables`, `lou_indexTables`  | `louis::metadata::Index`; it only returns tables that match every pair of the query, it doesn't rank them the way liblouis does |
| `lou_getTableInfo`, `lou_listTables`                  | Missing                                                                                        |
| `lou_hyphenate`, `lou_translatePrehyphenated`         | Missing. `lou_hyphenate` is waiting for a user, see [ADR-0022](adr/0022-hyphenate-chartodots-dotstochar.org); hyphenation inside translation (`nocross`) works |
| `lou_charToDots`, `lou_dotsToChar`                    | Missing, waiting for a user; they will go on `BrailleEncoding` ([ADR-0022](adr/0022-hyphenate-chartodots-dotstochar.org)) |
| Logging (`lou_registerLogCallback`, `lou_setLogLevel`, …) | The [`log`](https://docs.rs/log) crate                                                    |
| `lou_free`, `lou_version`, `lou_charSize`, …          | Not needed in Rust                                                                             |

**Translation modes** (`TranslationMode`): `noContractions`, `compbrlAtCursor`,
`compbrlLeftCursor`, `noUndefined` and `partialTrans` work. `dotsIO` and `ucBrl` choose
liblouis's output encoding and have no counterpart: louis-rs writes Unicode braille,
which is what `dotsIO | ucBrl` gives, unless given a `BrailleEncoding`.

## Known wrong output

If you translate one of the languages below, these are the errors to expect. Failure
counts are the `Failure` lines `check` prints for each file, rounded.

| Tables                                | Failing | What goes wrong                                                                                                                                                                       |
|---------------------------------------|--------:|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Dutch (`nl-g0`)                       |    ≈110 | A multi-word italic passage should open with ⠸⠸ once, louis-rs marks every word ⠸…⠠. Indicators also come out in the wrong order (⠰⠨, not ⠨⠰), and a stray ⠠ appears after a capsletter that follows a digit ("1E" gives ⠼⠁⠨⠠⠑). |
| Russian (`ru`)                        |     ≈30 | Generic `begmode` alphabet indicators: word tiers repeat per letter ("ABC" gives ⠨⠁⠨⠃⠨⠉, not ⠨⠁⠃⠉), and a letter after one from another alphabet gets its sign twice ("HАH" gives ⠨⠓⠘⠘⠁⠨⠓). Likely needs indication tiers unified across capitals, emphasis and modes ([ADR-0009](adr/0009-capsletter-vs-begcapsword.org)). |
| German detailed (`de-g0-detailed`, `de-g1-detailed`), backward |     ≈15 | After a number, ⠠ is read as an apostrophe (⠼⠓⠠⠋⠁⠉⠓ gives "8'fach", not "8fach"), and a few quotation marks and ¦ come back as other characters. |
| Punjabi (`pa`), backward              |     ≈10 | The number sign ⠼ is read as the letter ਣ, which has the same cells; liblouis takes the number sign when a digit follows.                                                             |
| everything else                       |    ≈820 | Roughly 85 files, each with fewer than 80 failures.                                                                                                                                    |

**Backward translation** is where louis-rs is weakest. The word-boundary checks
(`word_start` and friends) only work forward, caps passages have no backward path at
all, and backward `capsletter`/`begcapsword` fails in `hu-hu-g1`
([#4](https://github.com/liblouis/louis-rs/issues/4)).

**Patterns.** In `match` and `context` rules, negating a sequence or an alternation
is wrong: `!(e1e2)` is taken as `(!e1!e2)`, and `!(e1|e2)` as `(!e1|!e2)`.

## Not yet measured

About 40 blocks in liblouis's YAML files don't run, because their table or include
can't be found, their YAML doesn't parse, or, in three of liblouis's own multipass
tests, a `_N` steps back into a bracket. The `hyphenate`, `hyphenateBraille` and
`display` test modes aren't run at all. Those tests report neither pass nor fail;
the list is under "Test suite gaps" in [TODO.org](../TODO.org).

---

Working notes live in [TODO.org](../TODO.org); design decisions and the reasoning
behind them in [doc/adr/](adr/README.org); speed in
[Translation_Performance.md](Translation_Performance.md).
