# Fuzzing louis-rs

This directory contains three [cargo-fuzz](https://github.com/rust-fuzz/cargo-fuzz) targets for louis-rs. The fuzzing engine is [libafl_libfuzzer](https://github.com/AFLplusplus/LibAFL) (a drop-in replacement for the deprecated `libfuzzer-sys` crate).

## Targets

### `table`

Feeds arbitrary bytes as table source to `Translator::from_table_source`, then runs a fixed set of text samples through any valid translator that results — in both forward and backward directions. The rationale of running translation after the table was successfully parsed is to catch any bugs in the resulting runtime that would only emerge when actually using it for translation.

This target exercises the **full pipeline**: parser → rule compiler → translator runtime.

### `parse`

Calls `Translator::from_table_source` and discards the result. No translation is performed, so every execution budget goes to the **table parser and pipeline builder** (`src/parser.rs`). Faster per-exec than `table`, but it has a lower coverage.

Both `table` and `parse` share the same seed corpus (`seeds/table/`).

### `translate`

Loads a fixed pre-built table (`data/translate_tables/fuzz_maximal.ctb`) once at startup, then fuzzes arbitrary **input text** through it. This target exercises the translator runtime.

`fuzz_maximal.ctb` is a synthetic table that includes several real upstream tables (en-us-g2, en-ueb-g1, fr-bfu-g2, es-g2, uz-g1, ar-ar-g2) plus a short overlay with rules covering most opcode types.

## Prerequisites

Install cargo-fuzz if you don't have it:

```sh
cargo install cargo-fuzz
```

## Running the fuzzers

All commands are run from the **repository root**.

### `table` and `parse`

```sh
cargo fuzz run table fuzz/corpus/table fuzz/seeds/table -- -dict=fuzz/dict/table.dict -grimoire=1
cargo fuzz run parse fuzz/corpus/table fuzz/seeds/table -- -dict=fuzz/dict/table.dict -grimoire=1
```

### `translate`

```sh
cargo fuzz run translate fuzz/corpus/translate fuzz/seeds/translate -- -dict=fuzz/dict/translate.dict
```

The first directory is where the fuzzer writes newly discovered inputs; subsequent directories are read-only seeds. This keeps `fuzz/seeds/` unmodified.

### Reproduce a crash

```sh
cargo fuzz run table fuzz/artifacts/table/<crash-id>
```

## Seeds

`fuzz/seeds/table/` contains hand-crafted tables covering the full range of liblouis opcodes (basic characters, digits, punctuation, word rules, match patterns, context rules, multipass, swaps, attributes, etc.), plus a set of intentionally invalid inputs. `fuzz/seeds/translate/` contains hand-crafted text inputs for the `translate` target.

The fuzzer writes newly discovered interesting inputs to `fuzz/corpus/` (gitignored).

To additionally seed from real upstream liblouis tables, run (requires `liblouis/` cloned next to the repo root):

```sh
python3 fuzz/import-seeds.py
```

This populates `fuzz/seeds/table/` with one file per liblouis table, with `include` directives stripped. The generated `upstream__*` files are gitignored.

## Dictionaries

- `dict/table.dict` — liblouis opcode keywords and common table syntax tokens, used to guide mutations for `table` and `parse`.
- `dict/translate.dict` — braille character sequences and common text patterns, used to guide mutations for `translate`.

## OSS-Fuzz integration

Follow the official guide for Rust projects: https://google.github.io/oss-fuzz/getting-started/new-project-guide/rust-lang/
