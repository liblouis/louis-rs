# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Git Commit Messages

Use the Linux kernel's coding-assistants trailer format (https://docs.kernel.org/process/coding-assistants.html): `Assisted-by: Claude:<model>`, not `Co-authored-by:`. `<model>` is the model actually running the session.

## Project Overview

**louis-rs** is a Rust reimplementation of [liblouis](https://liblouis.io/), a braille translator and back-translator. It is **not** a direct port of the C code but a complete rewrite using different data structures and algorithms while maintaining compatibility with liblouis tables and tests.

**Current Status**: Alpha stage
- Translation: ~99.4% compatibility with the liblouis test suite (forward and
  backward combined)
- Table parsing: every table file under `tables/` that liblouis compiles, louis-rs
  compiles too (`louis check-table`, like `lou_checktable`). 3 that used
  to fail were table-authoring bugs, not louis-rs gaps, and have since been fixed
  upstream (liblouis/liblouis#2070, #2071, #2072); the last one was ours, an
  escaped `"\\"` multipass operand in `it-it-comp6.utb:248`, fixed in 4980ced
- Figures above measured at louis-rs `3a6e171` against liblouis `0d3be2c4`; see
  `doc/Liblouis_Parity.md`, which is the one place to update them
- Library API: Not yet stable

## Building and Testing

### Build Commands

```bash
# Development build
cargo build

# Release build (optimized)
cargo build --release

# Run the binary directly
cargo run -- <command>
```

### Testing

```bash
# Run all unit tests in the codebase
cargo test

# Run tests with output
cargo test -- --nocapture

# Run a specific test
cargo test test_name

# Run YAML integration tests from liblouis
export LOUIS_TABLE_PATH=~/src/liblouis/tables:~/src/liblouis
cargo run -- check ~/src/liblouis/tests/braille-specs/*.yaml --summary
```

### Key Commands for Development

```bash
# Check that a braille table compiles, like liblouis's lou_checktable
cargo run -- check-table path/to/table.ctb

# Translate text to braille
cargo run -- translate de-comp6.utb
# Then type text to translate at the REPL

# Trace translation rules applied (with table output)
cargo run -- trace en-us-g2.ctb
# Then type text at the REPL

# Check a single YAML test file
cargo run -- check path/to/test.yaml

# Query tables by metadata
cargo run -- query language=de,contraction=full
```

### Linting

The project uses standard Rust tools:

```bash
# Check code without building
cargo check

# Format code (uses rustfmt)
cargo fmt

# Lint with clippy
cargo clippy
```

## Architecture

**Read [`doc/Architecture.md`](doc/Architecture.md)** for the concepts and how they fit
together: the pipeline and its stages, `TableContext`, the trie and its transitions, the
regexp VM with match and context patterns, indicators, constrainers, amenders, `Rank` and
`Precedence`, effects, and what differs backward. It also has the table of where new logic
goes. Keep it up to date when a concept changes; don't repeat it here.

The short version:
- `parser.rs` turns each table line into a `Rule` (`AnchoredRule` keeps its file, line and
  definition order); `TableContext::compile` is the first pass over the rules, the
  character definitions and classes; each table's compile is the second
- `translator/pipeline.rs` sorts the rules into stages: `correct` (`MultipassTable`), the
  main stage (`table/primary.rs`, `PrimaryTable`), `pass2`-`pass4` (`MultipassTable`), and
  a display table; backward, in reverse
- `PrimaryTable::trace`: indicators and constrainers precompute from the input, the loop
  picks a candidate per position by `Rank` (`translation.rs`), the amenders rewrite the
  chosen translations
- Logic that depends on which rule was chosen goes in an amender, never in an indicator or
  constrainer guessing from the text

Other modules: `src/hyphenation.rs` reads `.dic` files; `src/emphasis.rs` turns a
typeform into emphasis spans.

The repository is a workspace (ADR-0019). The root package `louis-rs` is the library;
`louis-cli/` is the `louis` CLI (`check-table`, `translate`, `trace`, `check`, `query`), with
`louis-cli/src/test.rs` and `louis-cli/src/yaml.rs` running liblouis's YAML tests and
`louis-cli/src/metadata.rs` the table index. The CLI depends on the library like any other
user; what it needs beyond the public API comes from `louis::internals` (`#[doc(hidden)]`,
not semver-covered), which re-exports exactly those items. CLI-only dependencies (clap,
rayon, tabled, env_logger, saphyr-parser) belong in `louis-cli/Cargo.toml`, never in the
library's.

## Key Implementation Notes

### Testing Against liblouis

Tests use YAML files from the liblouis project. Current gaps:
- Emphasis indication (partial support)
- Some backward translation rules
- Computer braille cursor modes (`compbrlAtCursor`/`compbrlLeftCursor` not implemented)
- Typeform output (`TranslationResult::emphasis` is never populated)

See `TODO.org` for detailed tracking.

### Performance Considerations

- Trie lookup is O(n) where n is the length of the pattern
- Regexp compilation is done at table load time
- Translation is done in a single pass through the pipeline
- Word boundary detection uses Unicode segmentation

### Known Limitations (from TODO.org)

- Emphasis indication not fully implemented
- nocross-driven hyphenation dictionary lookup works natively (`src/hyphenation.rs`)
- Some backward translation modes missing
- Computer braille is implemented as a full pipeline feature; only `compbrlAtCursor`/`compbrlLeftCursor` modes are missing

## Relevant Files for Common Tasks

| Task | Files |
|------|-------|
| Add new opcode support | `src/parser.rs` (parsing) + `src/translator/table/primary.rs` (compilation) |
| Fix translation bug | `src/translator/trie.rs` or `src/translator/regexp.rs` depending on rule type |
| Add test harness | `louis-cli/src/yaml.rs`, `louis-cli/src/test.rs` |
| Improve regex engine | `src/translator/regexp.rs` |
| Optimize performance | `src/translator/trie.rs`, `src/translator/table/primary.rs` |
| Handle new character class | `src/parser/character_class.rs`, `src/translator/table.rs` |
| Change hyphenation behavior | `src/hyphenation.rs` -- parses liblouis's `.dic` hyphenation-pattern format directly, no external crate or build step |

## Running the Full Test Suite

```bash
export LOUIS_TABLE_PATH=~/src/liblouis/tables:~/src/liblouis
cargo run --release -- check ~/src/liblouis/tests/braille-specs/*.yaml ~/src/liblouis/tests/yaml/*.yaml 2>/dev/null
```

This runs ~700k tests across 70+ test files with various languages and braille standards.

## Code Organization Philosophy

- **Compilation is two-pass**: `TableContext` collects the definitions and classes, each table's compile uses them
- **Compilation is separate from translation**: Rules are compiled once at load time
- **Data structures optimize for speed**: Tries for fast lookups, VMs for regex
- **Features are isolated**: Indicators, effects, and stages are modular
- **Error handling**: Uses `thiserror` for ergonomic error types

