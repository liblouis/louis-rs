# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**louis-rs** is a Rust reimplementation of [liblouis](https://liblouis.io/), a braille translator and back-translator. It is **not** a direct port of the C code but a complete rewrite using different data structures and algorithms while maintaining compatibility with liblouis tables and tests.

**Current Status**: Alpha stage
- Forward translation: ~82% compatibility with liblouis test suite
- Backward translation: ~71% compatibility
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
# Parse a braille table and print debug info
cargo run -- parse path/to/table.ctb

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

### High-Level Design

The translation pipeline transforms input text through several stages:

1. **Pre-stage (Correct rules)**: Corrections applied before main translation (optional)
2. **Main stage (Primary translation)**: Core character and pattern-based rules
3. **Post-stages (Pass2, Pass3, Pass4)**: Additional transformations after main (optional)

For backward translation, the stages are applied in reverse order.

### Key Components

#### 1. Parser Module (`src/parser.rs`, `src/parser/*.rs`)

Parses liblouis braille tables (.ctb and .utb files) into rules. Two-pass compilation:
- **Pass 1**: Collects all character definitions, classes, and metadata
- **Pass 2**: Uses that context to parse transformation rules

Key types:
- `Rule`: Enum representing all liblouis opcodes (letter, always, word, match, etc.)
- `AnchoredRule`: Rule with file location metadata
- `Direction`: Forward (text→braille) or Backward (braille→text)
- `Constraint`: Restrictions on rules (nofor, noback, nocross)
- Character classes and character definitions are built during parsing

#### 2. Translator Module (`src/translator/`)

Executes the translation pipeline:

**`pipeline.rs`**: Orchestrates the multi-stage translation
- Separates rules by stage (Pre/Main/Post1-3)
- Compiles and chains transformations
- For backward translation, reverses the pipeline order

**`table/primary.rs`**: Main translation table
- **PrimaryTable**: Compiled representation of translation rules for a single stage
- Uses three data structures for rule matching:
  - **Trie** (`trie.rs`): Prefix tree for fast simple pattern matching
  - **MatchPatterns** (`match_pattern.rs`): Handles liblouis `match` rules via the custom regexp engine
  - **ContextPatterns** (`context_pattern.rs`): Handles liblouis `context` rules via the custom regexp engine
- Handles word boundaries, character definitions, and indicators

**`table/multipass.rs`**: Pass2/Pass3/Pass4 transformations
- Simpler pattern matching (sequential text replacement)
- Used for corrections and post-processing

**`trie.rs`**: Prefix tree data structure
- Stores character and word translations with boundaries
- Handles case-insensitive matching while preserving case sensitivity for lookups
- Boundary support: word starts/ends, number starts/ends, punctuation, etc.
- Precedence-based rule selection when multiple rules match

**`regexp.rs`**: Virtual machine-based regex engine
- Compiles regex ASTs to instruction sequences
- Supports character classes, quantifiers, captures, negation
- Linear time complexity matching (based on Russ Cox's VM approach)
- Handles liblouis-specific features like effects and variable matching

**`indication/` submodules**: Special rule handling
- `numeric.rs`: Number indication (inserting number signs)
- `uppercase.rs`: Capital indication (capitalization markers)
- `lettersign.rs`: Letter sign insertion for certain contexts
- `nocontract.rs`: Contraction prevention
- Each indicator builds during compilation and applies during translation

**`effect.rs`**: Rule effects and context environment
- Manages "effects" that rules can apply (numeric, uppercase context)
- Tracks environment (whether in number, caps, etc.)

**`context_pattern.rs`**: Handles liblouis `context` rules via the custom regexp engine
- Parallel to `match_pattern.rs` (which handles `match` rules); both are regex-based and have similar implementations but were developed independently in liblouis for different opcodes

### 3. Test Module (`src/test.rs`, `src/yaml.rs`)

Runs YAML test files (liblouis format):
- Parses test metadata, tables, modes, and test cases
- Runs tests in various modes (forward, backward, display, hyphenate)
- Generates success/failure/expected-failure reports

### 4. Main CLI (`src/main.rs`)

Subcommands:
- `parse`: Parse and debug a table
- `translate`: Interactive translator (REPL or single input)
- `trace`: Show rules applied during translation
- `check`: Run YAML test suites
- `query`: Find tables by metadata

## Important Design Decisions

### Character Handling

- **Character definitions**: Maps characters to their braille representation
- **Character classes**: Groups of related characters (letter, digit, space, punctuation, etc.)
- **Case sensitivity**: Internally case-insensitive matching, but character definitions and translation rules preserve case
- **Unicode support**: Handles full Unicode including virtual dots (Supplementary Private Use Area-A)
- **Fallback for undefined**: Unknown characters converted to hex representation

### Rule Precedence and Selection

- **Trie-based lookup**: Simple character/word matches found fastest
- **Match patterns**: Regex-based rules (liblouis `match` opcode) checked after trie
- **Context patterns**: Regex-based rules (liblouis `context` opcode) checked last
- **Precedence values**: Rules with higher precedence override lower ones
- **Feature flag**: `backwards_compatibility` mode affects rule selection (first rule wins vs last rule wins)

### Boundary Handling

Word boundaries are critical for rule matching:
- **Word boundaries**: Transitions between letters and non-letters
- **Number boundaries**: Transitions involving digits
- **Punctuation boundaries**: Transitions involving punctuation
- Separate tries for nocross rules (hyphenation-aware)

### Multi-stage Pipeline

Rules are segregated by opcode to execute in correct order:
1. **Correct rules**: Text replacements before main translation
2. **Primary rules**: Main character/word translations
3. **Pass2/Pass3/Pass4**: Post-processing rules

This order is crucial for correct braille generation and must be reversed for backward translation.

## Key Implementation Notes

### Testing Against liblouis

Tests use YAML files from the liblouis project. Current gaps:
- Emphasis indication (partial support)
- Compound-word contraction across a nocross hyphen (see `TODO.org`'s "Known gaps")
- Some backward translation rules
- Computer braille (incomplete)
- Input/output position mapping (typeforms and cursors)

See `TODO.org` for detailed tracking.

### Performance Considerations

- Trie lookup is O(n) where n is the length of the pattern
- Regexp compilation is done at table load time
- Translation is done in a single pass through the pipeline
- Word boundary detection uses Unicode segmentation

### Known Limitations (from TODO.org)

- Emphasis indication not fully implemented
- nocross-driven hyphenation dictionary lookup works natively (`src/hyphenation.rs`), but compound-word contraction across a hyphen still has a known gap
- Some backward translation modes missing
- Computer braille incomplete
- Regexp infinite loop potential (needs guarding against recursive patterns)

## Relevant Files for Common Tasks

| Task | Files |
|------|-------|
| Add new opcode support | `src/parser.rs` (parsing) + `src/translator/table/primary.rs` (compilation) |
| Fix translation bug | `src/translator/trie.rs` or `src/translator/regexp.rs` depending on rule type |
| Add test harness | `src/yaml.rs`, `src/test.rs` |
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

- **Parser is two-pass**: First pass collects context, second pass uses it
- **Compilation is separate from translation**: Rules are compiled once at load time
- **Data structures optimize for speed**: Tries for fast lookups, VMs for regex
- **Features are isolated**: Indicators, effects, and stages are modular
- **Error handling**: Uses `thiserror` for ergonomic error types

