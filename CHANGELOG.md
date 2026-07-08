# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/)
and this project adheres to [Semantic Versioning](https://semver.org/).

<!-- next-header -->

## [Unreleased] - ReleaseDate

### Changed
- `TranslationOptions` fields are now private. Construct it with
  `TranslationOptions::default()` and the `with_mode`/`with_emphasis`/
  `with_cursor_pos` builder methods, and read it back with the `mode`/
  `emphasis`/`cursor_pos` accessors.
- The `check` subcommand now runs YAML test files concurrently, and further
  parallelizes across the individual tests within each file/table
  combination, using `rayon`.
- Hyphenation dictionaries are now parsed natively (`src/hyphenation.rs`)
  instead of via the `hyphenation` crate. A `.dic` hyphenation-pattern file is
  resolved through the same `LOUIS_TABLE_PATH` search path as every other
  liblouis table, with no separate build/conversion step required.
  `LEFTHYPHENMIN`/`RIGHTHYPHENMIN`, unsupported encodings, and patterns
  carrying no hyphenation value now fail to parse instead of being silently
  accepted.

### Added
- Add `louis-py`, Python bindings for the translator built with
  [PyO3](https://pyo3.rs/). The bindings live in a new Cargo workspace
  member (`louis-py/`) and ship as the `louis-py` package (import name
  `louis_py`) with type stubs and a `py.typed` marker.
- Implement indication handling for backward-translation of `capsletter`,
  `begcapsword`/ `endcapsword`, and `numsign`/`nonumsign`. An opcode is only
  recognized as an indicator when its dots aren't already claimed by a real
  letter, since some tables (Malayalam, Punjabi) share a subtable whose
  `capsletter`/`numsign` coincide with one of their own script letters.
- Implement `begcaps`/`endcaps` and
  `begcapsphrase`/`endcapsphrase`/`lencapsphrase` caps-passage indication. A
  passage of two or more (or `lencapsphrase`, if set) consecutive
  whole-uppercase words is now wrapped in a single indicator instead of marking
  each word individually; hyphenated segments of one word don't count toward the
  threshold, and `begcapsphrase` is preferred over `begcaps` when both are
  defined.

### Fixed
- A `nocross` rule is now only rejected when it would cross a hyphenation break
  of the *whole word* it's part of (mirroring liblouis's `syllableBreak`). This
  check is now also forward translation only, matching liblouis
  (back-translation never consults it).
- YAML test files' `display`/`table` fields are now correctly recognized as an
  inline table by their YAML scalar style (block literal `|` or folded `>`)
  instead of guessing from whether the value happens to contain a newline.
- The `display` opcode's dots-to-character mapping is now also built correctly
  for backward translation.
- The `display` opcode's dots operand is now parsed as a single braille cell,
  as required by the liblouis spec.
- `dots_classes`'s `letter` class (used for backward-translation word-boundary
  checks) now also includes the dots of alphabetic `always` rules, not just
  `letter`/`lowercase`/`uppercase` opcodes.
- The `match`/`context` pattern parser now scopes `|` (alternation) over the
  whole surrounding sequence of tokens, matching standard regex precedence
  (concatenation binds tighter than alternation) and liblouis's own semantics.
- `base uppercase`/`base lowercase` (case-pairing) rules now also register the
  derived character in the `letter` character class, matching what the direct
  `uppercase`/`lowercase` opcodes already do.
- `partword` rules now fire when the pattern is preceded or followed by a letter
  (i.e. anywhere inside or adjacent to a word), matching the liblouis semantics.
- Unicode NFC normalization is no longer applied to input text or trie patterns.
  The normalization was causing spurious mismatches for tables that define rules
  for standalone combining characters (IPA, Hebrew, Yiddish).
- Whitespace detection now uses the table's own `space` class instead of Rust's
  built-in `char::is_whitespace()`.
- `before CLASS always` and `after CLASS always` constraints are now enforced
  during translation.
- `before`/`after` class constraints now work correctly in backward translation.
  Constraints now check against the corresponding braille character set when
  translating backward.
- Letter-sign isolation was incorrectly firing for isolated letters preceded by
  another letter (e.g. the final letter of `:bc`) or preceded by a space (word
  boundary, e.g. the "I" in "I'm"). The condition is now: fire only when the
  letter is isolated (not followed by another letter) AND its preceding
  character, if any, is neither a space nor a letter.
- The `` ` `` (beginning-of-input) and `~` (end-of-input) test anchors are
  now enforced for `context`/`correct`/`pass2`/`pass3`/`pass4` rules.
- The `^` boundary attribute inside `match` opcode patterns (e.g.
  `%[_~^]`, meaning "space, sequence-delimiter, or string boundary") is
  now enforced instead of silently contributing nothing.
- Fixed a latent infinite loop (stack overflow) in the regexp engine:
  a `*`/`+`-quantified pattern whose body always matches zero-width
  caused the matcher to recurse forever without ever advancing,
  instead of terminating after one attempt.
- `nocross` rules with no plain counterpart (the common case, e.g. liblouis'
  `nocross always en`) were never applied at all: they only competed for a
  position when a plain rule also matched there. They now compete for the
  position on their own.
- The `noletsign` opcode (letters that must never themselves be preceded by
  a letter sign) was parsed but silently ignored.
- A zero-length `context`/`match` action (e.g. liblouis' `$d[]$l`, used to
  insert a letsign between a digit and a single trailing letter) re-enters
  the translation loop at the same position without advancing, which caused
  any indicator scheduled there (letsign, numsign, capsletter, ...) to be
  emitted again on that second visit. Indicators are now emitted at most
  once per position.

## [0.2.8] - 2026-06-29

### Fixed
- `litdigit` rules now take precedence over `digit` rules in forward
  translation. In literary braille tables both opcodes are often present for the
  same character: `digit` carries the computer-style dot pattern used for
  backward translation, while `litdigit` carries the literary form used in
  numeric mode. Previously the winner was determined by insertion order, which
  caused Hungarian numbers to be translated with the wrong dot pattern.
- `\xHHHH` Unicode escape sequences in the test and action operands of
  `correct`, `pass2`, `pass3`, and `pass4` rules are now decoded correctly.
- `\xHHHH` escape sequences in YAML test file expected values are now decoded.
  liblouis test files use this non-standard syntax even in single-quoted YAML
  strings; our test runner now matches liblouis's own behaviour.

### Added
- Implement emphasis (typeform) indication: `emphletter`, `begemphword`,
  `endemphword`, `begemphphrase`, `endemphphrase`, `begemph`, `endemph`,
  `emphmodechars`, and `noemphchars` are all supported. When multiple emphasis
  classes open or close at the same position their indicators are ordered
  according to the liblouis stack rule (the class that closes last opens first).
- Implement computer braille: `comp6` rules are now activated only inside
  computer braille regions instead of firing unconditionally. Computer
  braille regions are detected from the `computer_braille` typeform and
  from `compbrl` pattern matches (which expand to the surrounding word).
  `begcomp` and `endcomp` indicators are emitted at region boundaries.
- The library now has a basic API that covers both simple and advanced use
  cases. See `translator.translate` and `translator.translate_with_options` in
  `lib.rs`.
- Implement (at least partly) the `largesign` opcode.
- Implement the basic behaviour of the `decpoint` and `hyphen` opcode.
- Add `trace` as a separate command (instead of `translate --tracing`)
- Add an option to change the style of the traces
- Normalize all input text to NFC Unicode form before translation. Users can
  supply NFD or NFC text and get correct output for tables that define rules for
  precomposed characters. Note: tables that define rules for standalone
  combining characters (IPA, Hebrew, Yiddish) are currently broken by NFC.

### Changed
- Braille indication (numeric, uppercase, letter sign, no-contract) is now
  pre-computed from the input text before the translation loop begins. Each
  indicator runs as a single pass over the input and records which events fire
  at each character position; the translation loop consults an immutable
  per-position array instead of advancing mutable state machines in parallel.
  User-supplied typeform annotations (`TextAttribute`) are merged into the same
  array, unifying built-in and typeform indication under one mechanism.
- `always` rules now correctly take precedence over character-definition
  opcodes (`letter`, `punctuation`, `sign`, etc.), matching liblouis
  behaviour. Previously both had equal precedence, so whichever appeared
  first in the table won — meaning `always` rules intended to override a
  character definition were silently ignored when the definition came from
  an included file. This fixes translation for many tables.
- When showing a trace of a translation show all rules in one table,
  instead a table for each stage.
- `begnum` now correctly matches non-word indicator characters (e.g.
  `#`, `(`) preceded by space or punctuation, matching the C liblouis
  behaviour (`beforeAttributes & (CTC_Space | CTC_Punctuation)`).
- `endnum` now correctly matches any indicator character preceded by a
  digit, with no constraint on what follows, matching the C liblouis
  behaviour (`beforeAttributes & CTC_Digit`).

### Fixed
- Handle numbered character classes in match regular expressions
  correctly instead of treating them as by order of definition
  character class.
- Make sure the matching of word and always rules is case insensitive
  (and gets precedence).
- Fix midnum handling: multiple `midnum` opcodes now all take effect
  (previously each one overwrote the previous), and the look-ahead
  correctly checks the character after the midnum character.

## [0.2.7] - 2026-02-10

### Added
- Implement regexp grouping as it is used in match opcodes.
- Add support for the `prepunc` and `postpunc` opcode.
- Implement regexp negation simply by rewriting the regexp AST into
  more basic negations like *not a character* or *not a character
  class*.

### Changed
- Convert all the Braille types into their own types using the [new
  type pattern](https://effective-rust.com/newtype.html).
- Since we no longer combine all regexps into one big one each regexp
  now just optionally returns one Translation. So
  `CompiledRegexp::find` returns Option now instead of a (possibly
  empty) Vec.

### Fixed
- Fixed the parsing of in alternation expressions `match` rules, i.e.
  `match a|b foo - 26`.
- Fix the parsing of the `swapdd` operands.
- Fix the parsing of the `grouping` braille tuple.
- Fix a problem with the '.' quantifier in Context and Multipass
  regular expressions.

## [0.2.6] - 2026-01-27

### Changed
- Migrate to `cargo release`.

## [0.2.5] - 2026-01-27

### Added
-   Add support for context opcodes. Everything works except for
    negation.
-   Add support for variables in context and multipass opcodes.
-   Add proper support to display `match`, `correct`, `context` and
    all the multipass rules.

## [0.2.4] - 2026-01-16
### Added
-   Add support for a translation pipeline. This simplifies the code and
    improves back-translation, where the pipeline is just applied in
    reverse.
-   Add support for multipass opcodes. Everything works except for
    negation and variables.
-   Migrate to a virtual machine based regular expression engine, because
    the NFA based engine was way too slow.
-   Add support for what liblouis calls *character attributes* in match
    patterns. They are essentially named character classes, like
    *digit*, *letter*, *lowercase*, etc.
### Removed
-   The NFA based regular expression engine.

## [0.2.3] - 2025-12-04
### Added
-   Implement support for the multipass opcodes, at least for the ones
    that do not require regular expressions, e.g. rules like
    
        pass2 @123 @15
    
    should work now.
-   Tracing now works across all translation stages. If any rules for
    pre and post translations are defined these translations are also
    shown.

## [0.2.2] - 2025-12-01
### Fixed
-   Fix an infinite loop when back-translating `correct` rules.

## 0.2.1
### Changed
-   Make it clear that the library API is unstable.

<!-- next-url -->
[Unreleased]: https://github.com/assert-rs/predicates-rs/compare/v0.2.8...HEAD
[0.2.8]: https://github.com/assert-rs/predicates-rs/compare/v0.2.7...v0.2.8
[0.2.7]: https://github.com/assert-rs/predicates-rs/compare/v0.2.6...v0.2.7
[0.2.6]: https://github.com/liblouis/louis-rs/compare/v0.2.5...v0.2.6
[0.2.5]: https://github.com/liblouis/louis-rs/compare/v0.2.4...v0.2.5
[0.2.4]: https://github.com/liblouis/louis-rs/compare/v0.2.3...v0.2.4
[0.2.3]: https://github.com/liblouis/louis-rs/compare/v0.2.2...v0.2.3
[0.2.2]: https://github.com/liblouis/louis-rs/compare/v0.1.0...v0.2.2
