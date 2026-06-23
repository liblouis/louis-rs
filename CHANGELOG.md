# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/)
and this project adheres to [Semantic Versioning](https://semver.org/).

<!-- next-header -->

## [Unreleased] - ReleaseDate

### Added
- The library now has a basic API that covers both simple and advanced
  use cases. See `translator.translate` and
  `translator.translate_with_options` in `lib.rs`.
- Implement (at least partly) the `largesign` opcode.
- Implement the basic behaviour of the `decpoint` and `hyphen` opcode.
- Add `trace` as a separate command (instead of `translate --tracing`)
- Add an option to change the style of the traces
- Normalize all input text to NFC Unicode form before translation.
  Users can supply NFD or NFC text and get correct output for tables
  that define rules for precomposed characters. Note: tables that
  define rules for standalone combining characters (IPA, Hebrew,
  Yiddish) are currently broken by NFC; NFD normalization is being
  evaluated as a better alternative.

### Changed
- `always` rules now correctly take precedence over character-definition
  opcodes (`letter`, `punctuation`, `sign`, etc.), matching liblouis
  behaviour. Previously both had equal precedence, so whichever appeared
  first in the table won — meaning `always` rules intended to override a
  character definition were silently ignored when the definition came from
  an included file. This fixes translation for many tables; the overall
  test suite improves from 71.6% to 79.7%.
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
[Unreleased]: https://github.com/assert-rs/predicates-rs/compare/v0.2.7...HEAD
[0.2.7]: https://github.com/assert-rs/predicates-rs/compare/v0.2.6...v0.2.7
[0.2.6]: https://github.com/liblouis/louis-rs/compare/v0.2.5...v0.2.6
[0.2.5]: https://github.com/liblouis/louis-rs/compare/v0.2.4...v0.2.5
[0.2.4]: https://github.com/liblouis/louis-rs/compare/v0.2.3...v0.2.4
[0.2.3]: https://github.com/liblouis/louis-rs/compare/v0.2.2...v0.2.3
[0.2.2]: https://github.com/liblouis/louis-rs/compare/v0.1.0...v0.2.2
