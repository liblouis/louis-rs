# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/)
and this project adheres to [Semantic Versioning](https://semver.org/).

<!-- next-header -->

## [Unreleased] - ReleaseDate

### Added
- Implement regexp grouping as it is used in match opcodes.

### Changed
- Convert all the Braille types into their own types using the [new
  type pattern](https://effective-rust.com/newtype.html).

### Fixed
- Fixed the parsing of in alternation expressions `match` rules, i.e.
  `match a|b foo - 26`.
- Fix the parsing of the `swapdd` operands.
- Fix the parsing of the `grouping` braille tuple.

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
[Unreleased]: https://github.com/assert-rs/predicates-rs/compare/v0.2.6...HEAD
[0.2.6]: https://github.com/liblouis/louis-rs/compare/v0.2.5...v0.2.6
[0.2.5]: https://github.com/liblouis/louis-rs/compare/v0.2.4...v0.2.5
[0.2.4]: https://github.com/liblouis/louis-rs/compare/v0.2.3...v0.2.4
[0.2.3]: https://github.com/liblouis/louis-rs/compare/v0.2.2...v0.2.3
[0.2.2]: https://github.com/liblouis/louis-rs/compare/v0.1.0...v0.2.2
