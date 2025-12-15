

# Changelog


## Unreleased


### Added

-   Add support for what liblouis calls *character attributes* in match
    patterns. They are essentially named character classes, like
    *digit*, *letter*, *lowercase*, etc.


### Changed


### Deprecated


### Removed


### Fixed


### Security


## 0.2.3 - 2025-12-04


### Added

-   Implement support for the multipass opcodes, at least for the ones
    that do not require regular expressions, e.g. rules like
    
        pass2 @123 @15
    
    should work now.
-   Tracing now works across all translation stages. If any rules for
    pre and post translations are defined these translations are also
    shown.


## 0.2.2 - 2025-12-01


### Fixed

-   Fix an infinite loop when backtranslating `correct` rules


## 0.2.1


### Changed

-   Make it clear that the library API is unstable

