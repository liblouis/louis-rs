

# Changelog


## 0.2.4 - 2026-01-16


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


### Changed


### Deprecated


### Removed

-   The NFA based regular expression engine


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

-   Fix an infinite loop when back-translating `correct` rules


## 0.2.1


### Changed

-   Make it clear that the library API is unstable

