# louis-rs: a liblouis re-implementation in Rust

louis-rs is a clean-room re-implementation of [liblouis](https://liblouis.io/),
the braille translator and back-translator, in Rust. It reads the same tables and
passes the same tests, but it is not a port: the data structures and the
translation algorithm are new, and the manual memory management behind most of
liblouis's CVEs is gone.

```shell
$ export LOUIS_TABLE_PATH=~/src/liblouis/tables:~/src/liblouis
$ louis translate en-us-g2.ctb "It's about the blind"
⠠⠭⠄⠎⠀⠁⠃⠀⠮⠀⠃⠇
```

> [!CAUTION]
> louis-rs is in alpha. It passes over 98% of the liblouis test suite, but the
> library API has not been worked out and is not stable.

## What is here

-   a hand-rolled two-pass parser for liblouis tables; all 458 table files under
    `tables/` parse cleanly
-   forward and backward translation through the full pipeline: correct rules,
    the main stage, and pass2/pass3/pass4
-   a virtual-machine regexp engine behind the `match` and `context` opcodes, and
    a trie for everything else
-   hyphenation straight from liblouis's `.dic` files, with no external crate and
    no build step
-   the `louis` binary: `translate`, `trace`, `parse`, `check` and `query`; see
    `louis help`
-   [doc/Liblouis_Parity.md](doc/Liblouis_Parity.md) — what works, what doesn't,
    and where the remaining failures are
-   [doc/Differences_From_Liblouis.md](doc/Differences_From_Liblouis.md) — where
    louis-rs deliberately does something other than what liblouis does, notably
    table lookup and display tables
-   [doc/Architecture_Decision_Records.org](doc/Architecture_Decision_Records.org)
    — why the design is the way it is

## Build and try louis-rs

You need the [Rust tool chain](https://www.rust-lang.org/). Then:

    $ cargo install louis-rs

Point `LOUIS_TABLE_PATH` at a set of tables and translate something:

    $ export LOUIS_TABLE_PATH=~/src/liblouis/tables:~/src/liblouis
    $ louis translate de-comp6.utb
    > Guten Tag
    ⠈⠛⠥⠞⠑⠝⠀⠈⠞⠁⠛

`trace` shows which rule produced each cell, and in which stage of the pipeline:

    $ louis trace en-us-g2.ctb
    > It's about the blind
    ⠠⠭⠄⠎⠀⠁⠃⠀⠮⠀⠃⠇
    ┌───┬───────┬─────┬─────────────────┬───────┐
    │   │ From  │ To  │ Rule            │ Stage │
    ├───┼───────┼─────┼─────────────────┼───────┤
    │ 1 │       │ ⠠   │ capsletter ⠠    │ Main  │
    │ 2 │ it's  │ ⠭⠄⠎ │ word it's ⠭⠄⠎   │ Main  │
    │ 3 │       │ ⠀   │ space   ⠀       │ Main  │
    │ 4 │ about │ ⠁⠃  │ word about ⠁⠃   │ Main  │
    │ 5 │       │ ⠀   │ space   ⠀       │ Main  │
    │ 6 │ the   │ ⠮   │ largesign the ⠮ │ Main  │
    │ 7 │       │ ⠀   │ space   ⠀       │ Main  │
    │ 8 │ blind │ ⠃⠇  │ word blind ⠃⠇   │ Main  │
    └───┴───────┴─────┴─────────────────┴───────┘

`check` runs liblouis's YAML test suites:

    $ louis check --summary ~/src/liblouis/tests/braille-specs/de-de-comp8.yaml
    ┌──────────────────┬───────┬───────────┬──────────┬──────────┬────────────┬────────────┐
    │ YAML File        │ Tests │ Successes │ Failures │ Expected │ Unexpected │ Position   │
    │                  │       │           │          │ Failures │ Successes  │ Mismatches │
    ├──────────────────┼───────┼───────────┼──────────┼──────────┼────────────┼────────────┤
    │ de-de-comp8.yaml │ 8     │ 100.0%    │ 0.0%     │ 0.0%     │ 0.0%       │ 0          │
    ┌──────────────────┌───────┌───────────┌──────────┌──────────┌────────────┌────────────┐
    │ Total            │ 8     │ 100.0%    │ 0.0%     │ 0.0%     │ 0.0%       │ 0          │
    └──────────────────└───────└───────────└──────────└──────────└────────────└────────────┘

and `query` finds tables by their metadata:

    $ louis query language=de,contraction=full
    {"[...]/liblouis/tables/de-g2-detailed.ctb", "[...]/liblouis/tables/de-g2.ctb"}

## Project status

Alpha. Compatibility with liblouis is driven by its own test suite, which
louis-rs runs in full — better than two million assertions, forward and backward
— on every change; [doc/Liblouis_Parity.md](doc/Liblouis_Parity.md) carries the
current figures and the reproduction steps.

## Contributing

If you have any improvements or comments please feel free to file a
pull request or an issue.

## Acknowledgments

A lot of inspiration for the hand-rolled parser comes from the
absolutely fantastic book [Crafting Interpreters](https://craftinginterpreters.com/) by Robert Nystrom.
Surely [Structure and Interpretation of Computer Programs](http://mitpress.mit.edu/9780262510875/structure-and-interpretation-of-computer-programs/) has had some
influence as must have the [Compiler Construction](https://people.inf.ethz.ch/wirth/CompilerConstruction/CompilerConstruction1.pdf) classes with Niklaus
Wirth ("as simple as possible but not simpler").

The parser is built from the grammar used in [tree-sitter-liblouis](https://github.com/liblouis/tree-sitter-liblouis),
which is a port of the [EBNF grammar](https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_form) in [rewrite-louis](https://github.com/liblouis/rewrite-louis), which in turn is
a just port of the [Parsing expression grammar](https://en.wikipedia.org/wiki/Parsing_expression_grammar) from [louis-parser](https://github.com/liblouis/louis-parser).

## License

Copyright (C) 2023-2026 Swiss Library for the Blind, Visually Impaired
and Print Disabled

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation, either version 2.1 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with this program.  If not, see
<https://www.gnu.org/licenses/>.
