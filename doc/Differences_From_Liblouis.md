# Deliberate differences from liblouis

louis-rs aims to be as compatible with [liblouis](https://liblouis.io/) as makes
sense: same tables, same YAML tests, same output. This document collects the
places where it deliberately does something else, and why.

Gaps that are merely unfinished work live in
[Liblouis_Parity.md](Liblouis_Parity.md) instead. Everything here is a decision,
not a to-do.

## Table lookup

Tables are looked up in `LOUIS_TABLE_PATH`. This is **not** liblouis's
`LOUIS_TABLEPATH`: the names differ by an underscore, and so does the format.
`LOUIS_TABLE_PATH` is separated by the platform path separator, a colon on Unix,
the way `PATH` is; `LOUIS_TABLEPATH` is separated by commas. Exporting one has no
effect on the other, and a comma separated value handed to louis-rs is read as a
single directory whose name contains commas.

They are kept apart deliberately, because the separator is not the only
difference: louis-rs does not search liblouis's compiled-in install locations,
nor the `<dir>/liblouis/tables/` form of each entry, so the same directories can
legitimately resolve differently under the two implementations.

A table named on the command line is looked up in its own directory first, so a
table can include one sitting next to it without that directory being on
`LOUIS_TABLE_PATH`:

```shell
$ louis parse /some/where/top.utb   # finds an "include base.utb" next to it
```

That first step belongs to the command line tool. The library resolves names
against the search path it is handed and nothing else, so an application owning
its own tables passes their directories to `Translator::with_search_path`.

## Display tables

A display table maps braille cells to the characters they are shown as. liblouis
takes it from the first table of a table list, so a table that includes one of
its own sets it: `da-dk-g28.ctb` includes `da-dk-octobraille.dis`, and liblouis
therefore renders Danish braille in that CP1252 encoding.

louis-rs does not. Braille is read and written as Unicode braille (U+2800) unless
a display table is named explicitly, whether or not the translation table
includes one:

```shell
$ louis translate da-dk-g28.ctb "ørene"
⠪⠷⠫
$ louis translate --display da-dk-octobraille.dis da-dk-g28.ctb "ørene"
øàë
```

Unicode braille is the more useful default for reading dot patterns off a
terminal, and coupling the display table to the translation table is a liblouis
design wart rather than something worth reproducing. The library takes one
through `TranslationPipeline::with_display`, and the YAML test harness through a
test's `display:` key.

The mapping is a stage of the pipeline like any other, so `trace` names the
`display` rule behind each character:

```shell
$ louis trace --display da-dk-octobraille.dis da-dk-g28.ctb "ørene"
øàë
┌───┬──────┬────┬───────────────┬─────────┐
│   │ From │ To │ Rule          │ Stage   │
├───┼──────┼────┼───────────────┼─────────┤
│ 1 │ ø    │ ⠪  │ lowercase ø ⠪ │ Main    │
│ 2 │ re   │ ⠷  │ partword re ⠷ │ Main    │
│ 3 │ ne   │ ⠫  │ partword ne ⠫ │ Main    │
│ 4 │ ⠪    │ ø  │ display ø ⠪   │ Display │
│ 5 │ ⠷    │ à  │ display à ⠷   │ Display │
│ 6 │ ⠫    │ ë  │ display ë ⠫   │ Display │
└───┴──────┴────┴───────────────┴─────────┘
```

Without `--display` there is no such stage, so the trace is unchanged and the
output stays Unicode braille.
