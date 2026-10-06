# Deliberate differences from liblouis

louis-rs aims to be as compatible with [liblouis](https://liblouis.io/) as makes
sense: same tables, same YAML tests, same output. This document collects the
places where it deliberately does something else, and why.

Gaps that are merely unfinished work live in
[Liblouis_Parity.md](Liblouis_Parity.md) instead. Everything here is a decision,
not a to-do.

Each difference names its **migration path**: how someone moving off liblouis
gets the output they had. A difference without one is not finished
([ADR-0018](adr/0018-parity-or-migration-path.org)), and "none yet" below marks
the work that is still owed.

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

**Migration path:** none yet. Rename `LOUIS_TABLEPATH` to `LOUIS_TABLE_PATH`,
replace its commas with colons, and name the install directory explicitly if you
relied on it. louis-rs does not yet notice a `LOUIS_TABLEPATH` it is ignoring;
a diagnostic for that is the missing piece.

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

**Migration path:** name the display table. The liblouis YAML tests that relied
on the fallback are being changed to name theirs (liblouis
[#2102](https://github.com/liblouis/liblouis/pull/2102) fixed three; more remain).
For an application, none yet: louis-rs does not say when a translation table
brings display rules that it is ignoring.

## Rules that tie

When two rules match at the same position, consume the same characters and check
the same amount of context, liblouis takes the one defined first in the table.
louis-rs does not use table order. Such a tie is an ill-formed table, two rules
with nothing to choose between them, and louis-rs resolves it arbitrarily but
reproducibly ([ADR-0016](adr/0016-order-independent-rule-selection.org)).

**Migration path:** fix the table so the rules no longer overlap, which fixes
liblouis too. The suite reaches ties in 14 tables, listed in ADR-0016; those
are still to be fixed upstream.

## Attributes `$w`, `$x`, `$y` and `$z`

In multipass and `correct`/`context` rules, `$w` to `$z` name the first four
user-defined character classes by the order of their `attribute` rules. louis-rs
does not implement them. They are slated for removal upstream
([liblouis#948](https://github.com/liblouis/liblouis/issues/948)), because a rule
whose meaning depends on declaration order is brittle.

**Migration path:** rewrite the rules with the class's own name. Still to be done
upstream for `da-dk-g16`, `da-dk-g26` and their variants, `es-g2.ctb`,
`sv-6common.uti`, `th-g1.utb`, `th-g1.uti`, `th-g2.ctb` and the `zhcn` tables.
Until then those rules are silently dropped.

## Opcodes not implemented

`uplow`, `locale`, `backmatch`, `compdots`, `nobreak` and `macro` are not
implemented: two are deprecated upstream, three are undocumented in liblouis's own
manual, and none appears in any table or test in liblouis's corpus
([ADR-0012](adr/0012-unimplemented-opcodes.org)).

**Migration path:** not needed for the corpus. A table using one fails to load
with `Opcode expected, got Some("uplow")`.
