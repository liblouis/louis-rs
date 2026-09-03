# louis-rs vs. liblouis

louis-rs is a from-scratch Rust reimplementation of [liblouis](https://liblouis.io/) —
not a port. Different data structures, different algorithms, same tables and the same
test suite.

This document says what it can do today, what it can't yet, and where the remaining
work is, so you can tell whether it's usable for your case and pick something up if
it isn't.

Measured at commit `83f6b1d` against liblouis's own suite:

|                                     |           |
|-------------------------------------|-----------|
| Assertions run                      | 2 224 651 |
| Pass                                | **98.6%** |
| Fail                                | 0.8%      |
| Expected failure                    | 0.5%      |
| Top-level tables that parse cleanly | 265 / 265 |

Forward and backward combined. Reproduce with:

```sh
export LOUIS_TABLE_PATH=~/src/liblouis/tables:~/src/liblouis
cargo run --release -- check \
    ~/src/liblouis/tests/braille-specs/*.yaml \
    ~/src/liblouis/tests/yaml/*.yaml --summary
```

## What works today

**Translation, both directions.** The full multi-stage pipeline: `correct` rules
before the main stage, character and pattern rules in it, `pass2`–`pass4` after —
reversed for back-translation.

**Every table under `tables/` parses.** All 265 of them. The three that used to fail
turned out to be table-authoring bugs rather than louis-rs gaps, and are fixed
upstream ([#2070](https://github.com/liblouis/liblouis/issues/2070),
[#2071](https://github.com/liblouis/liblouis/issues/2071),
[#2072](https://github.com/liblouis/liblouis/issues/2072)).

**Contractions and the word-family opcodes** — `word`, `begword`, `midword`,
`endword`, `midendword`, `prfword`, `sufword`, `partword`, `begmidword`, with their
`before`/`after` class constraints.

**`match` and `context` rules** via a purpose-built regexp VM (`src/translator/regexp.rs`)
that compiles patterns at table load time and matches in linear time.

**Braille indicators.** Number sign (`numsign`/`nonumsign`), capitalization at all
three tiers — `capsletter`, `begcapsword`/`endcapsword`, and caps passages via
`begcaps`/`endcaps` or `begcapsphrase`/`endcapsphrase`/`lencapsphrase` — plus letter
sign, `nocontract`, and the generic `begmode`/`endmode` family.

**Emphasis.** `begemph`/`endemph` and the word/symbol tiers. liblouis's own
`new_emph.yaml` and `en-us-emphasis_harness.yaml` pass at 100%.

**Computer braille** as a full pipeline feature — `comp6`, `compbrl`,
`begcomp`/`endcomp`, and a scanner that derives computer-braille spans from
table-defined trigger characters.

**Backward indication** for `capsletter`, `begcapsword`/`endcapsword` and
`numsign`/`nonumsign`, including the case where a table's indicator dots collide with
one of its own script's letters (Malayalam, Punjabi).

**Hyphenation, natively.** liblouis `.dic` pattern files are parsed directly
(`src/hyphenation.rs`) — no external crate, no build or conversion step. Resolved
through the same search path as any other table.

**Position mapping.** `inputPos`, `outputPos` and `cursorPos`, composed across every
pipeline stage in both directions. One known mismatch remains in the whole suite.

**Table metadata queries** — find tables by language, contraction grade and the rest.

**A CLI** with `parse`, `translate`, `trace` (shows every rule that fired), `check`
(runs YAML suites) and `query`.

## What doesn't work yet

Honest list, no dates attached:

- **The library API is not stable.** Expect breaking changes.
- **No C ABI and no bindings.** Rust consumers only, for now. PyO3 bindings are
  proposed in [#1](https://github.com/liblouis/louis-rs/issues/1) /
  [PR #12](https://github.com/liblouis/louis-rs/pull/12).
- **Two screen-reader modes are missing**: `partialTrans`
  ([#19](https://github.com/liblouis/louis-rs/issues/19)) and
  `compbrlAtCursor`/`compbrlLeftCursor`
  ([#20](https://github.com/liblouis/louis-rs/issues/20)). The option flags are
  accepted; nothing reads them yet.
- **Back-translation returns text but not typeform.** `TranslationResult::emphasis`
  is never populated, and nothing reports whether a back-translation result could
  still change if more cells arrive
  ([#29](https://github.com/liblouis/louis-rs/issues/29)).
- **No Unicode normalization.** NFC was implemented and reverted — it fixed tables
  with precomposed-character rules and broke tables with standalone
  combining-character rules (IPA, Hebrew, Yiddish). NFD is the proposed path.
- **Six opcodes are deliberately not implemented**: `uplow`, `locale`, `backmatch`,
  `compdots`, `nobreak` and `macro`. Two are deprecated upstream, three are
  undocumented in liblouis's own manual, and none appears in any table or test in
  liblouis's corpus. See the ADR of that name in
  [Architecture_Decision_Records.org](Architecture_Decision_Records.org).

## Where the remaining failures are

Most of the failing 0.8% sits in ten files, and several of those share a root cause.
Sizes are per item — *S* is up to a week, *M* about a week, *L* one to three weeks —
and are there so you can pick work by appetite, not to add up. Failure counts are
approximate: `check` reports a per-file percentage, so a count derived from it carries
a few hundred either way on the million-assertion files.

| Area                                                | Failing | Root cause                                                                                                                                                                       | Size |
|-----------------------------------------------------|--------:|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|------|
| `en-ueb-g2-dictionary_harness`, `afr-za-g2` forward |  ≈4 500 | Match-rule candidate selection: rule weight vs. table-definition order. Where two `match` rules both validate, liblouis returns the first in definition order and we favour the later. ADR still DRAFT — the direct fix regresses the suite by exposing a deeper gap. | L    |
| `afr-za-g2` backward                                |  ≈2 500 | Afrikaans's lower-sign contractions share dot patterns with punctuation, and backward boundary checks classify a cell by its dots.                                               | M    |
| `ml`, `pa`, `hi`, `ta`, `fa`, `th`, `bn`            |  ≈2 600 | Complex scripts: conjunct and reph forms, and stacked combining marks firing an indicator each instead of once per grapheme cluster.                                             | L    |
| `da-dk-g28-dictionary_harness` (+ `_1993`)          |  ≈1 900 | `letsign`, which are also used as the lead cell of some two-cell definitions.                                                                                                    | M    |
| `en-ueb-computer-code`                              |  ≈1 700 | Three bugs: letter sign fires only on the first occurrence in a run; wrong word sign before hyphen or closing bracket; leading indicator dropped at the very start of the input. | M    |
| `hu-hu-g1_dictionary_special_consonants`            |  ≈1 100 | `nocross` asks "can this hyphenate anywhere?" instead of liblouis's positional `syllableBreak()`.                                                                                | L    |
| `sw-ke-dictionary`, `sw-ke`                         |    ≈870 | Backward lookup splices in unrelated dictionary words ("wote" → "wizaraote"); single-letter words pick up a spurious letter sign.                                                | M    |
| `en-g3`, `en-ueb`                                   |    ≈600 | A literal hyphen breaks per-segment contraction in compounds — "do-it-yourself" gives `do-x-y\|rself`, not `d-x-yrf`.                                                            | M    |
| `rw-rw-g1`, `numericmode`                           |    ≈500 | Letters used as pseudo-digits still get a capital sign. Related: `endmode`/`endmodeword` don't fire across a `base` character chain.                                             | M    |
| `de-g0-detailed-specs`, `de-g1-…`                   |    ≈280 | `capsletter` vs. `begcapsword` tier selection. Needs `uppercase.rs` and `emphasis.rs` unified into one tier model — a blanket engine fix was tried and disproven.                | L    |
| all other files                                     |  ≈2 600 | Roughly 100 files, each contributing well under 350.                                                                                                                             | —    |

Two cross-cutting items have no single file behind them:

- **Backward direction, generally.** The boundary predicates (`word_start` and
  friends) only work forward, and caps passages and emphasis have no backward path at
  all.

  Underneath that sits one specific divergence worth fixing properly, because it has
  now been patched three times for three different opcode families. We evaluate *both*
  sides of a backward boundary check against the braille input, using a class table
  (`dots_classes`) populated from character definitions plus whichever contraction
  opcodes have been added to it so far — so whenever a check meets a family nobody
  added yet, it sees no letter and the rule silently never fires. liblouis is
  asymmetric instead (`lou_backTranslateString.c`):

  - the **before** side reads the last character of the *output produced so far* and
    classifies it with the text classes (`back_setBefore`, `isBegWord` — "look at what
    has already been translated")
  - the **after** side reads the next *braille cell* and classifies it with the dots
    classes, but when that cell isn't a letter by attribute it walks the rules attached
    to the cell and treats any multi-character contraction as word-continuation
    (`isEndWord`, excluding `begword`/`midword` per liblouis#360)

  So liblouis never needs contraction dots in its letter class; our class-table
  patching is an approximation of that rule walk. Matching the real structure removes
  the whole recurring bug family. *(L)*
- **Engine debt.** Regexp negation of concatenation and alternation is wrong
  (`!(e1e2)` ≠ `(!e1!e2)`); `Attribute::ByOrder` and `Any` are no-ops in
  `match_pattern.rs` and all three are no-ops in `context_pattern.rs`; the
  `syllable` opcode's cross-boundary restriction is unimplemented. *(M)*

## Twenty-five files that never run

These error out during table compilation, so they contribute neither passes nor
failures — they're the one genuine unknown here, and the cheapest work in this
document.

| Reason                                    | Files | What it needs                                                                                                                                    | Size |
|-------------------------------------------|------:|--------------------------------------------------------------------------------------------------------------------------------------------------|------|
| "Table queries have not been implemented" |    10 | The YAML harness can't resolve a `table:` given as a metadata query. The machinery already exists behind the `query` subcommand.                 | S    |
| `TableNotFound` on a table list           |     5 | `.uti` files next to the YAML aren't found; lists resolve against `LOUIS_TABLE_PATH` only. [#15](https://github.com/liblouis/louis-rs/issues/15) | S    |
| Parser gaps                               |     3 | An escaped `"\\"` in a multipass operand (`it-it-comp6.utb:248`), UTF-16 surrogate-pair escapes, and an `EmptyTest` in the multipass tests.      | S    |
| Hyphenation dictionaries                  |     2 | `hyph_en_US.dic` rejected for its ISO-8859-1 encoding; `hyphenation.dic` not found.                                                              | S    |
| `ImplicitCharacterNotDefined('ƒ')`        |     1 | Swedish. Probably a table-side bug worth reporting upstream.                                                                                     | S    |
| `macro.utb`                               |     1 | Deliberate non-goal.                                                                                                                             | —    |

Separately, the `hyphenate`, `hyphenateBraille` and `display` test modes fall through a
catch-all arm in `src/test.rs` and silently report nothing at all. `de-eurobrl6.yaml`
and `hu-hu-g1-hyph_harness.yaml` consist only of such tests and so report zero tests
run; `issue-332.yaml` has its `hyphenate` block skipped this way while its remaining 19
tests all fail. Two more of this kind are already counted above, blocked earlier by the
hyphenation dictionaries. *(S)*

## Missing API surface

| Capability                                                                                      | Status                                                                                                                                                    | Size   |
|-------------------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------|--------|
| `partialTrans` ([#19](https://github.com/liblouis/louis-rs/issues/19))                          | Flag accepted, nothing reads it. A screen reader sets it on every keystroke of contracted input; without it, word-final rules fire on an unfinished word. | M      |
| `compbrlAtCursor` / `compbrlLeftCursor` ([#20](https://github.com/liblouis/louis-rs/issues/20)) | Flags accepted, nothing reads them. Computer-braille rendering already works, so this is cursor plumbing.                                                 | M      |
| Back-translation finality ([#29](https://github.com/liblouis/louis-rs/issues/29))               | Nothing says whether more cells could change the characters already produced.                                                                             | M      |
| Display tables in the pipeline                                                                  | Only the YAML harness can supply one.                                                                                                                     | M      |
| `hyphenate`, `charToDots`, `dotsToChar`, `getEmphClasses`                                       | Working internals, nothing exposed on `Translator`.                                                                                                       | S each |
| Caller-controlled table resolution ([#16](https://github.com/liblouis/louis-rs/issues/16))      | In flight — `with_search_path` has landed; [PR #28](https://github.com/liblouis/louis-rs/pull/28) proposes a resolver abstraction.                        | S–M    |
| Multipass `*` action ([#21](https://github.com/liblouis/louis-rs/issues/21))                    | Keeps the matched context in the stream, so move and swap rules duplicate characters. [PR #22](https://github.com/liblouis/louis-rs/pull/22) open.        | S      |
| C ABI                                                                                           | Not started. cbindgen or Diplomat.                                                                                                                        | M–L    |

## Help wanted

Good entry points, roughly easiest first:

1. **Make the suite deterministic.** Repeated `check` runs of the same binary
   disagree by about ten tests on `pa.yaml` and `fr-bfu-g2.yaml`, from `HashMap`
   iteration order in `src/translator/trie.rs` (see the `FIXME` at line 81). Ten
   tests is exactly the size of a real regression, so this one blocks trusting every
   other number here. The `BTreeMap` fix is measured and parked. *(S)*
2. **Wake up the 25 files that don't run.** Six independent small fixes, listed
   above, and nobody knows what those tests will report. *(S each)*
3. **The three parser gaps.** Small, concrete, and each has a failing file to check
   against. *(S)*
4. **Expose the four missing entry points.** Mechanical. *(S each)*
5. **Survey an unclassified failure cluster** — `afr-za-g2` is the largest one left,
   and the residual Danish failures need re-surveying now that the `partword` cause is
   out of the way. Reading failure output rather than deep engine work, and the last
   two clusters that got this treatment each turned out to be one root cause rather
   than a tail. *(M)*

The two architectural items — match-rule candidate selection, and unifying the
capitalization and emphasis tier models — each already have one failed attempt behind
them, so they're poor first contributions but well documented in
[Architecture_Decision_Records.org](Architecture_Decision_Records.org). Read the
relevant ADR before starting; both record an approach that was tested and disproven.

## Also open

- Rule-level coverage tool for the YAML suite
  ([#27](https://github.com/liblouis/louis-rs/issues/27)) — ADR accepted, prototype
  parked
- gzip+bincode table bundling ([PR #10](https://github.com/liblouis/louis-rs/pull/10))
- Backward `capsletter`/`begcapsword` failures in `hu-hu-g1`
  ([#4](https://github.com/liblouis/louis-rs/issues/4))
- Performance has never been measured. Offset buckets for translations and a 32-bit
  regexp operand are the two ideas on file; benchmarking comes first.

---

Working notes live in [TODO.org](../TODO.org); design decisions and the reasoning
behind them in
[Architecture_Decision_Records.org](Architecture_Decision_Records.org).
