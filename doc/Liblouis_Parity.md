# louis-rs vs. liblouis

louis-rs is a from-scratch Rust reimplementation of [liblouis](https://liblouis.io/) —
not a port. Different data structures, different algorithms, same tables and the same
test suite.

This document says what it can do today, what it can't yet, and where the remaining
work is, so you can tell whether it's usable for your case and pick something up if
it isn't.

Measured at louis-rs `a7b8e47` against liblouis `011cfb3b` plus
[liblouis#2176](https://github.com/liblouis/liblouis/pull/2176) and
[liblouis#2190](https://github.com/liblouis/liblouis/pull/2190). Without the first, four
YAML files (`en-g3`, `en-ueb`, `it`, `de-g2-sbs`) don't parse and drop out; without the
second, `ht-g1.utb` doesn't compile and `ht.yaml` drops out:

|                                |           |
|--------------------------------|-----------|
| Assertions run                 | 2 241 031 |
| Pass                           | **99.4%** |
| Fail                           | 0.1%      |
| Expected failure               | 0.5%      |
| Tables that compile            | all       |

Forward and backward combined. Reproduce the assertion figures with:

```sh
export LOUIS_TABLE_PATH=~/src/liblouis/tables:~/src/liblouis
cargo run --release -- check \
    ~/src/liblouis/tests/braille-specs/*.yaml \
    ~/src/liblouis/tests/yaml/*.yaml --summary
```

and the table figure, which compiles each table on its own in both directions as
liblouis's `lou_checktable` does, with:

```sh
cargo build --release
export LOUIS_TABLE_PATH=~/src/liblouis/tables
for f in ~/src/liblouis/tables/*.{ctb,utb,uti,tbl,cti,dis}; do
    ./target/release/louis check-table -q "$f" || echo "$f"
done
```

The few files it lists are fragments that only work when included; liblouis can't
compile them on their own either.

## What works today

**Translation, both directions.** The full multi-stage pipeline: `correct` rules
before the main stage, character and pattern rules in it, `pass2`–`pass4` after —
reversed for back-translation.

**Every table under `tables/` compiles.**

**Contractions and the word-family opcodes** — `word`, `begword`, `midword`,
`endword`, `midendword`, `prfword`, `sufword`, `partword`, `begmidword`, `lowword`,
`joinword`, with their `before`/`after` class constraints.

**Every opcode a liblouis table uses compiles.**

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

**Backward indication** for `capsletter`, `begcapsword`/`endcapsword`,
`numsign`/`nonumsign`, and the no-contract family `letsign`/`nocontractsign`/
`nonumsign` — the last of these both consumes the indicator cell and suppresses the
word-family rules in its scope. Emphasis indicators are consumed too, but the
emphasis itself is not reported, see below.

**Caller-controlled table resolution.** `Translator::with_search_path` takes the
directories to search, so a host managing its own table locations doesn't have to
mutate `LOUIS_TABLE_PATH`.

**Hyphenation, natively.** liblouis `.dic` pattern files are parsed directly
(`src/hyphenation.rs`) — no external crate, no build or conversion step. Resolved
through the same search path as any other table.

**Position mapping.** `inputPos`, `outputPos` and `cursorPos`, composed across every
pipeline stage in both directions. Three known mismatches remain in the whole suite.

**Braille encodings (display tables)** as a pipeline stage, applied last when
translating to braille and first when back-translating: `BrailleEncoding` and
`Translator::with_braille_encoding` in the library, `--braille-encoding` on `translate`
and `trace`.

**Table metadata queries** — find tables by language, contraction grade and the rest.

**A CLI** with `check-table`, `translate`, `trace` (shows every rule that fired), `check`
(runs YAML suites) and `query`.

## What doesn't work yet

Honest list, no dates attached:

- **No C ABI and no bindings.** Rust consumers only, for now. PyO3 bindings are
  proposed in [#1](https://github.com/liblouis/louis-rs/issues/1) /
  [PR #12](https://github.com/liblouis/louis-rs/pull/12).
- **Two screen-reader modes are missing**: `partialTrans`
  ([#19](https://github.com/liblouis/louis-rs/issues/19)) and
  `compbrlAtCursor`/`compbrlLeftCursor`
  ([#20](https://github.com/liblouis/louis-rs/issues/20)). The option flags are
  accepted; nothing reads them yet.
- **Nothing reports whether a back-translation result could still change if more
  cells arrive** ([#29](https://github.com/liblouis/louis-rs/issues/29)).
- **No Unicode normalization.** NFC was implemented and reverted — it fixed tables
  with precomposed-character rules and broke tables with standalone
  combining-character rules (IPA, Hebrew, Yiddish). NFD is the proposed path.
- **Six opcodes are deliberately not implemented**: `uplow`, `locale`, `backmatch`,
  `compdots`, `nobreak` and `macro`. Two are deprecated upstream, three are
  undocumented in liblouis's own manual, and none appears in any table or test in
  liblouis's corpus. See
  [ADR-0012](adr/0012-unimplemented-opcodes.org).

## Where the remaining failures are

Most of the failing 0.1% sits in the files below, and several of those share a root cause.
Sizes are per item — *S* is up to a week, *M* about a week, *L* one to three weeks —
and are there so you can pick work by appetite, not to add up. Failure counts are the
`Failure` lines `check` prints per file, rounded.

| Area                                                | Failing | Root cause                                                                                                                                                                       | Size |
|-----------------------------------------------------|--------:|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|------|
| `th`, `pa`                                          |    ≈150 | Unrelated to each other. `th`: tone marks and vowels come out in the wrong order (`ค่ะ` gives ⠥⠔⠁, not ⠥⠁⠔), from the table's reordering `correct` rules, plus a few `context` rules with a variable. `pa`: backward, the number sign ⠼ is read as the letter ਣ, which has the same cells; liblouis takes the number sign when a digit follows. | M    |
| `de-g0-detailed-specs`, `de-g1-…`                   |    ≈140 | `capsletter` vs. `begcapsword` tier selection. Needs `uppercase.rs` and `emphasis.rs` unified into one tier model — a blanket engine fix was tried and disproven.                | L    |
| `nl-g0_harness`                                     |    ≈110 | Emphasis tiers: a multi-word italic passage should open with ⠸⠸ once, louis-rs marks every word ⠸…⠠. Also indicator order (⠨⠰, not ⠰⠨) and a stray ⠠ after a capsletter that follows a digit ("1E" gives ⠼⠁⠨⠠⠑). | M    |
| `ru`                                                |     ≈30 | Generic `begmode` alphabet indicators: word tiers repeat per letter ("ABC" gives ⠨⠁⠨⠃⠨⠉, not ⠨⠁⠃⠉), and a letter after one from another alphabet gets its sign twice ("HАH" gives ⠨⠓⠘⠘⠁⠨⠓). Likely the same tier model as `de-g0`. | M    |
| all other files                                     |    ≈950 | Roughly 90 files, each contributing under 150.                                                                                                                                   | —    |

Two cross-cutting items have no single file behind them:

- **Backward direction, generally.** The boundary predicates (`word_start` and
  friends) only work forward, and caps passages have no backward path at all.
- **Engine debt.** Regexp negation of concatenation and alternation is wrong
  (`!(e1e2)` ≠ `(!e1!e2)`); `Attribute::ByOrder` and `Any` are no-ops in
  `match_pattern.rs` and all three are no-ops in `context_pattern.rs`. *(M)*

## Forty-two blocks that never run

A YAML file is a sequence of blocks, each with its own table. A block whose table
won't load is reported on stderr and skipped, and the rest of the file still runs.
Forty-two blocks across twelve files are skipped this way, so those tests report
neither passes nor failures — the one genuine unknown in this document, and the
cheapest work in it.

| Reason                                    | Blocks | What it needs                                                                                                                                                                                           | Size |
|-------------------------------------------|-------:|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|------|
| `multipass-vs-match_common.uti` not found |     25 | The include sits next to the YAML file rather than on `LOUIS_TABLE_PATH`. One missing fixture takes out every block of `multipass-vs-match.yaml`. | S    |
| Parser gaps                               |      6 | An `InvalidAction` and an `EmptyTest` in each of `multipass-forward.yaml` and `multipass-backward.yaml`, plus an `OpcodeExpected` and a surrogate-pair escape.                                          | S    |
| Ambiguous metadata queries                |      7 | `language:grc region:en` matches two tables and `system:cmn-traditional` three. `lou_findTable` scores its candidates and always lands on one; we intersect and insist on exactly one.                  | M    |
| Hyphenation dictionaries                  |      2 | `hyph_en_US.dic` rejected for its ISO-8859-1 encoding; `hyphenation.dic` not found.                                                                                                                     | S    |
| `macro.utb` not found                     |      1 | Deliberate non-goal.                                                                                                                                                                                    | —    |
| A query matching no table at all          |      1 | `language:grc region:es` finds nothing on the search path. Possibly a bug in the spec rather than in us.                                                                                                | S    |

Ten files report zero tests. Seven because every one of their blocks is skipped for
a reason above: `multipass-vs-match.yaml`, `macro.yaml`, `grc-international-es.yaml`,
`ja-kantenji.yaml`, `face-with-tears-of-joy.yaml`, `hyphenation.yaml` and
`hyphenation_nocross_harness.yaml`.

The other three are a separate problem: the `hyphenate`, `hyphenateBraille` and
`display` test modes fall through a catch-all arm in `louis-cli/src/test.rs` and silently report
nothing at all, and `de-eurobrl6.yaml`, `hu-hu-g1-hyph_harness.yaml` and
`no_8dot_harness.yaml` consist only of such tests. `issue-332.yaml` has its
`hyphenate` block skipped the same way while its remaining 19 tests all fail. *(S)*

## Missing API surface

| Capability                                                                                      | Status                                                                                                                                                    | Size   |
|-------------------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------|--------|
| `partialTrans` ([#19](https://github.com/liblouis/louis-rs/issues/19))                          | Flag accepted, nothing reads it. A screen reader sets it on every keystroke of contracted input; without it, word-final rules fire on an unfinished word. | M      |
| `compbrlAtCursor` / `compbrlLeftCursor` ([#20](https://github.com/liblouis/louis-rs/issues/20)) | Flags accepted, nothing reads them. Computer-braille rendering already works, so this is cursor plumbing.                                                 | M      |
| Back-translation finality ([#29](https://github.com/liblouis/louis-rs/issues/29))               | Nothing says whether more cells could change the characters already produced.                                                                             | M      |
| `hyphenate`, `charToDots`, `dotsToChar`, `getEmphClasses`                                       | Working internals, nothing exposed on `Translator`.                                                                                                       | S each |
| C ABI                                                                                           | Not started. cbindgen or Diplomat.                                                                                                                        | M–L    |

## Help wanted

Good entry points, roughly easiest first:

1. **Wake up the 42 blocks that don't run.** Several independent small fixes,
   listed above, and nobody knows what those tests will report. *(S each)*
2. **The three parser gaps.** Small, concrete, and each has a failing file to check
   against. *(S)*
3. **Expose the four missing entry points.** Mechanical. *(S each)*
4. **Survey a failure cluster.** Every row in the table above now names a cause, so
   what's left is the ~1 700 failures spread across roughly 100 smaller files, and
   the residue of the two clusters already surveyed. Reading failure output rather
   than deep engine work — and each of the three clusters that got this treatment
   turned out to be one or two root causes rather than a long tail. *(M)*

The two architectural items — match-rule candidate selection, and unifying the
capitalization and emphasis tier models — each already have one failed attempt behind
them, so they're poor first contributions but well documented in
[ADR-0008](adr/0008-match-rule-candidate-selection.org) and
[ADR-0009](adr/0009-capsletter-vs-begcapsword.org). Read the
relevant ADR before starting; both record an approach that was tested and disproven.

## Also open

- Rule-level coverage tool for the YAML suite
  ([#27](https://github.com/liblouis/louis-rs/issues/27)) — [ADR-0014](adr/0014-rule-level-coverage-tool.org) accepted, prototype
  parked
- gzip+bincode table bundling ([PR #10](https://github.com/liblouis/louis-rs/pull/10))
- Backward `capsletter`/`begcapsword` failures in `hu-hu-g1`
  ([#4](https://github.com/liblouis/louis-rs/issues/4))
- Translation performance — the regexp VM was 59% of translation time, and there is a
  measured 2.0–3.4× in hand plus a larger structural idea (compiling all of a stage's
  `match`/`context` patterns into one automaton) not yet attempted. Everything on the
  subject, including the variants that were measured and disproven, is in
  [Translation_Performance.md](Translation_Performance.md).

---

Working notes live in [TODO.org](../TODO.org); design decisions and the reasoning
behind them in
[doc/adr/](adr/README.org); where
translation time goes and what to do about it in
[Translation_Performance.md](Translation_Performance.md).
