# Translation performance

Where translation time goes, what has been done about it, and what is still on the
table. Companion to [Liblouis_Parity.md](Liblouis_Parity.md), which covers
correctness; this document covers speed.

Everything here is measured, including the things that turned out not to work. Each
idea records its own numbers so a future attempt can tell whether it is re-treading
disproven ground.

Idea 3 is on `main`. Ideas 1 and 2 are prototyped on the `regexp-prefilter` branch and
have not been reviewed for merge; idea 4 is a sketch.

## Measuring

```sh
export LOUIS_TABLE_PATH=~/src/liblouis/tables:~/src/liblouis
cargo bench --bench translate
cargo bench --bench parse_and_compile
```

`benches/translate.rs` has three groups, and the split matters more than it looks:

- **`translate/en-ueb-g2`** — forward, English. Latin, so almost every input
  character is ASCII.
- **`translate/en-ueb-g2-backward`** — backward, the braille the forward paragraph
  produces. Every input character is a dot pattern in U+2800..U+28FF, so *nothing*
  is ASCII.
- **`translate/el`** — forward through `el.ctb`. Non-ASCII on the text side too.

An optimisation that keys on the input alphabet can look like a large win on the
first group and a regression on the other two. Idea 3 below was nearly mis-sized
exactly that way.

### What the input actually looks like

A static scan of the 323 top-level `.ctb`/`.utb`/`.tbl` tables (includes expanded,
counting the characters named by character-definition and `attribute` opcodes) puts
the character classes at 32 560 ASCII members against 316 962 non-ASCII ones. Only
six tables are ≤10% non-ASCII on their text side; 166 are over 50%. The median table
is `my-g1.utb` at 74 ASCII and 338 non-ASCII members.

And backward translation needs no survey: its classes are dot patterns, so they are
non-ASCII by construction, in every table.

So ASCII input is the exception, not the rule. `en-ueb-g2` forward is close to the
best case louis-rs will ever see.

### Profiling

Where `perf` is unavailable — a `perf_event_paranoid` setting of 2 or higher refuses
to open any events — callgrind needs no privileges:

```sh
valgrind --tool=callgrind --cache-sim=no --callgrind-out-file=cg.out <binary>
callgrind_annotate --threshold=85 cg.out
```

Two traps:

- Criterion's `--profile-time` profile of the bench binary is dominated by **table
  compilation**, not translation, because the harness only manages a handful of
  iterations under valgrind. Profile a small standalone binary that loads the table
  once and translates in a loop instead.
- `callgrind_annotate --tree=caller` attributes inlined code to the callee's symbol,
  so a caller list that doesn't add up to the self cost is expected. Don't read the
  gap as a missing caller.

## Where the time goes

Callgrind, forward `en-ueb-g2`, before any of the work below (2.98 G instructions for
20 paragraph translations):

|                                                                  | share |
|------------------------------------------------------------------|-------|
| `CompiledRegexp::find_anchored`                                  | 33.3% |
| `CompiledRegexp::add_thread` (+ recursive self-calls)            | 25.9% |
| `hash_one::<&char>` + SipHash                                    | 12.0% |
| `memcpy` / `memset`                                              | 11.8% |
| `Vec<ResolvedTranslation>` construction in `MatchPatterns::find` | 3.5%  |

The regexp VM was **59%** of translation, and the reason is structural rather than a
slow inner loop. `MatchPatterns::find` and `ContextPatterns::find` ran *every*
compiled pattern at *every* input position:

```rust
self.regexps
    .iter()
    .flat_map(|r| r.find_anchored(input, &Environment::new(), at_start))
    .collect()
```

For `en-ueb-g2` the main stage compiles 444 `match` patterns and 57 `context`
patterns, so a 600-character paragraph set up and ran the VM about 300 000 times, and
almost every one of those runs could not possibly have matched.

## Idea 1 — reject a pattern by its first character

A pattern that must start with `c` cannot match at a position holding anything else,
and that is decidable at table-compile time.

### Sketch

Walk the epsilon closure of `pc` 0 — the same instructions `add_thread` walks —
and collect the characters the consuming instructions it reaches accept. Follow
assertions and variable tests rather than evaluating them, since those depend on
run-time state; the result is then a safe superset. Reaching `Match` means the
pattern matches the empty string, and `Any`/`NotChar`/`NotClass` means it
accepts characters no finite set can list; either way there is nothing to filter
on and the answer is "can't narrow".

```rust
fn first_chars(instructions: &[Instruction], classes: &[CharacterSet]) -> Option<Vec<char>>
```

`find_anchored` then rejects before building any VM state. A narrowable first set also
implies the program consumes at least one character, so empty input can be rejected
too.

Two details that are easy to get wrong:

- `Instruction::CaseInsensitiveChar` lowercases the *input* before comparing, so the
  lowercased input character has to count as a possible start as well.
- That lowercasing must not itself become the cost. `char::to_lowercase` is a Unicode
  table lookup; a first cut added 8% of `to_lower`/`ToTitlecase` before an ASCII fast
  path went in. The same fix applies to `chars_match_case_insensitive`, which was
  calling `to_lowercase()` twice per comparison (`count() == 1 && next() == …`).

### Result

−25% on forward `en-ueb-g2`. 419 of 444 `match` patterns and all 57 `context`
patterns are narrowable.

### Precedent

Standard, under several names. PCRE2 computes exactly this as a compile-time
"start-up optimization" and exposes it as `PCRE2_INFO_FIRSTBITMAP`, a 256-bit
table of possible starting code units, with `PCRE2_INFO_FIRSTCODEUNIT` for the
single-character case. RE2 computes a required prefix and first-byte set. Rust's
`regex` extracts literal prefixes into a `Prefilter` and hands them to `memchr`.
It is the FIRST set from parsing theory.

The difference is how it gets used. Those engines search unanchored, so the first set
lets them **skip ahead** in the haystack. liblouis semantics match anchored at every
position, so we use the same computation as a **reject**.

Worth knowing: liblouis has no equivalent for `context`, `correct` or `pass2`–`pass4`.
`findForPassRule` in `lou_translateString.c` walks the whole `forPassRules[pass]`
linked list at every position and calls `passDoTest` on each. This idea beats the C
implementation there rather than catching up with it.

## Idea 2 — index the patterns by first character

Idea 1 still visits all 501 patterns per position to reject them. Bucketing them
means never looking at the ones that cannot match.

### Sketch

`FirstCharIndex` holds `HashMap<char, Vec<u32>>` of pattern indices by folded
first character, plus an ascending `unfiltered` list of the patterns idea 1
could not narrow. `find` walks the union of the bucket for the current character
and `unfiltered`.

Two things that are load-bearing:

- **Folding.** Register each first-set character `c` under `fold(c)` and look up with
  `fold(input_char)`, where `fold` is "lowercase, first char". An input `a` can only
  match a pattern starting with `c` if `a == c` or `a` lowercases to `c`, and both
  imply `fold(a) == fold(c)` — so one lookup with the same folding on both sides is
  exactly right, with no reverse-case-mapping table and no second lookup to dedupe.
- **Order.** The union must stay in ascending pattern order. Candidate order decides
  equal-rank ties in `PrimaryTable::trace` (`max_by_key` returns the last maximum), so
  reordering the candidate list silently changes which rule wins. Hence the
  two-pointer merge of bucket and `unfiltered` rather than chaining them.

### Result

−28% on forward `en-ueb-g2`, on top of idea 1.

### Precedent

This is multi-pattern dispatch, not a single-regexp technique, so a conventional
regexp engine has no need for it — and the nearest precedent is **liblouis
itself**. `_lou_stringHash` is

```c
(((unsigned long int)toLowercase(c[0]) << 8) + (unsigned long int)toLowercase(c[1])) % HASHNUM
```

Rules longer than one character go into `table->forRules[hash]` buckets
(`addForwardRuleWithMultipleChars`), single-character rules hang off the character's
`otherRules` list, and `lou_translateString` looks up
`forRules[_lou_stringHash(&input->chars[pos], 1, table)]` and walks only that bucket.
So the C implementation has always done this — and it keys on the first **two**
characters. Widening our index key from one character to two is the obvious cheap
follow-up.

Outside braille: Hyperscan selects candidate patterns out of thousands with
literal prefilters (Teddy/FDR) before confirming with an automaton, and Snort and
Suricata pick a "fast pattern" per rule and group rules so only a candidate group
runs. Same architecture in all three — cheap filter, expensive matcher on the
survivors.

## Idea 3 — an inline bitmap for character-class membership

**Landed on `main`** as `src/translator/regexp/character_set.rs`, without ideas 1 and 2.
Read the Result section below before quoting a number: taken alone it is a
forward-translation win only, and the branch's figures are not the ones `main` gets.

`Instruction::Class`/`NotClass` tested membership in a `HashSet<char>` for every
thread of every match attempt, and hashing a `char` with the default SipHash was ~10%
of translation.

### Sketch

An inline bitmap for ASCII, and a sorted fallback for everything else:

```rust
struct CharacterSet {
    ascii: [u64; 2],   // U+0000..U+007F
    chars: Vec<char>,  // the non-ASCII members, sorted
}
```

### Result

Forward is −26% either way. The other two regimes depend entirely on whether the
first-character prefilter of ideas 1 and 2 is already in place, so quote them together
with the baseline they were measured against:

|                            | forward `en-ueb-g2` | backward | Greek |
|----------------------------|---------------------|----------|-------|
| on top of ideas 1+2        | −26%                | −7%      | −3%   |
| on its own, against `main` | −26%                | **+4%**  | ±0%   |

Backward translation feeds the VM dot patterns, so it never touches the ASCII bitmap and
always takes the fallback. Standing alone that is not a win: binary search does *not*
beat hashing here, and the extra `cp < 128` branch costs 4%. It only turns into a gain
once the prefilter is present, because the prefilter is itself worth far more to backward
translation than to forward (13.0 ms → 4.2 ms), and what survives it is a different, much
smaller mix of match attempts.

So the bitmap is a forward-translation optimisation that the other two regimes tolerate.
Take it for the −26%, not for a uniform win.

A second bitmap over the 256 Unicode braille patterns is worth a further 13% of backward
translation, and costs ~2% of forward plus a conditionally-present field on a struct that
`en-ueb-g2` compiles 1587 of. Measured, and deliberately not taken: the fallback is fast
enough that the complexity isn't justified.

Keep the fallback `Vec` to the *non-ASCII* members only, rather than every member as the
`regexp-prefilter` branch does. The branch stores all of them because its
`first_char_index` needs `chars()` to be the complete set; where that requirement is
absent, dropping the ASCII members shortens every binary search and is worth 3% of
backward and 3% of Greek.

### Precedent

Bitmap character classes are universal — DFA transition tables, PCRE2's class bitmaps for
the low 256 code units, `regex-automata`'s dense `ByteClasses`.

**Worth knowing while reading this code.** There are two sets of character classes —
`TableContext::character_classes` over text characters and `dots_classes` over dot
patterns — and the pattern builders pick between them **by stage, not by direction**.
`MatchPatterns` always gets the text classes; `ContextPatterns` gets the text classes
for `Pre` and `Main`, and `dots_classes` only for `Post1`–`Post3`, the `pass2`–`pass4`
stages that translate braille to braille. So a `Main` pattern tests against classes of
text characters even when translating backward: in `en-ueb-g2` backward, 1550 of the
1552 compiled classes hold no dot pattern at all.

Whether a backward `Main` pattern *should* resolve `%letter` against `dots_classes` is
a correctness question rather than a performance one — liblouis's `passDoTest` carries
a `passCharDots` flag and checks dot attributes when running backward — and the suite
does not currently show it as a failure.

## Idea 4 — compile all patterns into one automaton

Ideas 1 and 2 reduce how many VM runs happen per position. The structural fix is to
stop having more than one.

Every pattern of a stage is tried at the same position against the same input, which
is precisely a **regexp set**: alternate all of them into a single program, each
branch ending in `Match(i)`, and run the VM once per position. Shared prefixes merge
for free, and the per-run setup cost is paid once instead of once per surviving
pattern.

**The VM is already shaped for this.** `Instruction::Match` carries a
`TranslationIndex` into `CompiledRegexp::translations: Vec<Translation>`. Today that
vector always holds exactly one entry — the payload plumbing for a set already
exists and is simply unused.

### Sketch

1. Build one `CompiledRegexp` per stage: `Split` over the patterns in table order,
   each branch ending in `Match(i)` indexing `translations[i]`.
2. `find_anchored` must return **all** matches at the position, not the first.
   Today it `break`s out of the step loop on `Match` because "every remaining thread
   in this step is lower priority than the one that just matched" — true for one
   pattern, wrong for a set, since `trace` ranks the full candidate list by
   length and weight. Collect every `Match` reached instead.
3. Thread priority already runs in `Split` order, so alternating in table order
   preserves the candidate ordering that idea 2 had to protect by hand.
4. Ideas 1 and 2 do not get thrown away — they become the **start-state**
   optimisation. Rather than `Split`ing into all 501 branches, enter only the
   branches whose first-character set admits the current character. That is how RE2
   and `regex-automata` handle start states, and it is the natural home for
   `FirstCharIndex`.

### What will need attention:

- **Program size.** 501 patterns at tens of instructions each is a program of some
  thousands of instructions, against a few dozen today. `Bitset` stops fitting its
  `SmallVec<[u64; 1]>` inline storage and `ThreadList`'s `SmallVec<[Thread; 64]>`
  stops bounding the live thread count. Reusable scratch buffers — see the
  `ThreadList` item below — become a prerequisite rather than a nice-to-have.
- **Captures.** `Thread` carries a single `(usize, usize)` capture span. That is
  per-thread, so it survives alternation as-is, but the assumption should be
  re-checked against patterns that capture in more than one branch.
- **Per-pattern environment.** `ContextPatterns::find` takes an `&Environment` shared
  by all patterns, so that much is fine, but `VariableEqual`/`NotVariableEqual`
  assertions inside individual branches need to keep failing only their own branch.

### Precedent

`RegexSet` in Rust's `regex` (and `PatternID` throughout `regex-automata`),
`RE2::Set`, and Thompson's original construction. Hyperscan is built entirely
around it.

The closest analogue is actually a **lexer generator**: flex unions all token rules
into one automaton and reports which rule matched. A liblouis translation table *is*
a lexer — many patterns, anchored at the current position, longest-and-highest-
priority match wins, consume it, repeat — which is a good sign this is the right
shape rather than a clever detour.

## Smaller items still open

Shares are of translation time after ideas 1–3, from the callgrind profile.

- **Per-position `Vec` churn** (~10%). Each position allocates fresh
  `Vec<ResolvedTranslation>`s for the trie, nocross, `match` and `context` lookups,
  and `partition` doubles them. Thread one reusable buffer through `find` /
  `find_translations` / `trace`.
- **`ResolvedTranslation` is a fat clone** (~5% of the profile, but see below). It is
  288 bytes, of which 184 are its `Option<AnchoredRule>`, and the trie clones one per
  candidate per node along the match path at every position. Making the *rule* cheaper
  to clone is not the answer, though — that was measured and is disproven below. What
  is left is the two `String`s and the `Vec<Effect>`. The `rule()` borrow in `7c79e6e`
  took care of the two worst individual offenders, in `trace`'s candidate filters, for
  0.7–2.9%.
- **VM scratch buffers** (~3%). Two `ThreadList`s are built per `find_anchored` and
  `memset` is still 3.3%. Needs actual reuse across calls; shrinking the inline
  capacity from 64 to 32 was measured as noise. Prerequisite for idea 4.
- **`translate()` string clones** (~1%). `self.trace(…).iter().map(|t| t.output())`
  clones every output `String`; `push_str` into one `String` instead. One line.
- **Two-character index key** (unmeasured). Follow liblouis's `_lou_stringHash` and
  key `FirstCharIndex` on the first two characters instead of one.

## Measured and rejected

Do not re-attempt these without a reason to think something has changed.

- **Sorted `Vec` + `binary_search` for the VM's character classes, with no bitmap.**
  8% *slower* than the `HashSet<char>` it replaced, on forward `en-ueb-g2`. The win in
  idea 3 is the bitmap, not the removal of hashing.
- **A heap range bitmap for character classes.** Replacing `CharacterSet::ascii`
  with a `Box<[u64]>` spanning whatever code point range the members occupy, so that
  any script gets a bitmap, costs **43%** on forward `en-ueb-g2` and gains nothing.
  Of `en-ueb-g2`'s 1587 compiled classes only ~183 span fewer than 256 code points;
  the rest span 8 000 to 973 000, because a class routinely holds ASCII plus accented
  Latin plus virtual dots. So the bitmap usually exceeded any sane cap and was never
  built, leaving the fallback *plus* a new branch — and where it was built, the
  pointer chase outweighed it. A single movable 256-bit window at a build-time base
  fails for the same reason: it covers only those ~183 classes.
- **Narrowing the regexp opcode operands to 32 bits.** `Split` is the widest
  `Instruction` variant, so shrinking `InstructionIndex`, `CharacterClassIndex`,
  `TranslationIndex` and `Thread`'s capture offsets from `usize` to `u32` does halve
  those structs as predicted, and does double how much bytecode and how much of a
  `ThreadList`'s inline thread set fits in a cache line. It bought nothing on
  `benches/translate.rs`.

  The premise didn't hold because the 42–45% the `ThreadList` ADR once measured came
  from removing heap allocation and from stopping a 1.5 KB *by-value* swap per input
  character. Neither cost scales with the struct size any more: `current` and `next`
  are references, so the per-step swap is two pointers whatever a `ThreadList`
  weighs. (Recorded in full in `TODO.org`.)
- **Shrinking `ThreadList`'s inline capacity** from `SmallVec<[Thread; 64]>` to 32,
  to cut the `memset` cost. Noise in both directions.
- **Handing out references instead of cloning stored translations** (2026-09-03). The
  premise was the fat clone above: 184 of a `ResolvedTranslation`'s 288 bytes are its
  `Option<AnchoredRule>`, cloned per candidate per node in
  `find_translations_from_node`. The experiment was an upper bound — store no origin
  in the trie at all, so a candidate clone skips the rule entirely, which saves
  strictly more than any `Arc` or `Cow` could. Doing *less* work measured 2% **slower**
  (word 188.7 µs → 193.0 µs, sentence 2.127 ms → 2.181 ms, paragraph 13.95 ms →
  14.19 ms) and unchanged on `check da-dk-g28-dictionary_harness.yaml` (6.63/6.82 s →
  6.75/6.68 s). Two workloads on purpose: the criterion bench is regexp-dominated by
  design, the dictionary harness leans on the trie. Neither can see the clone.

  So don't do this for the clone's sake. If it is ever wanted for another reason, the
  shape is: tables keep owning their translations and hand out
  `&ResolvedTranslation`, so the stored type needs no lifetime and cannot
  self-reference — but the candidate list is mixed. Trie and display candidates are
  stored, `match`/`context` ones are built per match from `Translation::Unresolved` via
  `resolve(capture, …)`, and the indicator modules derive theirs per position, so the
  collectors would want `Cow<'_, ResolvedTranslation>`.
- **Offset buckets for delayed translations**. Rejected without being built, because the
  list it would index is virtually always empty. `trace` keeps delayed translations in one
  flat `Vec` and rescans it twice per input position — `update_offsets` to decrement each
  offset, `partition_delayed_translations` to split off what has come due — so bucketing
  by offset looks like an obvious win. Instrumenting the length over the whole YAML suite
  says otherwise:

  |                                         |              |
  |-----------------------------------------|--------------|
  | input positions measured                | > 20 000 000 |
  | positions where the list holds anything | 0.84%        |
  | longest it ever gets                    | 8            |

  Note *why*, because the obvious reason is wrong: it is not that pre-patterns are
  rare. 230 of `en-ueb-g2`'s 356 `match` rules carry a real one. A position only gets
  a delayed entry when a pattern actually *matches* there with a non-zero pre-pattern
  length, and matches are rare relative to positions. Scanning up to eight elements
  beats indexing a bucket map, and the empty case already allocates nothing: `collect`
  on an exhausted-length iterator and `partition`'s two `Vec::new()`s never touch the
  heap.

  The idea is a queue keyed by when an entry comes due: deal with the entries at offset 0,
  then pop the ones at offset 1, and so on. This would remove the per-position *rebuild*,
  not just shorten a scan. Measured as an upper bound anyway, by skipping `update_offsets`
  and `partition_delayed_translations` entirely whenever nothing is pending, which is
  strictly better than any queue could manage: no measurable difference on any of the
  three benchmark groups.

  One argument does survive, and it is not about speed. `update_offsets` carries

  ```rust
  .filter(|t| t.offset() >= decrement)   // drop translations where the offet is smaller than the decrement
  ```

  so a pending translation whose due position falls *inside* the span just consumed is
  silently discarded. Keyed by absolute due position, that stops being a filter
  predicate and becomes visible structure — the buckets inside the consumed span are
  simply never visited, and whether that is right has to be decided rather than
  assumed.

## Standing lessons

Both of these cost about 12% of a suite run and neither is visible in the code that
caused it.

- **Keep the values of maps consulted per character small.** When `DisplayTable`
  started storing whole translations, widening `HashMap<char, char>` (4-byte values,
  ~1 KB) to `HashMap<char, ResolvedTranslation>` (288-byte values, ~73 KB for
  `unicode.dis`) turned a per-character lookup into a cache miss, over 2.2M
  assertions. Fixed by boxing the rule inside the value, which is 16 bytes.
  Interleaved four ways: baseline without rules 58.2 s, inline translations 65.3 s,
  small value plus a `Vec` indexed by `usize` 57.9 s, boxed rule 58.7 s. How the value
  is shrunk doesn't matter, only that it is.
- **A purely cosmetic move between modules can cost real time.** Moving
  `DisplayTable` from `translator.rs` into `translator::table::display` (`6e5b591`)
  cost 12% of a suite run for no change in code: 53.1/53.3 s before, 59.5/59.8 s
  after, interleaved. Release builds default to 16 codegen units split per module, so
  `DisplayTable::trace` and `displayed` lost cross-unit inlining with their caller in
  `pipeline.rs`. `#[inline]` on those two recovers all of it (53.3/53.0 s) — measured,
  then dropped, parity first. `codegen-units = 1` or LTO in the release profile would
  make the whole question go away.

## Results

### What landed: idea 3 alone

`cargo bench --bench translate`, against the `HashSet<char>` it replaced:

|                                | before   | after    |           |
|--------------------------------|----------|----------|-----------|
| `en-ueb-g2/word`               | 172.3 µs | 135.4 µs | 1.27×     |
| `en-ueb-g2/sentence`           | 1.999 ms | 1.468 ms | 1.36×     |
| `en-ueb-g2/paragraph`          | 12.71 ms | 9.403 ms | **1.35×** |
| `en-ueb-g2-backward/paragraph` | 13.01 ms | 13.59 ms | 0.96×     |
| `el/paragraph`                 | 1.373 ms | 1.373 ms | 1.00×     |

Table-compile cost is `en-ueb-g2` 19.3 → 20.4 ms (+6%) for building the sorted
fallbacks; `de-g2` and `zh-tw` are within noise. Paid once per table load.

Correctness unchanged: the full YAML summary — 2 224 727 assertions across 70 files,
both directions — is byte-identical to the pre-change baseline, and 271 unit tests pass.

### Still on the branch: ideas 1–3 together

`cargo bench --bench translate`, ideas 1–3 against `main`:

|                                | before   | after   |           |
|--------------------------------|----------|---------|-----------|
| `en-ueb-g2/word`               | 173.6 µs | 65.3 µs | 2.66×     |
| `en-ueb-g2/sentence`           | 1.947 ms | 821 µs  | 2.37×     |
| `en-ueb-g2/paragraph`          | 12.55 ms | 5.17 ms | 2.43×     |
| `en-ueb-g2-backward/paragraph` | 13.23 ms | 3.88 ms | **3.41×** |
| `el/paragraph`                 | 1.384 ms | 688 µs  | 2.01×     |

Backward gains most because it starts from the worst case: `context` rules, which
neither implementation prefilters at all.

The cost is at table-compile time, where the first-character sets get built:
`en-ueb-g2` 18.9 → 22.1 ms (+17%), `de-g2` +3%, `zh-tw` −1%. Paid once per table
load.

Correctness is unchanged. The full YAML summary — 2 224 727 assertions across 70
files, both directions — is byte-identical to the pre-change baseline, and the 270
unit tests pass.
