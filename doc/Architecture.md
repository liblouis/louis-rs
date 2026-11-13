# Architecture

How louis-rs turns a liblouis table and some text into braille (or braille into text): the
concepts, how they fit together, and where new logic belongs. The *why* behind individual
decisions is in the [ADRs](adr/README.org); this is the map they hang on. The doc comments
of each module have the details.

louis-rs is a rewrite, not a port. It reads liblouis's tables and passes liblouis's tests,
but its data structures are its own: where liblouis threads state through one long
translation loop, louis-rs works out as much as it can before the loop, as plain data, and
the loop only looks it up.

## One translation, end to end

```
table files ──parse──▶ rules ──TableContext──▶ TranslationPipeline
                                                 │
   text ──▶ [correct] ──▶ main stage ──▶ [pass2] ──▶ [pass3] ──▶ [pass4] ──▶ [display] ──▶ braille
                             │
                             ├─ before the loop:  indicators ─▶ indications per position
                             │                    constrainers ─▶ constraints per position
                             ├─ the loop, at each position:
                             │      candidates (trie, match, context, ...)
                             │      ─▶ filtered by the constraints ─▶ the best by Rank
                             └─ after the loop:   rewriters ─▶ the final translations
```

Backward (braille to text) runs the same stages in reverse order, the display table first.

### Compiling a table

1. **Parsing** (`parser.rs`, `parser/`): each line of a table becomes a `Rule`, one variant
   per liblouis opcode, wrapped in an `AnchoredRule` that remembers its file, line and
   definition order. `include`s are expanded as the files are read, and hyphenation
   dictionaries are parsed into a rule too, so nothing after parsing touches the filesystem.
2. **`TableContext`** (`translator/table.rs`): a first pass over all the rules collects what
   other rules need to look up: the character definitions (character to dots), the
   character classes (`letter`, `digit`, custom `attribute`s, ...), the same classes over
   dots for backward translation, and the swap classes.
3. **`TranslationPipeline`** (`translator/pipeline.rs`): drops the rules that are not for
   this direction (`nofor`, `noback`), sorts the rest into stages by opcode, and compiles
   each stage, with the context, into a table: a `MultipassTable` for `correct` and each of
   `pass2` to `pass4`, a `PrimaryTable` for everything else. Backward, the stages are
   reversed. A braille encoding (`BrailleEncoding`, from a liblouis display table), if
   the caller gives one, is added as the outermost stage, a `DisplayTable`; a display
   table a translation table includes is ignored.

### The main stage: `PrimaryTable::trace`

`translator/table/primary.rs` holds nearly all of liblouis's opcodes. Translating with it
has three phases.

**Before the loop**, two kinds of precomputation run over the whole input:

- **Indicators** (`indication/`) decide which indication *cells* go where: capital signs,
  number signs, emphasis, computer braille, mode and letter signs. The result,
  `PrecomputedIndications`, is a list of translations per position (ADR-0005, ADR-0006).
- **Constrainers** (`position_constraints.rs`) decide which *rules* may apply where: a set of
  `Constraint` flags per position, such as "inside a number, no contractions"
  (`DontContract`), "inside a `nocont` word" (`Uncontracted`), "a syllable of a `syllable`
  rule starts here" (`SyllableStart`) or "a hyphenation break falls here"
  (`HyphenationBreak`).

Both read only the input text, and the options. Neither knows which rules will be chosen.
Forward, with `compbrlAtCursor` or `compbrlLeftCursor`, the word at the cursor
(`CursorWordScanner`) is computer braille (`UseComp6`) without indicators, and no rule
reaches into it.

**The loop** walks the input. At each position it:

1. emits the indications precomputed for that position;
2. gathers the **candidates**, every rule that matches here:
   - the **trie** (`trie.rs`): every rule that is a fixed string — `always`, `word`,
     `begword`, `contraction`, the character definitions backward, ... Each rule's
     conditions on its surroundings are transitions in the trie path:
     `Start`/`NotStart`/`End` look at the character class before or after the match
     (ADR-0001, ADR-0010), `BegWord`/`EndWord`/`ContinuesWord` decide word boundaries
     backward, and `StandsAloneBefore`/`StandsAloneAfter` check the standing-alone
     sequence of a table with a `seqdelimiter`. A separate trie holds the `nocross` rules,
     and another the `comp6` rules;
   - **match patterns** (`match_pattern.rs`) for `match` rules, and **context patterns**
     (`context_pattern.rs`) for `context` rules, both compiled to the **regexp VM**
     (`regexp.rs`, ADR-0007). A `match` rule's `pre` is checked backwards from the cursor.
     A `context` rule whose test starts before its `[` is found early and kept as a
     **delayed translation** until the cursor reaches the `[`;
3. **filters** the candidates by the constraints at this position, and by checks the trie
   can't express (`prepunc`, `joinnum`, `repword`, ...);
4. picks the best candidate by **`Rank`** (`translation.rs`, ADR-0016), applies it, records
   its **effects** (a `context` rule's variables, `effect.rs`) and moves past the input it
   consumed. If nothing matches, the character's own definition applies, and failing that
   the `undefined` rule or an escape spelling out the character's code.

**After the loop**, the **rewriters** (`indication.rs`, `Rewriters`) rewrite the chosen
translations where the result depends on which rule was chosen: the `nocontractsign` and
letter sign in front of a `contraction` applied (forward), and the capitals (backward).

### The multipass stages

`correct` and `pass2` to `pass4` (`table/multipass.rs`) run the same loop with less: their
rules are all context patterns over the output of the previous stage, chosen by the same
`Rank`, with no indicators, constrainers or rewriters.

## Concepts

**Rule, `AnchoredRule`.** One parsed line of a table: the opcode, its operands and its
constraints (`nofor`, `noback`, `nocross`). Anchored to its file and line, and to its
definition order, which breaks ties.

**`TableContext`.** What the rules define for each other to look up: character
definitions, character classes, dots classes and swap classes.

**Pipeline, stage.** The sequence of tables a translation goes through: `Pre` (`correct`),
`Main`, `Post1` to `Post3` (`pass2` to `pass4`), and a display table. A stage is one table;
each produces `ResolvedTranslation`s whose output is the next stage's input.

**`ResolvedTranslation`.** One applied rule: the input it consumed, the output it produced,
its length, offset and weight, and the rule it came from. An indication is one that consumes
no input (`ResolvedTranslation::indication`). Keeping the rule lets the trace show it, and
lets the rewriters see what was chosen.

**Trie, transition.** A prefix tree over the input characters for every rule that is a
fixed string, case-insensitive with case-sensitive character definitions kept aside.
Boundary conditions are non-consuming transitions on the path, so a rule's conditions are
checked by the same walk that matches it, and all the rules that land on the same node are
kept for `Rank` to choose from.

**Regexp VM, match and context patterns.** A small regexp engine after Russ Cox's virtual
machine, with liblouis's extras: attribute classes, swaps, variables. `match` and `context`
rules, and every multipass rule, compile to it. The VM only runs forward; a look-behind, such
as a `match` rule's `pre`, is compiled from the reversed pattern (`Regexp::reverse`) and
run over the reversed text before the cursor, as liblouis does.

**Indicator.** Decides from the text alone where indication cells go, before translation.
One per kind of indication: `uppercase`, `numeric`, `emphasis`, `computer_braille`,
`lettersign`, and `mode` for the generic `begmode` family, which marks a run of any
character class the way capitals are marked.

**Constrainer, `Constraint`.** Decides from the text alone where rules may or may not
apply, before translation, as one set of flags per position. One per question: inside a
number, inside a `nocont` word, a syllable start, a hyphenation break, backward after a
number sign, and so on.

**Rewriter.** Rewrites the chosen translations after the loop, where the result depends on
the rule chosen rather than on the text. It may insert translations or change them:
`ContractionSigns` puts a chosen contraction's signs in front of it (forward),
`BackwardCaps` uppercases after a chosen `capsletter` or `begcapsword` (backward). A table
holds them as `Rewriters`, applied in turn.

**`Rank`, `Precedence`.** How the loop picks between candidates at the same position: a
key compared field by field. Roughly: a forward `context` rule with no leading literal
first (a quirk of liblouis's indexing that tables depend on), then zero-length candidates,
then the longest match, then the kind of rule (`Precedence`: character definition <
emphasis indicator < indicator < `always` < everything else), then backward the longest
output, `nocross`, a `context` rule's test, and finally the earliest definition. The order
of the fields is the order of precedence; ADR-0016 has the reasoning, and the doc comments
on `Rank` the details.

**Effects, environment.** The variables a `context` or multipass rule sets (`#1=0`,
`#1+`, `#1-`) and tests (`#1=3`, `#1<3`, ...), carried along one stage's loop. A
variable that was never set is 0.

**Scanners.** Small helpers that look further than the neighbouring character. The trie
consults `BegWordScanner` and `EndWordScanner` (backward word boundaries) and
`StandingAloneScanner` (`seqdelimiter` sequences) for its transitions; the main stage uses
`CompbrlScanner` (words with a `compbrl` string become computer braille) and
`CursorWordScanner` (the word at the cursor) before the loop.

## Where does new logic go?

| The logic depends on...                                          | Put it in                                  |
|------------------------------------------------------------------|--------------------------------------------|
| the surroundings of a match, one character on either side        | a trie transition (`Start`, `End`, ...)    |
| a pattern liblouis expresses as a regexp                         | the regexp VM, via a match/context pattern |
| the text, to decide where indication cells go                    | an indicator                               |
| the text, to decide where rules may apply                        | a constrainer and a `Constraint` flag      |
| which rule was chosen                                            | a rewriter                                 |
| which of several matching rules liblouis would prefer            | `Rank` (and an ADR)                        |
| what the table author wrote in a pass                            | nothing: it's a table rule                 |

The common mistake is the fifth row: guessing from the text which rule will be chosen, in an
indicator or constrainer, when the choice can simply be read afterwards. A contraction's
letter sign and a capital sign followed by another indicator were both fixed by moving
them into a rewriter.

## Backward

Backward translation runs the same machinery over braille, with these differences:

- **The pipeline runs in reverse**, display table first and `correct` last.
- **Indicators are rules.** An indicator's dots (`capsletter`, `numsign`, ...) go into the
  trie as rules that consume the cells and write nothing, ranked below the translation
  rules with the same dots (`Precedence::Indicator`, `Precedence::Emphasis`).
- **Constrainers read cells**: after a number sign, a cell shared by a digit and a letter
  reads as the digit (`PreferDigit` and the backward digit overrides); after a letter sign,
  the word rules stay off (`NoContract`, `NoWholeWord`).
- **Word boundaries** are decided from the text already written (`BegWordScanner`) and the
  cells still to come (`EndWordScanner`), since a cell doesn't say whether it is a letter.
- **`Rank`** prefers the rule that writes more text, as liblouis sorts its backward rules.
- **Capitals** are applied by the `BackwardCaps` rewriter, with the table's
  `base uppercase` rules.
- **Emphasis** comes out as the typeform, `TranslationResult::emphasis`:
  `backward_emphasis` reads the emphasis indicators chosen in the main stage, and the
  pipeline carries the result over the stages after it with the position map.
- **Partial input** (`partialTrans`): no word ends anywhere, so the trie's `EndWord` never
  holds and `ContinuesWord` always does, and `word` and `lowword` rules don't apply.

## Further reading

- [ADRs](adr/README.org): one decision per file. The ones behind this map: ADR-0001 and
  ADR-0010 (boundaries), ADR-0005 and ADR-0006 (precomputed indication), ADR-0007 (regexp
  VM), ADR-0016 (`Rank`).
- [Differences from liblouis](Differences_From_Liblouis.md): where louis-rs deliberately
  does something else.
- [Liblouis parity](Liblouis_Parity.md): how close louis-rs is to liblouis, and what it
  still does differently.
