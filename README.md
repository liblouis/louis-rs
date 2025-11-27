

# louis-rs: a liblouis re-implementation in Rust

This is the reduced-to-the-max re-write of [liblouis](https://liblouis.io/) in Rust.


## Rationale

Many if not most of the CVEs of liblouis are rooted in the manual
memory management in the C version of liblouis.

Moving to Rust is of tremendous help not only for the solid memory
management to avoid buffer overflow problems, but also to bring joy
back into liblouis maintenance.


## Status

The re-implementation is in alpha state.

That said, the `louis` binary currently, passes around 84% of the
liblouis test suite for forward translation successfully. Backward
translation works in principle but not much of it has been
implemented, so the success rate for both forward and backward
translation is much less, namely 55%.

The library and its API has not been worked out and is not stable.


## Relation to liblouis

louis-rs is **not** a direct port of the liblouis C code to Rust. It
uses the same tables and the same YAML tests but other than that it is
a complete rewrite. It uses different data structures and does the
translation using a different algorithm.

The goal is to be as compatible as possible with liblouis, when it
makes sense.


## Usage

Get help:

    $ cargo run -- help

Translate some text:

    $ export LOUIS_TABLE_PATH=~/src/liblouis/tables:~/src/liblouis
    $ cargo run -- translate ~/src/liblouis/tables/de-comp6.utb 
    > Guten Tag
    ⠈⠛⠥⠞⠑⠝⠀⠈⠞⠁⠛⠀

Trace a translation:

    $ cargo run -- translate --tracing ~/src/liblouis/tables/en-us-g2.ctb
    > It's about the blind
    ⠠⠊⠞⠄⠎⠀⠁⠃⠀⠹⠑⠀⠃⠇
    ┌────┬───────┬────┬────────────────────┐
    │    │ From  │ To │ Rule               │
    ├────┼───────┼────┼────────────────────┤
    │ 0  │       │ ⠠  │ capsletter ⠠       │
    │ 1  │ I     │ ⠊  │ base uppercase I i │
    │ 2  │ t     │ ⠞  │ lowercase t ⠞      │
    │ 3  │ 's    │ ⠄⠎ │ endword 's ⠄⠎      │
    │ 4  │       │ ⠀  │ space   ⠀          │
    │ 5  │ about │ ⠁⠃ │ word about ⠁⠃      │
    │ 6  │       │ ⠀  │ space   ⠀          │
    │ 7  │ th    │ ⠹  │ always th ⠹        │
    │ 8  │ e     │ ⠑  │ lowercase e ⠑      │
    │ 9  │       │ ⠀  │ space   ⠀          │
    │ 10 │ blind │ ⠃⠇ │ word blind ⠃⠇      │
    └────┴───────┴────┴────────────────────┘
    > 123st
    ⠼⠁⠃⠉⠌
    ┌───┬──────┬────┬──────────────┐
    │   │ From │ To │ Rule         │
    ├───┼──────┼────┼──────────────┤
    │ 0 │      │ ⠼  │ numsign ⠼    │
    │ 1 │ 1    │ ⠁  │ litdigit 1 ⠁ │
    │ 2 │ 2    │ ⠃  │ litdigit 2 ⠃ │
    │ 3 │ 3    │ ⠉  │ litdigit 3 ⠉ │
    │ 4 │ st   │ ⠌  │ endnum st ⠌  │
    └───┴──────┴────┴──────────────┘
    > about
    ⠁⠃
    ┌───┬───────┬────┬───────────────┐
    │   │ From  │ To │ Rule          │
    ├───┼───────┼────┼───────────────┤
    │ 0 │ about │ ⠁⠃ │ word about ⠁⠃ │
    └───┴───────┴────┴───────────────┘
    > ab
    ⠰⠁⠃
    ┌───┬──────┬────┬────────────────┐
    │   │ From │ To │ Rule           │
    ├───┼──────┼────┼────────────────┤
    │ 0 │      │ ⠰  │ letsign ⠰      │
    │ 1 │ ab   │ ⠁⠃ │ contraction ab │
    └───┴──────┴────┴────────────────┘

Test the parser:

    $ cargo run -- parse
    > nofor letter e 123-1
    Letter { character: 'e', dots: [EnumSet(Dot1 | Dot2 | Dot3), EnumSet(Dot1)], constraints: Constraints(EnumSet(Nofor)) }

Build a release version:

    $ cargo build --release

Run the tests in a YAML file:

    $ LOUIS_TABLE_PATH=~/src/liblouis/tables cargo run -- check --summary ~/src/liblouis/tests/braille-specs/de-de-comp8.yaml
    ┌──────────────────┬───────┬───────────┬──────────┬──────────┬────────────┐
    │ YAML File        │ Tests │ Successes │ Failures │ Expected │ Unexpected │
    │                  │       │           │          │ Failures │ Successes  │
    ├──────────────────┼───────┼───────────┼──────────┼──────────┼────────────┤
    │ de-de-comp8.yaml │ 8     │ 100.0%    │ 0.0%     │ 0.0%     │ 0.0%       │
    ┌──────────────────┌───────┌───────────┌──────────┌──────────┌────────────┐
    │ Total            │ 8     │ 100.0%    │ 0.0%     │ 0.0%     │ 0.0%       │
    └──────────────────└───────└───────────└──────────└──────────└────────────┘

Run all YAML tests:

    $ LOUIS_TABLE_PATH=~/src/liblouis/tables:~/src/liblouis ./target/release/louis check --summary ~/src/liblouis/tests/braille-specs/*.yaml ~/src/liblouis/tests/yaml/*.yaml 2> /dev/null
    ┌─────────────────────────────────────────────┬────────┬───────────┬──────────┬──────────┬────────────┐
    │ YAML File                                   │ Tests  │ Successes │ Failures │ Expected │ Unexpected │
    │                                             │        │           │          │ Failures │ Successes  │
    ├─────────────────────────────────────────────┼────────┼───────────┼──────────┼──────────┼────────────┤
    │ de-eurobrl6.yaml                            │ 0      │ NaN%      │ NaN%     │ NaN%     │ NaN%       │
    │ hu-hu-g1-hyph_harness.yaml                  │ 0      │ NaN%      │ NaN%     │ NaN%     │ NaN%       │
    │ no_8dot_harness.yaml                        │ 0      │ NaN%      │ NaN%     │ NaN%     │ NaN%       │
    │ en-us-comp8-ext-back_harness.yaml           │ 1      │ 0.0%      │ 100.0%   │ 0.0%     │ 0.0%       │
    │ en-us-g1.yaml                               │ 1      │ 100.0%    │ 0.0%     │ 0.0%     │ 0.0%       │
    │ pass0_typebuf.yaml                          │ 1      │ 0.0%      │ 100.0%   │ 0.0%     │ 0.0%       │
    │ hi_harness.yaml                             │ 2      │ 50.0%     │ 0.0%     │ 50.0%    │ 0.0%       │
    │ ko-g2_harness.yaml                          │ 2      │ 50.0%     │ 50.0%    │ 0.0%     │ 0.0%       │
    │ capsnocont.yaml                             │ 2      │ 50.0%     │ 50.0%    │ 0.0%     │ 0.0%       │
    │ compbrlAtCursor_with_equals.yaml            │ 2      │ 0.0%      │ 100.0%   │ 0.0%     │ 0.0%       │
    │ issue-479.yaml                              │ 2      │ 0.0%      │ 50.0%    │ 50.0%    │ 0.0%       │
    │ broken_equals_operand.yaml                  │ 3      │ 66.7%     │ 0.0%     │ 33.3%    │ 0.0%       │
    │ issue-615.yaml                              │ 3      │ 0.0%      │ 100.0%   │ 0.0%     │ 0.0%       │
    │ fr-bfu-g2_harness.yaml                      │ 4      │ 25.0%     │ 75.0%    │ 0.0%     │ 0.0%       │
    │ it.yaml                                     │ 4      │ 50.0%     │ 0.0%     │ 50.0%    │ 0.0%       │
    │ my-g2.yaml                                  │ 4      │ 0.0%      │ 100.0%   │ 0.0%     │ 0.0%       │
    │ back_cont_then_punc.yaml                    │ 4      │ 25.0%     │ 75.0%    │ 0.0%     │ 0.0%       │
    │ input-length.yaml                           │ 4      │ 0.0%      │ 100.0%   │ 0.0%     │ 0.0%       │
    │ before_begmidword.yaml                      │ 5      │ 0.0%      │ 100.0%   │ 0.0%     │ 0.0%       │
    │ critical-apparatus.yaml                     │ 6      │ 100.0%    │ 0.0%     │ 0.0%     │ 0.0%       │
    │ en-gb-g1_harness.yaml                       │ 6      │ 50.0%     │ 0.0%     │ 50.0%    │ 0.0%       │
    │ en-us-comp8-ext-for_harness.yaml            │ 6      │ 83.3%     │ 16.7%    │ 0.0%     │ 0.0%       │
    │ begcaps_endcaps.yaml                        │ 6      │ 33.3%     │ 33.3%    │ 33.3%    │ 0.0%       │
    │ computer_braille.yaml                       │ 6      │ 16.7%     │ 50.0%    │ 33.3%    │ 0.0%       │
    │ example_test.yaml                           │ 6      │ 66.7%     │ 16.7%    │ 16.7%    │ 0.0%       │
    │ issue-963.yaml                              │ 6      │ 50.0%     │ 50.0%    │ 0.0%     │ 0.0%       │
    │ attribute.yaml                              │ 7      │ 0.0%      │ 100.0%   │ 0.0%     │ 0.0%       │
    │ multipass-negation.yaml                     │ 7      │ 0.0%      │ 100.0%   │ 0.0%     │ 0.0%       │
    │ multipass.yaml                              │ 7      │ 0.0%      │ 100.0%   │ 0.0%     │ 0.0%       │
    │ chr-us-g1_harness.yaml                      │ 8      │ 0.0%      │ 0.0%     │ 100.0%   │ 0.0%       │
    │ cs-comp8_harness.yaml                       │ 8      │ 62.5%     │ 37.5%    │ 0.0%     │ 0.0%       │
    │ de-comp6.yaml                               │ 8      │ 100.0%    │ 0.0%     │ 0.0%     │ 0.0%       │
    │ de-de-comp8.yaml                            │ 8      │ 100.0%    │ 0.0%     │ 0.0%     │ 0.0%       │
    │ el-backward.yaml                            │ 8      │ 0.0%      │ 100.0%   │ 0.0%     │ 0.0%       │
    │ en-nabcc.yaml                               │ 8      │ 100.0%    │ 0.0%     │ 0.0%     │ 0.0%       │
    │ en-ueb-math.yaml                            │ 8      │ 100.0%    │ 0.0%     │ 0.0%     │ 0.0%       │
    │ hr-8dots_harness.yaml                       │ 8      │ 62.5%     │ 12.5%    │ 25.0%    │ 0.0%       │
    │ letterDefTest_harness.yaml                  │ 8      │ 100.0%    │ 0.0%     │ 0.0%     │ 0.0%       │
    │ precedence.yaml                             │ 8      │ 87.5%     │ 12.5%    │ 0.0%     │ 0.0%       │
    │ arabic.grade2.issue.yaml                    │ 9      │ 11.1%     │ 55.6%    │ 33.3%    │ 0.0%       │
    │ ko-2006-g2_harness.yaml                     │ 9      │ 11.1%     │ 66.7%    │ 22.2%    │ 0.0%       │
    │ present_progressive.yaml                    │ 9      │ 0.0%      │ 44.4%    │ 55.6%    │ 0.0%       │
    │ zh-chn.yaml                                 │ 10     │ 100.0%    │ 0.0%     │ 0.0%     │ 0.0%       │
    │ zhcn-g1.yaml                                │ 10     │ 60.0%     │ 40.0%    │ 0.0%     │ 0.0%       │
    │ zhcn-g2.yaml                                │ 10     │ 60.0%     │ 40.0%    │ 0.0%     │ 0.0%       │
    │ match-vs-always.yaml                        │ 10     │ 50.0%     │ 50.0%    │ 0.0%     │ 0.0%       │
    │ nonumsign.yaml                              │ 10     │ 0.0%      │ 100.0%   │ 0.0%     │ 0.0%       │
    │ face-with-tears-of-joy-ucs4.yaml            │ 11     │ 0.0%      │ 100.0%   │ 0.0%     │ 0.0%       │
    │ kk.yaml                                     │ 12     │ 8.3%      │ 91.7%    │ 0.0%     │ 0.0%       │
    │ sah.yaml                                    │ 12     │ 0.0%      │ 100.0%   │ 0.0%     │ 0.0%       │
    │ partialTrans.yaml                           │ 12     │ 33.3%     │ 66.7%    │ 0.0%     │ 0.0%       │
    │ he-IL.yaml                                  │ 14     │ 50.0%     │ 42.9%    │ 7.1%     │ 0.0%       │
    │ tt.yaml                                     │ 15     │ 0.0%      │ 86.7%    │ 13.3%    │ 0.0%       │
    │ case-sensitivity.yaml                       │ 15     │ 20.0%     │ 60.0%    │ 6.7%     │ 13.3%      │
    │ litdigits6Dots_backward.yaml                │ 16     │ 0.0%      │ 100.0%   │ 0.0%     │ 0.0%       │
    │ yi.yaml                                     │ 16     │ 31.2%     │ 68.8%    │ 0.0%     │ 0.0%       │
    │ en-ueb-g1_backward.yaml                     │ 17     │ 5.9%      │ 94.1%    │ 0.0%     │ 0.0%       │
    │ iu-ca-g1_harness.yaml                       │ 17     │ 0.0%      │ 94.1%    │ 5.9%     │ 0.0%       │
    │ akk-borger.yaml                             │ 18     │ 100.0%    │ 0.0%     │ 0.0%     │ 0.0%       │
    │ akk.yaml                                    │ 18     │ 100.0%    │ 0.0%     │ 0.0%     │ 0.0%       │
    │ pl-pl-comp8_harness.yaml                    │ 18     │ 100.0%    │ 0.0%     │ 0.0%     │ 0.0%       │
    │ repword.yaml                                │ 18     │ 0.0%      │ 100.0%   │ 0.0%     │ 0.0%       │
    │ cop.yaml                                    │ 19     │ 94.7%     │ 5.3%     │ 0.0%     │ 0.0%       │
    │ issue-332.yaml                              │ 19     │ 0.0%      │ 100.0%   │ 0.0%     │ 0.0%       │
    │ ka.yaml                                     │ 20     │ 60.0%     │ 40.0%    │ 0.0%     │ 0.0%       │
    │ squash_space.yaml                           │ 21     │ 0.0%      │ 100.0%   │ 0.0%     │ 0.0%       │
    │ en-us-comp6.yaml                            │ 22     │ 68.2%     │ 31.8%    │ 0.0%     │ 0.0%       │
    │ en-us-g2.yaml                               │ 22     │ 4.5%      │ 95.5%    │ 0.0%     │ 0.0%       │
    │ ro-g0.yaml                                  │ 23     │ 30.4%     │ 60.9%    │ 8.7%     │ 0.0%       │
    │ zh-tw.yaml                                  │ 23     │ 69.6%     │ 30.4%    │ 0.0%     │ 0.0%       │
    │ ar-ar-comp8.yaml                            │ 24     │ 100.0%    │ 0.0%     │ 0.0%     │ 0.0%       │
    │ various-regression-tests.yaml               │ 24     │ 12.5%     │ 87.5%    │ 0.0%     │ 0.0%       │
    │ uga.yaml                                    │ 27     │ 100.0%    │ 0.0%     │ 0.0%     │ 0.0%       │
    │ new_emph.yaml                               │ 27     │ 0.0%      │ 100.0%   │ 0.0%     │ 0.0%       │
    │ fr-bfu-comp8.yaml                           │ 28     │ 96.4%     │ 3.6%     │ 0.0%     │ 0.0%       │
    │ emphasis.yaml                               │ 28     │ 10.7%     │ 89.3%    │ 0.0%     │ 0.0%       │
    │ fil.yaml                                    │ 29     │ 0.0%      │ 100.0%   │ 0.0%     │ 0.0%       │
    │ mixed-case.yaml                             │ 30     │ 0.0%      │ 63.3%    │ 36.7%    │ 0.0%       │
    │ ipa.yaml                                    │ 34     │ 100.0%    │ 0.0%     │ 0.0%     │ 0.0%       │
    │ capitalization.yaml                         │ 34     │ 32.4%     │ 67.6%    │ 0.0%     │ 0.0%       │
    │ lt.yaml                                     │ 36     │ 11.1%     │ 88.9%    │ 0.0%     │ 0.0%       │
    │ syc.yaml                                    │ 36     │ 91.7%     │ 0.0%     │ 0.0%     │ 8.3%       │
    │ fi_harness.yaml                             │ 38     │ 50.0%     │ 47.4%    │ 2.6%     │ 0.0%       │
    │ kmr.yaml                                    │ 39     │ 100.0%    │ 0.0%     │ 0.0%     │ 0.0%       │
    │ capsword.yaml                               │ 41     │ 14.6%     │ 80.5%    │ 4.9%     │ 0.0%       │
    │ bel.yaml                                    │ 45     │ 73.3%     │ 26.7%    │ 0.0%     │ 0.0%       │
    │ nl-comp8_harness.yaml                       │ 45     │ 62.2%     │ 37.8%    │ 0.0%     │ 0.0%       │
    │ uk.yaml                                     │ 45     │ 77.8%     │ 22.2%    │ 0.0%     │ 0.0%       │
    │ en-us-emphasis_harness.yaml                 │ 50     │ 0.0%      │ 100.0%   │ 0.0%     │ 0.0%       │
    │ ga-g1_harness.yaml                          │ 51     │ 94.1%     │ 5.9%     │ 0.0%     │ 0.0%       │
    │ sr.yaml                                     │ 64     │ 43.8%     │ 15.6%    │ 37.5%    │ 3.1%       │
    │ en-ueb-g1_harness.yaml                      │ 67     │ 83.6%     │ 16.4%    │ 0.0%     │ 0.0%       │
    │ kn.yaml                                     │ 77     │ 39.0%     │ 61.0%    │ 0.0%     │ 0.0%       │
    │ ga-g2_harness.yaml                          │ 80     │ 37.5%     │ 62.5%    │ 0.0%     │ 0.0%       │
    │ fr-bfu-comp6.yaml                           │ 82     │ 35.4%     │ 58.5%    │ 6.1%     │ 0.0%       │
    │ vi.yaml                                     │ 82     │ 32.9%     │ 67.1%    │ 0.0%     │ 0.0%       │
    │ bn.yaml                                     │ 87     │ 88.5%     │ 11.5%    │ 0.0%     │ 0.0%       │
    │ el-forward.yaml                             │ 93     │ 71.0%     │ 29.0%    │ 0.0%     │ 0.0%       │
    │ cuneiform-transliterated.yaml               │ 102    │ 67.6%     │ 32.4%    │ 0.0%     │ 0.0%       │
    │ nemeth.yaml                                 │ 133    │ 100.0%    │ 0.0%     │ 0.0%     │ 0.0%       │
    │ ru.yaml                                     │ 142    │ 23.9%     │ 73.9%    │ 2.1%     │ 0.0%       │
    │ sl-g1.yaml                                  │ 143    │ 45.5%     │ 48.3%    │ 4.9%     │ 1.4%       │
    │ ar-ar-g2.yaml                               │ 168    │ 61.9%     │ 38.1%    │ 0.0%     │ 0.0%       │
    │ hu-hu-comp8_harness.yaml                    │ 173    │ 78.6%     │ 21.4%    │ 0.0%     │ 0.0%       │
    │ hu-hu-g1_braille_input_backward.yaml        │ 174    │ 6.9%      │ 92.0%    │ 1.1%     │ 0.0%       │
    │ hu-hu-g1_braille_input_forward.yaml         │ 175    │ 49.1%     │ 50.9%    │ 0.0%     │ 0.0%       │
    │ pl-g1.yaml                                  │ 202    │ 54.0%     │ 46.0%    │ 0.0%     │ 0.0%       │
    │ lv_harness.yaml                             │ 214    │ 98.1%     │ 0.5%     │ 1.4%     │ 0.0%       │
    │ ar-ar-g1.yaml                               │ 266    │ 90.6%     │ 9.4%     │ 0.0%     │ 0.0%       │
    │ es-g2.yaml                                  │ 266    │ 27.1%     │ 70.3%    │ 2.6%     │ 0.0%       │
    │ eo-g1_harness.yaml                          │ 285    │ 67.4%     │ 32.6%    │ 0.0%     │ 0.0%       │
    │ ethio-g1_harness.yaml                       │ 301    │ 100.0%    │ 0.0%     │ 0.0%     │ 0.0%       │
    │ nl-g0_harness.yaml                          │ 368    │ 43.8%     │ 55.4%    │ 0.3%     │ 0.5%       │
    │ hbo.yaml                                    │ 469    │ 16.8%     │ 82.3%    │ 0.9%     │ 0.0%       │
    │ de-g0-detailed-specs.yaml                   │ 476    │ 43.1%     │ 51.5%    │ 5.0%     │ 0.4%       │
    │ en-GB-g2.yaml                               │ 528    │ 53.0%     │ 22.7%    │ 22.9%    │ 1.3%       │
    │ de-g0.yaml                                  │ 550    │ 43.8%     │ 45.8%    │ 9.8%     │ 0.5%       │
    │ ur-pk-g2.yaml                               │ 578    │ 57.8%     │ 33.6%    │ 8.7%     │ 0.0%       │
    │ en-ueb-symbols_harness.yaml                 │ 593    │ 98.8%     │ 0.7%     │ 0.5%     │ 0.0%       │
    │ es-g0-g1.yaml                               │ 992    │ 66.8%     │ 5.7%     │ 27.4%    │ 0.0%       │
    │ pt.yaml                                     │ 1163   │ 82.2%     │ 14.7%    │ 3.0%     │ 0.1%       │
    │ ms-my-g2.yaml                               │ 1432   │ 95.7%     │ 4.2%     │ 0.1%     │ 0.0%       │
    │ tr.yaml                                     │ 1654   │ 41.2%     │ 28.6%    │ 30.1%    │ 0.1%       │
    │ sw-ke.yaml                                  │ 1884   │ 47.5%     │ 52.5%    │ 0.0%     │ 0.0%       │
    │ ve-g2.yaml                                  │ 2038   │ 46.0%     │ 53.9%    │ 0.0%     │ 0.0%       │
    │ fa-ir-g1-harness.yaml                       │ 2108   │ 97.7%     │ 2.3%     │ 0.0%     │ 0.0%       │
    │ fr-bfu-g2.yaml                              │ 2145   │ 48.6%     │ 51.4%    │ 0.0%     │ 0.0%       │
    │ hu-hu-g1_harness.yaml                       │ 2532   │ 38.5%     │ 61.5%    │ 0.0%     │ 0.0%       │
    │ lo.yaml                                     │ 2637   │ 100.0%    │ 0.0%     │ 0.0%     │ 0.0%       │
    │ ny-mw.yaml                                  │ 3250   │ 48.3%     │ 51.7%    │ 0.0%     │ 0.0%       │
    │ st-g2.yaml                                  │ 3774   │ 57.3%     │ 42.2%    │ 0.1%     │ 0.4%       │
    │ fa-ir-comp8-harness.yaml                    │ 4212   │ 99.9%     │ 0.1%     │ 0.0%     │ 0.0%       │
    │ ta-ta-g1_harness.yaml                       │ 4690   │ 8.8%      │ 90.4%    │ 0.7%     │ 0.1%       │
    │ pa.yaml                                     │ 5027   │ 50.6%     │ 49.4%    │ 0.0%     │ 0.0%       │
    │ zh-tw-dictionary.yaml                       │ 5139   │ 4.0%      │ 96.0%    │ 0.0%     │ 0.0%       │
    │ ja-rokutenkanji.yaml                        │ 7010   │ 99.8%     │ 0.2%     │ 0.0%     │ 0.0%       │
    │ ml.yaml                                     │ 7447   │ 34.3%     │ 65.6%    │ 0.1%     │ 0.0%       │
    │ ar-ar-g1_harness.yaml                       │ 7596   │ 61.2%     │ 38.8%    │ 0.0%     │ 0.0%       │
    │ lg-ug-g1.yaml                               │ 8283   │ 50.3%     │ 49.7%    │ 0.0%     │ 0.0%       │
    │ rw-rw-g1.yaml                               │ 9839   │ 48.4%     │ 51.6%    │ 0.0%     │ 0.0%       │
    │ sw-ke-dictionary.yaml                       │ 10966  │ 41.3%     │ 58.7%    │ 0.0%     │ 0.0%       │
    │ de-g0-detailed-dictionary.yaml              │ 19996  │ 50.1%     │ 49.8%    │ 0.0%     │ 0.0%       │
    │ hu-hu-g1_dictionary_numbers.yaml            │ 34246  │ 45.3%     │ 54.7%    │ 0.0%     │ 0.0%       │
    │ afr-za-g2.yaml                              │ 51824  │ 44.0%     │ 56.0%    │ 0.0%     │ 0.0%       │
    │ en-us-g2-dictionary_harness.yaml            │ 93796  │ 82.9%     │ 7.1%     │ 9.7%     │ 0.3%       │
    │ hu-hu-g1_dictionary_special_consonants.yaml │ 178624 │ 45.1%     │ 54.9%    │ 0.0%     │ 0.0%       │
    ┌─────────────────────────────────────────────┌────────┌───────────┌──────────┌──────────┌────────────┐
    │ Total                                       │ 482615 │ 54.3%     │ 43.4%    │ 2.1%     │ 0.1%       │
    └─────────────────────────────────────────────└────────└───────────└──────────└──────────└────────────┘

Test the table query functionality:

    $ LOUIS_TABLE_PATH=~/src/liblouis/tables cargo run -- query language=de,contraction=full
    {"[...]/liblouis/tables/de-g2-detailed.ctb", "[...]/liblouis/tables/de-g2.ctb"}


## Prerequisites

-   You need the [Rust tool chain](https://www.rust-lang.org/).


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

Copyright (C) 2023-2025 Swiss Library for the Blind, Visually Impaired
and Print Disabled

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see
<https://www.gnu.org/licenses/>.

