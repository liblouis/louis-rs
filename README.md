

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

That said, the `louis` binary currently passes around 89% of the
liblouis test suite (forward and backward combined).

The library and its API has not been worked out and is not stable.


## Relation to liblouis

louis-rs is **not** a direct port of the liblouis C code to Rust. It
uses the same tables and the same YAML tests but other than that it is
a complete rewrite. It uses different data structures and does the
translation using a different algorithm.

The goal is to be as compatible as possible with liblouis, when it
makes sense.


## Installation

    $ cargo install louis-rs


## Usage

Get help:

    $ louis help

Translate some text:

    $ export LOUIS_TABLE_PATH=~/src/liblouis/tables:~/src/liblouis
    $ louis translate de-comp6.utb
    > Guten Tag
    ⠈⠛⠥⠞⠑⠝⠀⠈⠞⠁⠛

Trace a translation:

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
    > 123st
    ⠼⠂⠆⠒⠌
    ┌───┬──────┬────┬─────────────┬───────┐
    │   │ From │ To │ Rule        │ Stage │
    ├───┼──────┼────┼─────────────┼───────┤
    │ 1 │      │ ⠼  │ numsign ⠼   │ Main  │
    │ 2 │ 1    │ ⠂  │ digit 1 ⠂   │ Main  │
    │ 3 │ 2    │ ⠆  │ digit 2 ⠆   │ Main  │
    │ 4 │ 3    │ ⠒  │ digit 3 ⠒   │ Main  │
    │ 5 │ st   │ ⠌  │ endnum st ⠌ │ Main  │
    └───┴──────┴────┴─────────────┴───────┘
    > about
    ⠁⠃
    ┌───┬───────┬────┬───────────────┬───────┐
    │   │ From  │ To │ Rule          │ Stage │
    ├───┼───────┼────┼───────────────┼───────┤
    │ 1 │ about │ ⠁⠃ │ word about ⠁⠃ │ Main  │
    └───┴───────┴────┴───────────────┴───────┘
    > ab
    ⠰⠁⠃
    ┌───┬──────┬────┬────────────────┬───────┐
    │   │ From │ To │ Rule           │ Stage │
    ├───┼──────┼────┼────────────────┼───────┤
    │ 1 │      │ ⠰  │ letsign ⠰      │ Main  │
    │ 2 │ ab   │ ⠁⠃ │ contraction ab │ Main  │
    └───┴──────┴────┴────────────────┴───────┘

Trace a translation with a pre-translation rule:

    $ louis trace en-us-mathtext.ctb
    > cornf abc
    ⠤⠋⠀⠁⠃⠉
    ┌───┬───────┬──────┬──────────────────────┬───────┐
    │   │ From  │ To   │ Rule                 │ Stage │
    ├───┼───────┼──────┼──────────────────────┼───────┤
    │ 1 │ cornf │ comf │ correct "cornf" comf │ Pre   │
    │ 2 │ com   │ ⠤    │ begword com ⠤        │ Main  │
    │ 3 │ f     │ ⠋    │ lowercase f ⠋        │ Main  │
    │ 4 │       │ ⠀    │ space   ⠀            │ Main  │
    │ 5 │ a     │ ⠁    │ largesign a ⠁        │ Main  │
    │ 6 │ b     │ ⠃    │ lowercase b ⠃        │ Main  │
    │ 7 │ c     │ ⠉    │ lowercase c ⠉        │ Main  │
    └───┴───────┴──────┴──────────────────────┴───────┘

Test the parser:

    $ louis parse
    > nofor letter e 123-1
    Letter { character: 'e', dots: BrailleChars([BrailleChar(EnumSet(Dot1 | Dot2 | Dot3)), BrailleChar(EnumSet(Dot1))]), constraints: Constraints(EnumSet(Nofor)) }

Build a release version:

    $ cargo build --release

Run the tests in a YAML file:

    $ export LOUIS_TABLE_PATH=~/src/liblouis/tables:~/src/liblouis
    $ louis check --summary ~/src/liblouis/tests/braille-specs/de-de-comp8.yaml
    ┌──────────────────┬───────┬───────────┬──────────┬──────────┬────────────┐
    │ YAML File        │ Tests │ Successes │ Failures │ Expected │ Unexpected │
    │                  │       │           │          │ Failures │ Successes  │
    ├──────────────────┼───────┼───────────┼──────────┼──────────┼────────────┤
    │ de-de-comp8.yaml │ 8     │ 100.0%    │ 0.0%     │ 0.0%     │ 0.0%       │
    ┌──────────────────┌───────┌───────────┌──────────┌──────────┌────────────┐
    │ Total            │ 8     │ 100.0%    │ 0.0%     │ 0.0%     │ 0.0%       │
    └──────────────────└───────└───────────└──────────└──────────└────────────┘

Run all YAML tests:

    $ export LOUIS_TABLE_PATH=~/src/liblouis/tables:~/src/liblouis
    $ louis check --summary ~/src/liblouis/tests/braille-specs/*.yaml ~/src/liblouis/tests/yaml/*.yaml 2> /dev/null
    ┌─────────────────────────────────────────────┬────────┬───────────┬──────────┬──────────┬────────────┐
    │ YAML File                                   │ Tests  │ Successes │ Failures │ Expected │ Unexpected │
    │                                             │        │           │          │ Failures │ Successes  │
    ├─────────────────────────────────────────────┼────────┼───────────┼──────────┼──────────┼────────────┤
    │ de-eurobrl6.yaml                            │ 0      │ NaN%      │ NaN%     │ NaN%     │ NaN%       │
    │ hu-hu-g1-hyph_harness.yaml                  │ 0      │ NaN%      │ NaN%     │ NaN%     │ NaN%       │
    │ no_8dot_harness.yaml                        │ 0      │ NaN%      │ NaN%     │ NaN%     │ NaN%       │
    │ en-ueb-g2_backward_no_dis.yaml              │ 1      │ 0.0%      │ 100.0%   │ 0.0%     │ 0.0%       │
    │ en-us-comp8-ext-back_harness.yaml           │ 1      │ 0.0%      │ 100.0%   │ 0.0%     │ 0.0%       │
    │ en-us-g1.yaml                               │ 1      │ 100.0%    │ 0.0%     │ 0.0%     │ 0.0%       │
    │ issue-1863-basechar-finalize.yaml           │ 1      │ 0.0%      │ 100.0%   │ 0.0%     │ 0.0%       │
    │ pass0_typebuf.yaml                          │ 1      │ 0.0%      │ 100.0%   │ 0.0%     │ 0.0%       │
    │ ko-g2_harness.yaml                          │ 2      │ 100.0%    │ 0.0%     │ 0.0%     │ 0.0%       │
    │ capsnocont.yaml                             │ 2      │ 50.0%     │ 50.0%    │ 0.0%     │ 0.0%       │
    │ compbrlAtCursor_with_equals.yaml            │ 2      │ 0.0%      │ 100.0%   │ 0.0%     │ 0.0%       │
    │ issue-479.yaml                              │ 2      │ 0.0%      │ 50.0%    │ 50.0%    │ 0.0%       │
    │ broken_equals_operand.yaml                  │ 3      │ 66.7%     │ 0.0%     │ 0.0%     │ 33.3%      │
    │ issue-615.yaml                              │ 3      │ 0.0%      │ 100.0%   │ 0.0%     │ 0.0%       │
    │ fr-bfu-g2_harness.yaml                      │ 4      │ 25.0%     │ 75.0%    │ 0.0%     │ 0.0%       │
    │ my-g2.yaml                                  │ 4      │ 0.0%      │ 100.0%   │ 0.0%     │ 0.0%       │
    │ back_cont_then_punc.yaml                    │ 4      │ 25.0%     │ 75.0%    │ 0.0%     │ 0.0%       │
    │ input-length.yaml                           │ 4      │ 0.0%      │ 100.0%   │ 0.0%     │ 0.0%       │
    │ before_begmidword.yaml                      │ 5      │ 0.0%      │ 100.0%   │ 0.0%     │ 0.0%       │
    │ critical-apparatus.yaml                     │ 6      │ 100.0%    │ 0.0%     │ 0.0%     │ 0.0%       │
    │ en-gb-g1_harness.yaml                       │ 6      │ 50.0%     │ 0.0%     │ 50.0%    │ 0.0%       │
    │ en-us-comp8-ext-for_harness.yaml            │ 6      │ 83.3%     │ 16.7%    │ 0.0%     │ 0.0%       │
    │ begcaps_endcaps.yaml                        │ 6      │ 66.7%     │ 0.0%     │ 33.3%    │ 0.0%       │
    │ computer_braille.yaml                       │ 6      │ 33.3%     │ 33.3%    │ 16.7%    │ 16.7%      │
    │ example_test.yaml                           │ 6      │ 83.3%     │ 0.0%     │ 16.7%    │ 0.0%       │
    │ issue-963.yaml                              │ 6      │ 50.0%     │ 50.0%    │ 0.0%     │ 0.0%       │
    │ attribute.yaml                              │ 7      │ 85.7%     │ 14.3%    │ 0.0%     │ 0.0%       │
    │ multipass-negation.yaml                     │ 7      │ 0.0%      │ 100.0%   │ 0.0%     │ 0.0%       │
    │ multipass.yaml                              │ 7      │ 0.0%      │ 100.0%   │ 0.0%     │ 0.0%       │
    │ chr-us-g1_harness.yaml                      │ 8      │ 0.0%      │ 0.0%     │ 100.0%   │ 0.0%       │
    │ cs-comp8_harness.yaml                       │ 8      │ 62.5%     │ 37.5%    │ 0.0%     │ 0.0%       │
    │ de-comp6.yaml                               │ 8      │ 100.0%    │ 0.0%     │ 0.0%     │ 0.0%       │
    │ de-de-comp8.yaml                            │ 8      │ 100.0%    │ 0.0%     │ 0.0%     │ 0.0%       │
    │ el-backward.yaml                            │ 8      │ 12.5%     │ 87.5%    │ 0.0%     │ 0.0%       │
    │ en-nabcc.yaml                               │ 8      │ 100.0%    │ 0.0%     │ 0.0%     │ 0.0%       │
    │ en-ueb-math.yaml                            │ 8      │ 100.0%    │ 0.0%     │ 0.0%     │ 0.0%       │
    │ hr-8dots_harness.yaml                       │ 8      │ 62.5%     │ 12.5%    │ 25.0%    │ 0.0%       │
    │ letterDefTest_harness.yaml                  │ 8      │ 100.0%    │ 0.0%     │ 0.0%     │ 0.0%       │
    │ precedence.yaml                             │ 8      │ 75.0%     │ 25.0%    │ 0.0%     │ 0.0%       │
    │ arabic.grade2.issue.yaml                    │ 9      │ 22.2%     │ 44.4%    │ 11.1%    │ 22.2%      │
    │ ko-2006-g2_harness.yaml                     │ 9      │ 77.8%     │ 0.0%     │ 22.2%    │ 0.0%       │
    │ present_progressive.yaml                    │ 9      │ 0.0%      │ 44.4%    │ 55.6%    │ 0.0%       │
    │ zh-chn.yaml                                 │ 10     │ 100.0%    │ 0.0%     │ 0.0%     │ 0.0%       │
    │ zhcn-g1.yaml                                │ 10     │ 100.0%    │ 0.0%     │ 0.0%     │ 0.0%       │
    │ zhcn-g2.yaml                                │ 10     │ 90.0%     │ 10.0%    │ 0.0%     │ 0.0%       │
    │ match-vs-always.yaml                        │ 10     │ 100.0%    │ 0.0%     │ 0.0%     │ 0.0%       │
    │ nonumsign.yaml                              │ 10     │ 0.0%      │ 100.0%   │ 0.0%     │ 0.0%       │
    │ face-with-tears-of-joy-ucs4.yaml            │ 11     │ 0.0%      │ 100.0%   │ 0.0%     │ 0.0%       │
    │ kk.yaml                                     │ 12     │ 91.7%     │ 8.3%     │ 0.0%     │ 0.0%       │
    │ sah.yaml                                    │ 12     │ 91.7%     │ 8.3%     │ 0.0%     │ 0.0%       │
    │ partialTrans.yaml                           │ 12     │ 33.3%     │ 66.7%    │ 0.0%     │ 0.0%       │
    │ he-IL.yaml                                  │ 14     │ 71.4%     │ 21.4%    │ 7.1%     │ 0.0%       │
    │ tt.yaml                                     │ 15     │ 66.7%     │ 20.0%    │ 13.3%    │ 0.0%       │
    │ case-sensitivity.yaml                       │ 15     │ 40.0%     │ 40.0%    │ 13.3%    │ 6.7%       │
    │ yi.yaml                                     │ 16     │ 93.8%     │ 6.2%     │ 0.0%     │ 0.0%       │
    │ iu-ca-g1_harness.yaml                       │ 17     │ 0.0%      │ 94.1%    │ 5.9%     │ 0.0%       │
    │ akk-borger.yaml                             │ 18     │ 100.0%    │ 0.0%     │ 0.0%     │ 0.0%       │
    │ akk.yaml                                    │ 18     │ 100.0%    │ 0.0%     │ 0.0%     │ 0.0%       │
    │ pl-pl-comp8_harness.yaml                    │ 18     │ 100.0%    │ 0.0%     │ 0.0%     │ 0.0%       │
    │ repword.yaml                                │ 18     │ 0.0%      │ 100.0%   │ 0.0%     │ 0.0%       │
    │ cop.yaml                                    │ 19     │ 89.5%     │ 10.5%    │ 0.0%     │ 0.0%       │
    │ issue-332.yaml                              │ 19     │ 0.0%      │ 100.0%   │ 0.0%     │ 0.0%       │
    │ ka.yaml                                     │ 20     │ 100.0%    │ 0.0%     │ 0.0%     │ 0.0%       │
    │ squash_space.yaml                           │ 21     │ 0.0%      │ 100.0%   │ 0.0%     │ 0.0%       │
    │ en-us-comp6.yaml                            │ 22     │ 90.9%     │ 9.1%     │ 0.0%     │ 0.0%       │
    │ en-us-g2.yaml                               │ 22     │ 59.1%     │ 40.9%    │ 0.0%     │ 0.0%       │
    │ ro-g0.yaml                                  │ 23     │ 69.6%     │ 21.7%    │ 8.7%     │ 0.0%       │
    │ zh-tw.yaml                                  │ 23     │ 69.6%     │ 30.4%    │ 0.0%     │ 0.0%       │
    │ ar-ar-comp8.yaml                            │ 24     │ 100.0%    │ 0.0%     │ 0.0%     │ 0.0%       │
    │ various-regression-tests.yaml               │ 24     │ 16.7%     │ 83.3%    │ 0.0%     │ 0.0%       │
    │ mk.yaml                                     │ 25     │ 48.0%     │ 40.0%    │ 12.0%    │ 0.0%       │
    │ en-ueb-g1_backward.yaml                     │ 26     │ 42.3%     │ 57.7%    │ 0.0%     │ 0.0%       │
    │ uga.yaml                                    │ 27     │ 100.0%    │ 0.0%     │ 0.0%     │ 0.0%       │
    │ new_emph.yaml                               │ 27     │ 100.0%    │ 0.0%     │ 0.0%     │ 0.0%       │
    │ fr-bfu-comp8.yaml                           │ 28     │ 100.0%    │ 0.0%     │ 0.0%     │ 0.0%       │
    │ emphasis.yaml                               │ 28     │ 96.4%     │ 3.6%     │ 0.0%     │ 0.0%       │
    │ fil.yaml                                    │ 29     │ 27.6%     │ 72.4%    │ 0.0%     │ 0.0%       │
    │ mixed-case.yaml                             │ 30     │ 0.0%      │ 63.3%    │ 36.7%    │ 0.0%       │
    │ ipa.yaml                                    │ 34     │ 100.0%    │ 0.0%     │ 0.0%     │ 0.0%       │
    │ capitalization.yaml                         │ 34     │ 50.0%     │ 50.0%    │ 0.0%     │ 0.0%       │
    │ et_harness.yaml                             │ 36     │ 72.2%     │ 27.8%    │ 0.0%     │ 0.0%       │
    │ lt.yaml                                     │ 36     │ 16.7%     │ 83.3%    │ 0.0%     │ 0.0%       │
    │ syc.yaml                                    │ 36     │ 91.7%     │ 0.0%     │ 8.3%     │ 0.0%       │
    │ fi_harness.yaml                             │ 38     │ 89.5%     │ 7.9%     │ 2.6%     │ 0.0%       │
    │ kmr.yaml                                    │ 39     │ 100.0%    │ 0.0%     │ 0.0%     │ 0.0%       │
    │ bel.yaml                                    │ 45     │ 88.9%     │ 11.1%    │ 0.0%     │ 0.0%       │
    │ nl-comp8_harness.yaml                       │ 45     │ 62.2%     │ 37.8%    │ 0.0%     │ 0.0%       │
    │ uk.yaml                                     │ 45     │ 84.4%     │ 15.6%    │ 0.0%     │ 0.0%       │
    │ capsword.yaml                               │ 45     │ 26.7%     │ 68.9%    │ 4.4%     │ 0.0%       │
    │ backtranslation_emphasis.yaml               │ 48     │ 2.1%      │ 97.9%    │ 0.0%     │ 0.0%       │
    │ mn-MN_harness.yaml                          │ 49     │ 79.6%     │ 20.4%    │ 0.0%     │ 0.0%       │
    │ en-us-emphasis_harness.yaml                 │ 50     │ 100.0%    │ 0.0%     │ 0.0%     │ 0.0%       │
    │ ga-g1_harness.yaml                          │ 51     │ 100.0%    │ 0.0%     │ 0.0%     │ 0.0%       │
    │ sr.yaml                                     │ 64     │ 51.6%     │ 7.8%     │ 40.6%    │ 0.0%       │
    │ en-ueb-g1_harness.yaml                      │ 67     │ 100.0%    │ 0.0%     │ 0.0%     │ 0.0%       │
    │ kn.yaml                                     │ 77     │ 74.0%     │ 26.0%    │ 0.0%     │ 0.0%       │
    │ ga-g2_harness.yaml                          │ 80     │ 82.5%     │ 17.5%    │ 0.0%     │ 0.0%       │
    │ fr-bfu-comp6.yaml                           │ 82     │ 73.2%     │ 20.7%    │ 6.1%     │ 0.0%       │
    │ vi.yaml                                     │ 82     │ 56.1%     │ 43.9%    │ 0.0%     │ 0.0%       │
    │ it.yaml                                     │ 84     │ 95.2%     │ 3.6%     │ 1.2%     │ 0.0%       │
    │ bn.yaml                                     │ 87     │ 88.5%     │ 11.5%    │ 0.0%     │ 0.0%       │
    │ el-forward.yaml                             │ 93     │ 87.1%     │ 12.9%    │ 0.0%     │ 0.0%       │
    │ cuneiform-transliterated.yaml               │ 102    │ 77.5%     │ 22.5%    │ 0.0%     │ 0.0%       │
    │ nemeth.yaml                                 │ 133    │ 100.0%    │ 0.0%     │ 0.0%     │ 0.0%       │
    │ ru.yaml                                     │ 142    │ 42.3%     │ 55.6%    │ 2.1%     │ 0.0%       │
    │ sl-g1.yaml                                  │ 143    │ 58.0%     │ 35.7%    │ 4.9%     │ 1.4%       │
    │ ar-ar-g2.yaml                               │ 168    │ 88.1%     │ 11.9%    │ 0.0%     │ 0.0%       │
    │ hu-hu-comp8_harness.yaml                    │ 173    │ 91.3%     │ 8.7%     │ 0.0%     │ 0.0%       │
    │ hu-hu-g1_braille_input_backward.yaml        │ 174    │ 80.5%     │ 18.4%    │ 0.6%     │ 0.6%       │
    │ hu-hu-g1_braille_input_forward.yaml         │ 175    │ 99.4%     │ 0.6%     │ 0.0%     │ 0.0%       │
    │ pl-g1.yaml                                  │ 202    │ 89.1%     │ 10.9%    │ 0.0%     │ 0.0%       │
    │ lv_harness.yaml                             │ 214    │ 98.6%     │ 0.0%     │ 1.4%     │ 0.0%       │
    │ ar-ar-g1.yaml                               │ 266    │ 93.2%     │ 6.8%     │ 0.0%     │ 0.0%       │
    │ es-g2.yaml                                  │ 266    │ 79.7%     │ 17.7%    │ 2.6%     │ 0.0%       │
    │ hu-hu-g2_dictionary_numbers.yaml            │ 272    │ 99.3%     │ 0.7%     │ 0.0%     │ 0.0%       │
    │ eo-g1_harness.yaml                          │ 285    │ 99.3%     │ 0.7%     │ 0.0%     │ 0.0%       │
    │ ethio-g1_harness.yaml                       │ 301    │ 100.0%    │ 0.0%     │ 0.0%     │ 0.0%       │
    │ en-ueb-g2_backward.yaml                     │ 348    │ 77.3%     │ 22.1%    │ 0.6%     │ 0.0%       │
    │ hbo.yaml                                    │ 469    │ 43.5%     │ 55.7%    │ 0.9%     │ 0.0%       │
    │ de-g0-detailed-specs.yaml                   │ 476    │ 73.9%     │ 20.6%    │ 4.8%     │ 0.6%       │
    │ en-gb-comp8.yaml                            │ 508    │ 100.0%    │ 0.0%     │ 0.0%     │ 0.0%       │
    │ hu-hu-g2_harness.yaml                       │ 511    │ 91.0%     │ 9.0%     │ 0.0%     │ 0.0%       │
    │ en-GB-g2.yaml                               │ 528    │ 70.5%     │ 8.7%     │ 16.5%    │ 4.4%       │
    │ de-g0.yaml                                  │ 550    │ 82.7%     │ 6.9%     │ 10.4%    │ 0.0%       │
    │ ur-pk-g2.yaml                               │ 578    │ 86.5%     │ 4.8%     │ 8.5%     │ 0.2%       │
    │ en-ueb-symbols_harness.yaml                 │ 593    │ 99.5%     │ 0.0%     │ 0.5%     │ 0.0%       │
    │ es-g0-g1.yaml                               │ 992    │ 72.2%     │ 0.4%     │ 27.4%    │ 0.0%       │
    │ pt.yaml                                     │ 1163   │ 94.8%     │ 2.1%     │ 3.0%     │ 0.1%       │
    │ ms-my-g2.yaml                               │ 1432   │ 96.4%     │ 3.5%     │ 0.1%     │ 0.0%       │
    │ tr.yaml                                     │ 1654   │ 60.2%     │ 9.7%     │ 30.0%    │ 0.1%       │
    │ sw-ke.yaml                                  │ 1884   │ 87.3%     │ 12.7%    │ 0.0%     │ 0.0%       │
    │ ve-g2.yaml                                  │ 2038   │ 97.5%     │ 2.5%     │ 0.0%     │ 0.0%       │
    │ fa-ir-g1-harness.yaml                       │ 2108   │ 95.8%     │ 4.2%     │ 0.0%     │ 0.0%       │
    │ fr-bfu-g2.yaml                              │ 2145   │ 90.1%     │ 9.9%     │ 0.0%     │ 0.0%       │
    │ en-ueb.yaml                                 │ 2232   │ 59.7%     │ 29.0%    │ 10.8%    │ 0.5%       │
    │ hu-hu-g1_harness.yaml                       │ 2532   │ 94.5%     │ 5.5%     │ 0.0%     │ 0.0%       │
    │ lo.yaml                                     │ 2637   │ 100.0%    │ 0.0%     │ 0.0%     │ 0.0%       │
    │ en-ueb-computer-code.yaml                   │ 2818   │ 47.6%     │ 51.1%    │ 1.3%     │ 0.0%       │
    │ ny-mw.yaml                                  │ 3250   │ 97.8%     │ 2.2%     │ 0.0%     │ 0.0%       │
    │ st-g2.yaml                                  │ 3774   │ 96.7%     │ 2.9%     │ 0.1%     │ 0.4%       │
    │ xh-g2.yaml                                  │ 4048   │ 96.9%     │ 3.0%     │ 0.0%     │ 0.0%       │
    │ hi_harness.yaml                             │ 4118   │ 90.5%     │ 8.5%     │ 0.9%     │ 0.2%       │
    │ fa-ir-comp8-harness.yaml                    │ 4212   │ 99.9%     │ 0.1%     │ 0.0%     │ 0.0%       │
    │ ta-ta-g1_harness.yaml                       │ 4690   │ 97.2%     │ 2.0%     │ 0.8%     │ 0.0%       │
    │ pa.yaml                                     │ 5027   │ 93.5%     │ 6.5%     │ 0.0%     │ 0.0%       │
    │ zh-tw-dictionary.yaml                       │ 5139   │ 100.0%    │ 0.0%     │ 0.0%     │ 0.0%       │
    │ ja-rokutenkanji.yaml                        │ 7010   │ 99.8%     │ 0.2%     │ 0.0%     │ 0.0%       │
    │ ml.yaml                                     │ 7524   │ 88.2%     │ 11.5%    │ 0.1%     │ 0.2%       │
    │ ar-ar-g1_harness.yaml                       │ 7596   │ 99.8%     │ 0.2%     │ 0.0%     │ 0.0%       │
    │ lg-ug-g1.yaml                               │ 8283   │ 99.9%     │ 0.1%     │ 0.0%     │ 0.0%       │
    │ rw-rw-g1.yaml                               │ 9839   │ 95.6%     │ 4.4%     │ 0.0%     │ 0.0%       │
    │ sw-ke-dictionary.yaml                       │ 10966  │ 90.8%     │ 9.2%     │ 0.0%     │ 0.0%       │
    │ de-g0-detailed-dictionary.yaml              │ 19996  │ 100.0%    │ 0.0%     │ 0.0%     │ 0.0%       │
    │ hu-hu-g1_dictionary_numbers.yaml            │ 34246  │ 99.0%     │ 1.0%     │ 0.0%     │ 0.0%       │
    │ afr-za-g2.yaml                              │ 51824  │ 95.4%     │ 4.6%     │ 0.0%     │ 0.0%       │
    │ en-us-g2-dictionary_harness.yaml            │ 93796  │ 89.3%     │ 0.9%     │ 9.7%     │ 0.1%       │
    │ hu-hu-g1_dictionary_special_consonants.yaml │ 181870 │ 99.3%     │ 0.7%     │ 0.0%     │ 0.0%       │
    │ en-ueb-g2-dictionary_harness.yaml           │ 213167 │ 92.1%     │ 7.8%     │ 0.0%     │ 0.0%       │
    ┌─────────────────────────────────────────────┌────────┌───────────┌──────────┌──────────┌────────────┐
    │ Total                                       │ 713827 │ 94.3%     │ 4.1%     │ 1.5%     │ 0.0%       │
    └─────────────────────────────────────────────└────────└───────────└──────────└──────────└────────────┘

Test the table query functionality:

    $ export LOUIS_TABLE_PATH=~/src/liblouis/tables:~/src/liblouis
    $ louis query language=de,contraction=full
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

Copyright (C) 2023-2026 Swiss Library for the Blind, Visually Impaired
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

