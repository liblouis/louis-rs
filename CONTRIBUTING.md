# Contributing to louis-rs

Thank you for helping. What helps most right now is reports from real use,
followed by the work that goes into understanding a problem. Code comes last,
and this page explains why.

## Report what breaks

Open an [issue](https://github.com/liblouis/louis-rs/issues) for a text that
comes out wrong, a table that doesn't load, or an API that gets in the way. A
good report is most of the fix. It says:

- **What you did:** the table, the direction, the input and any options
  (modes, typeform, cursor position).
- **What came out, and what you expected.** For a translation, give both as
  Unicode braille, from louis-rs and from liblouis:

  ```sh
  louis translate en-us-g2.ctb "of the"
  printf 'of the' | lou_translate --forward unicode.dis,en-us-g2.ctb
  ```

- **Where it matters to you:** the application, the kind of document, how often
  it happens. "Opening this 400 KB book takes twenty minutes" decides priorities
  better than any benchmark.

## Investigate it, if you can

The most useful contribution after a report is the understanding behind a fix:

- **What liblouis does, and where.** Point to the function in liblouis's C code
  (`lou_translateString.c`, `lou_backTranslateString.c`,
  `compileTranslationTable.c`) and say what it decides. louis-rs is not a port,
  so the C code tells us *what* to do, not *how*.
- **Edge cases, compared with liblouis.** A handful of inputs around the
  problem, with both outputs, shows the rule better than a description. Say
  which ones still differ and why.
- **A test.** Ideally in liblouis's YAML format, so that it can go upstream
  and both projects run it. `louis check your-test.yaml` runs one.
- **Numbers**, for anything about speed: the input size, the table, the
  timings before and after, and a profile if you have one.
  [doc/Translation_Performance.md](doc/Translation_Performance.md) has what was
  already measured, including the ideas that didn't work.

Sometimes the problem is in the table, not in louis-rs. liblouis's tables live
in the [liblouis repository](https://github.com/liblouis/liblouis), and a fix
there helps every user of both.

## Code

Pull requests are welcome, but read them as proposals: a worked-out way to fix
the problem, which the maintainers may change or rewrite before it is merged,
with credit to you. Most of what makes a change easy to take is not the code
but the investigation above, so put that in the description.

Before you write code:

- **For anything about design, open an issue first**, so we can agree on the
  shape before anyone writes it. A small fix whose place is obvious can go
  straight to a pull request.
- **Read [doc/Architecture.md](doc/Architecture.md).** It says where new logic
  goes: deciding from the text where indication cells go is an indicator,
  deciding where rules may apply is a constrainer, and anything that depends on
  which rule was chosen is a rewriter. A change that follows this is much
  easier to take.
- **Keep one change per pull request.** Many open pull requests at once each
  cost a review; a few good ones go further.

Before you open it, run:

```sh
cargo fmt
cargo clippy
cargo test
export LOUIS_TABLE_PATH=~/src/liblouis/tables:~/src/liblouis
cargo run --release -- check ~/src/liblouis/tests/braille-specs/*.yaml ~/src/liblouis/tests/yaml/*.yaml
```

and say in the description whether the liblouis test suite gives the same
results as on `main`, and if not, which files changed.

### Code written with an AI assistant

That's fine. Say so, with an `Assisted-by:` trailer in the commit message that
names the assistant and the model you used, as the Linux kernel asks
([coding assistants](https://docs.kernel.org/process/coding-assistants.html)):

```
Assisted-by: Claude:claude-opus-5-5
```

## License

By contributing, you agree that your contribution is licensed under the same
terms as louis-rs, the GNU Lesser General Public License, version 2.1 or later
(see [README.md](README.md#license)).
