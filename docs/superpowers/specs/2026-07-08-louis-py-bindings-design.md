# Design: `louis-py` — PyO3 Python bindings for `louis`

Date: 2026-07-08
Status: Approved for planning
Supersedes: the design spec in GitHub issue #1 (written ~2026-06, now partly stale)

## 1. Goal

Expose the public Rust API of the `louis` crate to Python in an idiomatic
("Pythonic") way, suitable for use by screen readers such as NVDA. The Python
bindings must not perturb the existing pure-Rust crate's build, dependencies, or
consumers: `cargo build` / `cargo test` at the repo root stay green and produce
the unchanged `louis` library and binary.

## 2. Non-goals

- A backwards-compatible C-style API matching the original liblouis Python
  bindings (`translateString`, integer return codes). May be added later as a
  thin pure-Python shim on top of these bindings; that shim is the natural place
  to claim the `import louis` name for drop-in NVDA use.
- Exposing `TranslationResult.spacing` — the Rust `SpacingInfo` struct is still
  empty (`// TODO:` in `src/lib.rs`).

## 3. What changed since issue #1

The issue's spec was written against an older API. Corrections folded into this
design:

1. **Naming.** The PyPI `louis` name is an unrelated junk squat ("A simple louis
   .py file", author "louis", v1.3), *not* the liblouis ctypes bindings. The
   real conflict is the **import module name**: the legacy liblouis bindings ship
   inside the C project and are imported as `import louis` (NVDA does this). PyPI
   distribution name and import module name are separable, so we sidestep the
   collision by not using the bare `louis` module name.
   - **Distribution name:** `louis-py` (`pip install louis-py`).
   - **Import module:** `louis_py` (`import louis_py`).
   - These normalize to the same PyPI name under PEP 503; the hyphen/underscore
     split mirrors the idiomatic `typing-extensions` / `typing_extensions`
     pattern. The `-py` suffix signals "Python binding of louis" (cf. `rpds-py`).
2. **`Typeform` is dead.** The issue's `typeforms: list[dict[str, str]]` no longer
   exists. Options now carry `emphasis: Vec<EmphasisSpan>` where
   `EmphasisSpan { class: String, range: Range<usize> }` (`src/emphasis.rs`).
3. **`TranslationOptions` is a private-field builder** (`src/translator/options.rs`,
   commit e307a57): constructed via
   `TranslationOptions::default().with_mode(..).with_emphasis(..).with_cursor_pos(..)`
   and read via `mode()` / `emphasis()` / `cursor_pos()`. The shim uses the
   builder, not a struct literal.
4. **pyo3 version.** Latest stable is **0.29.0** (issue pinned 0.28 "indicative").
   maturin latest is 1.14.1. Root crate is edition 2024.
5. **Result fields are mostly unpopulated today.** `Translator::translate_with_options`
   in `src/lib.rs` fills only `output`; `emphasis`, `output_positions`,
   `input_positions`, and `cursor_pos` are always `None` (the
   `compute_*_positions` helpers exist but are unwired). We expose the full
   `TranslationResult` struct anyway (forward-compatible); those fields return
   `None` until the Rust pipeline populates them. This is a documented limitation
   of the underlying crate, not a bug in the bindings.

## 4. Repository layout

Convert the repo into a Cargo workspace. The existing crate stays as the root
crate; the bindings live in a sibling crate `louis-py/`. (This aligns with the
multi-crate direction of PR #7.)

```
louis-rs/
├── Cargo.toml                     # + [workspace] table only
├── src/                           # unchanged
├── louis-py/
│   ├── Cargo.toml                 # cdylib + pyo3 0.29 + path dep on louis-rs
│   ├── pyproject.toml             # maturin backend, name = "louis-py"
│   ├── src/lib.rs                 # #[pymodule] fn _louis_py (compiled ext)
│   ├── python/louis_py/__init__.py    # IntFlag + NamedTuple + re-exports
│   ├── python/louis_py/py.typed
│   ├── python/louis_py/_louis_py.pyi  # hand-written stubs for the ext module
│   └── tests/                     # pytest suite + conftest.py
├── dictionaries/                  # unchanged
└── ...
```

Root `Cargo.toml` gains only:

```toml
[workspace]
members = [".", "louis-py"]
```

`louis-py/Cargo.toml`:

```toml
[package]
name = "louis-py"
version = "0.1.0"
edition = "2024"

[lib]
name = "_louis_py"
crate-type = ["cdylib"]

[dependencies]
louis-rs = { path = ".." }          # crate's lib name is `louis`; use louis::...
pyo3 = { version = "0.29", features = ["extension-module", "abi3-py39"] }

[dev-dependencies]
static_assertions = "1"
```

The main crate's `Cargo.toml` is otherwise untouched.

**Mixed Rust/Python layout.** The compiled extension is `_louis_py`; the public
import surface is the pure-Python package `louis_py`, which re-exports from
`._louis_py` and adds the `IntFlag` and `NamedTuple`. `pyproject.toml`:

```toml
[build-system]
requires = ["maturin>=1.14,<2"]
build-backend = "maturin"

[project]
name = "louis-py"
requires-python = ">=3.9"

[tool.maturin]
module-name = "louis_py._louis_py"
python-source = "python"
features = ["pyo3/extension-module"]
```

## 5. Public Python API

```python
import louis_py
from louis_py import (
    Translator,
    Direction,
    TranslationMode,
    TranslationResult,
    EmphasisSpan,
    LouisError,
    TableParseError,
    TranslationError,
)
```

### 5.1 `Direction`

`#[pyclass(eq, eq_int)]` over `parser::Direction`, surfaced as an `IntEnum`:

```python
Direction.FORWARD   # 0
Direction.BACKWARD  # 1
```

### 5.2 `TranslationMode`

Pure-Python `enum.IntFlag` in `__init__.py`, with explicit, stable bit values —
**not** derived from the `enumset` internal representation:

```python
class TranslationMode(enum.IntFlag):
    NO_CONTRACTIONS     = 1 << 0
    COMPBRL_AT_CURSOR   = 1 << 1
    DOTS_IO             = 1 << 2
    COMPBRL_LEFT_CURSOR = 1 << 3
    UC_BRL              = 1 << 4
    NO_UNDEFINED        = 1 << 5
    PARTIAL_TRANS       = 1 << 6
```

The shim receives a `u32` of OR-ed bits and builds `TranslationModes` by calling
`TranslationModes::empty()` then `insert(variant)` for each set bit — matching
each explicit bit to its `TranslationMode` Rust variant. It must **not** use any
raw `from_bits` path, since the exposed bit layout is independent of enumset's.

### 5.3 `EmphasisSpan`

A `typing.NamedTuple` declared in `__init__.py`:

```python
class EmphasisSpan(NamedTuple):
    class_: str   # `class` is a keyword; trailing underscore per PEP 8
    start: int    # inclusive
    end: int      # exclusive  (maps to Rust Range<usize> [start, end))
```

Used for both the `emphasis` input argument and the `TranslationResult.emphasis`
output. The shim converts `EmphasisSpan` <-> `louis::EmphasisSpan` by reading
`class_`/`start`/`end` (also accepts a plain 3-tuple, since a NamedTuple *is* a
tuple). Output spans are constructed as `EmphasisSpan` instances.

### 5.4 `Translator`

`#[pyclass(frozen)]` wrapping `louis::Translator`. Immutable after construction;
shared across threads without synchronization (see §7).

```python
Translator(
    tables: list[str | os.PathLike],
    direction: Direction = Direction.FORWARD,
)

t.translate(text: str) -> str

t.translate_with_options(
    text: str,
    *,
    mode: TranslationMode = TranslationMode(0),
    emphasis: list[EmphasisSpan] | None = None,
    cursor_pos: int | None = None,
) -> TranslationResult
```

- `tables` accepts any `str | os.PathLike`; each converts to `PathBuf`.
- Table resolution honors the `search_path` crate behavior (`LOUIS_TABLEPATH`);
  no extra Python-side logic.
- The constructor and both translate methods release the GIL via
  `py.allow_threads(...)` (table parsing and translation can be slow).
- Method naming mirrors the Rust API 1:1 (`translate` /
  `translate_with_options`).
- Argument conversion into the builder:
  `TranslationOptions::default().with_mode(modes)` then, conditionally,
  `.with_emphasis(spans)` / `.with_cursor_pos(pos)`.

### 5.5 `TranslationResult`

`#[pyclass(frozen, get_all)]`, read-only attributes mirroring the Rust struct:

```python
result.output: str
result.emphasis: list[EmphasisSpan] | None
result.output_positions: list[int] | None
result.input_positions: list[int] | None
result.cursor_pos: int | None
```

`__repr__` provided. `spacing` is not exposed. Per §3.5, all fields except
`output` currently return `None` at runtime.

### 5.6 Exceptions

Via `pyo3::create_exception!`:

```
LouisError(Exception)
├── TableParseError(LouisError)   # .errors: list[str]
└── TranslationError(LouisError)
```

`From<louis::TranslationError> for PyErr`:

- `TranslationError::ParseFailed(errs)` -> `TableParseError`, `.errors` set to
  `[format!("{e}") for e in errs]`.
- `TranslationError::TranslationFailed(e)` -> `TranslationError` with the
  formatted message.

### 5.7 Type stubs

`python/louis_py/_louis_py.pyi` (stubs for the compiled ext) is hand-written and
shipped in the wheel alongside `py.typed`. The `IntFlag` and `NamedTuple` live in
`__init__.py` and are self-typed.

## 6. Data flow

```
Python call
  -> shim converts args: list[PathLike] -> Vec<PathBuf>
                          IntFlag u32   -> TranslationModes (empty + insert/bit)
                          list[EmphasisSpan] -> Vec<louis::EmphasisSpan>
  -> py.allow_threads(|| louis::Translator::translate_with_options(text, opts))
  -> result -> TranslationResult (emphasis rebuilt as EmphasisSpan NamedTuples)
Errors flow back through From<louis::TranslationError> for PyErr.
```

## 7. Threading model

`Translator` is `#[pyclass(frozen)]` and immutable after `Translator::new`, so
concurrent `translate` calls need no synchronization and GIL release is safe.

**Must verify at implementation time:** `louis::Translator: Send + Sync` (auto-
derived; not asserted in `src/translator.rs`). Add a
`static_assertions::assert_impl_all!(louis::Translator: Send, Sync);` test in the
`louis-py` crate. If it fails, wrap the inner translator in a `Mutex` (calls are
short, contention low) and document the change. `#[pyclass(frozen)]` requires the
wrapped type be `Sync` for GIL-released sharing.

## 8. Testing

`louis-py/tests/`:

- `conftest.py` — set `LOUIS_TABLEPATH` to the repo `dictionaries/` dir.
- `test_translate.py` — forward translate with a known table; verify output.
- `test_options.py` — `translate_with_options` returns a `TranslationResult`;
  `output` is correct. Assert that `output_positions` / `input_positions` /
  `emphasis` / `cursor_pos` are `None` **today** (encodes the §3.5 limitation;
  flip these assertions when the Rust pipeline starts populating them).
- `test_modes.py` — `TranslationMode` flags compose with `|` and are accepted;
  spot-check that a mode actually reaches the Rust side (e.g. `NO_CONTRACTIONS`
  changes output for a contracted table).
- `test_emphasis.py` — `emphasis=[EmphasisSpan("italic", 0, 4)]` is accepted and
  round-trips through the shim without error (output effect asserted once the
  pipeline supports it).
- `test_errors.py` — bogus table path raises `TableParseError`; assert
  `isinstance(exc, LouisError)` and `exc.errors` is a non-empty list.
- `test_threading.py` — N threads call `translate` concurrently; assert no
  deadlock and identical outputs (GIL-release smoke test).
- `test_tablepath.py` — on Windows, a semicolon-separated `LOUIS_TABLEPATH`
  resolves tables (validates the `search_path` open risk from issue #1).

Local workflow: `cd louis-py && maturin develop && pytest`.

## 9. Build, CI, distribution

### 9.1 Local
```
cd louis-py
maturin develop      # editable install for pytest
maturin build --release
```

### 9.2 CI — `.github/workflows/python-wheels.yml` (PyO3/maturin-action)
- **On PR** touching `louis-py/**`: build wheel on `ubuntu-latest`, run `pytest`.
  No publish.
- **On tag `py-v*`**: full matrix:
  - `windows-latest` × `x86_64` (NVDA is 64-bit as of 2026.1, so no 32-bit `x86` wheel)
  - `ubuntu-latest` × `x86_64` (manylinux2014), `aarch64`
  - `macos-latest` × `x86_64`, `aarch64`
  - plus `sdist`; publish to PyPI via `pypa/gh-action-pypi-publish` (Trusted
    Publisher).

`abi3-py39` -> one wheel per (OS, arch) covers CPython 3.9+.

### 9.3 Versioning
`louis-py/pyproject.toml` carries an independent version; Python bindings may
iterate faster than the Rust crate. Release tag scheme `py-v<semver>` (e.g.
`py-v0.1.0`); the Rust crate keeps its own tags.

## 10. Deferred / out of scope

- `TranslationResult.spacing` — until Rust `SpacingInfo` is populated.
- C liblouis Python-API compatibility shim (`translateString`, return codes) —
  a later pure-Python module on top of these bindings; also the natural owner of
  the `import louis` name.
- Wiring `compute_*_positions` and emphasis into `translate_with_options` on the
  Rust side — separate crate-side work; the bindings are forward-compatible with
  it.

## 11. Open risks

- **`Send + Sync`** on `louis::Translator` — assert with `static_assertions`;
  fall back to `Mutex` (§7).
- **`abi3` floor** — confirm pyo3 0.29's minimum abi3 version; adjust
  `abi3-py3x` and `requires-python` to match.
- **pyo3 0.29 API drift** — GIL-release / `Bound<'py>` / attribute syntax may
  differ slightly from older examples; follow the 0.29 docs.
- **Windows `LOUIS_TABLEPATH`** — semicolon-separated paths; covered by
  `test_tablepath.py`.
- **enumset bit layout** — never assume it matches the exposed `1<<n` values;
  insert per bit (§5.2).
