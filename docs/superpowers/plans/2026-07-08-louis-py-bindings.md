# louis-py PyO3 Bindings Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Ship a `louis-py` crate that exposes the `louis` Rust braille translator to Python (import name `louis_py`) as an idiomatic PyO3 extension, built and tested end-to-end on this machine.

**Architecture:** New Cargo workspace member `louis-py/` builds a `cdylib` PyO3 extension `_louis_py` wrapping `louis::Translator`. A thin pure-Python package `louis_py` re-exports the Rust classes and adds an `enum.IntFlag` (`TranslationMode`) and a `NamedTuple` (`EmphasisSpan`). Options are converted to the Rust `TranslationOptions` builder; the GIL is released around translation. Tests are hermetic (bundled minimal braille table).

**Tech Stack:** Rust (edition 2024), PyO3 0.29, maturin 1.14, uv, pytest. Host is ARM64 Windows (aarch64-pc-windows-msvc); Python is uv-managed CPython 3.12 aarch64.

## Global Constraints

- **Root crate untouched** except adding the `[workspace]` table to `Cargo.toml`. `cargo build` and `cargo test` at the repo root must stay green and keep producing the existing `louis` lib + binary.
- **Rust package name** of the binding crate is `louis-py`; **library/extension name** is `_louis_py`; **PyO3 module fn** is `_louis_py`.
- **Distribution name** `louis-py`; **import package** `louis_py`. Do not use the bare module name `louis` (collides with legacy liblouis bindings).
- **PyO3** `= "0.29"`, features `["extension-module", "abi3-py39"]`. `requires-python = ">=3.9"`.
- **Path dep**: `louis-rs = { path = ".." }` — the crate's lib name is `louis`, so Rust code says `use louis::...`.
- **No `TranslationResult.spacing`.** `SpacingInfo` is empty upstream.
- **Result reality:** `louis::Translator::translate_with_options` fills only `output` today; `emphasis`/`output_positions`/`input_positions`/`cursor_pos` come back `None`. Tests assert that current reality.
- **`TranslationMode` bit values are explicit** `1 << n`; the shim builds `TranslationModes` via `empty()` + `insert(variant)` per set bit. Never assume enumset's internal layout.
- **PyO3 0.29 pymodule signature:** `fn _louis_py(m: &Bound<'_, PyModule>) -> PyResult<()>`.
- **`pyproject.toml` `[project]` MUST have a `version`** (maturin errors otherwise).

### Canonical dev environment (already bootstrapped on this machine)

The verified venv lives at `C:\Sources\louis-rs\louis-py\.venv` after Task 1. It
is an **aarch64** CPython 3.12 venv (must match the aarch64 Rust host).

**Every PowerShell tool call is a fresh shell** — always use absolute paths; do
not rely on `Activate.ps1` persisting between calls.

Build + install + test recipe (run from repo root; `$VENV` = `C:\Sources\louis-rs\louis-py\.venv`):

```powershell
$VENV = "C:\Sources\louis-rs\louis-py\.venv"
$PY   = "$VENV\Scripts\python.exe"
# build the wheel (do NOT use `maturin develop`; its uv auto-detect picks the wrong interpreter)
& "$VENV\Scripts\maturin.exe" build --manifest-path C:\Sources\louis-rs\louis-py\Cargo.toml --interpreter $PY
# install the freshly built wheel
$whl = Get-ChildItem C:\Sources\louis-rs\target\wheels\*.whl | Sort-Object LastWriteTime | Select-Object -Last 1
uv pip install --python $PY --reinstall $whl.FullName
# run the python tests
& $PY -m pytest C:\Sources\louis-rs\louis-py\tests -v
```

Note: the workspace shares `C:\Sources\louis-rs\target\`, so wheels land in
`C:\Sources\louis-rs\target\wheels\`.

---

## File Structure

- `Cargo.toml` (modify) — add `[workspace]` table.
- `louis-py/Cargo.toml` (create) — cdylib crate, pyo3 dep, path dep, static_assertions dev-dep.
- `louis-py/pyproject.toml` (create) — maturin backend + `[tool.maturin]`.
- `louis-py/src/lib.rs` (create) — `#[pymodule] _louis_py`; `Translator`, `Direction`, `TranslationResult` pyclasses; exceptions; error conversion; Send/Sync assertion test.
- `louis-py/python/louis_py/__init__.py` (create) — `TranslationMode` IntFlag, `EmphasisSpan` NamedTuple, re-exports.
- `louis-py/python/louis_py/py.typed` (create) — empty marker.
- `louis-py/python/louis_py/_louis_py.pyi` (create) — stubs for the Rust ext.
- `louis-py/tests/tables/mini.ctb` (already created) — hermetic table.
- `louis-py/tests/conftest.py` (create) — set `LOUIS_TABLE_PATH`.
- `louis-py/tests/test_*.py` (create) — pytest suite.
- `.github/workflows/python-wheels.yml` (create) — CI.
- `CHANGELOG.md` (modify) — note the bindings.

---

## Task 1: Workspace + crate scaffold + verified build/test loop

**Files:**
- Modify: `Cargo.toml` (add `[workspace]`)
- Create: `louis-py/Cargo.toml`, `louis-py/pyproject.toml`, `louis-py/src/lib.rs`
- Create: `louis-py/python/louis_py/__init__.py`, `louis-py/python/louis_py/py.typed`
- Create: `louis-py/tests/conftest.py`, `louis-py/tests/test_smoke.py`
- Test: `louis-py/tests/test_smoke.py`

**Interfaces:**
- Produces: importable `louis_py` package; Rust ext `_louis_py` with a `__version__` string; the dev venv at `louis-py/.venv`.

- [ ] **Step 1: Add the workspace table to root `Cargo.toml`.**

Append to `C:\Sources\louis-rs\Cargo.toml` (top of file, before `[package]` is fine, but appending a new table at the end is simplest):

```toml
[workspace]
members = [".", "louis-py"]
```

- [ ] **Step 2: Create `louis-py/Cargo.toml`.**

```toml
[package]
name = "louis-py"
version = "0.1.0"
edition = "2024"
description = "Python bindings for the louis braille translator"
license = "LGPL-2.1-or-later"
repository = "https://github.com/liblouis/louis-rs"

[lib]
name = "_louis_py"
crate-type = ["cdylib"]

[dependencies]
louis-rs = { path = ".." }
pyo3 = { version = "0.29", features = ["extension-module", "abi3-py39"] }

[dev-dependencies]
static_assertions = "1"
```

- [ ] **Step 3: Create `louis-py/pyproject.toml`.**

```toml
[build-system]
requires = ["maturin>=1.14,<2"]
build-backend = "maturin"

[project]
name = "louis-py"
version = "0.1.0"
description = "Python bindings for the louis braille translator"
requires-python = ">=3.9"
license = { text = "LGPL-2.1-or-later" }
readme = "README.md"
classifiers = [
    "Programming Language :: Rust",
    "Programming Language :: Python :: 3",
    "Topic :: Adaptive Technologies",
]

[tool.maturin]
module-name = "louis_py._louis_py"
python-source = "python"
features = ["pyo3/extension-module"]
```

- [ ] **Step 4: Create minimal `louis-py/src/lib.rs`.**

```rust
//! PyO3 bindings for the `louis` braille translator.
use pyo3::prelude::*;

/// The `_louis_py` extension module. Public API lives in the `louis_py`
/// Python package, which re-exports from here.
#[pymodule]
fn _louis_py(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add("__version__", env!("CARGO_PKG_VERSION"))?;
    Ok(())
}
```

- [ ] **Step 5: Create `louis-py/python/louis_py/__init__.py`.**

```python
"""Python bindings for the louis braille translator."""
from ._louis_py import __version__

__all__ = ["__version__"]
```

- [ ] **Step 6: Create `louis-py/python/louis_py/py.typed`** (empty file).

- [ ] **Step 7: Create `louis-py/tests/conftest.py`.**

```python
import os
from pathlib import Path

TABLES = Path(__file__).parent / "tables"

# louis resolves tables via the search_path crate, which reads LOUIS_TABLE_PATH.
os.environ["LOUIS_TABLE_PATH"] = str(TABLES)
```

- [ ] **Step 8: Create `louis-py/tests/test_smoke.py`.**

```python
def test_import_and_version():
    import louis_py
    assert isinstance(louis_py.__version__, str)
    assert louis_py.__version__
```

- [ ] **Step 9: Create the dev venv (once).**

Run (PowerShell):
```powershell
uv python install cpython-3.12-windows-aarch64
uv venv -p cpython-3.12-windows-aarch64 C:\Sources\louis-rs\louis-py\.venv
uv pip install --python C:\Sources\louis-rs\louis-py\.venv\Scripts\python.exe maturin pytest
```
Expected: venv created; `maturin` and `pytest` installed.

- [ ] **Step 10: Verify root Cargo build still works.**

Run: `cargo build`
Expected: builds workspace incl. `louis-py`; the existing `louis` bin/lib still build.

- [ ] **Step 11: Build the wheel, install, run the smoke test.**

Run (PowerShell), using the canonical recipe in Global Constraints.
Expected: wheel `louis_py-0.1.0-cp39-abi3-win_arm64.whl` built and installed; `pytest` shows `test_import_and_version PASSED`.

- [ ] **Step 12: Commit.**

```bash
git add Cargo.toml louis-py/
git commit -m "feat(louis-py): scaffold PyO3 crate + verified build/test loop"
```

---

## Task 2: Exceptions + error conversion

**Files:**
- Modify: `louis-py/src/lib.rs`
- Test: `louis-py/tests/test_errors.py`

**Interfaces:**
- Consumes: `louis::TranslationError` (`ParseFailed(Vec<louis::parser::TableError>)` is **not** public — see below; use `louis::TranslationError` top-level variants `ParseFailed(Vec<..>)` and `TranslationFailed(..)`). `louis::TranslationError` is re-exported at crate root.
- Produces: Python exceptions `LouisError`, `TableParseError` (with `.errors: list[str]`), `TranslationError`; a `From<louis::TranslationError> for PyErr` conversion used by later tasks.

Note: `louis::TranslationError` (in `src/lib.rs`) has variants:
`TranslationFailed(#[from] translator::TranslationError)` and
`ParseFailed(Vec<parser::TableError>)`. `parser::TableError` implements
`Display`. The `parser` module is private, but the variant is matchable via the
public `louis::TranslationError` enum and its `Vec` is iterable to `format!`
each element.

- [ ] **Step 1: Write the failing test `louis-py/tests/test_errors.py`.**

```python
import pytest


def test_bogus_table_raises_table_parse_error():
    import louis_py
    from louis_py import Translator, LouisError, TableParseError

    with pytest.raises(TableParseError) as excinfo:
        Translator(["definitely-not-a-real-table.ctb"])

    exc = excinfo.value
    assert isinstance(exc, LouisError)
    assert isinstance(exc.errors, list)
    assert len(exc.errors) >= 1
    assert all(isinstance(e, str) for e in exc.errors)
```

(This test also needs `Translator`, delivered in Task 4. Until then it will fail
at import of `Translator`. That is expected; run it after Task 4. For Task 2,
verify the exceptions exist with the sub-test below.)

Add to the same file:

```python
def test_exception_hierarchy():
    from louis_py import LouisError, TableParseError, TranslationError
    assert issubclass(TableParseError, LouisError)
    assert issubclass(TranslationError, LouisError)
    assert issubclass(LouisError, Exception)
```

- [ ] **Step 2: Run `test_exception_hierarchy` to confirm it fails.**

Run: `& $PY -m pytest louis-py/tests/test_errors.py::test_exception_hierarchy -v`
Expected: FAIL (ImportError: cannot import name 'LouisError').

- [ ] **Step 3: Implement exceptions + conversion in `louis-py/src/lib.rs`.**

Add near the top:

```rust
use pyo3::create_exception;
use pyo3::exceptions::PyException;

create_exception!(_louis_py, LouisError, PyException, "Base class for louis errors.");
create_exception!(_louis_py, TableParseError, LouisError, "Raised when a braille table fails to parse.");
create_exception!(_louis_py, TranslationError, LouisError, "Raised when translation fails.");

/// Wrap a `louis::TranslationError` into the appropriate Python exception.
fn to_pyerr(py: Python<'_>, err: louis::TranslationError) -> PyErr {
    match err {
        louis::TranslationError::ParseFailed(errs) => {
            let msgs: Vec<String> = errs.iter().map(|e| format!("{e}")).collect();
            let exc = TableParseError::new_err("Errors when reading given braille table(s)");
            // attach `.errors`
            if let Ok(bound) = exc.value(py).setattr("errors", msgs) {
                let _ = bound;
            }
            exc
        }
        other => TranslationError::new_err(format!("{other}")),
    }
}
```

Register in the module fn (inside `fn _louis_py`):

```rust
    m.add("LouisError", py.get_type::<LouisError>())?;
    m.add("TableParseError", py.get_type::<TableParseError>())?;
    m.add("TranslationError", py.get_type::<TranslationError>())?;
```

To get `py`, change the module signature to capture it:

```rust
#[pymodule]
fn _louis_py(m: &Bound<'_, PyModule>) -> PyResult<()> {
    let py = m.py();
    m.add("__version__", env!("CARGO_PKG_VERSION"))?;
    m.add("LouisError", py.get_type::<LouisError>())?;
    m.add("TableParseError", py.get_type::<TableParseError>())?;
    m.add("TranslationError", py.get_type::<TranslationError>())?;
    Ok(())
}
```

Note on `.errors`: setting an attribute on a freshly-created exception instance
is awkward via `new_err`. Prefer building the exception with the attribute using
`PyErr::from_value` on a constructed instance. Concretely, replace the
`ParseFailed` arm with:

```rust
        louis::TranslationError::ParseFailed(errs) => {
            let msgs: Vec<String> = errs.iter().map(|e| format!("{e}")).collect();
            let inst = TableParseError::new_err("Errors when reading given braille table(s)");
            Python::with_gil(|py| {
                let val = inst.value(py);
                let _ = val.setattr("errors", msgs);
            });
            inst
        }
```

If `.setattr` on the exception value does not persist (PyO3 exception instances
are frozen for some base types), fall back to defining `TableParseError` as a
real `#[pyclass(extends=PyException)]` with an `errors` field. The reviewer must
confirm `exc.errors` is readable from Python via the Task-4 integration test.

- [ ] **Step 4: Update `__init__.py` to re-export exceptions.**

```python
from ._louis_py import (
    __version__,
    LouisError,
    TableParseError,
    TranslationError,
)

__all__ = [
    "__version__",
    "LouisError",
    "TableParseError",
    "TranslationError",
]
```

- [ ] **Step 5: Rebuild, install, run `test_exception_hierarchy`.**

Run the canonical build recipe, then:
`& $PY -m pytest louis-py/tests/test_errors.py::test_exception_hierarchy -v`
Expected: PASS.

- [ ] **Step 6: Commit.**

```bash
git add louis-py/src/lib.rs louis-py/python/louis_py/__init__.py louis-py/tests/test_errors.py
git commit -m "feat(louis-py): exception hierarchy + error conversion"
```

---

## Task 3: `Direction` enum

**Files:**
- Modify: `louis-py/src/lib.rs`, `louis-py/python/louis_py/__init__.py`
- Test: `louis-py/tests/test_direction.py`

**Interfaces:**
- Consumes: `louis::Direction` (`Forward`, `Backward`).
- Produces: Python `Direction` IntEnum with `FORWARD=0`, `BACKWARD=1`, convertible to `louis::Direction`.

- [ ] **Step 1: Write failing test `louis-py/tests/test_direction.py`.**

```python
def test_direction_values():
    from louis_py import Direction
    assert int(Direction.FORWARD) == 0
    assert int(Direction.BACKWARD) == 1
    assert Direction.FORWARD != Direction.BACKWARD
```

- [ ] **Step 2: Run it, confirm fail.**

Run: `& $PY -m pytest louis-py/tests/test_direction.py -v`
Expected: FAIL (cannot import Direction).

- [ ] **Step 3: Implement `Direction` in `louis-py/src/lib.rs`.**

```rust
/// Translation direction, mirrors `louis::Direction`.
#[pyclass(eq, eq_int)]
#[derive(Clone, Copy, PartialEq)]
pub enum Direction {
    FORWARD = 0,
    BACKWARD = 1,
}

impl From<Direction> for louis::Direction {
    fn from(d: Direction) -> Self {
        match d {
            Direction::FORWARD => louis::Direction::Forward,
            Direction::BACKWARD => louis::Direction::Backward,
        }
    }
}
```

Register in module fn: `m.add_class::<Direction>()?;`

- [ ] **Step 4: Re-export in `__init__.py`** (add `Direction` to the import list and `__all__`).

- [ ] **Step 5: Rebuild, install, run test.**

Expected: PASS.

- [ ] **Step 6: Commit.**

```bash
git add louis-py/src/lib.rs louis-py/python/louis_py/__init__.py louis-py/tests/test_direction.py
git commit -m "feat(louis-py): expose Direction enum"
```

---

## Task 4: `Translator` + basic `translate` + Send/Sync assertion

**Files:**
- Modify: `louis-py/src/lib.rs`, `louis-py/python/louis_py/__init__.py`
- Test: `louis-py/tests/test_translate.py`, `louis-py/tests/test_errors.py` (now runnable in full)

**Interfaces:**
- Consumes: `louis::Translator::new(&[P], Direction) -> Result<Translator, louis::TranslationError>`, `Translator::translate(&str) -> Result<String, _>`; `Direction` (Task 3); `to_pyerr` (Task 2).
- Produces: Python `Translator(tables, direction=Direction.FORWARD)` and `t.translate(text) -> str`.

- [ ] **Step 1: Write failing test `louis-py/tests/test_translate.py`.**

```python
def test_forward_translate_hello_world():
    from louis_py import Translator, Direction
    t = Translator(["mini.ctb"], Direction.FORWARD)
    assert t.translate("hello world") == "⠓⠑⠇⠇⠕⠀⠺⠕⠗⠇⠙"


def test_default_direction_is_forward():
    from louis_py import Translator
    t = Translator(["mini.ctb"])
    assert t.translate("abc") == "⠁⠃⠉"
```

(The expected string for "hello world" is `⠓⠑⠇⠇⠕⠀⠺⠕⠗⠇⠙`, verified with the
`mini.ctb` table. `abc` → `⠁⠃⠉`.)

- [ ] **Step 2: Run, confirm fail.**

Run: `& $PY -m pytest louis-py/tests/test_translate.py -v`
Expected: FAIL (cannot import Translator).

- [ ] **Step 3: Implement `Translator` in `louis-py/src/lib.rs`.**

```rust
use std::path::PathBuf;

/// A compiled braille translator. Immutable and safe to share across threads.
#[pyclass(frozen)]
pub struct Translator {
    inner: louis::Translator,
}

#[pymethods]
impl Translator {
    #[new]
    #[pyo3(signature = (tables, direction = Direction::FORWARD))]
    fn new(py: Python<'_>, tables: Vec<PathBuf>, direction: Direction) -> PyResult<Self> {
        let dir: louis::Direction = direction.into();
        let inner = py
            .allow_threads(|| louis::Translator::new(&tables, dir))
            .map_err(|e| to_pyerr_nogil(e))?;
        Ok(Self { inner })
    }

    /// Translate `text` to braille.
    fn translate(&self, py: Python<'_>, text: &str) -> PyResult<String> {
        py.allow_threads(|| self.inner.translate(text))
            .map_err(|e| to_pyerr_nogil(e))
    }
}
```

Because `to_pyerr` in Task 2 took `py`, and here the error is produced inside/
after `allow_threads`, provide a GIL-free variant that defers attribute setting:

```rust
/// Convert without needing an explicit `Python` token (re-acquires the GIL).
fn to_pyerr_nogil(err: louis::TranslationError) -> PyErr {
    Python::with_gil(|py| to_pyerr(py, err))
}
```

Register: `m.add_class::<Translator>()?;`

- [ ] **Step 4: Add the Send/Sync assertion test (Rust) at the bottom of `lib.rs`.**

```rust
#[cfg(test)]
mod tests {
    use static_assertions::assert_impl_all;
    assert_impl_all!(louis::Translator: Send, Sync);
}
```

- [ ] **Step 5: Run the Rust assertion test.**

Run: `cargo test -p louis-py`
Expected: PASS. If it fails to compile (not Send+Sync), STOP and escalate: wrap
`inner` in `std::sync::Mutex<louis::Translator>`, adjust `translate`/`new`
accordingly, drop `#[pyclass(frozen)]`'s sharing assumption is still fine since
Mutex is Sync, and document in the spec's §7.

- [ ] **Step 6: Re-export `Translator` in `__init__.py`.**

- [ ] **Step 7: Rebuild, install, run translate + full error tests.**

Run: `& $PY -m pytest louis-py/tests/test_translate.py louis-py/tests/test_errors.py -v`
Expected: all PASS (including `test_bogus_table_raises_table_parse_error` and its
`.errors` list assertion — this is the confirmation gate for Task 2's `.errors`).

- [ ] **Step 8: Commit.**

```bash
git add louis-py/src/lib.rs louis-py/python/louis_py/__init__.py louis-py/tests/test_translate.py
git commit -m "feat(louis-py): Translator with GIL-released translate; assert Send+Sync"
```

---

## Task 5: `TranslationResult`, `TranslationMode`, `EmphasisSpan`, `translate_with_options`

**Files:**
- Modify: `louis-py/src/lib.rs`, `louis-py/python/louis_py/__init__.py`
- Test: `louis-py/tests/test_options.py`, `louis-py/tests/test_modes.py`, `louis-py/tests/test_emphasis.py`

**Interfaces:**
- Consumes: `louis::TranslationOptions::default().with_mode(TranslationModes).with_emphasis(Vec<louis::EmphasisSpan>).with_cursor_pos(usize)`; `louis::TranslationModes::{empty, insert}`; `louis::TranslationMode` variants (`NoContractions, CompbrlAtCursor, DotsIo, CompbrlLeftCursor, UcBrl, NoUndefined, PartialTrans`); `louis::EmphasisSpan::new(class, Range)`; `louis::TranslationResult` fields `output, emphasis, output_positions, input_positions, cursor_pos`; `Translator::translate_with_options(&str, TranslationOptions) -> Result<TranslationResult, _>`.
- Produces: Python `TranslationResult` (frozen, get_all), `TranslationMode` IntFlag, `EmphasisSpan` NamedTuple, and `Translator.translate_with_options(text, *, mode=0, emphasis=None, cursor_pos=None) -> TranslationResult`.

- [ ] **Step 1: Write failing tests.**

`louis-py/tests/test_options.py`:
```python
def test_translate_with_options_output():
    from louis_py import Translator
    t = Translator(["mini.ctb"])
    r = t.translate_with_options("abc")
    assert r.output == "⠁⠃⠉"


def test_result_fields_none_today():
    # Encodes the current upstream limitation: only `output` is populated.
    # Flip these when the Rust pipeline starts filling them.
    from louis_py import Translator
    t = Translator(["mini.ctb"])
    r = t.translate_with_options("abc", cursor_pos=1)
    assert r.emphasis is None
    assert r.output_positions is None
    assert r.input_positions is None
    assert r.cursor_pos is None


def test_repr():
    from louis_py import Translator
    t = Translator(["mini.ctb"])
    r = t.translate_with_options("a")
    assert "TranslationResult" in repr(r)
```

`louis-py/tests/test_modes.py`:
```python
def test_mode_flags_compose():
    from louis_py import TranslationMode
    combined = TranslationMode.NO_CONTRACTIONS | TranslationMode.UC_BRL
    assert combined & TranslationMode.NO_CONTRACTIONS
    assert int(TranslationMode.NO_CONTRACTIONS) == 1


def test_mode_accepted_by_translate():
    from louis_py import Translator, TranslationMode
    t = Translator(["mini.ctb"])
    # Should not raise; mini.ctb has no contractions so output is unchanged.
    r = t.translate_with_options("abc", mode=TranslationMode.NO_CONTRACTIONS)
    assert r.output == "⠁⠃⠉"
```

`louis-py/tests/test_emphasis.py`:
```python
def test_emphasis_span_namedtuple():
    from louis_py import EmphasisSpan
    s = EmphasisSpan("italic", 0, 3)
    assert s.class_ == "italic"
    assert s.start == 0
    assert s.end == 3
    assert tuple(s) == ("italic", 0, 3)


def test_emphasis_accepted_by_translate():
    from louis_py import Translator, EmphasisSpan
    t = Translator(["mini.ctb"])
    # Accepted without error; effect not asserted (pipeline gap).
    r = t.translate_with_options("abc", emphasis=[EmphasisSpan("italic", 0, 2)])
    assert r.output == "⠁⠃⠉"
```

- [ ] **Step 2: Run them, confirm fail.**

Run: `& $PY -m pytest louis-py/tests/test_options.py louis-py/tests/test_modes.py louis-py/tests/test_emphasis.py -v`
Expected: FAIL (imports / methods missing).

- [ ] **Step 3: Implement `TranslationResult` pyclass in `louis-py/src/lib.rs`.**

```rust
/// The result of a translation. Mirrors `louis::TranslationResult` minus `spacing`.
#[pyclass(frozen, get_all)]
pub struct TranslationResult {
    pub output: String,
    pub emphasis: Option<Vec<(String, usize, usize)>>,
    pub output_positions: Option<Vec<usize>>,
    pub input_positions: Option<Vec<usize>>,
    pub cursor_pos: Option<usize>,
}

#[pymethods]
impl TranslationResult {
    fn __repr__(&self) -> String {
        format!(
            "TranslationResult(output={:?}, emphasis={:?}, output_positions={:?}, input_positions={:?}, cursor_pos={:?})",
            self.output, self.emphasis, self.output_positions, self.input_positions, self.cursor_pos
        )
    }
}

impl TranslationResult {
    fn from_rust(r: louis::TranslationResult) -> Self {
        Self {
            output: r.output,
            emphasis: r.emphasis.map(|spans| {
                spans
                    .into_iter()
                    .map(|s| (s.class, s.range.start, s.range.end))
                    .collect()
            }),
            output_positions: r.output_positions,
            input_positions: r.input_positions,
            cursor_pos: r.cursor_pos,
        }
    }
}
```

Note: `louis::EmphasisSpan` fields are `class: String` and `range: Range<usize>`
(both public — see `src/emphasis.rs`).

- [ ] **Step 4: Add the mode-bit → `TranslationModes` conversion in `lib.rs`.**

```rust
/// Explicit Python-side bit values (must match `TranslationMode` IntFlag in __init__.py).
mod mode_bits {
    pub const NO_CONTRACTIONS: u32 = 1 << 0;
    pub const COMPBRL_AT_CURSOR: u32 = 1 << 1;
    pub const DOTS_IO: u32 = 1 << 2;
    pub const COMPBRL_LEFT_CURSOR: u32 = 1 << 3;
    pub const UC_BRL: u32 = 1 << 4;
    pub const NO_UNDEFINED: u32 = 1 << 5;
    pub const PARTIAL_TRANS: u32 = 1 << 6;
}

fn modes_from_bits(bits: u32) -> louis::TranslationModes {
    use louis::TranslationMode as M;
    let mut modes = louis::TranslationModes::empty();
    if bits & mode_bits::NO_CONTRACTIONS != 0 { modes.insert(M::NoContractions); }
    if bits & mode_bits::COMPBRL_AT_CURSOR != 0 { modes.insert(M::CompbrlAtCursor); }
    if bits & mode_bits::DOTS_IO != 0 { modes.insert(M::DotsIo); }
    if bits & mode_bits::COMPBRL_LEFT_CURSOR != 0 { modes.insert(M::CompbrlLeftCursor); }
    if bits & mode_bits::UC_BRL != 0 { modes.insert(M::UcBrl); }
    if bits & mode_bits::NO_UNDEFINED != 0 { modes.insert(M::NoUndefined); }
    if bits & mode_bits::PARTIAL_TRANS != 0 { modes.insert(M::PartialTrans); }
    modes
}
```

- [ ] **Step 5: Add `translate_with_options` to the `Translator` `#[pymethods]` block.**

```rust
    #[pyo3(signature = (text, *, mode = 0, emphasis = None, cursor_pos = None))]
    fn translate_with_options(
        &self,
        py: Python<'_>,
        text: &str,
        mode: u32,
        emphasis: Option<Vec<(String, usize, usize)>>,
        cursor_pos: Option<usize>,
    ) -> PyResult<TranslationResult> {
        let mut options = louis::TranslationOptions::default().with_mode(modes_from_bits(mode));
        if let Some(spans) = emphasis {
            let rust_spans: Vec<louis::EmphasisSpan> = spans
                .into_iter()
                .map(|(class, start, end)| louis::EmphasisSpan::new(class, start..end))
                .collect();
            options = options.with_emphasis(rust_spans);
        }
        if let Some(pos) = cursor_pos {
            options = options.with_cursor_pos(pos);
        }
        let result = py
            .allow_threads(|| self.inner.translate_with_options(text, options))
            .map_err(to_pyerr_nogil)?;
        Ok(TranslationResult::from_rust(result))
    }
```

Register `m.add_class::<TranslationResult>()?;`

- [ ] **Step 6: Add `TranslationMode` IntFlag and `EmphasisSpan` NamedTuple to `__init__.py`.**

```python
import enum
from typing import NamedTuple

from ._louis_py import (
    __version__,
    Direction,
    Translator,
    TranslationResult,
    LouisError,
    TableParseError,
    TranslationError,
)


class TranslationMode(enum.IntFlag):
    NO_CONTRACTIONS = 1 << 0
    COMPBRL_AT_CURSOR = 1 << 1
    DOTS_IO = 1 << 2
    COMPBRL_LEFT_CURSOR = 1 << 3
    UC_BRL = 1 << 4
    NO_UNDEFINED = 1 << 5
    PARTIAL_TRANS = 1 << 6


class EmphasisSpan(NamedTuple):
    class_: str
    start: int  # inclusive
    end: int    # exclusive


__all__ = [
    "__version__",
    "Direction",
    "Translator",
    "TranslationResult",
    "TranslationMode",
    "EmphasisSpan",
    "LouisError",
    "TableParseError",
    "TranslationError",
]
```

Note: `mode` is passed to Rust as `u32`. Because `TranslationMode` is an
`IntFlag` (int subclass), calling `translate_with_options(mode=TranslationMode.X)`
passes an int automatically. PyO3 extracts it as `u32`.

- [ ] **Step 7: Rebuild, install, run all three test files.**

Run: `& $PY -m pytest louis-py/tests/test_options.py louis-py/tests/test_modes.py louis-py/tests/test_emphasis.py -v`
Expected: all PASS.

- [ ] **Step 8: Commit.**

```bash
git add louis-py/src/lib.rs louis-py/python/louis_py/__init__.py louis-py/tests/test_options.py louis-py/tests/test_modes.py louis-py/tests/test_emphasis.py
git commit -m "feat(louis-py): translate_with_options + TranslationResult/Mode/EmphasisSpan"
```

---

## Task 6: Threading smoke test + Windows table-path test

**Files:**
- Test: `louis-py/tests/test_threading.py`, `louis-py/tests/test_tablepath.py`

**Interfaces:**
- Consumes: `Translator` (Task 4).

- [ ] **Step 1: Write `louis-py/tests/test_threading.py`.**

```python
import threading


def test_concurrent_translate_no_deadlock():
    from louis_py import Translator
    t = Translator(["mini.ctb"])
    expected = t.translate("hello world")
    results = []
    lock = threading.Lock()

    def worker():
        out = t.translate("hello world")
        with lock:
            results.append(out)

    threads = [threading.Thread(target=worker) for _ in range(16)]
    for th in threads:
        th.start()
    for th in threads:
        th.join()

    assert len(results) == 16
    assert all(r == expected for r in results)
```

- [ ] **Step 2: Write `louis-py/tests/test_tablepath.py`.**

```python
import os
from pathlib import Path


def test_semicolon_separated_table_path(monkeypatch):
    # Validates search_path handling of Windows-style semicolon-separated paths.
    from louis_py import Translator
    tables = str(Path(__file__).parent / "tables")
    monkeypatch.setenv("LOUIS_TABLE_PATH", f"C:\\does-not-exist;{tables}")
    t = Translator(["mini.ctb"])
    assert t.translate("abc") == "⠁⠃⠉"
```

- [ ] **Step 3: Rebuild not needed (tests only). Run them.**

Run: `& $PY -m pytest louis-py/tests/test_threading.py louis-py/tests/test_tablepath.py -v`
Expected: PASS. If `test_semicolon_separated_table_path` fails, the `search_path`
crate does not split on `;` on Windows — record this as a known limitation in the
spec's §11 and mark the test `xfail` with a reason rather than blocking.

- [ ] **Step 4: Run the whole suite.**

Run: `& $PY -m pytest louis-py/tests -v`
Expected: all green (or documented xfail).

- [ ] **Step 5: Commit.**

```bash
git add louis-py/tests/test_threading.py louis-py/tests/test_tablepath.py
git commit -m "test(louis-py): threading + table-path coverage"
```

---

## Task 7: Type stubs + README

**Files:**
- Create: `louis-py/python/louis_py/_louis_py.pyi`, `louis-py/README.md`

**Interfaces:**
- Produces: shipped stubs so consumers get typing; README for the wheel.

- [ ] **Step 1: Create `louis-py/python/louis_py/_louis_py.pyi`.**

```python
import os
from typing import Optional

class Direction:
    FORWARD: "Direction"
    BACKWARD: "Direction"
    def __int__(self) -> int: ...

class TranslationResult:
    output: str
    emphasis: Optional[list[tuple[str, int, int]]]
    output_positions: Optional[list[int]]
    input_positions: Optional[list[int]]
    cursor_pos: Optional[int]

class Translator:
    def __init__(
        self,
        tables: list[str | os.PathLike],
        direction: Direction = ...,
    ) -> None: ...
    def translate(self, text: str) -> str: ...
    def translate_with_options(
        self,
        text: str,
        *,
        mode: int = ...,
        emphasis: Optional[list[tuple[str, int, int]]] = ...,
        cursor_pos: Optional[int] = ...,
    ) -> TranslationResult: ...

class LouisError(Exception): ...
class TableParseError(LouisError):
    errors: list[str]
class TranslationError(LouisError): ...

__version__: str
```

- [ ] **Step 2: Create `louis-py/README.md`.**

```markdown
# louis-py

Python bindings for [louis](https://github.com/liblouis/louis-rs), a pure-Rust
braille translator.

```python
from louis_py import Translator, Direction

t = Translator(["en-us-g1.ctb"], Direction.FORWARD)
print(t.translate("hello world"))  # ⠓⠑⠇⠇⠕⠀⠺⠕⠗⠇⠙
```

Tables are resolved via `LOUIS_TABLE_PATH`. See the design spec in
`docs/superpowers/specs/` for the full API.
```

- [ ] **Step 3: Rebuild + install, confirm stubs ship and suite still green.**

Run the build recipe, then `& $PY -m pytest louis-py/tests -v`.
Expected: all PASS.

- [ ] **Step 4: Commit.**

```bash
git add louis-py/python/louis_py/_louis_py.pyi louis-py/README.md
git commit -m "docs(louis-py): type stubs + README"
```

---

## Task 8: CI workflow

**Files:**
- Create: `.github/workflows/python-wheels.yml`

**Interfaces:** none (CI only). Cannot be run locally; validated by YAML lint + review against maturin-action docs.

- [ ] **Step 1: Create `.github/workflows/python-wheels.yml`.**

```yaml
name: python-wheels

on:
  pull_request:
    paths:
      - "louis-py/**"
      - "src/**"
      - "Cargo.toml"
      - ".github/workflows/python-wheels.yml"
  push:
    tags:
      - "py-v*"

jobs:
  test:
    if: github.event_name == 'pull_request'
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-python@v5
        with:
          python-version: "3.12"
      - name: Build and install
        uses: PyO3/maturin-action@v1
        with:
          command: build
          args: --release --manifest-path louis-py/Cargo.toml --out dist
      - name: Install wheel and test
        run: |
          pip install --find-links dist louis-py
          pip install pytest
          pytest louis-py/tests -v

  wheels:
    if: startsWith(github.ref, 'refs/tags/py-v')
    strategy:
      matrix:
        include:
          - os: windows-latest
            target: x64
          - os: windows-latest
            target: x86
          - os: ubuntu-latest
            target: x86_64
          - os: ubuntu-latest
            target: aarch64
          - os: macos-latest
            target: x86_64
          - os: macos-latest
            target: aarch64
    runs-on: ${{ matrix.os }}
    steps:
      - uses: actions/checkout@v4
      - uses: PyO3/maturin-action@v1
        with:
          target: ${{ matrix.target }}
          command: build
          args: --release --manifest-path louis-py/Cargo.toml --out dist
          manylinux: "2014"
      - uses: actions/upload-artifact@v4
        with:
          name: wheels-${{ matrix.os }}-${{ matrix.target }}
          path: dist

  sdist:
    if: startsWith(github.ref, 'refs/tags/py-v')
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: PyO3/maturin-action@v1
        with:
          command: sdist
          args: --manifest-path louis-py/Cargo.toml --out dist
      - uses: actions/upload-artifact@v4
        with:
          name: wheels-sdist
          path: dist

  publish:
    if: startsWith(github.ref, 'refs/tags/py-v')
    needs: [wheels, sdist]
    runs-on: ubuntu-latest
    environment: pypi
    permissions:
      id-token: write
    steps:
      - uses: actions/download-artifact@v4
        with:
          pattern: wheels-*
          merge-multiple: true
          path: dist
      - uses: pypa/gh-action-pypi-publish@release/v1
```

- [ ] **Step 2: Lint the YAML.**

Run: `& $PY -c "import yaml,sys; yaml.safe_load(open('.github/workflows/python-wheels.yml')); print('yaml ok')"`
(Install pyyaml first if needed: `uv pip install --python $PY pyyaml`.)
Expected: `yaml ok`.

- [ ] **Step 3: Commit.**

```bash
git add .github/workflows/python-wheels.yml
git commit -m "ci(louis-py): wheel build + PyPI publish workflow"
```

---

## Task 9: Changelog + final full verification

**Files:**
- Modify: `CHANGELOG.md`

- [ ] **Step 1: Add a changelog entry.** Read the existing `CHANGELOG.md` format first and match it; add an entry noting "Add `louis-py` Python bindings (PyO3)".

- [ ] **Step 2: Final gate — clean rebuild + full test.**

```powershell
cargo build
cargo test
# then the canonical wheel build + install + full pytest
& $PY -m pytest C:\Sources\louis-rs\louis-py\tests -v
```
Expected: `cargo build`/`cargo test` green (root crate unaffected); full pytest green.

- [ ] **Step 3: Commit.**

```bash
git add CHANGELOG.md
git commit -m "docs: changelog entry for louis-py bindings"
```

---

## Self-Review notes (author)

- Spec coverage: naming (T1), layout/workspace (T1), Direction (T3), TranslationMode + explicit-bit conversion (T5), EmphasisSpan (T5), Translator + GIL release (T4), TranslationResult full struct with None-today (T5+T6 tests), exceptions + `.errors` (T2/T4), threading (T6), Windows table path (T6), stubs/py.typed (T1/T7), CI matrix incl. win32 x86 + sdist + Trusted Publisher (T8), versioning `py-v*` (T8), Send/Sync assertion + Mutex fallback (T4). All covered.
- Deferred correctly: spacing; C-compat shim; wiring positions/emphasis on the Rust side.
- Known execution risks called out inline: `.errors` attribute persistence (T2 fallback to `#[pyclass(extends)]`), `search_path` semicolon behavior (T6 xfail fallback), Send/Sync (T4 Mutex fallback).
