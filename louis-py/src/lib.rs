//! PyO3 bindings for the `louis` braille translator.
use std::path::PathBuf;

use pyo3::create_exception;
use pyo3::exceptions::PyException;
use pyo3::prelude::*;

create_exception!(
    _louis_py,
    LouisError,
    PyException,
    "Base class for louis errors."
);
create_exception!(
    _louis_py,
    TableParseError,
    LouisError,
    "Raised when a braille table fails to parse."
);
create_exception!(
    _louis_py,
    TranslationError,
    LouisError,
    "Raised when translation fails."
);

/// Wrap a `louis::TranslationError` into the appropriate Python exception.
fn to_pyerr(py: Python<'_>, err: louis::TranslationError) -> PyErr {
    match err {
        louis::TranslationError::ParseFailed(errs) => {
            let msgs: Vec<String> = errs.iter().map(|e| format!("{e}")).collect();
            let exc = TableParseError::new_err("Errors when reading given braille table(s)");
            let _ = exc.value(py).setattr("errors", msgs);
            exc
        }
        other => TranslationError::new_err(format!("{other}")),
    }
}

/// Convert without needing an explicit `Python` token (re-attaches to the interpreter).
fn to_pyerr_nogil(err: louis::TranslationError) -> PyErr {
    Python::attach(|py| to_pyerr(py, err))
}

/// Translation direction, mirrors `louis::Direction`.
#[pyclass(eq, eq_int, from_py_object)]
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
    if bits & mode_bits::NO_CONTRACTIONS != 0 {
        modes.insert(M::NoContractions);
    }
    if bits & mode_bits::COMPBRL_AT_CURSOR != 0 {
        modes.insert(M::CompbrlAtCursor);
    }
    if bits & mode_bits::DOTS_IO != 0 {
        modes.insert(M::DotsIo);
    }
    if bits & mode_bits::COMPBRL_LEFT_CURSOR != 0 {
        modes.insert(M::CompbrlLeftCursor);
    }
    if bits & mode_bits::UC_BRL != 0 {
        modes.insert(M::UcBrl);
    }
    if bits & mode_bits::NO_UNDEFINED != 0 {
        modes.insert(M::NoUndefined);
    }
    if bits & mode_bits::PARTIAL_TRANS != 0 {
        modes.insert(M::PartialTrans);
    }
    modes
}

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
            .detach(|| louis::Translator::new(&tables, dir))
            .map_err(to_pyerr_nogil)?;
        Ok(Self { inner })
    }

    /// Translate `text` to braille.
    fn translate(&self, py: Python<'_>, text: &str) -> PyResult<String> {
        py.detach(|| self.inner.translate(text))
            .map_err(to_pyerr_nogil)
    }

    /// Translate `text` to braille with full options.
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
            .detach(|| self.inner.translate_with_options(text, options))
            .map_err(to_pyerr_nogil)?;
        Ok(TranslationResult::from_rust(result))
    }
}

/// The `_louis_py` extension module. Public API lives in the `louis_py`
/// Python package, which re-exports from here.
#[pymodule]
fn _louis_py(m: &Bound<'_, PyModule>) -> PyResult<()> {
    let py = m.py();
    m.add("__version__", env!("CARGO_PKG_VERSION"))?;
    m.add_class::<Direction>()?;
    m.add_class::<Translator>()?;
    m.add_class::<TranslationResult>()?;
    m.add("LouisError", py.get_type::<LouisError>())?;
    m.add("TableParseError", py.get_type::<TableParseError>())?;
    m.add("TranslationError", py.get_type::<TranslationError>())?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use static_assertions::assert_impl_all;
    assert_impl_all!(louis::Translator: Send, Sync);
}
