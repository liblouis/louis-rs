//! PyO3 bindings for the `louis` braille translator.
use std::path::PathBuf;

use pyo3::create_exception;
use pyo3::exceptions::{PyException, PyValueError};
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
/// Re-attaches to the interpreter so it can be used directly in `.map_err`.
fn to_pyerr(err: louis::TranslationError) -> PyErr {
    match err {
        louis::TranslationError::ParseFailed(errs) => {
            let msgs: Vec<String> = errs.iter().map(ToString::to_string).collect();
            let exc = TableParseError::new_err("Errors when reading given braille table(s)");
            Python::attach(|py| {
                let _ = exc.value(py).setattr("errors", msgs);
            });
            exc
        }
        other => TranslationError::new_err(format!("{other}")),
    }
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
            .map_err(to_pyerr)?;
        Ok(Self { inner })
    }

    /// Translate `text` to braille.
    fn translate(&self, py: Python<'_>, text: &str) -> PyResult<String> {
        py.detach(|| self.inner.translate(text))
            .map_err(to_pyerr)
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
        let modes = louis::TranslationModes::from_bits(mode).ok_or_else(|| {
            PyValueError::new_err(format!("invalid translation mode bits: {mode:#x}"))
        })?;
        let mut options = louis::TranslationOptions::default().with_mode(modes);
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
            .map_err(to_pyerr)?;
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
