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
}

/// The `_louis_py` extension module. Public API lives in the `louis_py`
/// Python package, which re-exports from here.
#[pymodule]
fn _louis_py(m: &Bound<'_, PyModule>) -> PyResult<()> {
    let py = m.py();
    m.add("__version__", env!("CARGO_PKG_VERSION"))?;
    m.add_class::<Direction>()?;
    m.add_class::<Translator>()?;
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
