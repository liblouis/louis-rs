//! PyO3 bindings for the `louis` braille translator.
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
#[allow(dead_code)]
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

/// The `_louis_py` extension module. Public API lives in the `louis_py`
/// Python package, which re-exports from here.
#[pymodule]
fn _louis_py(m: &Bound<'_, PyModule>) -> PyResult<()> {
    let py = m.py();
    m.add("__version__", env!("CARGO_PKG_VERSION"))?;
    m.add("LouisError", py.get_type::<LouisError>())?;
    m.add("TableParseError", py.get_type::<TableParseError>())?;
    m.add("TranslationError", py.get_type::<TranslationError>())?;
    Ok(())
}
