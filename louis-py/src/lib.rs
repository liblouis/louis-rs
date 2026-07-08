//! PyO3 bindings for the `louis` braille translator.
use pyo3::prelude::*;

/// The `_louis_py` extension module. Public API lives in the `louis_py`
/// Python package, which re-exports from here.
#[pymodule]
fn _louis_py(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add("__version__", env!("CARGO_PKG_VERSION"))?;
    Ok(())
}
