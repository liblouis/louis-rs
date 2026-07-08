import os
from pathlib import Path

import pytest


@pytest.mark.skipif(os.name != "nt", reason="Windows uses ';' as LOUIS_TABLE_PATH separator; POSIX uses ':'")
def test_semicolon_separated_table_path(monkeypatch):
    # Validates search_path handling of Windows-style semicolon-separated paths.
    from louis_py import Translator
    tables = str(Path(__file__).parent / "tables")
    monkeypatch.setenv("LOUIS_TABLE_PATH", f"C:\\does-not-exist;{tables}")
    t = Translator(["mini.ctb"])
    assert t.translate("abc") == "⠁⠃⠉"
