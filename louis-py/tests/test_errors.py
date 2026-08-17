import pytest


def test_bogus_table_raises_table_parse_error():
    from louis_py import Translator, LouisError, TableParseError

    with pytest.raises(TableParseError) as excinfo:
        Translator(["definitely-not-a-real-table.ctb"])

    exc = excinfo.value
    assert isinstance(exc, LouisError)
    assert isinstance(exc.errors, list)
    assert len(exc.errors) >= 1
    assert all(isinstance(e, str) for e in exc.errors)


def test_exception_hierarchy():
    from louis_py import LouisError, TableParseError, TranslationError

    assert issubclass(TableParseError, LouisError)
    assert issubclass(TranslationError, LouisError)
    assert issubclass(LouisError, Exception)
