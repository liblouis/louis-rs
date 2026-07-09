def test_import_and_version():
    import louis_py

    assert isinstance(louis_py.__version__, str)
    assert louis_py.__version__
