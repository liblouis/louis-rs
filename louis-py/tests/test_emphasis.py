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
