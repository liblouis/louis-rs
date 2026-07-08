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
