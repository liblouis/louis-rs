def test_mode_flags_compose():
    from louis_py import TranslationMode

    combined = TranslationMode.NO_CONTRACTIONS | TranslationMode.UC_BRL
    assert combined & TranslationMode.NO_CONTRACTIONS
    assert int(TranslationMode.NO_CONTRACTIONS) == 1


def test_mode_accepted_by_translate():
    from louis_py import Translator, TranslationMode

    t = Translator(["mini.ctb"])
    # Should not raise; mini.ctb has no contractions so output is unchanged.
    r = t.translate_with_options("abc", mode=TranslationMode.NO_CONTRACTIONS)
    assert r.output == "⠁⠃⠉"
