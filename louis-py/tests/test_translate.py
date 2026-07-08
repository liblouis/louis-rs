def test_forward_translate_hello_world():
    from louis_py import Translator, Direction
    t = Translator(["mini.ctb"], Direction.FORWARD)
    assert t.translate("hello world") == "⠓⠑⠇⠇⠕⠀⠺⠕⠗⠇⠙"


def test_default_direction_is_forward():
    from louis_py import Translator
    t = Translator(["mini.ctb"])
    assert t.translate("abc") == "⠁⠃⠉"
