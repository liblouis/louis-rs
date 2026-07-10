import threading


def test_concurrent_translate_no_deadlock():
    from louis_py import Translator

    t = Translator(["mini.ctb"])
    expected = t.translate("hello world")
    results = []
    lock = threading.Lock()

    def worker():
        out = t.translate("hello world")
        with lock:
            results.append(out)

    threads = [threading.Thread(target=worker) for _ in range(16)]
    for th in threads:
        th.start()
    for th in threads:
        th.join()

    assert len(results) == 16
    assert all(r == expected for r in results)
