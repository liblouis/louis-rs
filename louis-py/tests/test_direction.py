def test_direction_values():
    from louis_py import Direction
    assert int(Direction.FORWARD) == 0
    assert int(Direction.BACKWARD) == 1
    assert Direction.FORWARD != Direction.BACKWARD
