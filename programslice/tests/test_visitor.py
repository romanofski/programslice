import pytest

from programslice.graph import Edge
from programslice.tests.conftest import get_sliced_testdata


def test_does_not_prematurely_delete_dependencies():
    """
    This test assures, that dependencies are not prematurely deleted
    during the visiting process.

    Example: Starting with the slice criterion *c* in c = 2 * b, the
    dependency tree includes the assignments to the variable a two lines
    down.
    """
    start = Edge('c', 29, 4)
    result = get_sliced_testdata('binsearch.py', start)
    assert Edge('a', 27, 4) not in result
    assert Edge('a', 31, 4) in result


def test_no_overlapping_slice_result():
    """
    Test verifies, that slice result does not include independent
    functions if the naming of the variables is the same.
    """
    start = Edge('a', 2, 4)
    result = get_sliced_testdata('overlapping.py', start)
    expected = [
        start,
        Edge('a', 3, 8),
        Edge('a', 4, 11),
        Edge('a', 3, 4),
    ]
    assert expected == result


# Currently fails, since last line 'a = 5' is not included in the
# result. Perhaps it shouldn't be, but rather the result itself only
# depends on that line.
@pytest.mark.xfail
def test_fix_issue_1_slices_a():
    """
    This test assures that the slice criterion starting whith `a = 1`
    includes all the additional dependencies, which are basically
    assignments to `a`.
    """
    start = Edge('a', 27, 4)
    result = get_sliced_testdata('binsearch.py', start)

    expected = [
        start,
        Edge('a', 30, 8),
        Edge('a', 31, 8),
        Edge('a', 32, 8),
        Edge('a', 34, 11),
        Edge('a', 30, 4),
        Edge('a', 31, 4),
        Edge('a', 32, 4),
        Edge('a', 33, 4),
    ]
    assert expected == result


def test_slices_over_while_and_branches():
    """
    Tests that the slice result includes items in the while and if
    constructs.
    """
    start = Edge('min', 12, 4)
    result = get_sliced_testdata('binsearch.py', start)
    expected = [
        start,
        Edge('min', 15, 14),
        Edge('min', 15, 37),
        Edge('min', 16, 18),
        Edge('min', 16, 31),
        Edge('mid', 16, 8),
        Edge('mid', 17, 23),
        Edge('mid', 19, 18),
        Edge('mid', 21, 18),
        Edge('mid', 23, 19),
        Edge('x', 17, 8),
        Edge('max', 19, 12),
        Edge('min', 21, 12),
        Edge('x', 18, 11),
        Edge('x', 20, 13),
        Edge('x', 22, 13),
        Edge('max', 16, 25),
        ]
    assert Edge('x', 14, 4) not in result
    assert expected == result
