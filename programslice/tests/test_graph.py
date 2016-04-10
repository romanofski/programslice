import ast

import pytest

import programslice.graph


def test_basic_block_errors_incorrect_indentation():
    """Incorrect indentation levels should result in an assertion."""
    n1 = ast.Num(5, lineno=0, col_offset=0)
    n2 = ast.Num(6, lineno=0, col_offset=1)
    with pytest.raises(AssertionError):
        programslice.graph.BasicBlock([n1, n2])
