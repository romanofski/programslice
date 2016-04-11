import ast

import pytest

import programslice.graph


def test_basic_block_errors_incorrect_indentation():
    """Incorrect indentation levels should result in an assertion."""
    n1 = ast.Num(5, lineno=0, col_offset=0)
    n2 = ast.Num(6, lineno=0, col_offset=1)
    with pytest.raises(AssertionError):
        programslice.graph.BasicBlock([n1, n2])


def test_graphs_edges():
    graph = programslice.graph.Graph()
    graph.add(1)
    graph.add(2)
    graph.connect(1, 2)
    graph.add(3)
    graph.connect(2, 3)
    assert graph.edges == [(1, 2), (2, 3)]
