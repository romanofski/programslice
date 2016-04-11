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


def test_finds_entry_block():
    graph = programslice.graph.Graph()
    block = programslice.graph.BasicBlock([ast.Num(4, lineno=0, col_offset=0)],
                                          type=programslice.graph.ENTRY)
    graph.add(block)
    assert programslice.graph.find_entry_block(graph) == block


def test_finds_all_exitblocks():
    graph = programslice.graph.Graph()
    block = programslice.graph.BasicBlock([ast.Num(4, lineno=0, col_offset=0)],
                                          type=programslice.graph.EXIT)
    graph.add(block)
    assert programslice.graph.find_exit_blocks(graph) == [block]


def test_doesnot_entry_block():
    graph = programslice.graph.Graph()
    graph.add(1)
    assert programslice.graph.find_entry_block(graph) is None
