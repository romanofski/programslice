import ast
import os.path
import unittest

import pytest

from programslice.graph import Slice, Edge
import programslice.visitor


def load_testdata(filename):
    filepath = os.path.join(os.path.dirname(__file__),
                            'testdata', filename)
    node = ast.parse(open(filepath, 'r').read(), filepath)
    return node


def test_visit_Assign(assignment_graph):
    assert assignment_graph[Edge('n', 3, 4)] == [Edge('n', 5, 6)]


def test_visit_Call(call_graph):
    graph = call_graph
    assert graph[Edge('n', 3, 4)] == [Edge('n', 4, 10)]


@pytest.mark.unsorted_result
def test_fix_issue_1_slices_a():
    node = load_testdata('binsearch.py')
    visitor = programslice.visitor.LineDependencyVisitor()
    visitor.visit(node)
    graph = visitor.graph

    start = Edge('a', 27, 4)
    result = Slice(graph)(start)

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


class TestLineDependencyVisitor(unittest.TestCase):

    def setUp(self):
        self.visitor = programslice.visitor.LineDependencyVisitor()

    def test_visit_FunctionDef(self):
        node = load_testdata('function.py')
        self.visitor.visit(node)
        graph = self.visitor.graph
        result = Slice(graph)(Edge('foo', 17, 4))
        self.assertEqual([
            Edge('foo', 17, 4),
            Edge('foo', 19, 10),
            Edge('baz', 19, 4),
            Edge('baz', 20, 10),
            ], result)

    def test_visit_While(self):
        node = load_testdata('binsearch.py')
        self.visitor.visit(node)
        graph = self.visitor.graph
        start = Edge('min', 12, 4)
        expected = [
            start,
            Edge('min', 15, 14),
            Edge('min', 16, 18),
            Edge('min', 16, 31),
            Edge('mid', 16, 8),
            Edge('min', 21, 12),
            Edge('mid', 17, 23),
            Edge('mid', 19, 18),
            Edge('mid', 21, 18),
            Edge('x', 17, 8),
            Edge('max', 19, 12),
            Edge('x', 18, 11),
            Edge('max', 16, 25),
            ]
        result = Slice(graph)(start)
        self.assertEqual(expected, result)
