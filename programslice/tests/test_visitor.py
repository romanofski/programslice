from programslice.graph import Graph, Slice, Edge
import ast
import os.path
import programslice.visitor
import unittest


def test_visit_Assign(assignment_graph):
    assert assignment_graph[Edge('n', 3, 4)] == [Edge('n', 5, 6)]


def test_visit_Call(call_graph):
    graph = call_graph
    assert graph[Edge('n', 3, 4)] == [Edge('n', 4, 10)]
    assert Edge('i', 7, 8) in graph[Edge('n', 4, 10)]


class TestLineDependencyVisitor(unittest.TestCase):

    def setUp(self):
        self.visitor = programslice.visitor.LineDependencyVisitor()

    def load_testdata(self, filename):
        filepath = os.path.join(os.path.dirname(__file__),
                                'testdata', filename)
        node = ast.parse(open(filepath, 'r').read(), filepath)
        return node

    def test_visit_FunctionDef(self):
        node = self.load_testdata('function.py')
        self.visitor.visit(node)
        graph = self.visitor.graph
        result = sorted([x.lineno
                         for x in Slice(graph)(Edge('foo', 17, 4))])
        self.assertEqual([17, 19, 19], result)

    def test_visit_While(self):
        node = self.load_testdata('binsearch.py')
        self.visitor.visit(node)
        graph = self.visitor.graph
        expected = set([12, 16, 17, 19, 21])
        result = set([x.lineno for x in Slice(graph)(Edge('min', 12, 4))])
        self.assertEqual(expected, result)
