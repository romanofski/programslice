from programslice import slice_vim_buffer
import ast
import os.path
import programslice.visitor
import unittest2 as unittest


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
        graph1, graph2 = self.visitor.graphs
        self.assertEqual(2, len(self.visitor.graphs))
        self.assertEqual('function innerfunc:2', graph1.name)
        self.assertEqual('function main:1', graph2.name)
        self.assertEqual([4, 6, 7], graph2.slice_forward(4))

    def test_visit_Assign(self):
        node = self.load_testdata('function.py')
        self.visitor.visit(node)
        graph1, graph2 = self.visitor.graphs
        self.assertEqual([3], graph1.edges)
        self.assertEqual([4, 5, 6, 7], graph2.edges)

    def test_visit_While(self):
        node = self.load_testdata('binsearch.py')
        self.visitor.visit(node)
        graph = self.visitor.graphs[0]
        self.assertEqual(1, len(graph.graph[graph.graph.keys()[-1]]))


class TestFunctions(unittest.TestCase):

    def test_slice_buffer(self):
        filepath = os.path.join(os.path.dirname(__file__),
                                'testdata', 'function.py')
        buffer = open(filepath, 'r').read()
        lines = slice_vim_buffer(4, buffer, 'function.py')
        self.assertEqual([4, 6, 7], lines)
