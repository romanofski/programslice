import ast
import os.path
import programslice.visitor
import unittest2


class TestDataDependencyVisitor(unittest2.TestCase):

    def setUp(self):
        self.visitor = programslice.visitor.DataDependencyVisitor()

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
        self.assertEqual('function main:1', graph1.name)
        self.assertEqual('function innerfunc:2', graph2.name)
