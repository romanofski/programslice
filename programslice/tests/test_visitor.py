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

    def test_visit_Assign(self):
        node = self.load_testdata('assign.py')
        self.visitor.visit(node)
        self.assertEqual([1, 3], self.visitor.graph['foo'])
        self.assertEqual([2, 3], self.visitor.graph['bar'])
        self.assertEqual([3, 4], self.visitor.graph['baz'])

    def test_visit_BinOp(self):
        node = self.load_testdata('binop.py')
        self.visitor.visit(node)
        # XXX that's not quite correct, as we currently don't visit
        # functions
        expected = {'foo': [4], 'number': [1, 2], 'do_baz': [4]}
        self.assertEquals(expected, self.visitor.graph)

