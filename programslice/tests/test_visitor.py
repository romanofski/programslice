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
        self.assertEqual([1, 3, 3, 4], self.visitor.graph.edges('foo'))
        self.assertEqual([2, 3, 3, 4], self.visitor.graph.edges('bar'))
        self.assertEqual([3, 4], self.visitor.graph.edges('baz'))

    def test_visit_BinOp(self):
        node = self.load_testdata('binop.py')
        self.visitor.visit(node)
        # XXX that's not quite correct, as we currently don't visit
        # functions
        expected = {'foo': [4], 'number': [1, 2], 'do_baz': [4, 4]}
        for key, val in expected.items():
            self.assertEqual(val, self.visitor.graph.edges(key),
                             'Edges for {0} differ.'.format(key))

    def test_visit_Function(self):
        node = self.load_testdata('binsearch.py')
        self.visitor.visit(node)
        # XXX not really sure here, as dependencies obviously can occur
        # in the same line ...
        self.assertEqual([12, 15, 15, 16, 16, 21, 16, 17, 19, 21, 23, 12],
                         self.visitor.graph.edges('min'))
        self.assertEqual([], self.visitor.graph.edges('n'))
