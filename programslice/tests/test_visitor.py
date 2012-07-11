import ast
import os.path
import programslice.visitor
import unittest2


class TestDataDependencyVisitor(unittest2.TestCase):

    def load_testdata(self, filename):
        filepath = os.path.join(os.path.dirname(__file__),
                                'testdata', filename)
        node = ast.parse(open(filepath, 'r').read(), filepath)
        return node

    def test_visit_Assign(self):
        node = self.load_testdata('assign.py')
        visitor = programslice.visitor.DataDependencyVisitor()
        visitor.visit_Assign(node.body[0])
        self.assertEqual([1], visitor.graph['foo'])
