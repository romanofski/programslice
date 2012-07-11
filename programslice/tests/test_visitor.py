import ast
import programslice.visitor
import unittest2


class TestDataDependencyVisitor(unittest2.TestCase):

    def setUp(self):
        self.node = ast.Assign([ast.Name('foo', ast.Store())],
                               ast.Name('bar', ast.Load()))
        self.node.lineno = 1
        self.col_offset = 0
        self.node.targets[0].lineno = 1
        self.node.value.lineno = 1

    def test_visit_Assign(self):
        visitor = programslice.visitor.DataDependencyVisitor()
        visitor.visit_Assign(self.node)
        self.assertEqual([1], visitor.graph['foo'])
        self.assertEqual([1], visitor.graph['bar'])
