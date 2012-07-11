import ast
import programslice.visitor
import unittest2


class TestDataDependencyVisitor(unittest2.TestCase):

    def setUp(self):
        self.tree = ast.Assign([ast.Name('foo'), ast.Store()],
                               ast.Name('foo', ast.Load()))

    def test_visit_Assign(self):
        visitor = programslice.visitor.DataDependencyVisitor()
        visitor.visit_Assign(self.tree)
