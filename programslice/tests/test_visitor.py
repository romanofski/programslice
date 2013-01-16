from programslice.graph import Graph, Slice, Edge
import ast
import os.path
import programslice.visitor
import unittest


class TestDependencyBuilder(unittest.TestCase):

    def test_builder_append(self):
        node = ast.Name('id', ast.Store())
        node.lineno = 1
        node.col_offset = 0
        builder = programslice.visitor.DependencyBuilder()

        self.assertIsNone(builder.targets)
        self.assertIsNone(builder.names)

        builder.append(node)
        self.assertTrue(builder.targets)

        node.ctx = ast.Load()
        builder.append(node)
        self.assertTrue(builder.names)


class TestLineDependencyVisitor(unittest.TestCase):

    def setUp(self):
        self.visitor = programslice.visitor.LineDependencyVisitor()

    def load_testdata(self, filename):
        filepath = os.path.join(os.path.dirname(__file__),
                                'testdata', filename)
        node = ast.parse(open(filepath, 'r').read(), filepath)
        return node

    def test_visit_Assign(self):
        node = ast.parse(
            """def foo():
                n=1
                m=1
                o=n+m
                n=o""")
        visitor = programslice.visitor.LineDependencyVisitor()
        visitor.visit(node)
        graph = visitor.graphs[0]
        self.assertEqual(8, len(graph.edges))
        edge_o = Edge('o', 4, 16)
        self.assertEqual([edge_o], graph[Edge('n', 2, 16)])
        self.assertEqual([edge_o], graph[Edge('m', 3, 16)])
        self.assertEqual([Edge('n', 5, 16)], graph[edge_o])

    def test_visit_FunctionDef(self):
        node = self.load_testdata('function.py')
        self.visitor.visit(node)
        graph1, graph2 = self.visitor.graphs[2:]
        self.assertEqual(4, len(self.visitor.graphs))
        self.assertEqual('innerfunc', graph1.name)
        self.assertEqual('main', graph2.name)
        result = sorted([x.lineno for x in Slice(graph2)(17)])
        self.assertEqual([17, 19], result)

    def test_visit_Call(self):
        node = self.load_testdata('function.py')
        self.visitor.visit(node)
        graph = self.visitor.graphs[0]
        result = sorted([x.lineno for x in Slice(graph)(5)])
        # see issue #16
        # self.assertEqual([5, 6, 7, 10, 11], result)
        self.assertEqual([5, 6], result)

    def test_visit_While(self):
        node = self.load_testdata('binsearch.py')
        self.visitor.visit(node)
        graph = self.visitor.get_graph_for(12)
        expected = [12, 16, 17, 19]
        result = sorted([x.lineno for x in Slice(graph)(12)])
        self.assertEqual(expected, result)


class TestVisitorFunctional(unittest.TestCase):

    def test_get_graph_for(self):
        visitor = programslice.visitor.LineDependencyVisitor()
        graph1 = Graph('function1')
        graph1.add(Edge('', 3))
        graph1.add(Edge('', 5))

        graph2 = Graph('function2')
        graph2.add(Edge('', 12))
        visitor.graphs = [graph1, graph2]

        self.assertEqual('function1', visitor.get_graph_for(4).name)
        self.assertEqual('function2', visitor.get_graph_for(12).name)
        self.assertEqual(None, visitor.get_graph_for(2))
