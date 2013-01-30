from programslice.graph import Graph, Slice, Edge
import unittest
import ast


class TestEdge(unittest.TestCase):

    def test_create_from_astnode(self):
        node = ast.Name('n', 0)
        node.col_offset = 0
        node.lineno = 0
        edge = Edge.create_from_astnode(node)
        self.assertTupleEqual(('n', 0, 0),
                              (edge.name, edge.lineno, edge.offset))


class TestGraph(unittest.TestCase):

    def setUp(self):
        self.graph = Graph('foo')

    def test_repr(self):
        self.assertEqual('<Graph foo>', repr(self.graph))

    def test_first_last(self):
        e1 = self.graph.add(Edge('', 1))
        e2 = self.graph.add(Edge('', 5))
        self.assertEqual(e1, self.graph.first)
        self.assertEqual(e2, self.graph.last)

    def test_graph_add(self):
        e1 = self.graph.add(Edge('', 1))
        self.assertListEqual([e1], self.graph.edges)

        e2 = self.graph.add(Edge('', 3))
        self.assertEqual(2, len(self.graph))
        self.assertListEqual([e1, e2], self.graph.edges)

        self.graph.add(Edge('', 3))
        self.assertEqual(2, len(self.graph))

    def test_connect(self):
        e1 = self.graph.add(Edge('', 1))
        e2 = self.graph.add(Edge('', 2))
        self.graph.connect(e1, e2)
        self.assertListEqual([e2], self.graph[e1])

        self.graph.connect_by_lineno(2, 1)
        self.assertEqual([e1], self.graph[e2])

        self.assertRaises(KeyError, self.graph.connect_by_lineno, 3, 4)

    def test_graph_in(self):
        data = ('n', 1)
        self.graph.add(Edge(*data))
        e2 = Edge(*data)
        self.assertTrue(e2 in self.graph)

    def test_graph__getitem__(self):
        data = ('n', 1)
        self.graph.add(Edge(*data))
        self.assertListEqual([], self.graph[Edge(*data)])

    def test_graph_get_edges_by_name(self):
        e1 = self.graph.add(Edge('n', 1, 0))
        e2 = self.graph.add(Edge('n', 1, 12))
        self.assertListEqual([e1, e2], self.graph.get_edges_by_name('n'))
        self.assertListEqual([e1], self.graph.get_edges_by_name('n', True))


class TestSlice(unittest.TestCase):

    def setUp(self):
        self.graph = Graph('foo')

    def test_graph_slice_forward(self):
        for i in range(1, 12):
            self.graph.add(Edge('a', i))

        self.graph.connect_by_lineno(1, 10)
        self.graph.connect_by_lineno(1, 6)
        self.graph.connect_by_lineno(2, 11)
        self.graph.connect_by_lineno(2, 7)
        self.graph.connect_by_lineno(2, 3)
        self.graph.connect_by_lineno(3, 5)
        self.graph.connect_by_lineno(3, 8)
        self.graph.connect_by_lineno(3, 4)
        self.graph.connect_by_lineno(4, 5)
        self.graph.connect_by_lineno(6, 6)
        self.graph.connect_by_lineno(6, 10)
        self.graph.connect_by_lineno(7, 7)
        self.graph.connect_by_lineno(8, 8)
        self.graph.connect_by_lineno(8, 5)
        self.graph.connect_by_lineno(8, 7)

        result = [x.lineno for x in Slice(self.graph)(Edge('a', 2))]
        self.assertSetEqual(set([2, 3, 4, 5, 7, 8, 11]), set(result))

    def test_graph_slice_with_graph(self):
        graph2 = Graph()
        graph2.add(Edge('', 4))
        graph2.add(Edge('', 5))
        graph2.add(Edge('', 6))
        graph2.connect_by_lineno(4, 5)
        graph2.connect_by_lineno(5, 6)

        e4 = self.graph.add(Edge('', 4))
        self.graph.connect_with_graph(e4, graph2)

        result = Slice(self.graph)(Edge('', 4))
        self.assertSetEqual(set([4, 5, 6]), set([x.lineno for x in result]))
