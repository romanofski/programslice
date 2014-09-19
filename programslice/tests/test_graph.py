from programslice.graph import Graph, Slice, Edge
import programslice.visitor
import unittest
import ast


def test_connect():
    graph = Graph('')
    e1 = graph.add(Edge('', 1))
    e2 = graph.add(Edge('', 2))
    graph.connect(e1, e2)
    assert graph[e2] == [e1]


def test_edge_identity():
    """Edges created with the same parameters are identical."""
    e1 = Edge('n', 3, 4)
    e2 = Edge('n', 2, 6)
    assert e1 == e1
    assert not (e1 != e1)
    assert not (e1 != Edge('n', 3, 4))
    assert e1 != e2


def test_graph_slice_forward(assignment_graph):
    result = [x.lineno for x in Slice(assignment_graph)(Edge('n', 3, 4))]
    assert set([3, 5, 6]) == set(result)


def test_repr():
    graph = Graph('foo')
    graph.add(Edge('o', 3, 4))
    expected = '<Graph foo [(<Edge o at #3@4>, [])]>'
    assert expected == repr(graph)


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

    def test_graph_add(self):
        e1 = self.graph.add(Edge('', 1))
        self.assertListEqual([e1], self.graph.edges)

        e2 = self.graph.add(Edge('', 3))
        self.assertEqual(2, len(self.graph))
        self.assertListEqual([e1, e2], self.graph.edges)

        self.graph.add(Edge('', 3))
        self.assertEqual(2, len(self.graph))

    def test_graph_in(self):
        data = ('n', 1)
        self.graph.add(Edge(*data))
        e2 = Edge(*data)
        self.assertTrue(e2 in self.graph)

    def test_graph__getitem__(self):
        data = ('n', 1)
        self.graph.add(Edge(*data))
        self.assertListEqual([], self.graph[Edge(*data)])
