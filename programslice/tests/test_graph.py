from programslice.graph import Graph, Slice, Edge
import unittest
import ast


def test_connect():
    """
    This test assures that a connection is being made from Edge 1 to 2.
    """
    graph = Graph('')
    e1 = graph.add(Edge('', 1, 1))
    e2 = graph.add(Edge('', 2, 1))
    graph.connect(e1, e2)
    assert graph[e2] == []
    assert graph[e1] == [e2]


def test_can_t_connect_twice():
    """
    This test assures that edges which already have been connected will
    not result in an additional edge again.
    """
    graph = Graph('')
    e1 = Edge('', 1, 1)
    e2 = Edge('', 1, 2)
    for i in range(5):
        graph.connect(e1, e2)

    assert graph[e1] == [e2]


def test_edge_identity():
    """Edges created with the same parameters are identical."""
    e1 = Edge('n', 3, 4)
    e2 = Edge('n', 2, 6)
    assert e1 == e1
    assert not (e1 != e1)
    assert not (e1 != Edge('n', 3, 4))
    assert e1 != e2


def test_repr():
    graph = Graph('foo')
    graph.add(Edge('o', 3, 4))
    expected = '<Graph foo [(<Edge o at #3@4>, [])]>'
    assert expected == repr(graph)


def test_edge_create_from_astnode():
    node = ast.Name('n', 0)
    node.col_offset = 0
    node.lineno = 0
    edge = Edge.create_from_astnode(node)
    assert ('n', 0, 0) == (edge.name, edge.lineno, edge.column)


class TestGraph(unittest.TestCase):

    def setUp(self):
        self.graph = Graph('foo')

    def test_graph_add(self):
        e1 = self.graph.add(Edge('', 1, 1))
        self.assertListEqual([e1], self.graph.edges)

        e2 = self.graph.add(Edge('', 3, 1))
        self.assertEqual(2, len(self.graph))
        self.assertListEqual([e1, e2], self.graph.edges)

        self.graph.add(Edge('', 3, 1))
        self.assertEqual(2, len(self.graph))

    def test_graph_in(self):
        data = ('n', 1, 1)
        self.graph.add(Edge(*data))
        e2 = Edge(*data)
        self.assertTrue(e2 in self.graph)

    def test_graph__getitem__(self):
        data = ('n', 1, 1)
        self.graph.add(Edge(*data))
        self.assertListEqual([], self.graph[Edge(*data)])
