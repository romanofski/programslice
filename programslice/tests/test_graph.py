import unittest2
from programslice.graph import Graph
from programslice.graph import Node


class TestGraph(unittest2.TestCase):

    def setUp(self):
        self.graph = Graph()

    def test_add(self):
        self.graph.add(Node('A', 1))
        self.graph.add(Node('A', 2))
        self.assertEqual([1, 2], self.graph.edges('A'))
        self.assertFalse(self.graph['A'][0].next)

        self.graph.add(Node('B', 3))
        self.assertTrue(len(self.graph) == 2)
        self.assertEqual([3], self.graph.edges('B'))

    def test_connect(self):
        self.graph.add(Node('A', 1))
        self.graph.add(Node('B', 1))
        self.graph.connect('A', 'B')
        self.assertEqual('B', self.graph['A'][0].next.name)

        self.graph.connect('B', 'A')
        self.assertEqual('A', self.graph['B'][0].next.name)

    def test_edges(self):
        self.graph.add(Node('A', 1))
        self.graph.add(Node('A', 2))
        self.graph.add(Node('B', 3))
        self.graph.connect('A', 'B')
        self.graph.connect('B', 'A')

        result = self.graph.edges('A')
        self.assertEqual([1, 2, 3, 1], result)
