import unittest2
from programslice.graph import Graph
from programslice.graph import Node


class TestGraph(unittest2.TestCase):

    def setUp(self):
        self.graph = Graph()

    def test_add(self):
        self.graph.add(Node('A', 1))
        self.graph.add(Node('A', 2))
        self.assertEqual([1, 2], self.graph.edges())

        self.graph.add(Node('B', 3))
        self.assertEqual(3, len(self.graph))
        self.assertEqual([1, 2, 3], self.graph.edges())

    def test_connect(self):
        self.graph.add(Node('A', 1))
        self.graph.add(Node('B', 1))
        self.graph.connect('A', 'B')
        self.assertEqual(['B'], self.graph['A'])

        self.graph.connect('B', 'A')
        self.assertEqual(['A'], self.graph['B'])

    def test_edges(self):
        n1 = Node('A', 1)
        n2 = Node('A', 2)
        n3 = Node('B', 3)
        self.graph.add(n1)
        self.graph.add(n2)
        self.graph.add(n3)
        self.graph.connect(n1, n2)
        self.graph.connect(n2, n3)

        result = self.graph.edges()
        self.assertEqual([1, 2, 3], result)
