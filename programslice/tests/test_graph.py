import unittest2
from programslice.graph import Graph
from programslice.graph import Node


class TestGraph(unittest2.TestCase):

    def setUp(self):
        self.graph = Graph()

    def test_add(self):
        self.graph.add(Node('A'))
        self.graph.add(Node('A'))
        self.assertEqual([], self.graph['A'])

        self.graph.add(Node('B'))
        self.assertTrue(len(self.graph) == 2)
        self.assertEqual(['A', 'B'], self.graph.edges())

    def test_connect(self):
        self.graph.add(Node('A'))
        self.graph.add(Node('B'))
        self.graph.connect('A', 'B')

        self.assertEqual(1, len(self.graph['A']))
