from programslice.graph import Graph
import unittest2 as unittest


class TestGraph(unittest.TestCase):

    def setUp(self):
        self.graph = Graph()

    def test_first_last(self):
        self.graph.add(3)
        self.graph.add(1)
        self.graph.add(5)
        self.assertEqual(1, self.graph.first)
        self.assertEqual(5, self.graph.last)

    def test_add(self):
        self.graph.add(1)
        self.graph.add(2)
        self.assertEqual([1, 2], self.graph.edges)

        self.graph.add(3)
        self.assertEqual(3, len(self.graph))
        self.assertEqual([1, 2, 3], self.graph.edges)

    def test_connect(self):
        self.graph.add(1)
        self.graph.add(2)
        self.graph.connect(1, 2)
        self.assertEqual([2], self.graph[1])

        self.graph.connect(2, 1)
        self.assertEqual([1], self.graph[2])

    def test_edges(self):
        self.graph.add(1)
        self.graph.add(2)

        result = self.graph.edges
        self.assertEqual([1, 2], result)

    def test_slice_forward(self):
        for i in range(1, 12):
            self.graph.add(i)

        self.graph.connect(1, 10)
        self.graph.connect(1, 6)
        self.graph.connect(2, 11)
        self.graph.connect(2, 7)
        self.graph.connect(2, 3)
        self.graph.connect(3, 5)
        self.graph.connect(3, 8)
        self.graph.connect(3, 4)
        self.graph.connect(4, 5)
        self.graph.connect(6, 6)
        self.graph.connect(6, 10)
        self.graph.connect(7, 7)
        self.graph.connect(8, 8)
        self.graph.connect(8, 5)
        self.graph.connect(8, 7)

        result = self.graph.slice_forward(2)
        self.assertEqual([2, 3, 4, 5, 7, 8, 11], result)

    def test_slice_with_graph(self):
        graph2 = Graph()
        graph2.add(4)
        graph2.add(5)
        graph2.add(6)
        graph2.connect(4, 6)

        self.graph.add(3)
        self.graph.add(4)
        self.graph.add(graph2)
        self.graph.connect(4, graph2)

        result = self.graph.slice_forward(4)
        self.assertEqual([4, 4, 6], result)
