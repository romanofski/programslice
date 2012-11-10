from programslice.graph import Graph
import unittest2 as unittest


class TestGraph(unittest.TestCase):

    def setUp(self):
        self.graph = Graph('foo')

    def test_repr(self):
        self.assertEqual('<Graph foo>', repr(self.graph))

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

    def test_slice_with_graph(self):
        graph2 = Graph()
        graph2.add(4)
        graph2.add(5)
        graph2.add(6)
        graph2.connect(4, 5)
        graph2.connect(5, 6)

        self.graph.add(3)
        self.graph.add(4)
        self.graph.add(graph2)
        self.graph.connect(4, graph2)

        result = self.graph.slice_forward(4)
        self.assertEqual([4, 5, 6], result)


class TestGraphSlicing(unittest.TestCase):

    def setUp(self):
        self.graph = Graph('foo')

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

        self.expected = [2, 3, 4, 5, 7, 8, 11]

    def test_slice_forward(self):
        result = self.graph.slice_forward(2)
        self.assertEqual(self.expected, result)

    def test_slice_backward(self):
        expected = self.expected
        result = self.graph.slice_backward(11)
        expected.reverse()
        self.assertEqual(expected, result)

        result = self.graph.slice_backward(5)
        self.assertListEqual([5, 4, 3, 2], result)

    def test_slice(self):
        result = self.graph.slice(2)
        self.assertListEqual(self.expected, result)

        result = self.graph.slice(11)
        self.assertListEqual(self.expected, result)
