# Copyright (C) 2012, Roman Joost <roman@bromeco.de>
#
# This library is free software: you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 3 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this library.  If not, see
# <http://www.gnu.org/licenses/>.
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
        self.graph.connect(Node('A'), Node('B'))

        self.assertRaises(AssertionError,
                          self.graph.connect,
                          'A', 'B')
        self.assertEqual(1, len(self.graph['A']))
