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


class Node(object):

    def __init__(self, name):
        self.name = name


class Graph(object):

    def __init__(self):
        self.graph = dict()

    def edges(self):
        return self.graph.keys()

    def add(self, node):
        self.graph.setdefault(node.name, [node])

    def __len__(self):
        return len(self.graph)
