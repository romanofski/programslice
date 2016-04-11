"""
Note: The reason for a self implemented graph and edge is not to be
smarter, since there are better modules available for this. It is an
educational project for me.
"""
from collections import OrderedDict
from collections import deque
from collections import Iterable
import hashlib

import astor


ENTRY = 1
MIDDLE = 2
EXIT = 3


class BasicBlock(object):

    def __init__(self, nodes, type=MIDDLE):
        self.assert_when_different_indent(nodes)
        self.type = type
        assert isinstance(nodes, Iterable)
        self.nodes = nodes
        self.name = '\\n'.join(
            [astor.to_source(n, add_line_information=False)
             for n in self.nodes
             ])
        h = hashlib.new('sha256')
        h.update(self.name)
        self.uid = h.hexdigest()

    def assert_when_different_indent(self, nodes):
        """In case we get nodes with different indentation levels passed, we
        want to error out. Something went wrong with identifying the basic
        blocks and we should not proceed with incomplete information."""
        indents = [x.col_offset for x in nodes]

        assert len(set(indents)) == 1, zip(indents, nodes)

    def __repr__(self):
        typename = 'EXIT'
        if self.type == ENTRY:
            typename = 'ENTRY'
        elif self.type == MIDDLE:
            typename = 'MIDDLE'
        return '<{self.__class__.__name__} ({typename})>'.format(
            self=self,
            typename=typename)


class Graph(object):
    """
    .. option:: synopsis

    A graph which represents a function visited by a
    programslice.visitor. The edges of this graph are the line numbers
    of the parsed function.

    :param name: A name identifying this graph (e.g. function X)
    :type name: string
    """

    def __init__(self, name=''):
        self.name = name
        self.graph = OrderedDict()

    def __repr__(self):
        return '<{} {} {}>'.format(
            self.__class__.__name__,
            self.name,
            self.graph.items())

    def add(self, block):
        """ Adds a new node to the graph

        :rtype: BasicBlock
        """
        self.graph.setdefault(block, [])
        return block

    @property
    def edges(self):
        result = []
        for node, deps in self.graph.items():
            result += [(node, n) for n in deps]
        return result

    def connect(self, n1, n2):
        blocks = self.graph.setdefault(n1, [])
        if n2 not in blocks:
            self.graph[n1].append(n2)
        self.add(n2)

    def get(self, block):
        """ Returns all referenced edges from the given edge an empty
            list otherwise.
        """
        return self.graph.get(block)

    def __len__(self):
        return len(self.graph)


class Slice(object):
    """ A simple search over a graph, starting from an edge.

    :param graph: The graph to slice
    :type graph: Graph
    """

    def __init__(self, graph):
        self.graph = graph

    def __call__(self, edge):
        assert isinstance(edge, Edge)
        result = self.slice_forward(edge)
        return result

    def slice_forward(self, edge):
        """
        A forward slice starting at the given edge. A match is when
        ``Edge.lineno == lineno`` and ``Edge.name == name``.

        :param edge: An edge.
        :type edge: Edge
        :rtype: list of edges
        """
        visited = [edge]
        children = deque(self.graph.get(edge))
        if not children:
            return []

        while children:
            edge = children.popleft()
            if isinstance(edge, Graph) and edge not in visited:
                slice = Slice(edge)
                visited.extend(slice(edge.first))
            elif edge not in visited:
                children.extend(deque(self.graph.get(edge)))
                visited.append(edge)

        return visited
