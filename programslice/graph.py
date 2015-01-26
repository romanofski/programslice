"""
The reason for a self implemented graph and edge is not to be
smarter, since there are better modules available for this. It is an
educational project for me, since my own implementation.
"""
from collections import OrderedDict
from collections import deque
import ast

AST_EDGE_MAPPING = {
    ast.Attribute: lambda x: x.attr,
    ast.Name: lambda x: x.id,
    ast.Subscript: lambda x: x.value.attr,
}


class Edge(object):
    """
    .. option:: synopsis

    Representing the edge of a :class:`graph`.
    """

    def __init__(self, name, ln, column):
        assert isinstance(ln, int), "line number needs to be an integer"
        assert isinstance(name, str), "name needs to be str"
        self.name = name
        self.lineno = ln
        self.column = column

    def __key(self):
        return (self.lineno, self.name, self.column)

    def __eq__(self, other):
        return type(self) == type(other) and self.__key() == other.__key()

    def __ne__(self, other):
        return not self.__eq__(other)

    def __hash__(self):
        return hash(self.__key())

    def __repr__(self):
        return ('<{self.__class__.__name__} {self.name} at '
                '#{self.lineno}@{self.column}>'.format(self=self))

    @classmethod
    def create_from_astnode(klass, node):
        return Edge(AST_EDGE_MAPPING[node.__class__](node),
                    node.lineno, node.col_offset)


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

    def add(self, edge):
        """ Adds a new edge with the given parameters to the graph.

        :rtype: Edge
        """
        self.graph.setdefault(edge, [])
        return edge

    @property
    def edges(self):
        return self.graph.keys()

    def connect(self, e1, e2):
        assert isinstance(e1, type(e1)) and isinstance(e2, type(e2))
        self.graph.setdefault(e1, []).append(e2)
        self.add(e2)

        # Tie up potential outstanding edges
        for key in self.graph:
            if (key.name == e1.name and
                    key != e1 and
                    e1 not in self.graph[key]):
                self.graph[key].append(e1)

    def get(self, edge, default=[]):
        """ Returns all referenced edges from the given edge an empty
            list otherwise.
        """
        return self.graph.get(edge, default)

    def __len__(self):
        return len(self.graph)

    def __getitem__(self, key):
        return self.graph[key]

    def __contains__(self, edge):
        for e in self.edges:
            if e == edge:
                return True


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

        # XXX filter out duplicates

        # XXX the sorting defies the whole purpose of traversing first
        # I don't even think it matters really.
        return visited
