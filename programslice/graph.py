"""
The reason for a self implemented graph and edge is not to be
smarter, since there are better modules available for this. It is an
educational project for me, since my own implementation.
"""
from collections import OrderedDict
from collections import deque
import ast


class Edge(object):
    """
    .. option:: synopsis

    Representing the edge of a :class:`graph`.

    :param name: Name of the edge (e.g. variable or function name)
    :type name: str
    :param ln: linenumber
    :type ln: int
    :param offset: Column offset from an ast node
    :param offset: int
    """

    def __init__(self, name, ln, offset=None):
        assert isinstance(ln, int), "line number needs to be an integer"
        assert isinstance(name, str), "name needs to be str"
        self.name = name
        self.lineno = ln
        self.offset = offset

    def __key(self):
        return (self.lineno, self.name, self.offset)

    def __eq__(self, other):
        return type(self) == type(other) and self.__key() == other.__key()

    def __hash__(self):
        return hash(self.__key())

    def __repr__(self):
        return ('<{self.__class__.__name__} {self.name} at '
                '#{self.lineno}@{self.offset}>'.format(self=self))

    @classmethod
    def create_from_astnode(klass, node):
        name = node.id if isinstance(node, ast.Name) else node.name
        return Edge(name, node.lineno, node.col_offset)


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
        # graph references
        self.references = dict()

    def __repr__(self):
        return '<{0} {1}>'.format(self.__class__.__name__, self.name)

    def add(self, edge):
        """ Adds a new edge with the given parameters to the graph.

        :rtype: Edge
        """
        self.graph.setdefault(edge, [])
        return edge

    @property
    def _linenumbers(self):
        return [x.lineno for x in self.graph]

    @property
    def edges(self):
        return self.graph.keys()

    @property
    def first(self):
        """
        The first line number parsed by this graph.

        :rtype: int
        """
        return min(self._linenumbers)

    @property
    def last(self):
        """
        The last line number parsed by this graph.

        :rtype: int
        """
        return max(self._linenumbers)

    def connect(self, e1, e2):
        # Sometimes we want to connect an edge with a graph. Instead of
        # checking multiple types, we check only if they're the same
        # type. TODO: Perhaps put the type checking in a decorator
        assert isinstance(e1, type(e1)) and isinstance(e2, type(e2))
        self.graph.setdefault(e1, []).append(e2)

    def connect_with_graph(self, edge, graph):
        """ Connects the edge with a graph.

        This is used when we would like to slice over boundaries, e.g.
        function call to a function which is defined in the same module.
        """
        self.references.setdefault(edge, []).append(graph)

    def connect_by_lineno(self, l1, l2):
        """ Connects the edges given by their line numbers.

        If edges can not be looked up by their line numbers, a KeyError
        is raised.

        :param l1: The line number of the first edge.
        :type l1: int
        :param l2: The line number of the second edge.
        :type l2: int
        """
        e1 = self.get_edge_by_lineno(l1)
        e2 = self.get_edge_by_lineno(l2)
        if e1 is None or e2 is None:
            empty = l1 if e1 is not None else l2
            raise KeyError('Edge for {} does not exist.'.format(empty))
        self.connect(e1, e2)

    def get_edge_by_lineno(self, lineno):
        """ Returns the first edge of the given line number.

        :param lineno: The line number
        :type lineno: int
        :rtype: Edge or None if no corresponding edge is found
        """
        for e in self.edges:
            if e.lineno == lineno:
                return e

    def get_edges_by_name(self, name, unique_lines=False):
        """ Returns all edges with the same name.

        :param name: The name of the edge
        :type name: str
        :param unique_lines: Only return edges which are on separate
                             line numbers.
        :type unique_lines: bool
        :rtype: List of Edges or [] if no corresponding edge is found
        """
        result = []
        lines = []
        for e in self.edges:
            if e.name != name:
                continue
            if unique_lines and e.lineno in lines:
                continue
            else:
                result.append(e)
                lines.append(e.lineno)
        return result

    def get_neighbors(self, edge):
        """ Returns all edges, such there is a vertice from the given
            edge to the neighbor edge.

        ..  note:: This can return Edges and Graphs
        """
        return (self.graph.get(edge, []) + self.references.get(edge, []))

    def __len__(self):
        return len(self.graph)

    def __getitem__(self, key):
        return self.graph[key]

    def __contains__(self, edge):
        for e in self.edges:
            if e == edge:
                return True


# TODO: This will have to be given a search criteria not a simple line
# number
class Slice(object):
    """ A simple search over a graph, starting from an edge given by a
        line number.

    :param graph: The graph to slice
    :type graph: Graph
    """

    def __init__(self, graph):
        self.graph = graph

    def __call__(self, start):
        lineno = (start.lineno if isinstance(start, Edge) else start)
        result = self.slice_forward(lineno)
        return result

    def slice_forward(self, lineno):
        """
        A forward slice starting at the given line number.

        :param lineno: A line number from which to calculate
                       dependent line numbers.
        :type lineno: int
        :rtype: list of edges
        """
        edge = self.graph.get_edge_by_lineno(lineno)
        if edge is None:
            return []

        visited = [edge]
        children = deque(self.graph.get_neighbors(edge))

        while children:
            edge = children.popleft()
            if isinstance(edge, Graph) and edge not in visited:
                slice = Slice(edge)
                visited.extend(slice(edge.first))
            elif edge not in visited:
                children.extend(deque(self.graph.get_neighbors(edge)))
                visited.append(edge)

        # XXX filter out duplicates

        # XXX the sorting defies the whole purpose of traversing first
        # I don't even think it matters really.
        return sorted(visited)
