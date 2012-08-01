from collections import OrderedDict
from collections import deque


class Graph(object):
    """
    .. module:: graph
    .. option:: synopsis

    A graph which represents a function visited by a
    programslice.visitor. The edges of this graph are the line numbers
    of the parsed function.

    :param name: A name identifying this graph (e.g. function X)
    """

    def __init__(self, name=''):
        self.name = name
        self.graph = OrderedDict()

    def add(self, lineno):
        self.graph.setdefault(lineno, [])

    @property
    def edges(self):
        return self.graph.keys()

    @property
    def first(self):
        """
        The first line number parsed by this graph.

        :rtype: integer
        """
        return min(self.edges)

    @property
    def last(self):
        """
        The last line number parsed by this graph.

        :rtype: integer
        """
        return max(self.edges)

    def connect(self, lineno1, lineno2):
        self.graph.setdefault(lineno1, []).append(lineno2)

    def slice_forward(self, lineno):
        """
        A forward slice starting at the given line number.

        :param lineno: A line number from which to calculate
                       dependent line numbers.
        :type lineno: integer
        :rtype: list of integers representing line numbers
        """
        visited = [lineno]
        children = deque(self.graph[lineno])

        while children:
            lineno = children.popleft()
            if lineno not in visited:
                children.extend(deque(self.graph[lineno]))
                visited.append(lineno)

        return sorted(visited)  # XXX perhaps not needed

    def __len__(self):
        return len(self.graph)

    def __getitem__(self, key):
        return self.graph[key]
