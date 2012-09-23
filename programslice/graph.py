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
    :type name: string
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

        :rtype: int
        """
        return min(self.edges)

    @property
    def last(self):
        """
        The last line number parsed by this graph.

        :rtype: int
        """
        return max(self.edges)

    def connect(self, lineno1, lineno2):
        self.graph.setdefault(lineno1, []).append(lineno2)

    def slice_forward(self, lineno):
        """
        A forward slice starting at the given line number.

        :param lineno: A line number from which to calculate
                       dependent line numbers.
        :type lineno: int
        :rtype: list of integers representing line numbers
        """
        visited = [lineno]
        children = deque(self.graph[lineno])

        while children:
            lineno = children.popleft()
            if isinstance(lineno, Graph) and lineno not in visited:
                visited.extend(
                    lineno.slice_forward(lineno.first))
            elif lineno not in visited:
                children.extend(deque(self.graph[lineno]))
                visited.append(lineno)

        # remove duplicates. This is not very performant. We concentrate
        # on performance later.
        visited = list(set(visited))

        # XXX the sorting defies the whole purpose of traversing first
        # I don't even think it matters really.
        return sorted(visited)

    def __len__(self):
        return len(self.graph)

    def __getitem__(self, key):
        return self.graph[key]
