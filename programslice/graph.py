from collections import OrderedDict
from collections import deque


class Graph(object):

    def __init__(self, name=''):
        self.name = name
        self.graph = OrderedDict()

    def edges(self):
        return self.graph.keys()

    def add(self, lineno):
        self.graph.setdefault(lineno, [])

    @property
    def first(self):
        """
        Returns the first line of the program the graph represents
        """
        return min(self.edges())

    @property
    def last(self):
        """
        Returns the last parsed line of the program
        """
        return max(self.edges())

    def connect(self, lineno1, lineno2):
        self.graph.setdefault(lineno1, []).append(lineno2)

    def slice_forward(self, lineno):
        """
        A forward slice, which is just a breadth first traversal.
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
