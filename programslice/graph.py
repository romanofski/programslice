from collections import OrderedDict
from collections import deque


class Node(object):

    def __init__(self, lineno):
        self.lineno = lineno


class Graph(object):

    def __init__(self, name=''):
        self.name = name
        self.graph = OrderedDict()

    def edges(self):
        return self.graph.keys()

    def add(self, node):
        self.graph.setdefault(node.lineno, [])

    def connect(self, lineno1, lineno2):
        self.graph.setdefault(lineno1, []).append(Node(lineno2))

    def slice_forward(self, lineno):
        """
        A forward slice, which is just a breadth first traversal.
        """
        visited = [lineno]
        children = deque(self.graph[lineno])

        while children:
            n = children.popleft()
            if n.lineno not in visited:
                children.extend(deque(self.graph[n.lineno]))
                visited.append(n.lineno)

        return sorted(visited)  # XXX perhaps not needed

    def __len__(self):
        return len(self.graph)

    def __getitem__(self, key):
        return self.graph[key]
