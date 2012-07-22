from collections import OrderedDict


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
        A forward slice, which is just a depth first traversal.
        """
        visited = [lineno]
        children = list(self.graph[lineno])

        while children:
            n = children.pop()
            if n.lineno not in visited:
                children.extend(list(self.graph[n.lineno]))
                visited.append(n.lineno)

        return sorted(visited)  # XXX perhaps not needed

    def __len__(self):
        return len(self.graph)

    def __getitem__(self, key):
        return self.graph[key]
