from collections import OrderedDict


class Node(object):

    next = None

    def __init__(self, name, lineno):
        self.name = name
        self.lineno = lineno


class Graph(object):

    def __init__(self, name=''):
        self.name = name
        self.graph = OrderedDict()

    def edges(self):
        return [x.lineno for x in self.graph.keys()]

    def add(self, node):
        self.graph.setdefault(node, [])

    def connect(self, n1, n2):
        self.graph.setdefault(n1, []).append(n2)

    def __len__(self):
        return len(self.graph)

    def __getitem__(self, key):
        return self.graph[key]
