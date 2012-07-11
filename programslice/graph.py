class Node(object):

    next = None

    def __init__(self, name, lineno):
        self.name = name
        self.lineno = lineno


class Graph(object):

    def __init__(self):
        self.graph = dict()

    def edges(self):
        return self.graph.keys()

    def add(self, node):
        if not node.name in self.graph.keys():
            self.graph.setdefault(node.name, [])
        self.graph[node.name].append(node)

    def connect(self, n1, n2):
        self.graph[n1][0].next = self.graph[n2][0]

    def __len__(self):
        return len(self.graph)

    def __getitem__(self, key):
        return self.graph[key]
