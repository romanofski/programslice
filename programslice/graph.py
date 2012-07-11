class Node(object):

    def __init__(self, name):
        self.name = name


class Graph(object):

    def __init__(self):
        self.graph = dict()

    def edges(self):
        return self.graph.keys()

    def add(self, *args):
        [self.graph.setdefault(node.name, [])
         for node in args]

    def connect(self, n1, n2):
        self.graph[n1].append(self.graph[n2])

    def __len__(self):
        return len(self.graph)

    def __getitem__(self, key):
        return self.graph[key]
