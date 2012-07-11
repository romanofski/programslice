from collections import deque


class Node(object):

    next = None

    def __init__(self, name, lineno):
        self.name = name
        self.lineno = lineno


class Graph(object):

    def __init__(self):
        self.graph = dict()

    def edges(self, name):
        result = []
        if name in self.graph.keys():
            visited = []
            children = deque(self.graph[name])
            while children:
                child = children.popleft()
                if child.next:
                    new = deque(self.graph[child.next.name])
                    children.extend(new)
                result.append(child.lineno)
                if child in visited:
                    break
                visited.append(child)
        return result

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
