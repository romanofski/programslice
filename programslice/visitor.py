import ast
import programslice.graph
from collections import deque


class DataDependencyVisitor(ast.NodeVisitor):

    def __init__(self):
        self.graphs = []
        self.current_graph = None
        self.stack = deque()

    def visit_FunctionDef(self, node):
        graph = programslice.graph.Graph(
            'function {0}:{1}'.format(node.name, node.lineno))
        self.stack.append(graph)
        [self.visit(x) for x in node.body]
        self.reset()

    def reset(self):
        self.graphs.append(self.stack.popleft())
