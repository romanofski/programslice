import ast
import programslice.graph
from collections import deque


class ControlDependencyVisitor(ast.NodeVisitor):

    def __init__(self):
        self.graphs = []
        self.current_graph = None
        self.stack = deque()
        self.variables = deque()

    def visit_FunctionDef(self, node):
        graph = programslice.graph.Graph(
            'function {0}:{1}'.format(node.name, node.lineno))
        self.stack.appendleft(graph)
        [self.visit(x) for x in node.body]
        self.reset()

    def visit_Assign(self, node):
        # XXX ignoring module-level assignments
        if not self.stack:
            return
        n = programslice.graph.Node(node.lineno)
        graph = self.stack[0]
        graph.add(n)
        [self.visit(x) for x in ast.iter_child_nodes(node)]

    def visit_Name(self, node):
        if not self.stack:
            return

        graph = self.stack[0]
        if node.lineno not in graph.edges():
            graph.add(programslice.graph.Node(node.lineno))
            self.variables.appendleft(node)
        if node.id in [x.id for x in self.variables]:
            oldnode = self.variables.popleft()
            graph.connect(oldnode.lineno, node.lineno)
        else:
            self.variables.extend([node])

    def visit_While(self, node):
        n = programslice.graph.Node(node.lineno)
        graph = self.stack[0]
        graph.add(n)
        [self.visit(x) for x in node.body]
        tail = graph.graph.keys()[-1]
        graph.connect(tail, n)

    def visit_Return(self, node):
        n = programslice.graph.Node(node.lineno)
        self.stack[0].add(n)

    def reset(self):
        self.graphs.append(self.stack.popleft())
