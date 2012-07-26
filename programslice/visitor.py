import ast
import programslice.graph
from collections import deque


class LineDependencyVisitor(ast.NodeVisitor):
    """
    A visitor which creates a data dependency graph.

    Note: I've called it LineDependencyVisitor, as currently it only
    matters are dependencies between lines of code and their variables.
    """

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
        graph = self.stack[0]
        graph.add(node.lineno)
        [self.visit(x) for x in ast.iter_child_nodes(node)]

    def visit_Name(self, node):
        if not self.stack:
            return

        graph = self.stack[0]
        if node.lineno not in graph.edges:
            graph.add(node.lineno)
        if node.id in [x.id for x in self.variables]:
            oldnode = self.variables.popleft()
            graph.connect(oldnode.lineno, node.lineno)
        else:
            self.variables.extend([node])

    def visit_While(self, node):
        graph = self.stack[0]
        graph.add(node.lineno)
        [self.visit(x) for x in node.body]
        tail = graph.graph.keys()[-1]
        graph.connect(tail, node.lineno)

    def visit_Return(self, node):
        self.stack[0].add(node.lineno)

    def reset(self):
        self.graphs.append(self.stack.popleft())
