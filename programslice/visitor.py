import ast
import programslice.graph
from collections import deque


class ControlDependencyVisitor(ast.NodeVisitor):

    def __init__(self):
        self.graphs = []
        self.current_graph = None
        self.stack = deque()

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
        n = programslice.graph.Node(node.targets[0].id, node.lineno)
        graph = self.stack[0]
        graph.add(n)

    def visit_While(self, node):
        n = programslice.graph.Node('while:{0}'.format(node.lineno),
                                    node.lineno)
        graph = self.stack[0]
        graph.add(n)
        [self.visit(x) for x in node.body]
        tail = graph.graph.keys()[-1]
        graph.connect(tail, n)

    def visit_Return(self, node):
        n = programslice.graph.Node('return', node.lineno)
        self.stack[0].add(n)

    def reset(self):
        self.graphs.append(self.stack.popleft())
