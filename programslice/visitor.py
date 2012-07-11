import ast


class DataDependencyVisitor(ast.NodeVisitor):

    def __init__(self):
        self.graph = dict()

    def visit_Assign(self, node):
        self.context = node.targets[0].id
        for c in ast.iter_child_nodes(node):
            meth = getattr(self, 'visit_' + c.__class__.__name__,
                           self.generic_visit)
            meth(c)

    def visit_BinOp(self, node):
        self.visit(node.left)
        self.visit(node.right)

    def visit_Name(self, node):
        if node.id not in self.graph.keys():
            self.graph.setdefault(node.id, [node.lineno])
        else:
            self.graph[node.id].append(node.lineno)

