import ast
import programslice.graph


class DataDependencyVisitor(ast.NodeVisitor):

    def __init__(self):
        self.graph = programslice.graph.Graph()

    def visit_Assign(self, node):
        self.context = node.targets[0].id
        for c in ast.iter_child_nodes(node):
            self.visit(c)

    def visit_BinOp(self, node):
        self.visit(node.left)
        self.visit(node.right)

    def visit_Name(self, node):
        graphnode = programslice.graph.Node(node.id, node.lineno)
        self.graph.add(graphnode)
        if self.context and node.id is not self.context:
            self.graph.connect(node.id, self.context)
