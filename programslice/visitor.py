import ast


class DataDependencyVisitor(ast.NodeVisitor):

    def __init__(self):
        self.graph = dict()

    def visit_Assign(self, node):
        return node
