# coding: utf-8
import ast
import programslice.graph


class LineDependencyVisitor(ast.NodeVisitor):
    """ A visitor which creates a data dependency graph.
    """

    def __init__(self):
        self.graph = programslice.graph.Graph('')
        self.contexts = []
        self.writes = {}
        self.reads = {}

    def visit_FunctionDef(self, node):
        self.reads[node.name] = node
        super(LineDependencyVisitor, self).generic_visit(node)
        self.reset()

    def visit_Call(self, node):
        if isinstance(node.func, ast.Name):
            self.writes[node.func.id] = node

        super(LineDependencyVisitor, self).generic_visit(node)

    def visit_Name(self, node):
        for i in self.contexts:
            if i == node:
                continue
            #
            # The control flow goes from left to right. For example, the
            # value assigned to a variable has to be reflected in a
            # graph so that we can pick the value and traverse to the
            # assigned variable.
            #
            # This will create the graph, so it can be traversed. From
            # the value to the assignment in line 1, then from the value
            # which was the assignment before to the next
            # assignment.
            #
            # v = 1
            # m = v
            #
            # 1 → v → v → m
            #
            assignment = programslice.graph.Edge.create_from_astnode(i)
            value = programslice.graph.Edge.create_from_astnode(node)
            self.graph.connect(value, assignment)

    def visit_Assign(self, node):
        # Save the targets first. Then let the visitor continue walk
        # over all children. We scoop up additional names and connect
        # them in our graph.
        # Once finished, we add all targets which are not yet connected.
        # They could correspond to numerical assignments and such.
        self.contexts = node.targets
        super(LineDependencyVisitor, self).generic_visit(node)
        for i in node.targets:
            edge = programslice.graph.Edge.create_from_astnode(i)
            if edge not in self.graph:
                self.graph.add(edge)
        self.contexts = []

    def reset(self):
        to_delete = []
        for i in self.writes:
            node = self.reads.get(i)
            if node is None:
                continue

            call = self.writes[i]
            for a in call.args:
                write = programslice.graph.Edge.create_from_astnode(a)
                for b in node.args.args:
                    read = programslice.graph.Edge.create_from_astnode(b)
                    self.graph.connect(write, read)
            to_delete.append(i)
        for k in to_delete:
            del self.writes[i]
            del self.reads[i]
