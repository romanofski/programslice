# coding: utf-8
import ast
import programslice.graph


class LineDependencyVisitor(ast.NodeVisitor):
    """ A visitor which creates a data dependency graph.
    """

    def __init__(self):
        self.graph = programslice.graph.Graph('')
        self.writes = {}
        self.reads = {}

    def visit_Name(self, node):
        if isinstance(node.ctx, ast.Store):
            self.writes.setdefault(node.id, []).append(node)
        elif isinstance(node.ctx, ast.Load):
            self.reads[node.id] = node
            self.connect_by_lineno(node)
            self.connect_by_name()
        elif isinstance(node.ctx, ast.Del):
            pass
        elif isinstance(node.ctx, ast.AugLoad):
            pass
        elif isinstance(node.ctx, ast.AugStore):
            pass
        elif isinstance(node.ctx, ast.Param):
            self.reads[node.id] = node

    def connect_by_lineno(self, node):
        """
        Connect reads and writes also by line number.

        We can not only connect by name, but also by line number. That
        ensures, that we connect the variables which are read, to
        assigned ones in the current line.
        """
        for i, objs in self.writes.items():
            for obj in objs:
                if obj.lineno == node.lineno:
                    read = programslice.graph.Edge.create_from_astnode(obj)
                    write = programslice.graph.Edge.create_from_astnode(node)
                    self.graph.connect(write, read)

    def connect_by_name(self):
        """
        Here we are connecting all variables which are stored with the
        variables which are read.

        For example:

            v = 1
            b = 1
            c = v

        writes: v, c
        reads: v
        visitor visits: v (store) → b (store) → c (store) → v (read)

        In order to produce a graph such that: v → v → c we need to
        check if we can find a read variable which now is written to.
        Just using the line number is not good enough, since variables
        written to can come way down the function/method.
        """
        to_delete = set()
        for i, objs in self.writes.items():
            for astobj in objs:
                node = self.reads.get(i)
                if node is None:
                    continue

                write = programslice.graph.Edge.create_from_astnode(node)
                read = programslice.graph.Edge.create_from_astnode(astobj)
                self.graph.connect(write, read)
                to_delete.add(node.id)

        for k in to_delete:
            del self.writes[k]
            del self.reads[k]
