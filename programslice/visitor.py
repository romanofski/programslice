# coding: utf-8
import ast
import programslice.graph


class LineDependencyVisitor(ast.NodeVisitor):
    """ A visitor which creates a data dependency graph.

    ..  note:: Read and Written variables are currently kept track of in
               a dictionary for each. This may cause wrong results when
               building the graph. Furthermore, we might want to create
               a data flow graph and a control flow graph.
    """

    def __init__(self):
        self.graph = programslice.graph.Graph('control flow')
        self.writes = {}
        self.reads = {}

    def visit_FunctionDef(self, node):
        """
        Visits function and resets `writes` and `reads` in order to
        prevent that current function variables are overwritten.

        Note: This currently just resets writes and reads which is used
        to build the dependency graph. But it should set the writes and
        reads in such a way, so that we can slice over function
        boundaries.
        """
        self.writes = {}
        self.reads = {}
        super(LineDependencyVisitor, self).generic_visit(node)

    def visit_Name(self, node):
        """
        Use the name and context in order to distinguish between
        variables written and read.

        Once the variable is read, we tie up dependencies by line number
        and name.
        """
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

        Example:

            v = a + b
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

        For example::

            >>> v = 1
            >>> b = 1
            >>> c = v

            writes: v, c
            reads: v
            visitor visits: v (store) → b (store) → c (store) → v (read)

        In order to produce a graph such that: v → v → c we need to
        check if we can find a read variable which now is written to.
        Just using the line number is not good enough, because written
        variables may appear at the end of the function/method, not in
        the next line.
        """
        for i, write_nodes in self.writes.items():
            for w_node in write_nodes:
                r_node = self.reads.get(i)
                if r_node is None:
                    continue

                assert isinstance(w_node.ctx, ast.Store)
                write = programslice.graph.Edge.create_from_astnode(w_node)
                read = programslice.graph.Edge.create_from_astnode(r_node)
                self.graph.connect(write, read)
