# coding: utf-8
import ast
import programslice.graph


class IndentVisitor(ast.NodeVisitor):

    def __init__(self):
        self.graphs = []
        self.graph = None
        self.tracker = DependencyTracker()
        self.stack = []
        self.blocks = []
        self.explicit_block_markers = []
        self.last_indent = None

    def generic_visit(self, node):
        if isinstance(node, ast.Name) or \
           isinstance(node, ast.Num) or \
           isinstance(node, ast.Compare):
            return super(IndentVisitor, self).generic_visit(node)

        if hasattr(node, 'col_offset'):
            # First time ever we see an interesting statement or expression
            if self.last_indent is None:
                self.last_indent = node.col_offset

            # TODO keep a context node?
            # E.g. if ast.If set do set jumpsources, or do something different
            # for an ast.While

            # This node is used to explicitly signal the end of a block
            if node in self.explicit_block_markers:
                self.stack.append(node)
                self._new_basic_block()

                del self.explicit_block_markers[
                    self.explicit_block_markers.index(node)]

            elif self.last_indent != node.col_offset:
                self._new_basic_block()

                # Node with the new tab indent goes into the next basic block
                self.stack = [node]
                self.last_indent = node.col_offset
            elif self.last_indent == node.col_offset:
                self.stack.append(node)

        super(IndentVisitor, self).generic_visit(node)

    def _new_basic_block(self):
        if not self.blocks:
            type = programslice.graph.ENTRY
        else:
            type = programslice.graph.MIDDLE

        block = programslice.graph.BasicBlock(self.stack, type)
        self.blocks.append(block)
        self.tracker.check_for_dependency(block)
        self.stack = []
        return block

    def visit_FunctionDef(self, node):
        if self.graph:
            self.graphs.append(self.graph)
        self.graph = programslice.graph.Graph(node.name)
        super(IndentVisitor, self).generic_visit(node)
        for b1, b2 in self.tracker.dependencies:
            self.graph.connect(b1, b2)

    def visit_If(self, node):
        n_if = ast.If(node.test, [], [])
        n_if.col_offset = node.col_offset
        self.stack.append(n_if)
        self._new_basic_block()

        # TODO keep the context node to identify these?
        if node.body:
            self.explicit_block_markers.append(node.body[-1])
            self.tracker.add_jumpnode(node.body[-1])
        if node.orelse:
            self.explicit_block_markers.append(node.orelse[-1])
            self.tracker.add_jumpnode(node.orelse[-1])

        super(IndentVisitor, self).generic_visit(node)

    def visit_Return(self, node):
        # In case we've collected additional nodes with the same indent,
        # they'll become part of the exit block.
        exit_block = [node]
        if self.stack and self.stack[-1].col_offset == node.col_offset:
            exit_block = self.stack + exit_block

        exitb = programslice.graph.BasicBlock(exit_block,
                                              programslice.graph.EXIT)
        self.blocks.append(exitb)
        self.tracker.check_for_dependency(exitb)


class DependencyTracker(object):

    def __init__(self):
        self.stack = []
        self.lastblock = None
        self.dependencies = []

        self._jumpsources = []
        self._jumpblocks = []

    def add_jumpnode(self, node):
        # Keep track of possible nodes which are sources of jumps. From here to
        # the next basic block.
        self._jumpsources.append(node)

    def check_for_dependency(self, block):
        if not self.lastblock:
            self.lastblock = block
            return

        block_is_jumpsource = False

        # TODO
        # We need something which works based on the nested blocks
        for n in self._jumpsources:
            if n in block.nodes:
                self.dependencies.append((self.lastblock, block))
                self._jumpblocks.append(block)
                del self._jumpsources[self._jumpsources.index(n)]
                block_is_jumpsource = True

        if not block_is_jumpsource:
            self.dependencies.append((self.lastblock, block))
            self.lastblock = block
        else:
            return

        # TODO
        # If the self.lastblock is None but jump blocks are set, connect them
        # with the sources and set self.lastblock to block
        if not self._jumpsources and self._jumpblocks:
            for b in self._jumpblocks:
                self.dependencies.append((b, block))
        self._jumpblocks = []



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
