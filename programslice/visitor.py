import ast
import programslice.graph
from collections import deque


class DependencyBuilder(object):
    """ Helper to connect edges.

    The idea behind the DependencyBuilder is that we connect
    variables which are read with variables which are written. In other
    words, read variables are assigned to written variables, which will
    become the new read variables and so on.

    Consider the following simple lines of code:

    >>> v = 1
    >>> p = v

    `v` is written (value 1 assigned), then it's value is assigned to p.
    We want to use the `DependencyBuilder` to create a graph, so that we
    have a vertice from v to p.

    >>> from programslice.graph import Graph, Edge
    >>> from programslice.visitor import DependencyBuilder
    >>> graph = Graph('func')

    The following lists:

    >>> targets = [Edge('n', 1, 0)]
    >>> variables = [Edge('p', 1, 0)]

    is equivalent to the following unparsed source code:

    >>> n = p

    The two lists won't build dependencies just yet, only edges:

    >>> builder = DependencyBuilder(targets, variables)
    >>> builder(graph)
    >>> len(graph)
    2

    The builder always keeps the last context but not the variables:

    >>> builder.targets
    [<Edge n at #1@0>]
    >>> builder.names

    Advancing our ast visitor we create dependencies:

    >>> builder.targets.append(Edge('p', 2, 0))
    >>> builder.names = [Edge('n', 2, 0)]
    >>> builder(graph)
    >>> graph[Edge('n', 1, 0)]
    [<Edge p at #2@0>]

    which would be similar to the second line of the program like:

    >>> p = n

    The new context is kept again, but not the variables:

    >>> builder.targets
    [<Edge p at #2@0>]
    >>> builder.names
    """

    def __init__(self, targets=None, names=None):
        self.targets = targets
        self.names = names

    def __call__(self, graph):
        if not self.names or not self.targets:
            return

        newtargets = []
        for target in self.targets:
            keep = True
            if target not in graph:
                graph.add(target)
            for var in self.names:
                if var not in graph:
                    graph.add(var)
                if var.name == target.name:
                    graph.connect(target, self.targets[-1])
                    keep = False
                    break
            if keep:
                newtargets.append(target)
        # Always keep at least one old target
        self.targets = newtargets if newtargets else [self.targets[-1]]
        self.names = None

    def append(self, astnode):
        """ Creates an Edge from the astnode and throws it in one of two
            troves:

            * targets
            * names

        The target trove is used if the name ctx is ast.Store, otherwise
        we consider it a simple read only variable.
        """
        assert isinstance(astnode, ast.Name), 'has to be ast.Name instance'
        edge = programslice.graph.Edge.create_from_astnode(astnode)
        if isinstance(astnode.ctx, ast.Store):
            if self.targets is None:
                self.targets = []
            self.targets.append(edge)
        else:
            if self.names is None:
                self.names = []
            self.names.append(edge)


class LineDependencyVisitor(ast.NodeVisitor):
    """
    A visitor which creates a data dependency graph.

    .. note:: I've called it LineDependencyVisitor, as currently what
        matters are dependencies between lines of code. This is
        determined by simply using the occurences of variables in their
        lines as the graphs edges. This is not very precise, but makes
        of a nice prototype to play with the vim integration.
    """

    def __init__(self):
        self.graphs = []
        self.scope = deque()
        self.variables = deque()
        self.calls = {}
        self.contexts = []
        self.builder = DependencyBuilder()

    def get_graph_for(self, lineno):
        """
        Returns a :mod:`graph`, which visited the given lineno
        """
        for graph in self.graphs:
            # TODO: aw... violation of demeters law. Please refactor me
            if lineno >= graph.first.lineno and lineno <= graph.last.lineno:
                return graph

    def visit_FunctionDef(self, node):
        graph = programslice.graph.Graph(node.name)
        graph.add(programslice.graph.Edge.create_from_astnode(node))

        self.scope.append(graph)

        [self.visit(x) for x in ast.iter_child_nodes(node)]
        self.reset()

    def visit_Call(self, node):
        if isinstance(node.func, ast.Name):
            self.calls.setdefault(node.func.id, node.func)
        [self.visit(x) for x in ast.iter_child_nodes(node)]

    def visit_Name(self, node):
        self.builder.append(node)

    def visit_Assign(self, node):
        super(LineDependencyVisitor, self).generic_visit(node)
        if self.scope:
            graph = self.scope[-1]
            self.builder(graph)

    def reset(self):
        graph = self.scope.pop()
        node = self.calls.get(graph.name)
        if node is not None:
            for g in self.graphs:
                if node.lineno in range(g.first, g.last + 1):
                    edge = g.get_edge_by_lineno(node.lineno)
                    if edge is None:
                        return
                    edge = programslice.graph.Edge.create_from_astnode(node)
                    g.add(edge)
                    g.connect_with_graph(edge, graph)
                    del self.calls[graph.name]

        self.graphs.append(graph)
