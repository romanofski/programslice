import ast
import programslice.visitor


def slice_vim_buffer(currentline, buffer, name):
    """
    Utility function which can be used for integrating with vim
    """
    node = ast.parse(buffer, name)
    visitor = programslice.visitor.ControlDependencyVisitor()
    visitor.visit(node)
    graph = visitor.graphs[-1]
    return graph.slice_forward(currentline)
