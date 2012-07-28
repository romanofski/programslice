import ast
import programslice.visitor


def slice_vim_buffer(currentline, buffer, name):
    """
    Utility function which can be used for integrating with vim
    """
    node = ast.parse(buffer, name)
    visitor = programslice.visitor.LineDependencyVisitor()
    visitor.visit(node)
    graph = visitor.get_graph_for(currentline)
    return graph.slice_forward(currentline)
