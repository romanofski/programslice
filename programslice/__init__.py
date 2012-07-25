import ast
import programslice.visitor


def slice_buffer(currentline, buffer, name):
    node = ast.parse(buffer.read(), name)
    visitor = programslice.visitor.ControlDependencyVisitor()
    visitor.visit(node)
    graph = visitor.graphs[-1]
    return graph.slice_forward(currentline)
