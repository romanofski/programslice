import ast
import os.path
import programslice.visitor
import programslice.graph


def get_sliced_testdata(filename, start_criteria):
    """
    Parses given filename and slices the source by given start_criteria.
    Returns the slice result
    """
    node = load_testdata(filename)
    visitor = programslice.visitor.LineDependencyVisitor()
    visitor.visit(node)
    graph = visitor.graph
    return programslice.graph.Slice(graph)(start_criteria)


def load_testdata(filename):
    filepath = os.path.join(os.path.dirname(__file__),
                            'testdata', filename)
    with open(filepath, 'r') as f:
        node = ast.parse(f.read(), filepath)
    return node
