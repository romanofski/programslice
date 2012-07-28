import ast
import re
import programslice.visitor


def slice_vim_buffer(currentline, contents, name):
    """
    Utility function which can be used for integrating with vim
    """
    lines = []
    # catch encoding declarations and shebangs
    head = re.compile(r'#!\/.*\n#.*coding[:=]\s*([-\w.]+)')
    encoding = (head.match(contents).group(1)
                if head.match(contents) else u'utf-8')
    contents = head.sub('', contents)

    node = ast.parse(contents.encode(encoding), name)
    visitor = programslice.visitor.LineDependencyVisitor()
    visitor.visit(node)
    graph = visitor.get_graph_for(currentline)
    if graph:
        lines = graph.slice_forward(currentline)
    return lines
