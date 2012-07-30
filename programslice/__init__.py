import argparse
import ast
import logging
import os.path
import programslice.visitor
import re
import sys


logger = logging.getLogger('programslice')
stdout = logging.StreamHandler(sys.stdout)
stdout.setFormatter(logging.Formatter())
logger.addHandler(stdout)
logger.setLevel(logging.INFO)


def slice_file():
    """
    Command utility to slice a given file
    """
    parser = argparse.ArgumentParser(
        description="Static analysis tool to slice python programs")
    parser.add_argument(
        "filename",
        help=("Path to a file to be sliced"),
        type=str)
    parser.add_argument(
        "line",
        help=("The line to slice."),
        type=int)
    parser.add_argument(
        "-i",
        "--invert",
        dest='invert',
        help=("Invert the result: return all lines which are not "
              "depending on the given line."),
        action="store_true")
    arguments = parser.parse_args()

    if not os.path.exists(arguments.filename):
        logger.error("Can't open {0}.".format(arguments.filename))
        sys.exit(1)

    with open(arguments.filename, 'r') as f:
        contents = f.read()
        lines = slice_vim_buffer(arguments.line, contents,
                                 arguments.filename, arguments.invert)
        if lines:
            logger.info("{0}".format(lines))
        sys.exit(0)


def slice_vim_buffer(currentline, contents, name, invert=False):
    """
    Utility function which can be used for integrating with vim
    """
    lines = []
    # catch encoding declarations and shebangs
    head = re.compile(r'#!\/.*\n|#.*coding[:=]\s*(?P<enc>[-\w.]+)')
    encoding = (head.match(contents).group('enc')
                if (head.match(contents) and
                    head.match(contents).group('enc')) else u'utf-8')
    contents = head.sub('', contents)

    node = ast.parse(contents.decode(encoding), name)
    visitor = programslice.visitor.LineDependencyVisitor()
    visitor.visit(node)
    graph = visitor.get_graph_for(currentline)
    if graph:
        lines = graph.slice_forward(currentline)
        inverted = set(range(graph.first, graph.last + 1)) - set(lines)
    return list(inverted) if invert else lines
