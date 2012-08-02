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


def command_slice_file():
    """
    Command line utility which can slice a given file and return the
    depending lines.
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
        lines = slice_string(arguments.line, contents,
                                 arguments.filename, arguments.invert)
        if lines:
            logger.info("{0}".format(lines))
        sys.exit(0)


def slice_string(currentline, source, name, invert=False):
    """
    Slices the given source code from the given currentline.

    :param currentline: A line from which to start the slicing.
    :type currentline: integer
    :param source: The source code to parse.
    :type source: string
    :param name: filename of the given source code.
    :type name: string
    :param invert: Invert the result and return lines which don't depend
                    on the ``currentline``. Defaults to **False**.
    :type invert: boolean
    """
    lines = []
    # catch encoding declarations and shebangs
    head = re.compile(r'#!\/.*\n|#.*coding[:=]\s*(?P<enc>[-\w.]+).*')
    encoding = (head.match(source).group('enc')
                if (head.match(source) and
                    head.match(source).group('enc')) else u'utf-8')
    source = head.sub('', source)

    node = ast.parse(source.decode(encoding), name)
    visitor = programslice.visitor.LineDependencyVisitor()
    visitor.visit(node)
    graph = visitor.get_graph_for(currentline)
    if graph:
        lines = graph.slice_forward(currentline)
        inverted = set(range(graph.first, graph.last + 1)) - set(lines)
    return list(inverted) if invert else lines
