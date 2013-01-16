import argparse
import ast
import logging
import os.path
import programslice.formatter
import programslice.graph
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
        description='Static analysis tool to slice python programs')
    parser.add_argument(
        'filename',
        help=('Path to a file to be sliced'),
        type=str)
    parser.add_argument(
        'line',
        help=('The line to slice.'),
        type=int)
    parser.add_argument(
        '-i',
        '--invert',
        dest='invert',
        help=('Invert the result: return all lines which are not '
              'depending on the given line.'),
        action='store_true')
    parser.add_argument(
        '-o',
        '--output',
        dest='output',
        default='linenumbers',
        help=('Choose an output: linenumbers (default), text.'),
        type=str)
    arguments = parser.parse_args()
    if not os.path.exists(arguments.filename):
        logger.error("Can't open {0}.".format(arguments.filename))
        sys.exit(1)

    formatter = (programslice.formatter.TextOutputFormatter
              if arguments.output.startswith('text')
              else programslice.formatter.LineFormatter)
    with open(arguments.filename, 'r') as f:
        contents = f.read()
        lines = slice_string(arguments.line,
                             contents,
                             arguments.filename,
                             arguments.invert,
                             formatter)
        if lines:
            [logger.info('{0}'.format(x)) for x in lines]
        sys.exit(0)


def slice_string(currentline, source, name, invert=False,
                 formatter=programslice.formatter.LineFormatter):
    """
    Slices the given source code from the given currentline.

    :param currentline: A line from which to start the slicing.
    :type currentline: int
    :param source: The source code to parse.
    :type source: string
    :param name: filename of the given source code.
    :type name: string
    :param formatter: Formatter class to format the slice result.
                        Defaults to LineFormatter which only outputs the
                        line numbers.
    :type formatter: class

    .. deprecated:: 0.3
    :param invert: Invert the result and return lines which don't depend
                    on the ``currentline``. Defaults to **False**.
    :type invert: bool
    """
    result = []
    # catch encoding declarations and shebangs
    head = re.compile(r'#!\/.*\n|#.*coding[:=]\s*(?P<enc>[-\w.]+).*')
    source = head.sub('', source)

    node = ast.parse(source, name)
    visitor = programslice.visitor.LineDependencyVisitor()
    visitor.visit(node)
    graph = visitor.get_graph_for(currentline)
    if graph:
        result = programslice.graph.Slice(graph)(currentline)
    return formatter(result, source)()
