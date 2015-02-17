import argparse
import ast
import logging
import os.path
import re
import sys

import programslice.formatter
import programslice.graph
import programslice.visitor
import programslice.package


logger = logging.getLogger('programslice')
stdout = logging.StreamHandler(sys.stdout)
stdout.setFormatter(logging.Formatter())
logger.addHandler(stdout)
logger.setLevel(logging.INFO)


def get_formatter_klass(name):
    """ Returns the slice result formatter given by it's name.  """
    form = programslice.formatter.VimOutPutFormatter
    if name.startswith('line'):
        form = programslice.formatter.LineFormatter
    elif name.startswith('text'):
        form = programslice.formatter.TextOutputFormatter
    return form


def command_slice_file():
    """
    Command line utility which can slice a given file and return the
    depending lines.
    """
    parser = argparse.ArgumentParser(
        prog=programslice.package.__name__,
        description='Static analysis tool to slice python programs')
    parser.add_argument(
        'filename',
        help=('Path to a file to be sliced'),
        type=str)
    parser.add_argument(
        'varname',
        help=('The variable name.'),
        type=str)
    parser.add_argument(
        'line',
        help=('The line number to slice.'),
        type=int)
    parser.add_argument(
        'offset',
        help=('Position offset of the variable.'),
        type=int)
    parser.add_argument(
        '-o',
        '--output',
        dest='output',
        default='vim',
        help=('Choose an output: vim (default), linenumbers, text.'),
        type=str)
    parser.add_argument(
        '--version',
        help=('Print version and exit.'),
        version=programslice.package.__version__,
        action='version',
        )
    arguments = parser.parse_args()
    if not os.path.exists(arguments.filename):
        logger.error("Can't open {0}.".format(arguments.filename))
        sys.exit(1)

    formatter = get_formatter_klass(arguments.output)
    with open(arguments.filename, 'r') as f:
        contents = f.read()
        lines = slice_string(arguments.varname,
                             arguments.line,
                             arguments.offset,
                             contents,
                             arguments.filename,
                             formatter)
        if lines:
            [logger.info('{0}'.format(x)) for x in lines]
        sys.exit(0)


def slice_string(varname, currentline, offset, source, filename,
                 formatter=programslice.formatter.VimOutPutFormatter):
    """
    Slices the given source code from the given currentline.

    :param varname: The variable name to slice from.
    :type varname: str
    :param currentline: A line from which to start the slicing.
    :type currentline: int
    :param offset: The position offset of the variable.
    :type offset: int
    :param source: The source code to parse.
    :type source: str
    :param filename: filename of the given source code.
    :type filename: str
    :param formatter: Formatter class to format the slice result.
                      Defaults to VimOutPutFormatter which only outputs the
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

    node = ast.parse(source, filename)
    visitor = programslice.visitor.LineDependencyVisitor()
    visitor.visit(node)
    graph = visitor.graph
    if graph:
        start = programslice.graph.Edge(varname, currentline, offset)
        result = programslice.graph.Slice(graph)(start)
    return formatter(result, source)()
