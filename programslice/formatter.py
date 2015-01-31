

class LineFormatter(object):
    """
    .. module:: formatter

    A base formatter which outputs a list of linenumbers on call.

    :param slice_result: The slice result which should be a list of line
                        numbers.
    :type slice_result: list
    :param source: The parsed source code as a string.
    :type source: str

    TODO: No checking in place wether `source` is a unicode object or
    str.

    >>> from programslice.graph import Edge
    >>> slice_result = [Edge('foo', 12, 3), Edge('bar', 13, 3)]
    >>> formatter = LineFormatter(slice_result, '')
    >>> formatter()
    [12, 13]
    """

    def __init__(self, slice_result, source):
        self.source = source
        self.slice_result = slice_result

    def __call__(self):
        return [x.lineno for x in self.slice_result]


class VimOutPutFormatter(LineFormatter):
    """Returns slice output information parsable by vim.

    This class is different to the Line formatter in that it outputs
    additional detail from the edges.
    """

    def __call__(self):
        return ['{node.name},{node.lineno},{node.column}'.format(node=x)
                for x in self.slice_result]


class TextOutputFormatter(LineFormatter):
    """Returns a list of source code lines based on the given slice
       result line numbers.
    """

    def __call__(self):
        splitted = self.source.splitlines()
        linenumbers = [x.lineno for x in self.slice_result]
        return [splitted[x - 1] for x in set(linenumbers)]
