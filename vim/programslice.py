# coding: utf-8
# this is a glue function which uses the output from the executed
# programslice and returns it back to vim
# copied from pep8checker.py
from subprocess import Popen, PIPE
import os
import re
import tempfile


def glue(cmd, data, lineno):
    """Glue function to write the current buffer contents into a
    temporary file, slice it and hand the results back to vim.

    ..  note:: This function is not considered to be an API. The line
               numbers are returned as strings.

    :param cmd: the programslice command to execute.
    :type cmd: string
    :param data: the source code
    :type data: unicode
    :param lineno: the line number to slice forward from.
    :type lineno: int
    :rtype: list of strings.

    >>> data = u'# Róman'
    >>> glue('programslice', data, 14)
    []
    >>> data = u'def foo():\\n  n=1\\n  x=2\\n  return n+x'
    >>> glue('programslice', data, 2)
    ['2', '4']
    """
    assert isinstance(data, unicode)

    # dump current data to a temp file to check on the fly.
    head = re.compile(r'#!\/.*\n|#.*coding[:=]\s*(?P<enc>[-\w.]+).*')
    encoding = (head.match(data).group('enc')
                if (head.match(data) and
                    head.match(data).group('enc'))
                else u'utf-8')
    temp_file_fd, temp_file_path = tempfile.mkstemp()
    try:
        os.write(temp_file_fd, data.encode(encoding))
    except:
        os.unlink(temp_file_path)
        raise
    finally:
        os.close(temp_file_fd)

    cmd = '{cmd} {tempfilepath} {lineno}'.format(
        cmd=cmd, lineno=lineno, tempfilepath=temp_file_path)

    try:
        p = Popen(cmd, shell=True, stdout=PIPE, stderr=PIPE)
        stdout, _stderr = p.communicate()
    finally:
        os.unlink(temp_file_path)

    return stdout.splitlines()


if __name__ == '__main__':
    import doctest
    doctest.testmod()
