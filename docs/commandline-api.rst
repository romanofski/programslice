Commandline API
===============

This utility can be invoked by editor plugins.

Version
-------

Call::

    programslice --version

Example::

    # call
    programslice --version

    # output
    0.3


Slicing a File
--------------

Editor Compatible Output
~~~~~~~~~~~~~~~~~~~~~~~~
.. note:: This is the default output.

Call::

    programslice <filename> <varname> <line> <offset>

Example::

    # call
    programslice myproject.py obj 70 16

    # output
    obj,70,16
    obj,71,19
    obj,72,71
    read,72,20
    read,74,46

Error Case Example::

    Traceback (most recent call last):
      File "/home/foo/programslice/bin/programslice", line 11, in <module>
        sys.exit(programslice.command_slice_file())
      File "/home/foo/programslice/programslice/__init__.py", line 72, in command_slice_file
        formatter)
      File "/home/foo/programslice/programslice/__init__.py", line 111, in slice_string
        visitor.visit(node)
      File "/home/foo/programslice/programslice/visitor.py", line 71, in connect_by_lineno
        raise ValueError
    ValueError

Linenumbers Output
~~~~~~~~~~~~~~~~~~

Call::

    programslice -o linenumbers <filename> <varname> <line> <offset>

Example::

    # call
    programslice -o linenumbers myproject.py obj 70 16

    # output
    70
    71
    72
    72
    74

Text Output
~~~~~~~~~~~

This output returns the sliced source code including indentation.

.. note:: Due to the limited functionality, the sliced source code will
   most likely not syntactically correct.

Call::

    programslice -o text <filename> <varname> <line> <offset>

Example::

    # call
    programslice -o text myproject.py obj 70 16

    # output
    min = 0
    while not min > max or not max < min:
        mid = int(min + (max - min)/2)
        x = searchlist[mid]
        if x > n:
            max = mid
        elif x < n:
            min = mid
        elif x == n:
            return mid
