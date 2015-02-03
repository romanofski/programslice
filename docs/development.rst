Development
===========

Overview
--------

The project is hosted on github:

    https://github.com/romanofski/programslice
    https://github.com/romanofski/programslice.vim

Github issues is used as backlogs:

    https://github.com/romanofski/programslice/issues
    https://github.com/romanofski/programslice.vim/issues

Documentation is available on:

    http://programslice.readthedocs.org/

Screenshots are at:

    https://github.com/romanofski/programslice.vim

Design Decisions
----------------

The implementation makes use of the Python ASTs `ctx` attribute in order
to find out if variables are written or read. The control flow graph
representing the source code is currently very basic; most likely
leading to errors in the slice result.

At this point in time, I'm concentrating not on exact accuracy (if even
possible in a duck typed language), but on an overall good user
experience.

.. include:: ../programslice/tests/test_slice_forward.txt

