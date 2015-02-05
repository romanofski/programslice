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

Development Environment
-----------------------

A development environment is easily made ready. Create a buildout
environment which will create the command line utility ``programslice``::

    python bootstrap.py
    bin/buildout

Run all tests to establish a baseline::

    bin/test

Now you need the editor plugin, which is described in detail on it's
`github page <https://github.com/romanofski/programslice.vim>`_

Building the documentation
--------------------------

Documentation can easily be build by running::

    bin/sphinxbuilder

The HTML can be found under `docs-build`.

Contributing
------------

If you hack on your own version, here is the policy for acceptable pull
requests:

* should follow PEP-8
* each feature should provide tests which cover the new code
* the patch should not break any existing tests
* it should add an entry in the `CHANGES.txt`
* if applicable, the feature/patch should be documented under `docs`

Design Decisions
----------------

The implementation makes use of the Python ASTs `ctx` attribute in order
to find out if variables are written or read. The control flow graph
representing the source code is currently very basic; most likely
leading to errors in the slice result.

At this point in time, I'm concentrating not on exact accuracy, but on
an overall good user experience.
