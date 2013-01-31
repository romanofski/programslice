Design
======

Usergroup
---------

Advanced Python developers.

Data Representation
-------------------

Each function is represented as a :py:class:`graph`. Each name in the function is
represented as an :py:class:`edge`.

.. note:: Open Question: What about scopes? See #16 on github.

Integration - Vim
-----------------

A classic slice run goes like this:

    1. The user marks a statement in VIM and calls the vim slice
       command.

    2. The command passes the slice criterion [#scriterion]_ and filename to the
       programslice command line utility.

    3. Programslice parses the source code and builds internal
       datastructures.

    4. A slice is run on the data structures and the result printed to
       stdout.

    5. The slice is parsed by vim and the result is highlighted in the
       vim buffer.


.. rubric:: Footnotes

.. [#scriterion] The slice criterion is currently a variable name,
    linenumber and offset.
