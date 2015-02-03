Programslice
~~~~~~~~~~~~

Static analysis tool for python programs to see depending lines of code.

Note: The Python program ``programslice`` in itself is rather useless
for the end user. Use it with the `Vim editor
plugin <https://github.com/romanofski/programslice.vim>`__. See
Installation for more information.

Non-Goals
---------

-  Find defects in your code e.g. like PyChecker
-  Being used as a command line utility

Installation
------------

Requirements: Python 2.7

-  Install ``programslice`` first in your favourite virtualenv:

   $ pip install
   https://github.com/romanofski/programslice/archive/master.zip

-  Install the Vim plugin by following the instructions on it's `github
   page <https://github.com/romanofski/programslice.vim>`__.

-  Once the plugin is installed, either make sure that the
   ``programslice`` command is in your ``$PATH`` variable or you adjust
   the ``g:programslice_cmd`` variable in your ``~/.vimrc`` to point it
   to the executable.

Development
===========

Overview
--------

The project is hosted on github:

::

    https://github.com/romanofski/programslice
    https://github.com/romanofski/programslice.vim

Github issues is used as backlogs:

::

    https://github.com/romanofski/programslice/issues
    https://github.com/romanofski/programslice.vim/issues

Documentation is available on:

::

    http://programslice.readthedocs.org/

Screenshots are at:

::

    https://github.com/romanofski/programslice.vim

