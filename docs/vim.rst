VIM Integration
===============

Installation
------------

Install programslice like any regular python module (e.g. using pip). It
should generate a command line utility called ``programslice``.

Copy the files from the ``vim`` directory into your
``.vim/ftplugins/python`` directory::

    $ cp vim/programslice.py vim/programslice.vim ~/.vim/ftplugins/python

If the directories not exist, create them. Depending where you install
programslice, you may need to adjust your ``.vimrc`` and configure the
path to the command line utility.

Slicing
-------

Use the ex command to slice the current line:

    :SliceBuffer

in a vim session. Depending lines will be marked if the slicing is
successful. If nothing appears, no lines are depending on the current
cursor line.

Configuration
-------------

Marker Color
    You can overwrite the highlight group depending lines are marked:

        let g:programslice_dependent_lines = 'WarningMsg'

    A list of possible groups to link against can be obtained in a vim
    session by:

        :highlight

Path to the Command Line Utility
    You may need to set the path to the command line utility manually if
    ``programslice`` is not installed in your ``$PATH``. For example, in
    your ``vim.rc`` add:

        let g:programslice_cmd = "/home/user/programslice/bin/programslice"
