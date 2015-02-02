==============
 Programslice
==============

Static analysis tool for python programs to see depending lines of code.


.. toctree::
   :maxdepth: 2

   intro
   vim
   api


.. note:: The Python program `programslice` in itself is rather useless for the end user. Use it
   with the `Vim editor plugin <https://github.com/romanofski/programslice.vim>`_ . See :ref:`programslice-installation` for more information.

Non-Goals
=========

    * Find defects in your code e.g. like PyChecker
    * Being used as a command line utility

.. _programslice-installation:

Installation
============

Requirements: Python 2.7

* Install `programslice` first in your favourite virtualenv::

    $ pip install https://github.com/romanofski/programslice/archive/master.zip

* Install the Vim plugin by following the instructions on it's `github
  page <https://github.com/romanofski/programslice.vim>`_.

* Once the plugin is installed, either make sure that the
  ``programslice`` command is in your ``$PATH`` variable or you adjust
  the ``g:programslice_cmd`` variable in your ``~/.vimrc`` to point it
  to the executable.

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

Credits
=======

This work would not be possible without:

    * Andreas Zellers book "Why Programs Fail: A Guide to Systematic
      Debugging"; ISBN13: 9780123745156. It introduced me to the idea of
      program slicing.

    * Ned Batchelders code complexity micro tool. I've studied his
      program and taken a few implementation ideas on board. Thank you
      very much for this.
      http://nedbatchelder.com/blog/200803/python_code_complexity_microtool.html

    * Jens Krinkes "Advanced Slicing of Sequential and Concurrent
      Programs" thesis:
      http://www.opus-bayern.de/uni-passau/volltexte/2004/37/pdf/thesis_a5.pdf

    * Wikipedia
      http://en.wikipedia.org/wiki/Program_slicing

    * VIM plug-ins: Muraoka Yusuke for pep8.vim, Kevin Watters for
      pychecker.vim and Tim Pope for fugitive.vim

    * Mark Harman, A piece of cake; http://www0.cs.ucl.ac.uk/staff/mharman/exe1.html
