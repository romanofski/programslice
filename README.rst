==============
 Programslice
==============

Static analysis tool for python programs to see depending lines of code.


.. toctree::
   :maxdepth: 2

   design
   screenshots
   intro
   vim
   api



Non-Goals
---------

    * Find defects in your code e.g. like PyChecker
    * Being used as a command line utility

Design Decisions
----------------

The implementation makes use of the Python ASTs `ctx` attribute in order
to find out if variables are written or read. The control flow graph
representing the source code is currently very basic; most likely
leading to errors in the slice result.

At this point in time, I'm concentrating not on exact accuracy (if even
possible in a duck typed language), but on an overall good user
experience.

Installation Requirements
-------------------------

    * Python 2.7

Development
===========

The project is hosted on github:

    https://github.com/romanofski/programslice

Documentation is available on:

    http://programslice.readthedocs.org/

Screenshots are at:

    http://programslice.readthedocs.org/en/latest/screenshots.html

Build Status:

    "!https://travis-ci.org/romanofski/programslice.png!":https://travis-ci.org/romanofski/programslice

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
