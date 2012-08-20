Motivation
==========

I would like to see what statements depend on a given statement. For
debugging it is sometimes very helpful to focus on the **important**
parts of a program.

More information can be found here:

    http://en.wikipedia.org/wiki/Program_slicing

.. note::

    The current implementation does not provide Weiser-style slicing as
    described on Wikipedia. It is a goal to support it though.

Disclaimer
----------

An educational project of mine. I'm not sure how I go with this, as it
is new territory for me.

Development
===========

The project is hosted on github:

    https://github.com/romanofski/programslice

Documentation is available on:

    http://programslice.readthedocs.org/

TODO
----

The current implementation is a prototype in every direction.

    Slicing - Increase Precision
        The slicing is currently very dumb. It builds a graph based on
        variable occurrences. The line numbers are the edges. This has
        it's dis-advantages. It's good to explore the vim integration
        possibilities.

    Slicing - Increase Slicing Quality
        By increasing precision, we could improve the slicing quality.

    VIM Integration
        The vim integration is currently a prototype. I'd like to use it
        from early on to explore the possibilities. Please see
        :ref:`vim-integration` for more details.


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

