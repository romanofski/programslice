==============
 Programslice
==============

Static analysis tool for python programs.

An educational project of mine. The goal is to be able to slice
programs. Given a statement, I'd like to forward, or backward slice a
python program and see what statements are affected by the given
statement. More information can be found here:

    http://en.wikipedia.org/wiki/Program_slicing

I'm not sure how I go with this, as it is new territory for me.

TODO
====

The current implementation is a prototype in every direction:

    Slicing - Increase Precision
        The slicing is currently very dumb. It builds a graph based on
        variable occurrences. The line numbers are the edges. This has
        it's dis-advantages. It's good to explore the vim integration
        possibilities.

    Slicing - Increase Slicing Quality
        By increasing precision, we could improve the slicing quality.

    VIM Integration
        The vim integration is currently a prototype. I'd like to use it
        from early on to explore the possibilities.

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
