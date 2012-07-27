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

What's it Good For?
===================

During a debugging session you can concentrate to debug the relevant
parts and ignore non-depending lines of code.

TODO
====

The current implementation is a prototype in every direction:

    Slicing
        The slicing is currently very dumb. It builds a graph based on
        variable appearences. The line numbers are the edges. This is a
        very low-tech approach and has it's dis-advantages.

    VIM Integration
        The vim integration is also a meager prototype. I'd like to use
        it and tweak it to make it the most useful integration ever.

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
