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


What is currently implemented
=============================

The current implementation is a prototype in every direction:

    * The slicing is currently very dumb. It simply builds a graph based
      on variable appearences. I'd like to see how usefull that is. Esp.
      because I can currently only see where a variable is declared.
      This seems to be an improvement.

    * The vim integration is also a meager prototype. I'd like to use it
      and tweak it to make it the most useful integration ever.

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
