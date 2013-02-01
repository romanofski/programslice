Design
======

Usergroup
---------

Advanced Python developers.

Scenarios
---------

Roman is a passionate python developer. Sometimes however his passion
slumps when debugging `megamoths <http://www.codinghorror.com/blog/2012/07/new-programming-jargon.html>`_
The challenge when debugging these huge lines of code is to find how
each of the variables are connected in order to track down the problem.
Now he has programslice. It helps him to focus on the important
variables which are connected to a variable which might be the possible
cause of failure.

Data Representation
-------------------

Each function is represented as a :py:class:`graph`. Each name in the function is
represented as an :py:class:`edge`.

.. note:: Open Question: What about scopes? See #16 on github.

Slice
^^^^^

A slice is currently a depth-first traversal over a :py:class:`graph`.

.. note:: Open Question: How to implement slicing backwards effectively
    a.k.a. search the graph backwards.


Integration - Vim
-----------------

The following parts are involved:

    * vim
    * vim script/python script acting as an API to pass slice
      information to programslice and parse the result
    * programslice as a command line utility

This provides (hopefully) the following benefits:

    * most of the python code is testable
    * a minimum of non-testable code is covered by vimML
    * other IDEs/editors can provide an integration with the command
      line utility

but also disadvantages:

    * possible cause of integration errors
