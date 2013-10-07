py2scala -- Use scala compiler for static analysis of well-typed python
=======================================================================

:author: Dan Connolly
:copyright: Dan Connolly and the University of Kansas Medical Center
:license: MIT

Usage
-----

Convert one module::

  $ python py2scala/p2s.py my_module.py >my_module.scala

:TODO: Test, document details of testing the output with the scala compiler.
:TODO: Test p2s.main().
:TODO: Test, document --package option.

Testing
-------

@@@
  $ mkdir -p src/test/scala

Use nose__ as usual::

  py2scala$ nosetests
  ..
  ----------------------------------------------------------------------
  Ran 2 tests in 0.014s
  
  OK

__ https://pypi.python.org/pypi/nose/


Issues
------
 - case class loses encapsulation
