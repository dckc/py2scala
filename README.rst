py2scala -- Use scala compiler for static analysis of well-typed python
=======================================================================

:author: Dan Connolly
:copyright: Dan Connolly and the University of Kansas Medical Center
:license: MIT


Usage
-----

Convert one module::

  $ python py2scala/p2s.py my_module.py >my_module.scala

:TODO: Test, document --package option.


Testing
-------

Tests are set up to run under nose__, assuming `fsc`, the fast scala__
compiler, is installed::

  py2scala$ nosetests
  ..................
  ../src/test/scala-err/distant_types.scala:23: error: value % is not a member of Any
    if ((x % 2) == 0) { 1 } else { "abc" }
           ^
  ./src/test/scala-err/distant_types.scala:31: error: type mismatch;
   found   : Any
   required: Seq[?]
      len(f1(x))
          ^
  two errors found
  .
  ----------------------------------------------------------------------
  Ran 20 tests in 5.243s

  OK

__ https://pypi.python.org/pypi/nose/
__ http://www.scala-lang.org/

Alternatively, to test that the conversion works (i.e. doesn't crash):

  py2scala$ python -m py2scala.test.test_convert

And to test compilation output with `fsc`:

  py2scala$ python -m py2scala.test.test_well_typed



Object Capability Discipline
----------------------------

This code follows* a `security and testing discipline`__ where
module-level code uses only authority passed to it by callers. Only
the top level script environment is trusted with the full authority
of the python standard library.

*mostly. There's an apparent conflict between object capability
discipine and test discovery that I haven't worked out.

__ http://www.madmode.com/2013/python-capability-idioms-part-1.html
