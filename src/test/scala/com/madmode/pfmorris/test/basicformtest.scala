package com.madmode.pfmorris.test

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class basicformtest extends FunSpec with ShouldMatchers {
  import com.madmode.pfmorris.basicforms
  import basicforms.{ CSym, Language, T, UBGParser }

  val leftparen = CSym(Symbol("("))
  val one = CSym(Symbol("1"))
  val plus = CSym(Symbol("+"))
  val rightparen = CSym(Symbol(")"))
  val sum = List(Left(leftparen), Right(T), Left(plus), Right(T))

  val simple = Language(
    Set(leftparen, one, plus, rightparen),
    (sym => 1),
    formulaSignatures = Set(),
    termSignatures = Set(sum))

  describe("Simple unification based grammar parser") {
    val doc = """(1 + 1)"""
    it("should grok a simple expression") {
      val p = new UBGParser(simple)

      val fr = p.parseAll(p.start, doc)
      (fr match {
        case p.Success(f, _) => f.toString()
        case x: p.Failure => x.toString()
        case x: p.Error => x.toString()
      }) should equal(
        "@@")

    }

    /*@@
    it("should have a decent API") {
      val f = new NTriplesParser().toFormula(doc)

      (f.quote().print()) should equal(
        "(exists (_:somewhere) (and (holds (data:bob) (data:home) _:somewhere) (holds _:somewhere (data:in) (data:Texas))))")
    }
    * 
    */
  }
}
