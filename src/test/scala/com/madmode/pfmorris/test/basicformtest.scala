package com.madmode.pfmorris.test

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class basicformtest extends FunSpec with ShouldMatchers {
  import com.madmode.pfmorris.basicforms
  import basicforms.{ CSym, PSym, Language, T, UBGParser }

  val leftparen = CSym(Symbol("<"))
  val one = CSym(Symbol("1"))
  val plus = CSym(Symbol("plus"))
  val rightparen = CSym(Symbol(">"))
  val sum = List(Left(leftparen), Right(T), Left(plus), Right(T), Left(rightparen))
  val one_t = List(Left(one))

  val simple = Language(
    Set(leftparen, one, plus, rightparen),
    { sym =>
      sym match {
        case _ if sym == leftparen => 3
        case _ => 1
      }
    },
    formulaSignatures = Set(),
    termSignatures = Set(sum, one_t))

  describe("one term") {
    val doc = """1"""
    it("should grok a simple term") {
      val p = new UBGParser(simple)

      val fr = p.parseAll(p.start, doc)
      (fr match {
        case p.Success(f, _) => f.toString()
        case x: p.Failure => x.toString()
        case x: p.Error => x.toString()
      }) should equal(
        "SigTerm(List(Const('1)))" )
    }
  }

  describe("Simple unification based grammar parser") {
    val doc = """< 1 plus 1 >"""
    it("should grok a simple expression") {
      val p = new UBGParser(simple)

      val fr = p.parseAll(p.start, doc)
      (fr match {
        case p.Success(f, _) => f.toString()
        case x: p.Failure => x.toString()
        case x: p.Error => x.toString()
      }) should equal(
        "SigTerm(List(Const('<), SigTerm(List(Const('1))), Const('plus), SigTerm(List(Const('1))), Const('>)))")
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
