package com.madmode.pfmorris

import scala.util.parsing.combinator.{ Parsers, RegexParsers }
import scala.annotation.tailrec
import scala.util.matching.Regex

/**
 * Uniﬁcation-Based Grammar
 * Bob Neveln
 * Bob Alps
 * January 16, 2011
 * Abstract
 * Context-free languages are easily parsed. Language used for the expression of mathematics needs to be unambiguous. A.P. Morse devised a
 * method for generating an essentially context-free mathematical language
 * which formalizes the common practice of using the deﬁnitions occurring
 * in a mathematical text as the basis for the mathematical language of that
 * text. Morse obtained the unambiguity and preﬁx-free properties of any
 * such language by placing syntactic constraints on the set of deﬁnienda.
 * Although eﬀective they lacked generality. We show here that greater generality can be achieved using a uniﬁcation-based constraint.
 *
 * 1 Introduction
 * One of Morse’s basic ideas about mathematical syntax was that any well
 * done set of mathematical deﬁnitions implicitly deﬁnes the grammar of a
 * formal language. Morse’s syntax is essentially a method for translating
 * a set of mathematical deﬁnitions into a grammar together with a set of
 * conventions about the way that bound variables work. The role of a
 * grammar is to distinguish the well-formed expressions of a language from
 * all other expressions. The determination of the scopes of bound variables
 * is an additional task, one for which these grammars do not suﬃce. In
 * Section 7 conventions for this purpose are established. It is also shown
 * there that the presence of bound variables can add a layer to the notion of
 * “unambiguous” which is not deﬁneable in terms of context-free grammars.
 */
object basicforms {
  /**
   * 2 The Symbols
   * Given that mathematical language is to be built using a set of (terminal)
   * symbols Σ which is a disjoint union Σ = C ∪ V ∪ P ∪ U of:
   * • C a set of constants
   * • V a set of object variables
   * • P a set of (second order) predicate variables
   * • U a set of (second order) function variables.
   * 1The elements of P ∪ U are also referred to as schemators. We further
   * assume that there is an arity function a : P ∪ U → N, which assigns a
   * natural number to each schemator. The arity of a schemator p may be
   * zero only if p ∈ P and in this case p is called a sentence variable.
   */
  sealed abstract class TerminalSymbol {
    def syntax: Regex
  }
  case class CSym(name: Symbol) extends TerminalSymbol {
    def syntax = name.name.r
  }
  case class VSym(syntax: Regex, name: Symbol) extends TerminalSymbol
  sealed abstract class Schemator extends TerminalSymbol
  case class PSym(syntax: Regex) extends Schemator
  case class USym(syntax: Regex) extends Schemator

  case class Language(
    symbols: Set[TerminalSymbol],
    a: TerminalSymbol => Int,
    formulaSignatures: Set[Signature],
    termSignatures: Set[Signature]) {

    require(symbols forall {
      case PSym(_) => true
      case u: USym => a(u) > 0
      case _ => true
    })

    def signatures = formulaSignatures union termSignatures
  }

  sealed abstract class Form {
    /**
     * A schemator of arity n followed by n object variables is called a schematic
     * expression.
     */
    def schematic_expression(): Option[(Boolean, List[Var])] = {
      def testAll(args: List[Term]): Option[List[Var]] = args match {
        case Nil => Some(Nil)
        case t :: ts => t match {
          case v: Var => testAll(ts) map { rest => v :: rest }
          case _ => None
        }
      }

      this match {
        case Pred(_, args) => testAll(args) map { vs => (true, vs) }
        case Fun(_, args) => testAll(args) map { vs => (false, vs) }
        case _ => None
      }
    }
  }
  sealed abstract class Formula extends Form
  case class SigFormula(fs: List[Form]) extends Formula
  sealed abstract class Term extends Form
  case class SigTerm(fs: List[Form]) extends Term
  case class Pred(p: Symbol, args: List[Term]) extends Formula
  case class Fun(u: Symbol, args: List[Term]) extends Term
  case class Var(v: Symbol) extends Term
  case class Const(c: Symbol) extends Term

  /**
   * 3 Grammars
   * We need a set N = {S, F, T, V } of four non-terminal symbols, S for
   * “start,” F for “formula,” T for “term,” and V for “variable”, a set of
   * formula signatures SF , a set of term signatures ST , such that the set of
   * signatures S = SF ∪ ST ⊂ (C ∪ {F, T, V })∗, and a set R consisting of the
   * following production rules:
   */
  sealed abstract class NonTerminal
  object F extends NonTerminal
  object T extends NonTerminal
  object V extends NonTerminal
  type SigItem = Either[CSym, NonTerminal]
  type Signature = List[SigItem]
  class UBGParser(l: Language) extends RegexParsers {

    /**
     * 1. S → F
     * 2. S → T
     */
    def start: Parser[Form] = formula | term

    /**
     * 3. F → f, for each f ∈ SF
     * 4. F → p T . . . T
     *          nterms
     *          for each p ∈ P and where the arity of p is n
     */
    def formula: Parser[Formula] = (log(formulaSignature)("formulaSignature")
      | log(p)("p") >> { case (s, a) => repN(a, term) ^^ { case args => Pred(s, args) } })

    def p = pickSym({ case ps: PSym => ps }, "no pred")

    def formulaSignature: Parser[Formula] = parseSigs(l.formulaSignatures, "no formula sigs") ^^ { case fs => SigFormula(fs) }

    /**
     * 5. T → t, for each t ∈ ST
     * 6. T → u T . . . T
     *          nterms
     *          for each u ∈ U and where the arity of u is n
     * 7. T → V
     * 8. V → v, for each v ∈ V
     */
    def term: Parser[Term] = (log(termSignature)("termSignature")
      | log(u)("u") >> { case (s, a) => repN(a, term) ^^ { case args => Fun(s, args) } }
      | log(v)("v"))
    def u: Parser[(Symbol, Int)] = pickSym({ case ps: USym => ps }, "no u")
    def v = pickSym({ case ps: VSym => ps }, "no variables in language's symbol set") ^^ { case (s, _) => Var(s) }
    def termSignature: Parser[Term] = parseSigs(l.termSignatures, "no term sigs") ^^ { case fs => SigTerm(fs) }

    def pickSym[T <: TerminalSymbol](filter: PartialFunction[TerminalSymbol, T], msg: String): Parser[(Symbol, Int)] = {
      val parsers = for {
        sym <- l.symbols collect filter
        a = l.a(sym)
        strparse = regex(sym.syntax)
      } yield (strparse ^^ { case s => (Symbol(s), a) })

      parsers.reduceOption((p1, p2) => p1 | p2).getOrElse(failure(msg))
    }

    def parseSigs(sigs: Iterable[Signature], msg: String): Parser[List[Form]] = {
      val parsers = sigs map sigParser
      parsers.reduceOption((p1, p2) => p1 | p2).getOrElse(failure(msg))

    }
    def sigParser(sig: Signature): Parser[List[Form]] = {
      sig match {
        case Nil => failure("empty signature")
        case x :: Nil => log(sig1Parser(x))("last sig:" + x) ^^ { case f => List(f) }
        case x :: ps => log(sig1Parser(x))("nth sig:" + x) ~ sigParser(ps) ^^ { case f1 ~ fs => f1 :: fs }
      }
    }
    def sig1Parser(s1: SigItem): Parser[Form] = s1 match {
      case Left(c) => log(regex(c.syntax))("const: " + c.syntax) ^^ { case c => Const(Symbol(c)) }
      case Right(F) => formula
      case Right(T) => term
      case Right(V) => v
    }
  }

  /*   
   * Since without loss of generality we may assume that (V 6∈ ST ) we see
   * that the number of rules in R, #R = #SF + #P + #ST + #U + 3.
   * The grammar (N,Σ, R, S) generates the language. The formulas of the
   * language are generated by the grammar (N,Σ, R, F) and the terms by
   * (N,Σ, R, T).1 Notice that the schemators do not occur in signatures.
   * Further, as will be seen later V occurs in signatures only to mark index
   * variable locations. In section 8 it will be shown how the set of signatures
   * S is obtained from the deﬁnitions and primitive terms and formulas of the
   * language.
   */

  /**
   * For each signature, b with n occurrences of the non-terminals F,T,
   * and V we may form a term or formula by replacing these non-terminals
   * by formulas, terms, and variables. We may represent the resulting term
   * or formula by b(f1, . . . , fn) where each fi is a formula, term, or variable
   * replacing the i-th occurrence in b of a non-terminal F,T, or V respectively.
   */
  def subst(b: Signature, forms: List[Form]): Option[List[Form]] = {
    (b, forms) match {
      case (Nil, Nil) => Some(Nil)
      case (Left(c) :: ss, fs) => subst(ss, fs) map (Const(c.name) :: _)
      case (Right(F) :: ss, (f: Formula) :: fs) => subst(ss, fs) map (f :: _)
      case (Right(T) :: ss, (t: Term) :: fs) => subst(ss, fs) map (t :: _)
      case (Right(V) :: ss, (v: Var) :: fs) => subst(ss, fs) map (v :: _)
      case _ => None // consider Left(b.length) to give location of mismatch
    }
  }

  /* We note that constants can appear at the beginning of terms and formulas
   * only by virtue of the rules in 5 and 3 above.
   * Theorem If t is any term or formula beginning with a constant then t
   * can be represented as b(f1, . . . , fn) for some signature b and components
   * fi.
   */
  /* 1Morse’s syntax made no distinction between terms and formulas. To describe his grammars using this formalism, we should do away with the non-terminal symbols S and F, delete
   * the rules in 1 through 4 and make T the initial symbol.
   */

  /* 24 Two Desirable Properties
   * When a language is generated from such a grammar the distinguishing
   * features of the language result from the kinds of expressions which constitute S. For example a preﬁx (or Polish) style of syntax results if each
   * signature b ∈ S begins with a unique constant. Among the desirable
   * properties of a language with a preﬁx grammar are that it is:
   * 1. Unambiguous. Each term or formula beginning with a constant
   * stems from a unique signature.
   * 2. Preﬁx-Free. No term or formula of the language begins another.
   * When a language has a grammar of the type given in section 3 the
   * property of being unambiguous, which is ordinarily stated in terms of the
   * uniqueness of a leftmost derivation, can be stated as follows:
   */

  /**
   * Deﬁnition of Unambiguous If b(f1, . . . , fn) is c(g1, . . . , gm) then b is c,
   * (m = n), and fi is gi for i = 1, . . . , n.
   */
  def unambiguous(g: UBGParser)(b: Signature, fi: List[Form], c: Signature, gi: List[Form]) = {
    if (subst(b, fi) == subst(c, gi)) {
      b == c && fi == gi
    } else {
      true
    }
  }

  /* The preﬁx-free property can be stated as follows:
   * Deﬁnition of Preﬁx-Free If b(f1, . . . , fn) is an initial segment of c(g1, . . . , gm)
   * then b(f1, . . . , fn) is c(g1, . . . , gm).
   */
  def prefix_free(g: UBGParser)(b: Signature, fi: List[Form], c: Signature, gi: List[Form]) = {
    val tryb = subst(b, fi)
    val tryc = subst(c, gi)
    (tryb, tryc) match {
      case (Some(bfs), Some(cfs)) => if (cfs startsWith bfs) {
        cfs == bfs
      } else {
        true
      }
      case _ => true // hmm...
    }
  }

  /**
   * In [3], Morse obtained conditions on an arbitrary set of signatures
   * suﬃcient to guarantee that the resulting language would have these two
   * properties.2 We seek conditions which are necessary as well as suﬃcient.
   * 5 Conditions on the Set of Signatures
   * We have that all signatures begin with some constant. Conversely we
   * deﬁne an introductor as a constant that occurs as the initial symbol of
   * some signature.
   */
  def introductor(c: CSym, l: Language) = l.signatures exists (_.head == Left(c))

  /* The following two conditions were considered by Morse:
   * 1. No signature may be the initial segment of some other signature, nor
   * is this allowed if the V ’s in the signatures are changed to T’s.
   * 2. No introductor may occur in a signature except as the initial symbol.
   * To get the two desired properties of section 4 the ﬁrst of these two
   * conditions is clearly necessary. The second however is so strong that
   * it rules out the standard absolute value notation. Morse modiﬁed this
   * condition in an ad hoc way to accommodate the absolute value sign using
   * a notion of ﬂanker. The resulting condition, although weaker, still rules
   * out some rather standard usages such as that of parentheses in the power
   * set notation P(X) where, because it is an introductor, the left parenthesis
   * is disallowed from the position it occupies in this notation. More in spirit
   * 
   * 2Morse’s constraints are given on pages 154-155 of [4] and pages 113-114 of [3]. The
   * suﬃciency of these constraints is shown in [5].
   * 3
   * 
   * with the rest of Morse’s work would be the determination of conditions
   * only as strong as necessary.
   * Given two signatures b and c, we seek to determine whether there
   * are f1, . . . , fn and g1, . . . , gm so that b(f1, . . . , fn) is an initial segment
   * of c(g1, . . . , gm). If so we say that b preﬁx-uniﬁes with c. Almost by
   * deﬁnition then we have the following theorem.
   * 
   * Theorem If no signature preﬁx-uniﬁes with another signature then the
   * resulting language is unambiguous and preﬁx-free.
   * 
   * The value of the preﬁx-uniﬁcation concept depends on its eﬀective
   * determinacy.
   * 6 Preﬁx-Uniﬁcation Algorithm
   * We begin by deﬁning concepts that are used in the algorithm.
   * For any expression s of length n, we use s[k] to denote the k-th symbol
   * of s, where 1 ≤ k ≤ n.
   */

  /**
   * A signature-list is a list [b, m1, . . . , mk] whose ﬁrst element is a signature and such that if the length of b is n there are k additional entries in
   * the list where 1 ≤ k ≤ n. If k = n, we say the signature-list is complete;
   * otherwise it is incomplete.
   */
  case class SignatureList(b: Signature, m1mk: List[Either[SignatureList, SigItem]]) {
    def k = m1mk.length
    def n = b.length
    require(1 <= k && k <= n)

    def complete = k == n
    def incomplete = !complete
  }

  /**
   * We recursively deﬁne a signature match as a signature-list
   * [b, m1, . . . , mk], where for each i ∈ {1, . . . , k}:
   * 1. if b[i] = V or b[i] ∈ C then mi is b[i],
   * 2. if b[i] = T then mi is either V or T or a signature match that is a
   * complete signature-list with a term signature as the ﬁrst item of the
   * list, and
   * 3. if b[i] = F then mi is either F or a signature match that is a complete
   * signature-list with a formula signature as the ﬁrst item of the list.
   * A complete signature match is a signature match that is a complete
   * signature-list. An incomplete signature match is a signature match that
   * is an incomplete signature-list.
   */
  def signature_match(l: Language, b_m1mk: SignatureList, complete_opt: Option[Boolean]): Boolean = {
    with_signature_match(l, b_m1mk) {
      (complete, _) => Some(complete_opt.fold(true)(complete == _))
    } getOrElse false
  }
  /* TODO: consider moving signature_match etc. to methods on Language */
  /* TODO: consider changing f(SignatureList, ...) : Option[X] to PartialFunction (again) */

  def with_signature_match[T](l: Language, b_m1mk: SignatureList)(f: (Boolean, SignatureList) => Option[T]): Option[T] = {
    def match_with_first_in(mi: SignatureList, target: Set[Signature]): Option[Boolean] = {
      with_signature_match(l, mi) { (_, mi) =>
        if (target.contains(mi.b)) { Some(true) } else { None }
      }
    }

    def each_i(bi: SigItem, mi: Either[SignatureList, SigItem]): Option[Boolean] = bi match {
      case Right(V) => mi match {
        case Right(Right(V)) => Some(true)
        case _ => Some(false)
      }
      case Left(bc) => mi match {
        case Right(Left(mic)) => Some(bc == mic)
        case _ => Some(false)
      }
      case Right(T) => mi match {
        case Right(Right(V)) | Right(Right(T)) => Some(true)
        case Right(_) => Some(false)
        case Left(misl) => match_with_first_in(misl, l.termSignatures)
      }
      case Right(F) => mi match {
        case Right(Right(F)) => Some(true)
        case Right(_) => Some(false)
        case Left(misl) => match_with_first_in(misl, l.formulaSignatures)
      }
    }

    val allok = (b_m1mk.b zip b_m1mk.m1mk) forall { case (bi, mi) => each_i(bi, mi) getOrElse false }
    if (allok) { f(b_m1mk.complete, b_m1mk) } else { None }
  }

  type UnificationList = List[SignatureList]

  /**
   * A partial-uniﬁcation list is a list of signature matches each of which,
   * except for possibly the last, is incomplete.
   */
  def partial_unification(l: Language, sigs: UnificationList): Option[UnificationList] = {
    signature_matches(l, sigs) match {
      case None => None
      case Some(sigs) => if (sigs.init forall (_.complete)) { Some(sigs) } else { None }
    }
  }

  /**
   * A partial-uniﬁcation list is reduced if its ﬁnal signature match is incomplete or if its length is 1.
   */
  def reduced(l: Language, sigs: UnificationList): Option[UnificationList] = {
    signature_matches(l, sigs) match {
      case None => None
      case Some(sigs) => if (sigs.length == 1 || sigs.last.incomplete) { Some(sigs) } else { None }
    }
  }

  def signature_matches(l: Language, sigs: UnificationList): Option[UnificationList] = {
    if (sigs forall { sig => signature_match(l, sig, Some(false)) }) { Some(sigs) } else { None }
  }

  /**
   * The reduction of a partial-uniﬁcation list A is the result of applying
   * the following algorithm to A:
   * Repeat:
   * If A is reduced, then halt the algorithm and return A.
   * Otherwise, let the last two signature matches ofAbe [s, m1, . . . , mj]
   * and M where M is a complete signature match, and remove
   * both matches from the list A and append the signature match
   * [s, m1, . . . , mj, M] to A.
   */
  def reduction(l: Language, A: UnificationList): Option[UnificationList] = {
    partial_unification(l, A) match {
      case None => None
      case Some(_) => reduced(l, A) match {
        case Some(halt) => Some(halt)
        case None => {
          val last_two = (A(A.length - 2), A(A.length - 1))
          val (SignatureList(s, m1mj), bigm) = last_two
          assert(bigm.incomplete)
          Some((A filterNot (sig => sig == SignatureList(s, m1mj) || sig == bigm)) :+ bigm)
        }
      }
    }
  }

  /**
   * A partial uniﬁcation is an ordered pair (A, B) of partial-uniﬁcation
   * lists.
   */
  type UnificationListPair = (UnificationList, UnificationList)
  def partial_unification_pair(l: Language, A_B: UnificationListPair) = {
    !partial_unification(l, A_B._1).isEmpty && !partial_unification(l, A_B._2).isEmpty
  }

  /* 4
   */
  /**
   * In the following preﬁx-uniﬁcation algorithm a set X of partial uniﬁcations is maintained.
   * X changes as the algorithm proceeds. If X becomes
   * empty, then the algorithm has shown that no uniﬁcation exists and it
   * returns 0. If a preﬁx-uniﬁcation is found then 1 is returned.
   * Using the above deﬁnitions, the algorithm may now be described.
   * The input to the algorithm consists of two signatures b and c having
   * the same initial symbol α0.
   */
  def unification_exists(l: Language, b: Signature, c: Signature, a0: CSym): Boolean = {
    /* Let A0 = [[b, α0]].
     * Let B0 = [[c, α0]].
     * Let X = {(A0, B0)}. 
     */
    val A0 = List(SignatureList(b, List(Right(Left(a0)))))
    val B0 = List(SignatureList(c, List(Right(Left(a0)))))
    val X: Set[UnificationListPair] = Set((A0, B0))

    /* Repeat: */
    def repeat(X: Set[UnificationListPair], Y: Set[UnificationListPair]): Boolean = {
      /* If X = ∅ halt the algorithm and return 0. */
      if (X.isEmpty) { false }
      else {
        /* Let Y = ∅.
         * For each partial uniﬁcation (A, B) ∈ X,
         * Let A' be the reduction of A.
         * Let B' be the reduction of B.
         * If either A' or B'
         * ends with a complete signature
         * match then halt the algorithm and return 1.
         * Otherwise add the pair (A' , B') to Y . 
         */
        val Y = for {
          (bigA, bigB) <- X
          ap <- reduction(l, bigA)
          bp <- reduction(l, bigB)
        } yield (ap, bp)
        if (Y exists { case (ap, bp) => ap.last.complete || bp.last.complete }) { true }
        else {
          /* Let X = Y .
           * Let Y = ∅.
           */
          val nextX = Y
          /* For each partial uniﬁcation (A, B) in X,
             */
          val nextY = for {
            (a, b) <- X
            more <- addToY(a, b)
          } yield more
          repeat(nextX, nextY)
        }
      }
    }

    def addToY(a: UnificationList, b: UnificationList): Iterable[UnificationListPair] = {
      /* Let [s, `1, . . . , `j] be the ﬁnal signature match of A.
       * Let [t, m1, . . . , mk] be the ﬁnal signature match of B.
       * Let α = s[j + 1].
       * Let β = t[k + 1].
       */
      val (s, l1lj, j) = (a.last.b, a.last.m1mk, a.last.k)
      val (t, m1mk, k) = (b.last.b, b.last.m1mk, b.last.k)
      val alpha = s(j + 1)
      val beta = t(k + 1)

      (alpha, beta) match {
        /* Since every symbol of a signature is one of V, T, F, or
         * a constant, we can cover all possibilities for α and β
         * in 16 cases. Note that the ﬁrst two numbered steps
         * below cover 4 of the 16 cases.
         */

        /* 1. If α = β then:
         * Let A' be A with the ﬁnal signature match of
         * A, [s, `1, . . . , `j], replaced by [s, `1, . . . , `j, α].
         * Let B' be B with its ﬁnal signature match
         * [t, m1, . . . , mk] replaced by [t, m1, . . . , mk, β].
         * Add the pair (A', B') to the set Y .
         */
        case _ if alpha == beta => {
          val app = a.init :+ SignatureList(s, l1lj :+ Right(alpha))
          val bpp = b.init :+ SignatureList(t, m1mk :+ Right(beta))
          Some((app, bpp))
        }

        /* 2. If α, β,∈ C and α 6= β then make no change to Y
         * since no extension is possible.
         */
        case (Left(CSym(_)), Left(CSym(_))) => None

        /* 3. If α = T and β = F or α = F and β = T then
         * make no change to Y since no extension is possible.
         */
        case (Right(T), Right(F)) | (Right(F), Right(T)) => None

        /* 4. If α = V and β ∈ {F} ∪ C or β = V and
         * α ∈ {F} ∪ C then make no change to Y since
         * no extension is possible.
         */
        case (Right(V), Right(F)) | (Right(V), Left(_)) => None
        case (Right(F), Right(V)) | (Left(_), Right(V)) => None

        /* 5. If α = T and β = V or α = V and β = T then:
         *   pg 5
         * Let A' be A with the ﬁnal signature match of
         * A, [s, `1, . . . , `j], replaced by [s, `1, . . . , `j, V ].
         * Let B' be B with its ﬁnal signature match
         * [t, m1, . . . , mk] replaced by [t, m1, . . . , mk, V ].
         * Add the pair (A', B') to the set Y .
         */
        case (Right(T), Right(V)) | (Right(V), Right(T)) => {
          val ap = a.init :+ SignatureList(s, l1lj :+ Right(Right(V)))
          val bp = b.init :+ SignatureList(t, m1mk :+ Right(Right(V)))
          Some((ap, bp))
        }
        /* 6. If α = T and β ∈ C then for each term signature
         * u ∈ ST :
         * If the initial symbol of u is β:
         * Let A' be A with [u, β] appended.
         * Let B' be B with the ﬁnal signature match
         * replaced by [t, m1, . . . , mk, β].
         * Add the pair (A', B0) to the set Y .
         */
        case (Right(T), Left(c)) => {
          for {
            u <- l.termSignatures if u(0) == c
            ap = a :+ SignatureList(u, List(Right(Left(c))))
            bp = b.init :+ SignatureList(t, m1mk :+ Right(beta))
          } yield (ap, bp)
        }
        /* 7. If α = F and β ∈ C then for each formula signature u ∈ SF :
         * If the initial symbol of u is β:
         * Let A' be A with [u, β] appended.
         * Let B' be B with the ﬁnal signature match
         * replaced by [t, m1, . . . , mk, β].
         * Add the pair (A', B') to the set Y .
         */
        case (Right(F), Left(c)) => {
          for {
            u <- l.formulaSignatures if u(0) == c
            ap = a :+ SignatureList(u, List(Right(beta)))
            bp = b.init :+ SignatureList(t, m1mk :+ Right(beta))
          } yield (ap, bp)
        }

        /* 8. If β = T and α ∈ C then for each term signature
         * u ∈ ST :
         * If the initial symbol of u is α:
         * Let B' be B with [u, α] appended.
         * Let A' be A with the ﬁnal signature match
         * replaced by [s, `1, . . . , `k, α].  @@@typo?
         * Add the pair (A', B') to the set Y .
         */
        case (Left(c), Right(T)) => {
          for {
            u <- l.termSignatures if u(0) == c
            bp = b :+ SignatureList(u, List(Right(alpha)))
            ap = b.init :+ SignatureList(s, l1lj :+ Right(alpha))
          } yield (ap, bp)
        }

        /* 9. If β = F and α ∈ C then for each formula signature u ∈ SF :
         * If the initial symbol of u is α:
         * Let B' be B with [u, α] appended.
         * Let A' be A with the ﬁnal signature match
         * replaced by [s, `1, . . . , `k, α].  @@@typo
         * Add the pair (A', B') to the set Y .
         * Let X = Y .
         */
        case (Left(c), Right(F)) => {
          for {
            u <- l.formulaSignatures if u(0) == c
            bp = b :+ SignatureList(u, List(Right(alpha)))
            ap = a.init :+ SignatureList(s, l1lj :+ Right(beta))
          } yield (ap, bp)
        }
      }
    }
    repeat(X, Set())
  }

  /* Clearly this algorithm terminates only under favorable conditions. We
   * seek enhancements to this algorithm, checking for repeated states, which
   * will allow more partial uniﬁcations to be discarded.
   * 
   * 7 Bound Variables
   * Bound variables cause a problem in creating context free grammars for
   * Morse languages because Morse’s rules require that all the bound variables
   * of a deﬁniendum be distinct. We must ﬁrst note in passing that deﬁnienda
   * (as opposed to composite expressions) with more than a single bound
   * variable are not that common in mathematics. For example, mathematics
   * 6actually carried out in ﬁrst order logic has none at all since the only bound
   * variables used in a strict ﬁrst order language occur in quantiﬁcation and
   * these have single not multiple occurrences. Nonetheless to illustrate the
   * point suppose the following expression is used as a deﬁniendum for a triple
   * sum: ‘SIGMA x, y, z ∈ A x B x Cu'' xyz’ In this expression ‘x’, ‘y’, and ‘z’ are
   * bound. So according to Morse’s treatment the expressions
   * • ‘SIGMA t, t, z ∈ AxB x Cu''ttz’
   * • ‘SIGMA t, y, t ∈ AxB x Cu''tyt’
   * • ‘SIGMA x, t, t ∈ AxB x Cu''xtt’
   * • ‘SIGMAt, t, t ∈ AxB x Cu''ttt’
   * cannot be deﬁned, whereas if ‘SIGMA V, V, V ∈ T x T x T T’ is the signature
   * corresponding to the original triple sum deﬁniendum then these other
   * expressions must be deﬁned since each occurrence of V is replaced independently in a context-free grammar. We resolve this diﬃculty by requiring that these additional forms receive deﬁnitions.3 Because of the
   * way that bound variable replacement is deﬁned these additional forms
   * are completely independent of each other and of the original so that the
   * mathematician is free to deﬁne them in whatever way is convenient. Of
   * the deﬁnienda corresponding to a signature, exactly one will have distinct
   * index variables. We shall call this the principal deﬁniendum.
   * For principal deﬁnienda we require that:
   * 1. All the schemators in the deﬁniendum occur just once.
   * 2. Each schemator occurs as the initial symbol of a schematic expression.
   * 3. All the variables which occur in some schematic expression occur
   * exactly once not in a schematic expression and these occurrences
   * correspond to occurrences of V in the signature. These variables we
   * refer to as the index variables.
   * 4. All the remaining variables occur just once.
   * We further require that deﬁnienda which are not principal be obtained
   * from the principal deﬁniendum by substituting some index variable for
   * other index variables.
   * Each of the four conditions above is included in Morse’s rules. His
   * version of the third condition was stronger in that he required that each
   * bound variable occur in each schematic expression. Our weakened condition has notable consequences, as noted below.
   * The problem of deﬁning the scope of each bound variable remains. In
   * standard ﬁrst order logic the scope of a bound variable is simply the entire
   * form in which it occurs. As noted by Morse actual mathematical practice
   * is not always this simple. For example formulas such as
   * Z x
   * 0
   * xdx =
   * Z x
   * 0
   * ydy
   * 3
   * If there are n occurrences of a bound variable then the number of these forms is B(n),
   * the Bell number. The ﬁrst few Bell numbers are: B(1) = 1, B(2) = 2, B(3) = 5, B(4) = 15.
   * 7are often accepted without question. Morse accommodated this practice
   * by localizing the scope of a bound variable so that it did not necessarily
   * include the entire form. We proceed to localize it further.
   * The scope of each bound variable of the deﬁniendum is by convention
   * the set of schematic expressions in which that variable occurs.
   * Applying this convention to the triple sum deﬁniendum we see that all
   * the index variables occur in a single schematic expression. This expression
   * therefore constitutes their shared scope. The variables ‘A’, ‘B’, and ‘C’
   * are not included in the scope. Consequently we may infer that Xx, y, z ∈ x x y x zu
   * 00
   * xyz ≡
   * Xu, v, w ∈ x x y x zu
   * 00
   * uvw
   * from Xu, v, w ∈ x x y x zu
   * 00
   * uvw ≡
   * Xu, v, w ∈ x x y x zu
   * 00
   * uvw
   * simply by virtue of indicial variable substitution.
   * The rule of inference for indicial variable substitution may be stated
   * as follows:
   * Indicial Variable Substitution Rule
   * If A is a constituent of a theorem T and α is an index variable of A,
   * and β is a variable which does not occur in the scope of α in A and B
   * is obtained from A by replacing α by β and T
   * 0
   * is obtained from T by
   * replacing A by B then T
   * 0
   * is a theorem.
   * Incidentally this rule provides a diﬀerent solution to the following problem raised by Morse: 4 Suppose that ‘(V
   * ∗xyuxvy ≡
   * V
   * x
   * V
   * y(ux→vy))’
   * were allowed as a deﬁnition. In this case the formula ‘V
   * ∗xyu
   * 0xyv
   * 0xy’
   * could not be translated using that deﬁnition. However according to the
   * rule just given this formula translates as follows: ‘V
   * s
   * V
   * t(u0
   * sy→v
   * 0xt)’
   * where ‘x’ and ‘y’ are free variables in the translated formula. Using this
   * rule the need for Morse’s rule A.9 disappears. The argument however is
   * important in that it justiﬁes all four of the rules stated in section 7.
   */

  /**
   * 8 From Basic Forms to Signatures
   * In section 1 Morse’s syntax was described as a method of obtaining a
   * grammar from a set of deﬁnitions. In section 3 the process of deriving a
   * grammar from the set of signatures was described. We now describe how
   * the set of signatures is obtained from a set of deﬁnitions.
   * We begin with the left sides of all deﬁnitions. These we refer to as
   * deﬁnienda. To these we add all primitive or undeﬁned terms and formulas.
   * The resulting set we call the set of basic forms.
   */
  /**
   * The indicial variables of a
   * basic form are the variables occurring in some schematic expression of the
   * form.
   */
  def indical_variables(f: Form): List[Var] = {
    f.schematic_expression match {
      case Some((is_fmla, vs)) => vs
      case None => f match {
        case SigFormula(fs) => fs flatMap indical_variables
        case SigTerm(fs) => fs flatMap indical_variables
        case Pred(_, args) => args flatMap indical_variables
        case Fun(_, args) => args flatMap indical_variables
        case Var(_) | Const(_) => List()
      }
    }
  }

  /**
   * To obtain the signature of a basic form we replace each schematic
   * expression by T or F depending on whether its initial symbol is in U or
   * P. Of the remaining variables the indicial variables are replaced by V and
   * the others by T. The result is the signature.
   */
  def toSignature(f: Form): Signature = {
    val ivs = indical_variables(f)
    f.schematic_expression match {
      case Some((is_fmla, vs)) => List(Right(if (is_fmla) { T } else { F }))
      case None => f match {
        case SigFormula(fs) => fs flatMap toSignature
        case SigTerm(fs) => fs flatMap toSignature
        case Pred(_, args) => args flatMap toSignature
        case Fun(_, args) => args flatMap toSignature
        case v: Var => List(Right(if (ivs contains v) { V } else { T }))
        case Const(c) => List(Left(CSym(c)))
      }
    }
  }

  /* 4See page 161 of [4] or page 119 or [3].
   * 89 Conclusion
   * A.P. Morse devised rules for generating an essentially context-free dynamic mathematical language, permitting the language to expand, according to common practice, through the addition of deﬁnitions. In order
   * to obtain the unambiguity and preﬁx properties of such a language, Morse
   * included rules that placed syntactic constraints on the set of deﬁnienda.
   * These constraints involved restrictions on the use of constant symbols
   * which were used as the ﬁrst symbol of some deﬁniendum. Although eﬀective, they lacked generality. We show here that greater generality can be
   * achieved using a uniﬁcation-based constraint.
   * References
   * [1] R.A. Alps. A Translation Algorithm for Morse Systems, PhD dissertation, Northwestern University, 1979.
   * [2] R.A. Alps and R.C. Neveln, A Predicate Logic Based on Indeﬁnite Description and Two Notions of Identity. Notre Dame Journal of Formal
   * Logic 22(3) 1981.
   * [3] A.P. Morse. A Theory of Sets Academic Press 1965.
   * [4] A.P. Morse. A Theory of Sets Second Edition. Academic Press 1984.
   * [5] R.C. Neveln. Basic Theory of Morse Languages, PhD dissertation,
   * Northwestern University, 1975.
   * [6] Bob Neveln and Bob Alps. Writing and Checking Complete Proofs in
   * TEX PracJourn 1- 2007.
   * 9
   */

}
