package com.madmode.pfmorris

import com.madmode.py2scala.batteries._
import com.madmode.py2scala.__builtin__._
import scala.collection.mutable

/**
 * ##############################################################
 *
 *            Code for various parsing applications including
 *            code for parsing one term or formula
 *
 * *###############################################################
 */
object synt {

  /**
   * Each parse begins with a header.  Each header begins with a
   * tag which is one of the following numbers.
   */
  sealed abstract class Parse {
    def complete: Boolean
  }

  object Tag extends Enumeration {
    def topLevel(tag: Tag) = tag >= t1 && tag <= t23
    def complete(tag: Tag) = tag >= t1 && tag <= t51
    def incomplete(tag: Tag) = tag <= tm1

    type Tag = Value

    val // Tags which may appear only at the beginning of an incomplete parse:
    //
    // -1. Non-Parenthetical Expression  -7. Formula Quantifying Form
    // -2. Parenthetical Expression      -8. Scope     
    // -3. Undefined Expression          -9. Parade
    // -4. Schematic Term               -10. 
    // -5. Schematic Formula            -11. New Definition     
    // -6. Term Quantifying Form        -12. Undefined Parade 
    tm12, tm11, tm9, tm8, tm7, tm6, tm5, tm4, tm3, tm2, tm1, // Tags which appear only at the top level of a complete parse:
    //
    //  1. Term Definor                 16. Left Parenthesis        
    //  2. Formula Definor              17. Introductor      
    //  3. Connector                    18. Decimal Numeration
    //  4.                              19. Left Scope Bracket 
    //  5. Unrecognized constant        20. Right Scope Bracket   
    //  6. Right Parenthesis            21. Colon 
    //  7. Other known constant         22. Semi-colon
    //  8. Term Seeking Notarian        23. Ignore Token  (Not parsed)
    //  9. Formula Seeking Notarian 
    // 10. Variable
    // 11. Sentence variable, 0-ary schemator
    // 12. Function symbol, n-ary schemator n > 0
    // 13. Predicate symbol, n-ary schemator n > 0
    // 14. Noun             (Length 1 constant term) 
    // 15. Boolean Constant (Length 1 constant formula)
    t1, t2, t3, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, //
    // Tags which may appear at any level of a complete parse:
    //
    //  40. Term       (Not length 1) 
    //  41. Formula    (Not length 1) 
    //  42. Schematic Term        
    //  43. Schematic Formula    (Not zero-ary) 
    //  44. Parade Term
    //  45. Parade Formula
    //  47. Newly  Defined Form
    //  48. Scope 
    //  49. Undefined Expression (2nd level only)  
    //  50. Undefined Parade (3rd level only)  
    //  51. New  Definition
    t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50, t51 = Value

    //###############################################################
  }
  import Tag._
  implicit def test_tag_opt(tag_opt: Option[Tag]): Boolean = !tag_opt.isEmpty

  //@@import sys
  //@@import re

  //@@ import pattern

  /**
   * This database is loaded from the dfs file.  It is modified by parse and mathparse
   *   but not by check.  It is initialized by resetdefs and resetprops.
   */
  var mathdb: MD = null
  case class MD(MD_SYMTYPE: SYMTYPE, MD_PRECED: PRECED, MD_DEFS: DEFS,
    MD_ARITY: ARITY, MD_TROPS: TROPS, MD_TRMUL: TRMUL, MD_CAOPS: CAOPS,
    MD_THMS: THMS, MD_REFD: REFD, MD_MACR: MACR, MD_RSFLG: RSFLG,
    var MD_PFILE: PFILE, var MD_RFILE: RFILE) {
    def dump(fp: File): Unit = throw new Exception("TODO")
  }
  object MD {
    def load(fp: File): MD = throw new Exception("TODO")
  }
  /** Dictionary storing the type of each symbol */
  type SYMTYPE = Dict[String, Tag]
  /** Dictionary storing the precedence of each connector */
  type PRECED = Dict[String, Int]
  /** Dictionary storing a list of definienda for each introductor */
  type DEFS = Dict[String, String]
  /** Dictionary storing the arity of each schemator  */
  type ARITY = Dict[String, Int]
  /** List of Transitive operators */
  type TROPS = List[String]
  /** Dictionary mapping 2-tuples of transitive ops to a transitive op */
  type TRMUL = Dict[(String, String), String]
  /** List of commutative associative ops */
  type CAOPS = List[String]
  /** List of all theorems from some properties file */
  type THMS = List[String]
  /** Dictionary of external file reference definitions  */
  type REFD = Dict[String, String]
  /** Dictionary of user macro definitions */
  type MACR = Dict[String, String]
  /** Boolean reset definitions flag */
  type RSFLG = Boolean
  /** Name of properties file */
  type PFILE = String
  /** Name of rules of inference file */
  type RFILE = String
  /** Length of mathdb */
  val MD_LEN = 13
  //
  // The following fast moving variable keeps track of which numbers have been
  // used already as variable tags.   
  //
  val newvarnum = 0
  //The following are tokens added so that TeX spacing characters can be used.
  val ignore_tokens = List("""\,""", """\>""", """\;""", """\!""")
  val reference_punctuator_list = List(",", ";", "<=", """\C""", "G", "H", "!", "D", "P", "A", "S", "U", ")", "(", "),", ",(", ";(", "+", "-", """\char124""", ":", "|")

  def makemathdb(): MD = {
    val td: SYMTYPE = new Dict()
    val precedence: PRECED = new Dict()
    val defs: DEFS = new Dict()
    val arity: ARITY = new Dict()
    td("""\dft""") = t1
    td("""\dff""") = t2
    td(")") = t6
    td("(") = t16
    td("""\ls""") = t19
    td("""\rs""") = t20
    td(":") = t21
    td(";") = t22
    td("""\}""") = t7
    td("""\false""") = t15
    td("""\true""") = t15
    td("""\Nul""") = t14
    td("0") = t14
    td("1") = t14
    td("2") = t14
    td("3") = t14
    td("4") = t14
    td("5") = t14
    td("6") = t14
    td("7") = t14
    td("8") = t14
    td("9") = t14
    td("""\ten""") = t14
    //##############################
    //  
    //   Connector Symbols
    //
    //############################## 
    //
    //  The number introducing each connector list is the
    //  precedence value.  
    //
    //##############################
    val connectors = List(
      (1, List("""\case""")),
      (2, List("""\c""")),
      (3, List("""\cond""", """\els""")),
      (4, List("""\Iff""")),
      (5, List("""\And""", """\Or""")),
      (6, List("=", """\ne""", """\le""", """\ge""", """\notin""",
        """\noti""", """\ident""", """\in""", "<", ">",
        """\i""", """\j""", """\subset""", ",")),
      (9, List("+", "-")),
      (13, List("""\cdot""", "/")))
    for (x <- connectors) {
      val prec = x._1
      val plist = x._2
      for (s <- plist) {
        precedence(s) = prec
        td(s) = t3
      }
    }
    td("""\ident""") = t1
    td("""\Iff""") = t2
    precedence("""\dff""") = 4
    precedence("""\dft""") = 6
    //######################################################
    //
    //  Initialize Transitive Properties
    //
    //######################################################
    val transitive_ops = List("""\ident""", "=", """\Iff""")
    val trans_mult: TRMUL = new Dict()
    //######################################################
    //
    //  Initialize Commutative Associative Properties
    //
    //######################################################
    val commutative_associative_ops = List()
    val theorems = List()
    val reference_dictionary: REFD = new Dict()
    reference_dictionary("0") = "common.tex"
    val user_dictionary: MACR = new Dict()
    val reset_flag = True
    val properties_file = "properties.tex"
    val rules_file = "rules.tex"
    return MD(td, precedence, defs, arity, transitive_ops, trans_mult,
      commutative_associative_ops, theorems, reference_dictionary,
      user_dictionary, reset_flag, properties_file, rules_file)
  }

  def dbmerge(mathdb: MD, db: MD): Any = {
    mathdb.MD_SYMTYPE.update(db.MD_SYMTYPE)
    mathdb.MD_PRECED.update(db.MD_PRECED)
    mathdb.MD_DEFS.update(db.MD_DEFS)
    mathdb.MD_ARITY.update(db.MD_ARITY)
    if (db.MD_REFD != null) {
      mathdb.MD_REFD.update(db.MD_REFD)
    } else {
      println("Warning: format does not match.")
    }
    if (db.MD_MACR != null) {
      mathdb.MD_MACR.update(db.MD_MACR)
    } else {
      println("Warning: format does not match.")
    }
  }

  def symtype(token: String): Option[Tag] = {
    val td = mathdb.MD_SYMTYPE
    val arity = mathdb.MD_ARITY
    assert(token != null)
    def save(t: Tag) = { td(token) = t; Some(t) }
    val retval = td.get(token)
    retval match {
      case Some(_) => retval
      case None => token match {
        case pattern.ignore_token => save(t23)
        case pattern.TeX_leftdelimiter => Some(t6)
        case pattern.TeX_rightdelimiter => Some(t16)
        case _ => validschemator(token) match {
          case Right((t, a)) => {
            arity(token) = a
            save(t)
          }
          case _ => {
            if (validvar(token)) {
              save(t10)
            } else {
              if (validnum(token)) {
                save(t18)
              } else {
                save(t5)
              }
            }
          }
        }
      }
    }
  }

  type SchematorInfo = Either[Int, (Tag, Int)]
  implicit def test_either(x: SchematorInfo): Boolean = x.isRight

  def validschemator(token: String): SchematorInfo = {
    implicit def win(tag_arity: (Tag, Int)) = Right((tag_arity._1, tag_arity._2))
    implicit def lose(x: Int) = Left(x)
    val schemm = pattern.newschem.match_(token)
    if (schemm) {
      val arity = int(schemm.group(1))
      if (token(1) == 'w') {
        return (t12, arity)
      } else {
        if (arity == 0) {
          return (t11, arity)
        } else {
          return (t13, arity)
        }
      }
    }
    val genschemm = pattern.genschem.match_(token)
    if (genschemm) {
      val arity = (len(genschemm.group(2)) + 1)
      val sym2 = genschemm.group(1)(0)
      if (List('p', 'q', 'r').contains(sym2)) {
        return (t13, arity)
      } else {
        return (t12, arity)
      }
    }
    val gensentm = pattern.gensent.match_(token)
    if (gensentm) {
      return (t11, 0)
    }
    if (len(token) < 5) {
      return 0
    }
    if (token(0) != '\\') {
      return 0
    }
    if (!List('o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w').contains(token(1))) {
      return 0
    }
    if (len(token) == 6 && token(5) != 'p') {
      if (token.substring(3, 6) != "var") {
        return 0
      }
      if (!List('p', 'q', 'r', 's', 't').contains(token(1))) {
        return 0
      }
      if (!List('p', 'q', 'r', 's', 't').contains(token(2))) {
        return 0
      }
      return (t11, 0)
    }
    if (token.substring(3, 5) != "ar") {
      return 0
    }
    if (token(2) == 'v') {
      if (List('o', 'p', 'q', 'r', 's', 't').contains(token(1)) && len(token) == 5) {
        return (t11, 0)
      } else {
        return 0
      }
    } else {
      if (token(2) != 'b') {
        return 0
      }
    }
    if (!List('p', 'q', 'r', 'u', 'v', 'w').contains(token(1))) {
      return 0
    }
    val tail = token.substring(5)
    val lt = len(tail)
    if (tail.count(_ == 'p') == lt) {
      if (List('p', 'q', 'r').contains(token(1))) {
        return (t13, (1 + lt))
      } else {
        return (t12, (1 + lt))
      }
    } else {
      return 0
    }
  }

  def validvar(token: String) = {
    if (token(0).isalpha()) {
      True
    } else {
      if (token(0) == '{') {
        val x = token.substring(1, -1).strip()

        if (x.isalpha()) {
          True
        } else {
          if (x(0) == '\\') {
            val y = x.find(' ')
            if (y == -1) {
              False
            } else {
              if (x.substring(1, y) == "cal" && x.substring(y).strip().isalpha()) {
                True
              } else {
                False
              }
            }
          } else {
            False
          }
        }
      } else {
        val tokenm = pattern.token.match_(token)
        if (allowed_variables.contains(tokenm.group(3))) {
          True
        }
        False
      }
    }
  }

  val allowed_variables = List("""\alpha""", """\beta""", """\gamma""",
    """\delta""", """\epsilon""", """\varepsilon""",
    """\zeta""", """\eta""", """\theta""", """\vartheta""",
    """\iota""", """\kappa""", """\lambda""", """\mu""", """\nu""",
    """\xi""", """\pi""", """\varpi""", """\rho""", """\varrho""",
    """\sigma""", """\varsigma""", """\tau""", "\\upsilon",
    """\phi""", """\varphi""", """\chi""", """\psi""",
    """\Gamma""", """\Delta""", """\Theta""", """\Lambda""",
    """\Xi""", """\Pi""", """\Sigma""", """\Upsilon""",
    """\Phi""", """\Psi""", """\imath""", """\jmath""", """\ell""")

  def validnum(token: String): Boolean = {
    return token.isdigit()
  }

  // A basic form is either a basic term or a basic formula.
  import basicforms.{
    Form => BasicForm,
    SigFormula,
    Term => BasicTerm,
    Formula,
    SigTerm,
    Const,
    Var,
    Fun,
    Pred,
    CSym
  }

  def tokenparse(token: String): BasicForm = TODO /*{
    //
    val precedence = mathdb.MD_PRECED
    val defs = mathdb.MD_DEFS
    val arity = mathdb.MD_ARITY
    val n = symtype(token)
    n.id match {
      case 18 => {
        decimalparse(token)
      }
      /*@@
      case 12 => {
        List(List(-4, arity(token)), (List(n), token))
      }
      case 13 => {
        List(List(-5, arity(token)), List(List(n), token))
      }
      case 16 => {
        List(List(-2, List()), List(List(n), token))
      }
      case 17 => {
        List(List(-1, defs(token)), List(List(n), token))
      }
      case 8 => {
        List(List(-6, defs(token)), List(List(n), token))
      }
      case 9 => {
        List(List(-7, defs(token)), List(List(n), token))
      }
      case _ if (List(1, 2, 3).contains(n)) => {
        List(List(n), token, precedence(token))
      }
      */
      case _ => {
        // Even unknown constants are passed on as complete 
        Noun(n, token)
      }
    }
  }*/

  class ParadeTerm(prec: Int, args: BasicTerm*) extends SigTerm(args toList)

  def decimalparse(token: String): BasicTerm = {
    val precedence = mathdb.MD_PRECED
    val n = len(token)

    def noun(s: String) = Const(s, CSym(s))
    implicit def as_term(sym: String): BasicTerm = Const(sym, CSym(sym))

    if (n == 1) {
      noun(token)
    } else {
      def recur(k: Int): BasicTerm = {
        val tens: BasicTerm = if (k == 1) { token.slice(0, 1) } else { recur(k - 1) }
        new ParadeTerm(precedence('+'),
          new ParadeTerm(precedence("""\cdot"""),
            tens, """\cdot""", """\ten"""),
          "+",
          token.slice(k - 1, k))
      }
      SigTerm(List("(", recur(n), ")"))
    }
  }

  def addtoken(tree: Parse, token: String): Option[Parse] = {
    if (symtype(token) == Some(t23)) {
      Some(tree)
    } else {
      //@@addnode(tree, tokenparse(token))
      None
    }
  }

  case class PendingParses(ts: List[Parse]) extends Parse {
    def complete = TODO
  }
  def addnode(tree: PendingParses, item: BasicForm): Option[Parse] = {
    None // @@
    /*
    //tree is a list which has one entry for each pending incomplete parse tree
    //item is one complete parse
    //
    val header = item.header
    var syntype = header.tag

    //@@val okval = 1
    // An unknown symbol which appears immediately following
    // a single open parenthesis can only be the beginning
    // of a definition which makes it an introductor.
    // In this case it must be repackaged as an incomplete node.
    // A new symbol appears
    syntype.id match {
      //		print "New symbol:", item[1] 
      case 5 if len(tree.ts) < 2 => None
      
      if (tree.ts(0).header.tag != toTag(-2) || tree.ts(1).header.tag != toTag(-9)) {
        return 0
        }
      if (len(tree) == 2) {
        if (len(tree(1)) == 1) {
          // Change new node to incomplete
          item = List(List(-3, List()), item)
          header = item(0)
          syntype = header(0)
          }
         else {
          //				print "New Introductor"
          return 0
          }
        }
       else if (len(tree) == 3) {
        if (List(-1, -3).contains(tree(2)(0)(0))) {
          // Neither an introductor nor a connector. 
          /* pass */
          }
         else {
          // if tree[2][0][0] == -1: 
          // tree[2][0][0] = -3 will be done in nodecheck() 
          return 0
          }
        }
       else if (len(tree) == 4 && len(tree(1)) == 1) {
        // This must be a new connector.
        if (tree(2)(0)(0) == -2 && tree(3)(0)(0) == -12) {
          /* pass */
          }
         else if (tree(2)(0)(0) == -2 && tree(3)(0)(0) == -9) {
          //				print "new connector"
          tree(3)(0)(0) = -12
          item(0)(0) = 3
          item.append(-2)
          }
         else {
          return 0
          }
        }
       else {
        return 0
        }
      }
    // Recognize the end of a parade or a definition
    if (syntype == 6) {
      if (len(tree) == 0) {
        println("Error: Extra right paren")
        return 0
        }
      // Parade
      if (tree(-1)(0)(0) == -9) {
        //Calls paradecheck
        if (! paradecrop(tree(-1), 0)) {
          if (len(tree) == 4 && len(tree(3)) == 2 && tree(3)(1)(0)(0) == 50) {
            val parsed_parade = tree.pop().pop()
            val current = tree.pop()
            current.append(parsed_parade)
            tree.append(current)
            }
           else {
            println("Error: parade syntax.")
            return 0
            }
          }
         else {
          val parsed_parade = tree.pop().pop()
          val current = tree.pop()
          current.append(parsed_parade)
          tree.append(current)
          }
        }
       else //Undefined parade 
      if (tree(-1)(0)(0) == -12) {
        // 			print "Undefined parade", tree[-1]
        val current = tree.pop()
        promote(current, 50)
        tree(-1).append(current)
        }
       else // New or old definition
      if (List(-11, -10).contains(tree(-1)(0)(0))) {
        println("Should not reach this point")
        throw new SystemExit()
        }
      }
    if (List(1, 2).contains(syntype)) {
      if (len(tree) < 2) {
        return 0
        }
      if (len(tree) == 3) {
        if (tree(0)(0)(0) == -2 && tree(1)(0)(0) == -9 && tree(2)(0)(0) == -3) {
          // This is the only valid way out of a -3 state.
          // Change from a parade to a definition in the next if block 
          val current = tree.pop()
          promote(current, 49)
          tree(-1).append(current)
          }
        }
      }
    // Change parades to definitions
    if (List(1, 2).contains(syntype)) {
      if (tree(-1)(0)(0) == -9 && len(tree(-1)) == 2) {
        if (tree(-1)(1)(0)(0) == 49) {
          tree(-1)(0)(0) = -11
          }
        }
      }
    //End scope
    if (len(tree) > 1 && len(tree(-1)) > 1 && tree(-1)(0)(0) == -8) {
      //
      // No need for recursion here since a scope 
      // is not allowed at the end of a formula
      //
      //Header of the notarian expression
      val lasthead = tree(-2)(0)
      // Semi-colon
      if (syntype == 22) {
        if (scopecheck(tree(-1))) {
          // No length determination made yet
          if (len(lasthead) == 2) {
            // E xiA ; px qx
            lasthead.append(5)
            }
           else if (lasthead(2) != 7) {
            return 0
            }
          val current = tree.pop()
          promote(current, 48)
          tree(-1).append(current)
          }
         else {
          return 0
          }
        }
       else // Colon
      if (syntype == 21) {
        if (scopecheck(tree(-1))) {
          if (len(lasthead) > 2) {
            return 0
            }
           else {
            // E xiA : px
            lasthead.append(4)
            }
          val current = tree.pop()
          promote(current, 48)
          tree(-1).append(current)
          }
         else {
          return 0
          }
        }
       else // Left Scope Bracket
      if (syntype == 19) {
        if (len(tree(-1)) == 2 && List(10, 14, 40, 42).contains(tree(-1)(-1)(0)(0))) {
          // E ux < xiA ; px >
          lasthead.append(7)
          val current = tree.pop()
          // Not a scope after all.
          val current = current.pop()
          tree(-1).append(current)
          }
         else {
          return 0
          }
        }
       else //Right Scope Bracket
      if (syntype == 20) {
        if (len(lasthead) <= 2 || lasthead(2) != 7) {
          return 0
          }
         else if (scopecheck(tree(-1))) {
          if (len(tree(-2)) == 4 && tree(-2)(-1)(0)(0) == 19) {
            // E ux < xiA >  , No px after all.
            lasthead(2) = 6
            }
           else {
            return 0
            }
          val current = tree.pop()
          promote(current, 48)
          tree(-1).append(current)
          }
         else {
          return 0
          }
        }
       else if (! List(3).contains(tree(-1)(-1)(0)(0)) || List(3).contains(syntype)) {
        if (len(lasthead) != 2) {
          /* pass */
          }
         else if (scopecheck(tree(-1))) {
          val okval = 2
          // E xiA qx
          lasthead.append(3)
          val current = tree.pop()
          promote(current, 48)
          tree(-1).append(current)
          }
         else {
          return 0
          }
        }
      }
    //###############################################################
    //
    //    Main Algorithm 
    //
    //###############################################################
    // Incomplete nodes are just appended, not checked!
    if (syntype < 0) {
      //		print "item = ", item
      tree.append(item)
      if (item(1)(0)(0) == 16 && len(item) == 2) {
        tree.append(List(List(-9, 0)))
        }
      if (List(19, 8, 9).contains(item(-1)(0)(0))) {
        tree.append(List(List(-8, List())))
        }
      for ( x <- tree) {
        if (x(0)(0) > 0) {
          return 0
          }
        }
      return okval
      }
     else // When a complete node arrives it is added to the last incomplete
    if (tree) {
      // node, possibly completing it.  Recursion follows. 
      for ( i <- range(len(tree))) {
        // Since known introductors can introduce unknown forms
        // it catches errors sooner to check for unknown forms here.
        if (tree(i)(0)(0) == -3) {
          if (i != 2) {
            return 0
            }
          if (tree(0)(0)(0) != -2) {
            return 0
            }
          }
        }
      val current = tree.pop()
      current.append(item)
      val ndc = nodecheck(current)
      if (ndc == 0) {
        return 0
        }
      val adn = addnode(tree, current)
      if (adn == 0) {
        return 0
        }
      return max(adn, ndc)
      }
     else {
      // If no incomplete nodes are left we are done.
      tree.append(item)
      return okval
      }
    }
  // Change incomplete node to complete
  def promote(node: IncompleteParse, newvalue: Tag): BasicForm = {
    val children = node.children map { ch =>
      ch.header match {
        case Header(TopComplete, x) => x
        case _ => ch
      }
      }
    for (k <- range(1, len(node.children))) {
      if (node(k)(0)(0) < 40) {
        node(k) = node(k)(1)
        }
      }
    node(0)(0) = newvalue
    */
  }

  def nodecheck(item: Any): Option[BasicForm] = {
    None //@@
    /*
    // 
    //item is a tree with a newly added node
    // nodecheck determines whether it should
    // converted to a complete node and does
    // so if item is ready.  It returns 1 unless
    // there is a parse error.
    //
    //	pprint(item)
    val header = item(0)
    val syntype = header(0)
    val thisheader = item(-1)(0)
    if (syntype == -1) {
      val r = deflistupdate(item)
      if (r == 0) {
        // Undefined expression
        header(0) = -3
        return 1
        }
      if (r == 1) {
        val d = item(0)(1)(0)
        if (len(d) == len(item)) {
          if (d(0)(0) == 40) {
            promote(item, 40)
            }
           else if (d(0)(0) == 41) {
            promote(item, 41)
            }
           else {
            throw new SystemExit()
            }
          //				item[0].remove(item[0][1])
          val defs = mathdb(MD_DEFS)
          item(0)(1)(0) = defs(item(1)).index(item(0)(1)(0))
          for (val x <- d(0)(1).substring(1)) {
            item(0)(1).append(x)
            }
          }
        }
      //				item[0][1] = [defs[item[1]].index(item[0][1][0])]
      return 1
      }
     else //Parenthetical expression
    if (syntype == -2) {
      if (item(-1)(0)(0) == 45) {
        return 1
        }
       else if (item(-1)(0)(0) == 44) {
        return 1
        }
       else if (item(-1)(0)(0) == 47) {
        return 1
        }
       else if (item(-1)(0)(0) != 6) {
        return 0
        }
      //		Checks already done.
      if (item(-2)(0)(0) == 44) {
        promote(item, 40)
        }
       else if (item(-2)(0)(0) == 45) {
        promote(item, 41)
        }
       else if (item(-2)(0)(0) == 47) {
        promote(item, 51)
        }
       else if (item(-2)(0)(0) == 50) {
        promote(item, 49)
        }
       else {
        // This location is reached if x_ instead of x\_ is used"
        return 0
        }
      return 1
      }
     else // Undefined Expression
    if (syntype == -3) {
      if (! List(1, 2, 6, 14, 15).contains(thisheader(0))) {
        return 1
        }
      }
     else // Schematic Expressions 
    if (List(-4, -5).contains(syntype)) {
      // A term 
      if (List(10, 14, 40, 42).contains(thisheader(0))) {
        if (len(item) == (header(1) + 2)) {
          // Get values: 42 term, 43 formula 
          promote(item, (38 - syntype))
          }
        return 1
        }
      }
     else // A New Definition
    if (syntype == -11) {
      val lastheader = item(-2)(0)
      if (item(1)(0)(0) != 49) {
        println("Mistake")
        throw new SystemExit()
        }
      if (len(item) > 4) {
        return 0
        }
       else if (len(item) == 3) {
        return List(1, 2).contains(thisheader(0))
        }
       else if (len(item) == 4) {
        if (definitioncheck(item)) {
          promote(item, 47)
          return 1
          }
         else {
          println("Error: Failed definition check")
          return 0
          }
        }
      }
     else // Undefined Parade
    if (syntype == -12) {
      if (List(10, 11, 14, 15, 40, 41, 42, 43).contains(thisheader(0))) {
        if (len(item) < 3) {
          return 1
          }
        if (List(10, 11, 14, 15, 40, 41, 42, 43).contains(item(-2)(0)(0))) {
          println("Error: Connector missing.")
          return 0
          }
         else {
          return 1
          }
        }
       else if (List(3, 5, 7).contains(thisheader(0))) {
        return 1
        }
      }
     else // A Parade
    if (syntype == -9) {
      if (thisheader(0) == 49) {
        if (len(item) == 2) {
          item(0)(0) = -11
          return 1
          }
        }
       else if (List(10, 11, 14, 15, 40, 41, 42, 43).contains(thisheader(0))) {
        if (len(item) < 3) {
          return 1
          }
        if (List(10, 11, 14, 15, 40, 41, 42, 43).contains(item(-2)(0)(0))) {
          // Precedence of the empty connector
          header(1) = 1000
          println("Error: Connector missing.")
          }
        return 1
        }
       else if (List(5, 7, 44, 45).contains(thisheader(0))) {
        return 1
        }
       else if (List(1, 2, 3).contains(thisheader(0))) {
        if (len(item) == 3 && List(1, 2).contains(thisheader(0)) && item(1)(0)(0) == 49) {
          item(0)(0) = -11
          return 1
          }
        // compare with parade's max
        if (item(-1)(2) >= header(1)) {
          header(1) = item(-1)(2)
          return 1
          }
        val savelast = item.pop()
        if (! paradecrop(item, savelast(2))) {
          return 0
          }
        item.append(savelast)
        item(0)(1) = savelast(2)
        return 1
        }
      }
     else // A scope 
    if (syntype == -8) {
      if (List(3, 5, 7, 10, 11, 14, 15, 40, 41, 42, 43).contains(thisheader(0))) {
        return 1
        }
      }
     else // A notarian expression 
    if (List(-6, -7).contains(syntype)) {
      val lasthead = header(2)
      // x = raw_input("Definition check")  #Find the dft dff access values
      val term_or_formula = header(1)(0)(0)
      // Scope rescue section does the checking
      if (len(item) < 4) {
        return 1
        }
       else if (len(item) == 4) {
        if (lasthead == 7) {
          // Left scope bracket
          return thisheader(0) == 19
          }
         else if (lasthead == 6) {
          // Left scope bracket
          return thisheader(0) == 19
          }
         else if (lasthead == 5) {
          // Semi-colon
          return thisheader(0) == 22
          }
         else if (lasthead == 4) {
          // Colon
          return thisheader(0) == 21
          }
         else if (lasthead == 3) {
          if (syntype == -6 && List(10, 14, 40, 42).contains(thisheader(0))) {
            /* pass */
            }
           else if (syntype == -7 && List(11, 15, 41, 43).contains(thisheader(0))) {
            /* pass */
            }
           else {
            return 0
            }
          // Remove definition list
          item(0).remove(item(0)(1))
          promote(item, term_or_formula)
          item(0)(0) = item(0)(0)(0)
          //				print "item inside nc after len 4 = ", item
          return 1
          }
        }
       else if (len(item) == 5) {
        if (lasthead == 7) {
          return thisheader(0) == 48
          }
         else if (lasthead == 6) {
          return thisheader(0) == 48
          }
         else if (lasthead == 5) {
          if (List(11, 15, 41, 43).contains(thisheader(0))) {
            return 3
            }
           else {
            return 0
            }
          }
         else if (lasthead == 4) {
          if (List(11, 15, 41, 43).contains(thisheader(0))) {
            // Remove definition list
            item(0).remove(item(0)(1))
            promote(item, term_or_formula)
            //					item[0][:1] = item[0][0]  # Testing??
            item(0)(0) = item(0)(0)(0)
            return 1
            }
          }
        }
       else if (len(item) == 6) {
        if (lasthead == 7) {
          // Semi-colon
          return thisheader(0) == 22
          }
         else if (lasthead == 6) {
          // Right scope bracket
          if (thisheader(0) == 20) {
            // Remove definition list
            item(0).remove(item(0)(1))
            promote(item, term_or_formula)
            //					item[0][:1] = item[0][0]  # Testing??
            item(0)(0) = item(0)(0)(0)
            return 1
            }
          }
         else if (lasthead == 5) {
          if (syntype == -6 && List(10, 14, 40, 42).contains(thisheader(0))) {
            /* pass */
            }
           else if (syntype == -7 && List(11, 15, 41, 43).contains(thisheader(0))) {
            /* pass */
            }
           else {
            return 0
            }
          // Remove definition list
          item(0).remove(item(0)(1))
          promote(item, term_or_formula)
          //				item[0][:1] = item[0][0]  # Testing??
          item(0)(0) = item(0)(0)(0)
          return 1
          }
        }
       else if (len(item) == 7) {
        if (List(11, 15, 41, 43).contains(thisheader(0))) {
          return 1
          }
        }
       else if (len(item) == 8) {
        // Right scope bracket
        if (thisheader(0) == 20) {
          // Remove definition list
          item(0).remove(item(0)(1))
          promote(item, term_or_formula)
          //				item[0][:1] = item[0][0]  # Testing??
          item(0)(0) = item(0)(0)(0)
          return 1
          }
        }
      }
    return 0
    */
  }

  /*@@
  def deflistupdate(item: Any): Any = {
    val deflist = item(0)(1)
    //	print "Number of defs: ", len(deflist)
    val n = (len(item) - 1)
    val a = List()
    for (val definiendum <- deflist) {
      //		definiendum = d[1]
      val ibvlist = definiendum(0)(1).substring(1)
      if (ibvlist.contains(n)) {
        if (item(-1)(0)(0) == 10) {
          a.append(definiendum)
          }
        }
       else if (syntmatch(definiendum(n), item(-1))) {
        a.append(definiendum)
        }
      }
    item(0)(1) = a
    return len(a)
    }
  */

  def isTermDefinor(f: BasicForm) = f match {
    case Const(txt, c) => symtype(c.name) == t1
    case _ => false
  }
  def isFormulaDefinor(f: BasicForm) = f match {
    case Const(txt, c) => symtype(c.name) == t2
    case _ => false
  }
  def isTerm(f: BasicForm) = f match {
    case t: BasicTerm => t match {
      //10. Variable
      case x: Var => true
      // 14. Noun
      case x: Const => true
      // 40. Term
      case x: SigTerm => true
      // 42. Schematic Term
      case x: Fun => true
    }
    case _ => false
  }

  def isFormula(f: BasicForm) = f match {
    case x: Formula => x match {
      // 11. Sentence variable
      case Pred(_, _, List()) => true
      // 15. Boolean Constant
      case SigFormula(List(Const(_, _))) => true
      // 41. Formula
      case x: SigFormula => true
      // 43. Schematic Formula
      case x: Pred => true
    }
    case _ => false
  }

  def definitioncheck(item: SigFormula): Either[String, DEFS] = {
    val definiendum = item.fs(0)
    val definor = item.fs(1)
    val definiens = item.fs(2)
    //	print definiendum, definor, definiens
    if (isTermDefinor(definor)) {
      // A Term
      if (!isTerm(definiens)) {
        Left("Error: Type mismatch, term expected")
      }
    } else if (isFormulaDefinor(definor)) {
      // A formula 
      if (!isFormula(definiens)) {
        Left("Error: Type mismatch, formula expected")
      }
    }
    register_definiendum(definiendum, isTermDefinor(definor))
  }

  /*
  def paradecrop(item: Any, reducedmax: Any): Any = {
    val header = item(0)
    //compare with parade's max
    while (reducedmax < header(1)) {
      val thisprec = header(1)
      val newitem = List()
      val x = item.pop()
      if (List(1, 2, 3).contains(x(0)(0))) {
        val lastprec = x(2)
        }
      while (! List(1, 2, 3).contains(x(0)(0)) || x(2) >= thisprec) {
        newitem.append(x)
        if (len(item) == 1) {
          newitem.reverse()
          val newitem = (List(List(44, thisprec)) + newitem)
          val a = paradecheck(newitem)
          if (a == 0) {
            promote(newitem, 50)
            item.append(newitem)
            println("Error: Parade syntax.")
            return 0
            }
          promote(newitem, a)
          item.append(newitem)
          return 1
          }
        val x = item.pop()
        if (List(1, 2, 3).contains(x(0)(0))) {
          val lastprec = x(2)
          }
        }
      if (List(1, 2, 3).contains(x(0)(0))) {
        item.append(x)
        }
      item(0)(1) = lastprec
      newitem.reverse()
      val newitem = (List(List(44, thisprec)) + newitem)
      val a = paradecheck(newitem)
      if (a == 0) {
        newitem(0)(0) = 50
        item.append(newitem)
        println("Error: Parade syntax")
        return 0
        }
      promote(newitem, a)
      if (len(item) > 1) {
        item.append(newitem)
        }
       else {
        item.append(newitem)
        break
        }
      }
    return 1
    }
  val programmed_precedences = List(1, 2, 3, 4, 5, 6, 7, 9, 11, 13, 15, 17, 19, 25, 1000)
  
  def paradecheck(item: Any): Any = {
    if (len(item) == 1) {
      println("Error: Empty parade")
      return 0
      }
    val prec = item(0)(1)
    val lastsix = "Irrelevant initial value"
    //Check for variations in the precedence 
    for (val x <- item.substring(1)) {
      if (List(1, 2, 3).contains(x(0)(0))) {
        if (x(2) == 6) {
          val lastsix = x(1)
          }
        if (x(2) != prec) {
          println("Error: Mixed precedence values:" + prec + x(2))
          return 0
          }
        }
      }
    val allands = 1
    val same_op = ""
    val allands = 1
    for (val x <- item.substring(1)) {
      if (x(0)(0) == 3) {
        if (x(1) != same_op) {
          if (same_op) {
            val allands = 0
            }
           else {
            val same_op = x(1)
            }
          }
        }
      }
    if (allands && mathdb(MD_CAOPS).contains(same_op)) {
      //	Flag commutative-associative expressions for special treatment
      item(0).append(-1)
      }
    if (prec < 0) {
      return 1
      }
     else if (prec == 1) {
      if (n_arycheck(item)) {
        return 44
        }
      }
     else if (prec == 2) {
      if (n_arycheck(item, 1)) {
        return 45
        }
      }
     else if (prec == 3) {
      if (condelscheck(item, 2)) {
        return 44
        }
      }
     else if (prec == 4) {
      if (n_arycheck(item, 1)) {
        return 45
        }
      }
     else if (prec == 5) {
      if (n_arycheck(item, 1)) {
        return 45
        }
      }
     else if (prec == 6) {
      if (lastsix == ',') {
        if (n_arycheck(item)) {
          return 44
          }
        }
       else if (binverbcheck(item)) {
        return 45
        }
      }
     else if (List(9, 13).contains(prec)) {
      if (mixcheck(item)) {
        return 44
        }
      }
     else if (List(7, 11, 15, 17, 19, 25).contains(prec)) {
      if (n_arycheck(item)) {
        return 44
        }
      }
     else if (prec == 1000) {
      println("Error: Adjacent writing undefined")
      return 0
      }
     else {
      return deflistcheck(item, prec)
      }
    return 0
    }
  
  def binverbcheck(item: Any): Any = {
    //
    // values of state variable:
    //    0  subject
    //    1  verb
    //    2  object
    // 
    val state = 0
    val j = 0
    for (val x <- item.substring(1)) {
      val j = (j + 1)
      val y = x(0)(0)
      if (state == 0) {
        if (y == 3 || y == 1) {
          if (j == 1) {
            println("Error: Start with a term.")
            return 0
            }
           else if (x(1) != ',') {
            if (x(2) != 6) {
              throw new "Non verb!"()
              }
            val state = 1
            }
           else if ((j % 2) == 1) {
            println("Error: Term needed.")
            return 0
            }
          }
         else //				else x[1] == ',' and j % 2 == 0 which is ok.
        if (List(10, 14, 40, 42, 44).contains(y)) {
          if ((j % 2) == 0) {
            println("Rescue code should catch this")
            throw new SystemExit()
            }
          }
         else {
          //				else: This is the indicial variable case
          println("Error: Term needed.")
          return 0
          }
        }
       else if (state == 1) {
        if (List(10, 14, 40, 42, 44).contains(y)) {
          val state = 2
          }
         else if (y != 3) {
          println("Error: Bad Nexus.")
          return 0
          }
        }
       else if (state == 2) {
        if (y == 3 || y == 1) {
          if (x(1) == ',') {
            val state = 0
            val j = 0
            }
          }
         else if (! List(10, 14, 40, 42, 44).contains(y)) {
          println("Error: object needed")
          return 0
          }
        }
       else {
        throw new "Programming error"()
        }
      }
    if (state == 2) {
      return 1
      }
     else {
      return 0
      }
    }
  
  def scopecheck(item: Any): Any = {
    if (len(item) == 1) {
      println("Error: Empty scope.")
      return 0
      }
    //
    // values of state variable:
    //    0  subject
    //    1  verb
    //    2  object
    //
    val state = 0
    val j = 0
    // 
    for (val x <- item.substring(1)) {
      val j = (j + 1)
      val y = x(0)(0)
      if (state == 0) {
        if (y == 3) {
          if (j == 1) {
            println("Error: Indicial variable needed.")
            return 0
            }
           else if (x(1) != ',') {
            if (x(2) < 6) {
              println((("Error: " + x(1)) + " not allowed in scope"))
              return 0
              }
            val state = 1
            }
           else if ((j % 2) == 1) {
            println("Error: Indicial variable or verb needed.")
            return 0
            }
          }
         else //				else x[1] == ',' and j % 2 == 0 which is ok.
        if (y == 10) {
          if ((j % 2) == 0) {
            throw new "Rescue code should catch this"()
            }
          }
         else {
          //				else: This is the indicial variable case
          println("Error: Indicial variable needed.")
          return 0
          }
        }
       else if (state == 1) {
        if (List(10, 14, 40, 42, 44).contains(y)) {
          val state = 2
          }
         else if (y != 3) {
          println("Error: Bad nexus.")
          return 0
          }
        }
       else if (state == 2) {
        if (y == 3) {
          if (x(1) == ',') {
            val state = 0
            val j = 0
            }
          }
         else if (! List(10, 14, 40, 42, 44).contains(y)) {
          println("Error: object needed")
          return 0
          }
        }
       else {
        throw new "Programming error"()
        }
      }
    return 1
    }
  
  def n_arycheck(item: Any, tf_flag: Any = 0): Any = {
    val n = len(item)
    if (n < 4) {
      return 0
      }
    if (! List(1, 2, 3).contains(item(2)(0)(0))) {
      println("Error: " + item(2)(1) + "not a connector")
      return 0
      }
    val binarian = item(2)(1)
    for (val i <- range(1, n)) {
      if ((i % 2) == 1) {
        if (tf_flag == 0) {
          if (! List(10, 14, 40, 42, 44).contains(item(i)(0)(0))) {
            println("Error: " + item(i) + "is not a term")
            return 0
            }
          }
         else if (! List(11, 15, 41, 43, 45).contains(item(i)(0)(0))) {
          println("Error: " + item(i) + "is not a formula")
          return 0
          }
        }
       else if (item(i) != item(2)) {
        println("Error: " + item(i) + "!=" + item(2))
        return 0
        }
      }
    return 1
    }
  
  def mixcheck(item: Any): Any = {
    val n = len(item)
    if (n < 3) {
      return 0
      }
    if (item(1)(0)(0) == 3) {
      if (! List(10, 14, 40, 42, 44).contains(item(2)(0)(0))) {
        return 0
        }
      }
    for (val i <- range(1, (n - 2))) {
      if (List(10, 14, 40, 42, 44).contains(item(i)(0)(0))) {
        if (item((i + 1))(0)(0) != 3) {
          return 0
          }
        if (! List(10, 14, 40, 42, 44).contains(item((i + 2))(0)(0))) {
          return 0
          }
        }
       else if (item(i)(0)(0) != 3) {
        return 0
        }
      }
    return 1
    }
  
  def binarycheck(item: Any, tf_flag: Any = 0): Any = {
    val n = len(item)
    if (n != 4) {
      return 0
      }
    if (item(2)(0)(0) != 3) {
      return 0
      }
    // both terms
    if (tf_flag == 0) {
      if (! List(10, 14, 40, 42, 44).contains(item(1)(0)(0))) {
        return 0
        }
      if (! List(10, 14, 40, 42, 44).contains(item(3)(0)(0))) {
        return 0
        }
      }
     else // both formulas
    if (tf_flag == 1) {
      if (! List(11, 15, 41, 43, 45).contains(item(1)(0)(0))) {
        return 0
        }
      if (! List(11, 15, 41, 43, 45).contains(item(3)(0)(0))) {
        return 0
        }
      }
     else // formula term
    if (tf_flag == 2) {
      if (! List(11, 15, 41, 43, 45).contains(item(1)(0)(0))) {
        return 0
        }
      if (! List(10, 14, 40, 42, 44).contains(item(3)(0)(0))) {
        return 0
        }
      }
    return 1
    }
  
  def condelscheck(item: Any, tf_flag: Any = 0): Any = {
    val n = len(item)
    if (n < 4 || (n % 2) == 1) {
      return 0
      }
    // All terms or formulas
    for (val k <- range(1, n, 2)) {
      if (List(10, 14, 40, 42, 44).contains(item(k)(0)(0))) {
        /* pass */
        }
       else if (List(11, 15, 41, 43, 45).contains(item(k)(0)(0))) {
        /* pass */
        }
       else {
        return 0
        }
      }
    return 1
    for (val k <- range(2, n, 2)) {
      if (item(k)(1) != """\els""") {
        break
        }
      }
    /* for ... else: */
    if ((range(2, n, 2)).isEmpty) {
      // All terms
      for (val k <- range(1, n, 2)) {
        if (! List(10, 14, 40, 42, 44).contains(item(k)(0)(0))) {
          return 0
          }
        }
      return 1
      }
    for (val k <- range(2, n, 2)) {
      if (((k / 2) % 2) == 1) {
        if (item(k)(1) != """\cond""") {
          return 0
          }
        if (! List(11, 15, 41, 43, 45).contains(item((k - 1))(0)(0))) {
          return 0
          }
        }
       else {
        if (item(k)(1) != """\els""") {
          return 0
          }
        if (! List(10, 14, 40, 42, 44).contains(item((k - 1))(0)(0))) {
          return 0
          }
        }
      }
    if (! List(10, 14, 40, 42, 44).contains(item((n - 1))(0)(0))) {
      return 0
      }
    return 1
    }
  
  def deflistcheck(item: Any, prec: Any): Any = {
    //For parenthetical expressions only.
    val defs = mathdb(MD_DEFS)
    for (val k <- range(len(defs(prec)))) {
      if (len(item) != len(defs(prec)(k))) {
        continue
        }
      for (val j <- range(1, len(item))) {
        if (! syntmatch(defs(prec)(k)(j), item(j))) {
          break
          }
        }
      /* for ... else: */
      if ((range(1, len(item))).isEmpty) {
        // Parade Term or Formula Indicator
        return item(0)(0)
        }
      }
    return 0
    }
  @@*/

  def register_definiendum(definiendum: BasicForm, termorformula: Boolean): Either[String, DEFS] = TODO /* {
    val td = mathdb(MD_SYMTYPE)
    val precedence = mathdb(MD_PRECED)
    val defs = mathdb(MD_DEFS)
    val introductor = definiendum(1)
    //	print "DEFINIENDUM =", definiendum
    // Left parenthesis
    if (symtype(definiendum(1)) == 16) {
      if (definiendum(2)(0)(0) != 50) {
        return 0
        }
      if (definiendum(3) != ')') {
        return 0
        }
      val definiendum = definiendum(2)
      if (termorformula == 1) {
        definiendum(0)(0) = 44
        }
       else {
        definiendum(0)(0) = 45
        }
      val precedence_from_context = -1
      for (val x <- definiendum.substring(1)) {
        if (type(x) == str) {
          if (List(1, 2, 3).contains(symtype(x))) {
            if (precedence_from_context == -1) {
              val precedence_from_context = precedence(x)
              }
             else if (precedence_from_context != precedence(x)) {
              println("Error: Mixed precedence values in definiendum!")
              return 0
              }
            }
          }
        }
      for (val x <- definiendum.substring(1)) {
        if (type(x) == str) {
          if (symtype(x) == 5 || symtype(x) == 7) {
            //					print "New connector", x
            if (precedence_from_context > -1) {
              precedence(x) = precedence_from_context
              }
             else if (precedence.contains(x)) {
              /* pass */
              }
             else {
              println("Error: Precedence not set for " + x)
              return 0
              }
            td(x) = 3
            val p = precedence(x)
            }
           else if (List(1, 2, 3).contains(symtype(x))) {
            val p = precedence(x)
            }
          }
        }
      if (! programmed_precedences.contains(p)) {
        //			definiendum[0][1] = p
        if (defs.keys().contains(p)) {
          definiendum(0)(1) = List((len(defs(p)) - 1))
          defs(p).append(definiendum)
          }
         else {
          definiendum(0)(1) = List(0)
          defs(p) = List(definiendum)
          }
        }
       else if (len(definiendum) == 4) {
        /* pass */
        }
       else {
        println("Error: Non-binary connector with programmed precedence!")
        return 0
        }
      }
     else {
      // An introductor which is not a parenthesis
      // A term
      if (termorformula == 1) {
        definiendum(0)(0) = 40
        }
       else {
        // A formula 
        definiendum(0)(0) = 41
        }
      val bvlist = List()
      val varlist = List()
      val schematorlist = List()
      val term_schemexp = List()
      val formula_schemexp = List()
      for (val x <- definiendum.substring(2)) {
        if (type(x) == str) {
          if (symtype(x) == 5) {
            td(x) = 7
            }
           else if (List(10, 11).contains(symtype(x))) {
            if (varlist.contains(x)) {
              println("Error: Repeated occurrence of " + x)
              return 0
              }
             else {
              varlist.append(x)
              }
            }
          }
         else if (List(42, 43).contains(x(0)(0))) {
          if (schematorlist.contains(x(1))) {
            println("Error: " + x(1) + "repeated schemator in definiendum")
            return 0
            }
           else {
            schematorlist.append(x(1))
            }
          if (x(0)(0) == 42) {
            val term_schemexp = x
            }
           else {
            val formula_schemexp = x
            }
          for (val y <- x.substring(2)) {
            if (type(y) != str || symtype(y) != 10) {
              println("Error: Non-variable " + y + "not allowed")
              return 0
              }
             else if (! bvlist.contains(y)) {
              bvlist.append(y)
              }
            }
          }
         else {
          //print "Bound variables", bvlist 
          val y = norepeat_varlist(x)
          if (y == 0) {
            println("Error: Repeated variable")
            return 0
            }
           else {
            for (val z <- y) {
              if (varlist.contains(z)) {
                println("Error: Repeated occurrence of " + z)
                return 0
                }
               else if (List(12, 13).contains(symtype(z))) {
                println("Error: Imbedded schemator" + y)
                return 0
                }
               else {
                varlist.append(z)
                }
              }
            }
          }
        }
      for (val y <- bvlist) {
        if (! varlist.contains(y)) {
          println("Error: Indicial position missing for" + y)
          return 0
          }
        }
      if (bvlist) {
        val initialsegment = List(introductor, bvlist(0))
        for (val y <- bvlist.substring(1)) {
          val initialsegment = (initialsegment + List(',', y))
          }
        }
      if (term_schemexp && formula_schemexp && term_schemexp.substring(2) == formula_schemexp.substring(2) && formula_schemexp.substring(2) == bvlist && definiendum.substring(1) == (initialsegment + List(';', formula_schemexp, term_schemexp))) {
        if (symtype(introductor) == 5) {
          td(introductor) = 8
          // Needed to determine T or F status
          defs(introductor) = List(definiendum)
          }
         else {
          //				print "Error: Term Seeking Notarian" , introductor, definiendum
          println("Error: Defining a known constant as a notarian not allowed.")
          return 0
          }
        }
       else if (formula_schemexp && formula_schemexp.substring(2) == bvlist && definiendum.substring(1) == ((List(introductor) + bvlist) + List(formula_schemexp))) {
        if (symtype(introductor) == 5) {
          td(introductor) = 9
          // Needed to determine T or F status
          defs(introductor) = List(definiendum)
          }
         else {
          //				print "Error: Formula Seeking Notarian", introductor, definiendum 
          println("Error: Defining a known constant as a notarian not allowed.")
          return 0
          }
        }
       else {
        val ibvlist = List()
        for (val k <- range(len(definiendum))) {
          if (bvlist.contains(definiendum(k))) {
            ibvlist.append(k)
            }
          }
        if (symtype(introductor) == 5) {
          if (len(definiendum) == 2) {
            if (termorformula == 1) {
              td(introductor) = 14
              }
             else {
              td(introductor) = 15
              }
            }
           else {
            definiendum(0).substring(1) = List(List(0))
            definiendum(0)(1).extend(ibvlist)
            defs(introductor) = List(definiendum)
            td(introductor) = 17
            }
          }
         else if (symtype(introductor) == 17) {
          //           If definiendum is subsumed it just parses.
          definiendum(0).substring(1) = List(List(len(defs(introductor))))
          definiendum(0)(1).extend(ibvlist)
          defs(introductor).append(definiendum)
          }
        }
      }
    //				introductor of parsed_exp = parsed_exp[1]  
    //				definiendum of parsed_exp = defs[introductor][parsed_exp[0][1][0]]
    //				definiendum = defs[introductor]
    //				ibvlist of definiendum = definiendum[0][1][1:]
    return 1
    }
  
  def syntmatch(form: Any, instance: Any): Any = {
    val headeri = instance(0)
    val arity = mathdb(MD_ARITY)
    if (type(form) == str) {
      if (symtype(form) == 10) {
        // This should never happen.
        return List(10, 14, 40, 42).contains(headeri(0))
        }
       else if (symtype(form) == 11) {
        return List(11, 15, 41, 43).contains(headeri(0))
        }
       else {
        return form == instance(1)
        }
      }
     else {
      val headerf = form(0)
      if (headerf(0) == 42) {
        // This should never happen.
        return List(10, 14, 40, 42).contains(headeri(0))
        }
      if (headerf(0) == 43) {
        // This should never happen.
        return List(11, 15, 41, 43).contains(headeri(0))
        }
      if (List(40, 41).contains(headerf(0))) {
        val indvars = indvlist(form)
        for (val n <- range(1, len(form))) {
          val ok = subsyntmatch(form(n), instance(n), indvars)
          if (! ok) {
            return 0
            }
          }
        return 1
        }
      }
    }
  
  def subsyntmatch(form: Any, instance: Any, indvars: Any): Any = {
    val headeri = instance(0)
    val arity = mathdb(MD_ARITY)
    if (type(form) == str) {
      if (symtype(form) == 10) {
        if (indvars.contains(form)) {
          return headeri(0) == 48
          }
         else if (type(instance) == str) {
          return List(10, 14).contains(symtype(instance))
          }
         else {
          return List(10, 14, 40, 42).contains(headeri(0))
          }
        }
       else // form is a schemator
      if (List(11, 12, 13).contains(symtype(form)) && arity(form) == 0) {
        return List(11, 15, 41, 43).contains(headeri(0))
        }
       else {
        return form == instance
        }
      }
     else {
      val headerf = form(0)
      if (headerf(0) == 42) {
        return List(10, 14, 40, 42).contains(headeri(0))
        }
      if (headerf(0) == 43) {
        return List(11, 15, 41, 43).contains(headeri(0))
        }
      if (List(40, 41).contains(headerf(0))) {
        for (val n <- range(1, len(form))) {
          val ok = subsyntmatch(form(n), instance(n), List())
          if (! ok) {
            return 0
            }
          }
        return 1
        }
      }
    }
  */

  object ModeNum extends Enumeration {
    type ModeCode = Value
    val m1, // text
    m2, // math
    m3, // margin
    m4, // error
    m5 /* end */ = Value
  }
  import ModeNum._

  /*
  def intext(mode: Any, linetail: Any, outfragments: Any = None): Any = {
    val TeXdollars = pattern.TeXdollar.search(linetail(0))
    val Noparsem = pattern.Noparse.search(linetail(0))
    if (TeXdollars) {
      if (Noparsem) {
        if (Noparsem.start(1) < TeXdollars.start(1)) {
          if (type(outfragments) == list) {
            outfragments.append(linetail(0))
            }
          linetail(0) = ""
          }
        }
       else {
        // Change to math-on mode
        mode(0) = 2
        if (type(outfragments) == list) {
          outfragments.append(linetail(0).substring(0, TeXdollars.end(1)))
          }
        linetail(0) = linetail(0).substring(TeXdollars.end(1))
        }
      }
     else if (Noparsem) {
      if (Noparsem.group("TeXcomment")) {
        val p = process_directive(linetail(0))
        if (p == -1) {
          //				print "New primitive formula" 
          /* pass */
          }
         else if (p == -2) {
          //				print "New primitive term" 
          /* pass */
          }
         else if (p == -7) {
          mode(0) = 4
          return
          }
        }
      if (type(outfragments) == list) {
        outfragments.append(linetail(0))
        }
      linetail(0) = ""
      }
     else {
      if (type(outfragments) == list) {
        outfragments.append(linetail(0))
        }
      linetail(0) = ""
      }
    return
    }
  @@*/

  def process_directive(comment_line: String, hereditary_only: Boolean = True): Tag = TODO /*{
    val directivem = pattern.directive.match_(comment_line)
    if (! directivem) {
       return 0
      }
    if (directivem.group(1) == "set_precedence") {
      if (! directivem.group(4).isdigit()) {
        println("Error: Numerical precedence value needed.")
         return -7
        }
      if (mathdb.MD_PRECED.isDefinedAt(directivem.group(3))) {
        if (mathdb.MD_PRECED(directivem.group(3)) == int(directivem.group(4))) {
          return 0
          }
         else {
          println("Error: Precedence already defined as " + mathdb.MD_PRECED(directivem.group(3)))
          }
        }
      mathdb.MD_PRECED(directivem.group(3)) = int(directivem.group(4))
      if (List(1, 2).contains(mathdb.MD_SYMTYPE.get(directivem.group(3)))) {
        /* pass */
        }
       else {
        mathdb.MD_SYMTYPE(directivem.group(3)) = 3
        }
      return -3
      }
     else if (directivem.group(1) == "def_symbol") {
      mathdb.MD_MACR(directivem.group(3).substring(1)) = directivem.group(4).drop(1).rstrip()
      }
     else if (directivem.group(1) == "external_ref" && ! hereditary_only) {
      mathdb.MD_REFD(directivem.group(3)) = directivem.group(4).rstrip()
      }
     else if (directivem.group(1) == "major_unit:") {
      // Handle this in renum.
      /* pass */
      }
     else if (directivem.group(1) == "subfile:") {
      // Handle this in makedf.
      /* pass */
      }
     else if (directivem.group(1) == "term_definor:") {
      mathdb.MD_SYMTYPE(directivem.group(2).strip()) = 1
      }
     else if (directivem.group(1) == "formula_definor:") {
      mathdb.MD_SYMTYPE(directivem.group(2).strip()) = 2
      }
     else if (directivem.group(1) == "rules_file:") {
      mathdb.MD_RFILE = directivem.group(2).strip()
      }
     else if (directivem.group(1) == "props_file:") {
      mathdb.MD_PFILE = directivem.group(2).strip()
      }
     else if (directivem.group(1) == "undefined_term:") {
      val tree = List()
      val tempmode = List(2)
      val linetailcopy = List(directivem.group(2).strip())
      addtoken(tree, '(')
      mathparse(tempmode, linetailcopy, tree)
      addtoken(tree, """\ident""")
      if (tree(-1)(0)(0) == -11) {
        val new_term = tree(-1)(1)
        //			print  "Primitive term: ", directivem.group(2) 
        val rd = register_definiendum(new_term, 1)
        if (! rd) {
          println("Error: Register definiendum failed on " + new_term)
          return -7
          }
        return -2
        promote(tree(-1), 45)
        }
       else if (tree(-1)(1)(0)(0) == 40) {
        /* pass */
        }
       else if (tree(-1)(1)(0)(0) == 14) {
        /* pass */
        }
       else {
        println("Error: Could not parse primitive term")
        return -7
        }
      }
     else if (directivem.group(1) == "undefined_formula:") {
      val tree = List()
      val tempmode = List(2)
      val linetailcopy = List(directivem.group(2).strip())
      addtoken(tree, '(')
      mathparse(tempmode, linetailcopy, tree)
      addtoken(tree, """\Iff""")
      if (tree(-1)(0)(0) == -11) {
        val new_formula = tree(-1)(1)
        //			print  "Primitive formula: ", directivem.group(2) 
        val rd = register_definiendum(new_formula, 2)
        if (! rd) {
          println("Error: Register definiendum failed on" + new_formula)
          return -7
          }
        return -1
        promote(tree(-1), 45)
        }
       else if (tree(-1)(1)(0)(0) == 41) {
        /* pass */
        }
       else if (tree(-1)(1)(0)(0) == 15) {
        /* pass */
        }
       else {
        println("Error: Could not parse primitive formula")
        return -7
        }
      }
    return 0
    }    @@* */

  def mathmargin(mode: mutable.Seq[ModeCode], linetail: LineTail, outfragments: Option[Any] = None): Any = TODO /*{
    val newlinetail = linetail(0).lstrip()
    val trimlen = (len(linetail(0)) - len(newlinetail))
    if (trimlen) {
      val blanks = linetail(0).substring(0, trimlen)
      if (type(outfragments) == list) {
        outfragments.append(blanks)
        }
      linetail(0) = newlinetail
      }
    if (linetail(0)) {
      val TeXdollarm = pattern.TeXdollar.match(linetail(0))
      if (pattern.TeXcomment.match(linetail(0))) {
        if (type(outfragments) == list) {
          outfragments.append(linetail(0))
          }
        linetail(0) = ""
        }
       else if (TeXdollarm) {
        if (type(outfragments) == list) {
          //				outfragments.append(linetail[0][:1])
          outfragments.append(linetail(0).substring(0, TeXdollarm.end(1)))
          }
        linetail(0) = linetail(0).substring(TeXdollarm.end(1))
        mode(0) = 2
        }
       else {
        val notem = pattern.note.match(linetail(0))
        if (notem) {
          println("Error: Previous note unfinished.")
          mode(0) = 4
          return
          }
         else {
          val linem = pattern.line.match(linetail(0))
          if (linem) {
            val nn = (linem.start(2) - 1)
            if (type(outfragments) == list) {
              outfragments.append(linetail(0).substring(0, nn))
              }
            linetail(0) = linetail(0).substring(nn)
            }
           else if (linetail(0).substring(0, 3) == """\By""") {
            if (type(outfragments) == list) {
              outfragments.append(linetail(0))
              }
            linetail(0) = ""
            }
           else {
            // Signal an error
            mode(0) = 4
            }
          }
        }
      }
    return
    }@@*/

  def notemargin(mode: mutable.Seq[ModeCode], linetail: LineTail): Unit = TODO /*@@{
    if (! linetail(0)) {
      return
      }
    val TeXcommentm = pattern.TeXcomment.match(linetail(0))
    val TeXdollarm = pattern.TeXdollar.match(linetail(0))
    if (TeXcommentm) {
      linetail(0) = ""
      return
      }
     else if (TeXdollarm) {
      linetail(0) = linetail(0).substring(TeXdollarm.end(1))
      mode(0) = 2
      return
      }
    val linem = pattern.line.match(linetail(0))
    if (linem) {
      linetail(0) = linem.group(2)
      mode(0) = 2
      return
      }
    val bym = pattern.by.match(linetail(0))
    if (bym) {
      val rp = refparse(bym.group(1))
      if (rp == 0) {
        println("Error in reference: " + bym.group(1))
        mode(0) = 4
        }
       else {
        linetail(0) = ""
        }
      return
      }
    val newlinetail = linetail(0).lstrip()
    val TeXdollarm = pattern.TeXdollar.match(newlinetail)
    val TeXcommentm = pattern.TeXcomment.match(newlinetail)
    if (! newlinetail) {
      linetail(0) = ""
      }
     else if (TeXcommentm) {
      linetail(0) = ""
      }
     else if (TeXdollarm) {
      linetail(0) = newlinetail.substring(TeXdollarm.end(1))
      mode(0) = 2
      }
     else {
      println("Error: Only references allowed in note margins")
      // Signal an error
      mode(0) = 4
      }
    return
    }
  
  def stringparse(wffstring: Any): Any = {
    val line_list = List(wffstring)
    val linetail = List(line_list(0), 0, 1, line_list)
    val parsetree = List()
    val mode = List(2)
    mathparse(mode, linetail, parsetree)
    return parsetree(0)
    }
  */

  def mathparse(mode: mutable.Seq[ModeCode], linetail: LineTail, tree: Any, outfragments: Any = None,
    pfcdict: Dict[String, String] = null): Unit = TODO /*{
    val currentpos = if (linetail.all == null) {
       0
      }
     else {
      linetail.ix
      }
    if (mode(0) == m4) {
      return
      }
    val currentline = linetail.tail
    val lenline = len(currentline)
    val blanklinem = pattern.blankline.match_(currentline, currentpos)
    if (blanklinem) {
      // If the parse is done
      if (tree(0)(0)(0) > 0) {
        // Change to text mode
        mode(0) = m1
        }
       else {
        // Change to Margin mode
        mode(0) = m3
        }
      val currentpos = blanklinem.end(2)
      if (type(outfragments) == list) {
        outfragments.append(currentline.substring(0, currentpos))
        }
      linetail(0) = currentline.substring(currentpos)
      return
      }
    val tokenm = pattern.token.match(currentline, currentpos)
    if (! tokenm) {
      println("Error: Line empty following TeX dollar sign")
      mode(0) = 4
      return
      }
    if (tokenm.group(1)) {
      if (type(outfragments) == list) {
        outfragments.append(tokenm.group(1))
        }
      val currentpos = tokenm.end(1)
      }
    while (currentpos < lenline) {
      val TeXdollarm = pattern.TeXdollar.match(currentline, currentpos)
      if (TeXdollarm) {
        // If the parse is done
        if (tree(0)(0)(0) > 0) {
          // Change to text mode
          mode(0) = 1
          }
         else {
          // Change to Margin mode
          mode(0) = 3
          }
        if (type(outfragments) == list) {
          outfragments.append(currentline(currentpos))
          }
        val currentpos = TeXdollarm.end(1)
        linetail(0) = currentline.substring(currentpos)
        return
        }
      // If the parse is done
      if (tree != List() && tree(0)(0)(0) > 0) {
        // Change to end mode
        mode(0) = 5
        linetail(0) = currentline.substring(currentpos)
        return
        }
      val tokenm = pattern.token.match(currentline, currentpos)
      if (! tokenm) {
        mode(0) = 4
        return
        }
      if (tokenm.group(1)) {
        if (type(outfragments) == list) {
          outfragments.append(tokenm.group(1))
          }
        }
      val token = tokenm.group(2)
      if (pfcdict == None || len(token) == 1) {
        val pfctoken = token
        }
       else if (pfcdict.contains(token.substring(1))) {
        val pfctoken = ('\\' + pfcdict(token.substring(1)))
        }
       else {
        val pfctoken = token
        }
      val ck = addtoken(tree, pfctoken)
      if (tokenm.group(1)) {
        outfragments.append(tokenm.group(1))
        }
      val currentpos = tokenm.end(0)
      linetail(0) = currentline.substring(currentpos)
      if (! ck) {
        mode(0) = 4
        linetail(0) = currentline.substring(currentpos)
        return
        }
      if (type(outfragments) == list) {
        if (ck == 1) {
          outfragments.append(token)
          }
         else if (ck == 2) {
          outfragments.append(('\\' + pattern.skipstring))
          outfragments.append(token)
          }
         else if (ck == 3) {
          outfragments.append(token)
          outfragments.append(('\\' + pattern.skipstring))
          }
         else {
          outfragments.append(token)
          }
        if (tokenm.group(4)) {
          outfragments.append(tokenm.group(4))
          }
        }
      }
    return
    }
  */

  /*
  def refparse(ref: Any): Any = {
    val reflast = False
    val reflist = List()
    val t = ref
    //	if not t.strip():
    //		print "Error: \\By without any justification"
    //		return 0
    while (t) {
      val t = t.lstrip()
      if (! t) {
        break
        }
      if (t(0) == '$') {
        return 0
        }
      if (t(0) == '.' || t(0).isdigit()) {
        val refmatch = pattern.ref.match(t)
        if (! refmatch) {
          return 0
          }
        if (reflast) {
          println("Error: Punctuator missing")
          return 0
          }
        val reflast = True
        reflist.append((refmatch.group(1) + refmatch.group(4)))
        if (refmatch.group(4)) {
          if (! mathdb(MD_REFD).contains(refmatch.group(4))) {
            println(refmatch.group(4) + "undefined file reference key.")
            return 0
            }
          }
        val t = refmatch.group(5)
        continue
        }
      val reflast = False
      val punctsmatch = pattern.puncts.match(t)
      val puncts = punctsmatch.group(1)
      val findsinglematch = pattern.findsingle.match(puncts)
      if (findsinglematch) {
        if (findsinglematch.start(1) == 0) {
          reflist.append(puncts(0))
          val t = t.substring(1)
          }
         else if (reference_punctuator_list.contains(findsinglematch.group(1))) {
          reflist.append(findsinglematch.group(1))
          val t = t.substring(findsinglematch.start(2))
          }
         else {
          println("Error:" + findsinglematch.group(1) + " not in reference_punctuator_list")
          return 0
          }
        }
       else {
        val u = punctsmatch.group(2)
        if (len(puncts) > 4 && puncts.substring(-5) == """\char""") {
          val nummatch = pattern.nums.match(u)
          if (nummatch) {
            reflist.append((puncts + nummatch.group(1)))
            val t = nummatch.group(2)
            }
           else {
            println("""Error: \char without number""")
            return 0
            }
          }
         else if (reference_punctuator_list.contains(puncts)) {
          reflist.append(puncts)
          val t = u
          }
         else {
          println("Error:" + puncts + "not in reference_punctuator_list")
          return 0
          }
        }
      }
    return reflist
    }
  
  def ruleparse(textline: Any): Any = {
    val rule = List()
    val rulevars = List()
    val rulesignature = List()
    val t = textline
    while (t) {
      val t = t.lstrip()
      if (! t) {
        break
        }
      if (t(0) == '$') {
        val TeXmatch = pattern.TeXmath.match(t)
        if (! TeXmatch) {
          println("Error: Unmatched Tex dollar sign")
          return 0
          }
        val rvar = workparse(TeXmatch.group(1))
        if (rvar == 0) {
          println("Error: Bad rule" + TeXmatch.group(1) + "in" + textline)
          return 0
          }
        rule.append(rvar)
        rulesignature.append('$')
        val t = TeXmatch.group(2)
        for (val x <- varlist(rvar)) {
          if (! rulevars.contains(x)) {
            rulevars.append(x)
            }
          }
        continue
        }
      val punctsmatch = pattern.puncts.match(t)
      val puncts = punctsmatch.group(1)
      val findsinglematch = pattern.findsingle.match(puncts)
      if (findsinglematch) {
        if (findsinglematch.start(1) == 0) {
          rule.append(puncts(0))
          rulesignature.append(puncts(0))
          val t = t.substring(1)
          }
         else if (reference_punctuator_list.contains(findsinglematch.group(1))) {
          rule.append(findsinglematch.group(1))
          rulesignature.append(findsinglematch.group(1))
          val t = t.substring(findsinglematch.start(2))
          }
         else {
          println("Error: " + findsinglematch.group(1) + " not in reference_punctuator_list")
          return 0
          }
        }
       else {
        val u = punctsmatch.group(2)
        if (len(puncts) > 4 && puncts.substring(-5) == """\char""") {
          val nummatch = pattern.nums.match(u)
          if (nummatch) {
            rule.append((puncts + nummatch.group(1)))
            rulesignature.append((puncts + nummatch.group(1)))
            val t = nummatch.group(2)
            }
           else {
            println("""Error: \char without number""")
            return 0
            }
          }
         else if (reference_punctuator_list.contains(puncts)) {
          rule.append(puncts)
          rulesignature.append(puncts)
          val t = u
          }
         else {
          println("Error: " + puncts + "not in reference_punctuator_list")
          return 0
          }
        }
      }
    if (rulesignature.contains("""\C""")) {
      val turnstyle_spot = rulesignature.index("""\C""")
      }
     else {
      println("Error: No turnstyle in rule")
      return 0
      }
    assert(len(rulevars) == len(set(rulevars)))
    return List(rule, rulevars, rulesignature)
    }
  
  def sigsig(parsedrule: Any): Any = {
    List(val rule, val rulevars, val sig) = parsedrule
    assert(len(rule) == len(sig))
    val depth = 0
    val retlist = List()
    for (val k <- range(len(sig))) {
      if (sig(k) == '(') {
        if (depth == 0) {
          val chunk = List()
          }
        val depth = (depth + 1)
        }
       else if (sig(k) == ')') {
        val depth = (depth - 1)
        if (depth == 0) {
          retlist.append(chunk)
          }
        }
       else if (sig(k) == '$') {
        val localvarlist = List()
        if (type(rule(k)) == list && len(rule(k)(0)) < 2) {
          assert(rule(k)(0) == List(11))
          val localvarlist = List(rule(k)(1))
          }
         else {
          for (val x <- nblist(rule(k))) {
            if (! localvarlist.contains(x)) {
              localvarlist.append(x)
              }
            }
          }
        if (depth == 0) {
          val chunk = localvarlist
          retlist.append(chunk)
          }
         else {
          for (val x <- localvarlist) {
            if (! chunk.contains(x)) {
              chunk.append(x)
              }
            }
          }
        }
       else if (reference_punctuator_list.contains(sig(k))) {
        assert(! sig(k).contains('(') && ! sig(k).contains(')'))
        if (depth == 0) {
          retlist.append(sig(k))
          }
        }
       else {
        println("Programming error:" + sig(k))
        }
      }
    return retlist
    }
  */

  def getformula(linetail: LineTail, verbose: Boolean = True): Any = {
    val mode = mutable.ArraySeq() :+ m2
    val parsetree = List()
    var fetched_tf = ""
    var stuff = ""
    while (linetail.tail && mode(0) == m2 || mode(0) == m3) {
      if (mode(0) == 2) {
        val TeXdollars = pattern.TeXdollar.search(linetail.tail)
        if (TeXdollars) {
          stuff = linetail.tail.substring(0, TeXdollars.start(1))
        } else {
          stuff = linetail.tail.strip()
        }
        fetched_tf = ((fetched_tf + ' ') + stuff)
        mathparse(mode, linetail, parsetree)
      } else if (mode(0) == m3) {
        mathmargin(mode, linetail)
      }
      if (!linetail.tail) {
        getline(linetail, verbose)
        while (linetail.tail(0) == '%') {
          getline(linetail, verbose)
        }
      }
    }
    if (mode(0) == m4) {
      return List()
    }
    if (mode(0) == m5) {
      println("Error: At most one term or formula allowed.")
      return List()
    }
    TODO /*@@
     val catch_ = parsetree(0)(0)(0)
    if (List(10, 11).contains(parsetree(0)(0)(0))) {
      return List(fetched_tf, linetail(0), parsetree(0)(1))
      }
    return List(fetched_tf, linetail(0), parsetree(0))
    * 
    */
  }

  case class LineTail(var tail: String, ix: Int, var line_num: Int, all: List[String])
  def getline(linetail: LineTail, verbose: Boolean = False): String = {
    // linetail = [tail of first line, index into tail, line number, list of all lines] 
    if (linetail.line_num == len(linetail.all)) {
      linetail.tail = ""
      return linetail.tail
    }
    linetail.tail = linetail.all(linetail.line_num)
    // line_num
    linetail.line_num = (linetail.line_num + 1)
    if (verbose && (linetail.line_num % 100) == 0) {
      print((linetail.line_num / 100))
      sys.stdout.flush()
    }
    return linetail.tail
  }

  /*@@
  def varlist(pexp: Any): Any = {
    if (type(pexp) == str) {
      if (List(10, 11, 12, 13).contains(symtype(pexp))) {
        return List(pexp)
        }
       else {
        return List()
        }
      }
     else if (type(pexp) == list) {
      val r = List()
      for (val t <- pexp) {
        val s = varlist(t)
        for (val u <- s) {
          if (! r.contains(u)) {
            r.append(u)
            }
          }
        }
      return r
      }
     else {
      return List()
      }
    }
  
  def norepeat_varlist(pexp: Any): Any = {
    if (type(pexp) == str) {
      if (List(10, 11, 12, 13).contains(symtype(pexp))) {
        return List(pexp)
        }
       else {
        return List()
        }
      }
     else if (type(pexp) == list) {
      val r = List()
      for (val t <- pexp) {
        val s = varlist(t)
        if (s == 0) {
          return 0
          }
        for (val u <- s) {
          if (r.contains(u)) {
            return 0
            }
           else {
            r.append(u)
            }
          }
        }
      return r
      }
     else {
      return List()
      }
    }
  
  def scopecondition(scopenode: Any): Any = {
    val state = 0
    val tree = List()
    addtoken(tree, '(')
    val verbfound = 0
    for (val xi <- scopenode.substring(1)) {
      if (type(xi) == list) {
        if (state == 1) {
          val state = 2
          }
        addnode(tree, xi)
        }
       else if (symtype(xi) == 14) {
        if (state == 1) {
          val state = 2
          }
        addtoken(tree, xi)
        }
       else if (symtype(xi) == 10) {
        if (state == 0) {
          /* pass */
          }
         else if (state == 1) {
          val state = 2
          }
        addtoken(tree, xi)
        }
       else if (xi == ',') {
        if (state == 2) {
          val state = 0
          addtoken(tree, """\And""")
          }
         else {
          addtoken(tree, xi)
          }
        }
       else if (symtype(xi) == 3) {
        if (state == 0) {
          val state = 1
          val verbfound = 1
          }
        addtoken(tree, xi)
        }
       else {
        throw new "Scope error"()
        }
      }
    if (verbfound) {
      addtoken(tree, ')')
      return tree(0)(2)
      }
     else {
      return List()
      }
    }
  
  def verbexpand(pexp: Any): Any = {
    if (type(pexp) == str || pexp(0)(1) != 6) {
      print(pexp)
      throw new " sent to verbexpand"()
      }
    val verblist = List()
    val termlist = List()
    val lastterm = List()
    val lastverb = List()
    val subjects = List()
    val conjunctlist = List()
    for (val x <- (pexp.substring(1) + List('='))) {
      if (x == ',') {
        if (lastterm == lastverb && lastverb == List()) {
          println("Initial commas and double commas not allowed")
          throw new SystemExit()
          }
         else if (lastterm == List()) {
          verblist.append(lastverb)
          val lastverb = List()
          }
         else if (lastverb == List()) {
          termlist.extend(lastterm)
          val lastterm = List()
          }
        }
       else if (type(x) == list || List(10, 14).contains(symtype(x))) {
        if (lastterm != List()) {
          throw new "Parse failure"()
          }
        val lastterm = List(x)
        if (lastverb != List()) {
          verblist.append(lastverb)
          val modifiers = verblist
          val lastverb = List()
          val verblist = List()
          }
         else if (verblist != List()) {
          throw new "Parse wrong"()
          }
        }
       else {
        if (lastverb == verblist && verblist == List()) {
          if (lastterm != List()) {
            termlist.extend(lastterm)
            // Here is the possible change!!!!!!!!!!!
            val lastterm = List()
            }
          if (len(termlist) == 1) {
            val object = termlist(0)
            }
           else {
            val object = List(List(44, 6))
            for (val y <- termlist) {
              object.append(y)
              object.append(',')
              }
            object -= -1}
          for (val s <- subjects) {
            val clause = List(List(45, 6))
            clause.append(s)
            clause.extend(modifiers(-1))
            clause.append(object)
            conjunctlist.append(clause)
            for (val t <- subjects) {
              if (t == s) {
                break
                }
              for (val r <- modifiers.substring(0, -1)) {
                val clause = List(List(45, 6))
                clause.append(t)
                clause.extend(r)
                clause.append(s)
                conjunctlist.append(clause)
                }
              }
            }
          if (len(termlist) > 1 && lastterm == List()) {
            val subjects = termlist
            }
           else {
            val subjects = List(object)
            }
          //
          //To accomodate Morse's tuple scheme uncomment this:		
          //
          // 				if len(termlist) == 1 and lastterm == []:
          // 					lastverb.append(',')
          //
          val lastterm = List()
          val termlist = List()
          }
        lastverb.append(x)
        }
      }
    if (verblist != List() || lastverb != List('=')) {
      println("Mistake")
      throw new SystemExit()
      }
    if (len(conjunctlist) == 1) {
      return conjunctlist(0)
      }
     else {
      if (mathdb(MD_CAOPS).contains("""\And""")) {
        val retlist = List(List(45, 5, -1), conjunctlist(0))
        }
       else {
        val retlist = List(List(45, 5), conjunctlist(0))
        }
      for (val x <- conjunctlist.substring(1)) {
        retlist.append("""\And""")
        retlist.append(x)
        }
      return retlist
      }
    }
  
  def workparse(strexp: Any): Any = {
    val parse_tree = List()
    val mode = List(2)
    mathparse(mode, List(strexp), parse_tree)
    if (mode(0) != 4) {
      return deep(parse_tree(0))
      }
     else {
      return 0
      }
    }
  
  def negdeep(exp: Any): Any = {
    val newexp = deep(exp)
    if (type(newexp) == str) {
      return newexp
      }
     else if (len(newexp) == 4 && newexp(2) == """\c""" && newexp(1) == """\true""") {
      return negdeep(newexp(3))
      }
    if (len(newexp) == 4 && newexp(2) == """\c""" && newexp(3) == """\false""") {
      if (type(newexp(1)) == list && len(newexp(1)) == 3 && newexp(1)(1) == """\Not""") {
        return newexp(1)(2)
        }
       else {
        return List(List(41, List(0)), """\Not""", newexp(1))
        }
      }
     else {
      return newexp
      }
    }
  
  def deep(exp: Any): Any = {
    val cmopp = cmop(exp)
    if (type(exp) == str) {
      return exp
      }
     else if (exp(1) == '(') {
      return deep(exp(2))
      }
     else if (List(14, 15).contains(exp(0)(0))) {
      return exp(1)
      }
     else //	elif len(exp) == 4 and exp[2] == '\\c' and exp[1] == '\\true':
    //		return deep(exp[3])
    if (exp(0)(0) == 45 && exp(0)(1) == 6) {
      val recurselist = List(exp(0))
      for (val r <- exp.substring(1)) {
        recurselist.append(deep(r))
        }
      return verbexpand(recurselist)
      }
     else if (cmopp) {
      val retlist = List(exp(0))
      for (val r <- exp.substring(1)) {
        val d = deep(r)
        if (cmop(d) == cmopp) {
          retlist.extend(d.substring(1))
          }
         else {
          retlist.append(d)
          }
        }
      return retlist
      }
     else //	elif exp[0] == [45,5,1]:
    //		retlist = [exp[0]]
    // 		for r in exp[1:]:
    //			d = deep(r)
    //			if type(d) is str: 
    //				retlist.append(d)
    //			elif d[0] == [45,5,1]:
    //				retlist.extend(d[1:])
    //			else:
    //				retlist.append(d)
    //		return retlist
    if (len(exp(0)) > 1 && List(40, 41).contains(exp(0)(0)) && List(3, 4, 5, 6, 7).contains(exp(0)(1))) {
      return notariancondense(exp)
      }
     else {
      val retlist = List(exp(0))
      for (val x <- exp.substring(1)) {
        retlist.append(deep(x))
        }
      return retlist
      }
    }
  
  def newvarlist(list: Any): Any = {
    /* global newvarnum */
    // This is only called by notariancondense when some variable
    // occurs in both indicial and accepted positions
    val r = List()
    for (val x <- list) {
      val newvarnum = (newvarnum + 1)
      r.append((("v_{" + ("%d" % newvarnum)) + '}'))
      }
    return r
    }
  
  def notariancondense(pexp: Any): Any = {
    val styp = symtype(pexp(1))
    val ntyp = pexp(0)(1)
    val indvs = indvlist(pexp)
    val accvs = accvlist(pexp)
    for (val x <- indvs) {
      if (accvs.contains(x)) {
        val nlist = newvarlist(indvs)
        val newform = indvsubst(nlist, indvs, pexp)
        val indvs = nlist
        break
        }
      }
    /* for ... else: */
    if ((indvs).isEmpty) {
      val newform = subst(List(), List(), pexp)
      }
    val newscope = List(List(48, List()), indvs(0))
    for (val x <- indvs.substring(1)) {
      newscope.append(',')
      newscope.append(x)
      }
    for (val x <- newform.substring(1)) {
      if (type(x) == str) {
        /* pass */
        }
       else if (x(0)(0) == 48) {
        val scopecond = scopecondition(x)
        }
      }
    if (ntyp == 4) {
      if (scopecond) {
        val scopecond = makeand(scopecond, deep(newform(-1)))
        }
       else {
        val scopecond = newform(-1)
        }
      }
     else if (List(5, 7).contains(ntyp)) {
      if (scopecond) {
        val scopecond = makeand(scopecond, deep(newform(-2)))
        }
       else {
        val scopecond = newform(-2)
        }
      }
    if (scopecond == List()) {
      if (styp == 8 || List(6, 7).contains(ntyp)) {
        val scopecond = """\true"""
        }
      }
    if (List(3, 5).contains(ntyp)) {
      val indform = newform(-1)
      }
     else if (ntyp == 4) {
      if (len(indvs) == 1) {
        val indform = indvs(0)
        }
       else {
        //We may never use this:
        val indform = (List(List(45, 6)) + newscope.substring(1))
        }
      }
     else {
      val indform = newform(2)
      }
    if (List(6, 7).contains(ntyp) && styp == 9) {
      if (ntyp == 7) {
        newform(2) = deep(newform(2))
        newform(4) = newscope
        newform(6) = deep(scopecond)
        }
       else if (ntyp == 6) {
        newform(0)(1) = 7
        newform(2) = deep(newform(2))
        newform(4) = newscope
        newform.substring(5, 5) = List(';', deep(scopecond))
        }
      }
     else if (styp == 8) {
      newform.substring(2) = List()
      newform(0)(1) = 5
      newform.append(newscope)
      newform.append(';')
      newform.append(deep(scopecond))
      newform.append(deep(indform))
      }
     else {
      newform.substring(2) = List()
      newform(0)(1) = 3
      val indform = deep(indform)
      if (newform(1) == """\Each""") {
        if (ntyp == 4) {
          throw new "Colon not expected"()
          }
         else if (type(indform) == list && indform(0) == List(45, 2) && indform(2) == """\c""" && scopecond) {
          indform(1) = makeand(deep(scopecond), indform(1))
          }
         else if (scopecond) {
          val indform = List(List(45, 2), deep(scopecond), """\c""", indform)
          }
        }
       else if (scopecond) {
        val indform = makeand(deep(scopecond), indform)
        }
      newform.append(newscope)
      newform.append(indform)
      if (len(indvs) > 1 && List("""\Each""", """\Some""").contains(newform(1))) {
        val indform = deep(newform(-1))
        val singlequant = newform.substring(0, 2)
        val rindvs = indvs.substring(0)
        rindvs.reverse()
        for (val x <- rindvs) {
          val newscope = List(List(48, List()), x)
          val newform = (singlequant + List(newscope))
          newform.append(indform)
          val indform = newform
          }
        }
      }
    return newform
    }
  
  def makeand(exp1: Any, exp2: Any): Any = {
    if (mathdb(MD_CAOPS).contains("""\And""")) {
      val andback = List(List(45, 5, -1))
      }
     else {
      val andback = List(List(45, 5))
      }
    if (cmop(exp1) == """\And""") {
      andback.extend(exp1.substring(1))
      }
     else {
      andback.append(exp1)
      }
    andback.append("""\And""")
    if (cmop(exp2) == """\And""") {
      andback.extend(exp2.substring(1))
      }
     else {
      andback.append(exp2)
      }
    return andback
    }
  
  def bvarreplace(form: Any, newbvarlist: Any): Any = {
    if (type(form) != list) {
      return form
      }
    val oldindvlist = indvlist(form)
    if (oldindvlist) {
      val newindvlist = List()
      for (val x <- oldindvlist) {
        newindvlist.append(newbvarlist.pop())
        }
      val nextform = indvsubst(newindvlist, oldindvlist, form)
      }
     else {
      val nextform = form.substring(0)
      }
    val retform = List(nextform(0))
    for (val x <- nextform.substring(1)) {
      retform.append(bvarreplace(x, newbvarlist))
      }
    return retform
    }
  
  def nfvlist(form: Any): Any = {
    "Return a list of variables with a non-free occurrence with repeats for distinct scopes"
    val retlist = List()
    if (type(form) != list) {
      return List()
      }
    if (type(form(0)(1)) == list && len(form(0)(1)) > 1) {
      val retlist = indvlist(form)
      for (val x <- form.substring(1)) {
        retlist.extend(nfvlist(x))
        }
      return retlist
      }
    for (val x <- form.substring(1)) {
      if (type(x) == str) {
        /* pass */
        }
       else if (x(0)(0) == 48) {
        val state = 0
        for (val xi <- x.substring(1)) {
          if (type(xi) == list) {
            if (state == 1) {
              val state = 2
              retlist.extend(nfvlist(xi))
              }
            }
           else if (symtype(xi) == 14) {
            if (state == 1) {
              val state = 2
              }
            }
           else if (symtype(xi) == 10) {
            if (state == 0) {
              retlist.append(xi)
              }
             else if (state == 1) {
              val state = 2
              }
            }
           else if (xi == ',') {
            if (state == 2) {
              val state = 0
              }
            }
           else if (symtype(xi) == 3) {
            if (state == 0) {
              val state = 1
              }
            }
           else {
            throw new "Scope error"()
            }
          }
        }
       else {
        retlist.extend(nfvlist(x))
        }
      }
    return retlist
    }
  @@*/

  def indvlist(form: BasicForm): List[String] = TODO /*{
    val retlist = List()
    if (type(form) != list) {
      return retlist
      }
    if (len(form(0)) > 1 && type(form(0)(1)) == list && len(form(0)(1)) > 1) {
      for (val x <- form(0)(1).substring(1)) {
        retlist.append(form(x))
        }
      return retlist
      }
    for (val x <- form.drop(1)) {
      if (type(x) == str) {
        /* pass */
        }
       else if (x(0)(0) == 48) {
        val state = 0
        for (val xi <- x.drop(1)) {
          if (type(xi) == list) {
            if (state == 1) {
              val state = 2
              }
            }
           else if (symtype(xi) == 14) {
            if (state == 1) {
              val state = 2
              }
            }
           else if (symtype(xi) == 10) {
            if (state == 0) {
              retlist.append(xi)
              }
             else if (state == 1) {
              val state = 2
              }
            }
           else if (xi == ',') {
            if (state == 2) {
              val state = 0
              }
            }
           else if (symtype(xi) == 3) {
            if (state == 0) {
              val state = 1
              }
            }
           else {
            println("form = " + form)
            println("x = " + x)
            println("xi = " + xi)
            throw new "Scope error"()
            }
          }
        }
      }
    return retlist
    }*/

  /*@@
  def indvsubst(inlist: Any, outlist: Any, form: Any): Any = {
    // It is expected that outlist = indvlist(form)
    // and that inlist consists of fresh bound variables
    if (type(form) != list) {
      if (inlist == List()) {
        return form
        }
       else {
        println(form)
        throw new "Not a bound variable form."()
        }
      }
    val newform = List(form(0))
    if (len(form(0)) == 1) {
      println("form = " + form)
      }
    if (type(form(0)(1)) == list && len(form(0)(1)) > 1) {
      val definiendum = mathdb(MD_DEFS)(form(1))(form(0)(1)(0))
      val indeflist = indvlist(definiendum)
      val ibvlist = definiendum(0)(1).substring(1)
      for (val n <- range(1, len(form))) {
        if (type(definiendum(n)) == str) {
          if (ibvlist.contains(n) && outlist.contains(form(n))) {
            newform.append(inlist(outlist.index(form(n))))
            }
           else {
            newform.append(form(n))
            }
          }
         else if (List(42, 43).contains(definiendum(n)(0)(0))) {
          val subinlist = List()
          val suboutlist = List()
          for (val m <- ibvlist) {
            if (definiendum(n).contains(definiendum(m))) {
              if (outlist.contains(form(m))) {
                subinlist.append(inlist(outlist.index(form(m))))
                suboutlist.append(form(m))
                }
               else {
                throw new "Bound variable error"()
                }
              }
            }
          newform.append(subst(subinlist, suboutlist, form(n)))
          }
         else {
          newform.append(form(n))
          }
        }
      return newform
      }
    for (val x <- form.substring(1)) {
      if (type(x) == str) {
        if (outlist.contains(x)) {
          newform.append(inlist(outlist.index(x)))
          }
         else {
          newform.append(x)
          }
        }
       else if (x(0)(0) == 48) {
        val accum_inlist = List()
        val accum_outlist = List()
        val new_inv = ""
        val new_outv = ""
        val newscope = List(x(0))
        val state = 0
        for (val xi <- x.substring(1)) {
          if (type(xi) == list) {
            if (state == 1) {
              val state = 2
              newscope.append(subst(accum_inlist, accum_outlist, xi))
              }
             else if (state == 2) {
              newscope.append(subst(accum_inlist, accum_outlist, xi))
              }
            }
           else if (symtype(xi) == 14) {
            if (state == 1) {
              val state = 2
              newscope.append(xi)
              }
             else if (state == 2) {
              newscope.append(xi)
              }
            }
           else if (symtype(xi) == 10) {
            if (state == 0) {
              if (new_outv) {
                accum_outlist.append(new_outv)
                }
              if (new_inv) {
                accum_inlist.append(new_inv)
                }
              if (outlist.contains(xi)) {
                val new_outv = xi
                val new_inv = inlist(outlist.index(xi))
                newscope.append(new_inv)
                }
               else {
                val new_outv = ""
                val new_inv = ""
                newscope.append(xi)
                }
              }
             else if (state == 1) {
              val state = 2
              newscope.append(xi)
              }
             else if (state == 2) {
              newscope.append(xi)
              }
            }
           else if (xi == ',') {
            if (state == 2) {
              val state = 0
              }
            newscope.append(xi)
            }
           else if (symtype(xi) == 3) {
            if (state == 0) {
              val state = 1
              }
            newscope.append(xi)
            }
           else {
            throw new "Scope error"()
            }
          }
        newform.append(newscope)
        }
       else {
        newform.append(subst(inlist, outlist, x))
        }
      }
    return newform
    }
  
  def accvlist(form: Any): Any = {
    // Only called on notarian forms
    val retlist = List()
    if (type(form) != list) {
      return retlist
      }
    for (val x <- form.substring(1)) {
      if (type(x) == str) {
        /* pass */
        }
       else if (x(0)(0) == 48) {
        val state = 0
        val newbv = ""
        val accum = List()
        for (val xi <- x.substring(1)) {
          if (type(xi) == list) {
            if (state == 1) {
              val state = 2
              }
            retlist.extend(nblist(xi, accum))
            }
           else if (symtype(xi) == 14) {
            if (state == 1) {
              val state = 2
              }
            }
           else if (symtype(xi) == 10) {
            if (state == 0) {
              if (newbv) {
                accum.append(newbv)
                }
              val newbv = xi
              }
            if (state == 1) {
              val state = 2
              }
            if (state == 2) {
              if (! accum.contains(xi)) {
                retlist.append(xi)
                }
              }
            }
           else if (xi == ',') {
            if (state == 2) {
              val state = 0
              }
            }
           else if (symtype(xi) == 3) {
            if (state == 0) {
              val state = 1
              }
            }
           else {
            println("form = " + form)
            println("x = " + x)
            println("xi = " + xi)
            throw new "Scope error"()
            }
          }
        }
      }
    return retlist
    }
  
  def subst(inlist: Any, outlist: Any, pexp: Any): Any = {
    " Indiscriminate string substitution "
    if (type(pexp) == list) {
      val r = List()
      for (val t <- pexp) {
        r.append(subst(inlist, outlist, t))
        }
      return r
      }
     else if (type(pexp) == str) {
      if (outlist.contains(pexp)) {
        return inlist(outlist.index(pexp))
        }
       else {
        return pexp
        }
      }
     else {
      //Numbers don't get trashed
      return pexp
      }
    }
  
  def cmop(form: Any): Any = {
    if (type(form) != list) {
      return ""
      }
    if (len(form(0)) < 3) {
      return ""
      }
    if (form(0)(2) != -1) {
      return ""
      }
    return form(2)
    }
  
  def nblist(form: Any, boundlist: Any = List()): Any = {
    val indvars = indvlist(form)
    if (type(form) == str) {
      if (boundlist.contains(form)) {
        return List()
        }
      if (List(10, 11, 12, 13).contains(symtype(form))) {
        if (pattern.bvar.match(form)) {
          return List()
          }
        return List(form)
        }
      return List()
      }
    //else	 type(form) is list: 
    assert(type(form(0)) == list)
    if (form(0) == List(11)) {
      return form.substring(1)
      }
    val retlist = List()
    if (type(form(0)(1)) == list && len(form(0)(1)) > 1) {
      val definiendum = mathdb(MD_DEFS)(form(1))(form(0)(1)(0))
      val ibvlist = definiendum(0)(1).substring(1)
      val addvars = List()
      for (val n <- range(1, len(form))) {
        if (type(definiendum(n)) == str) {
          if (ibvlist.contains(n)) {
            /* pass */
            }
           else if (List(10, 11).contains(symtype(definiendum(n)))) {
            addvars.append(definiendum(n))
            }
          }
         else if (List(42, 43).contains(definiendum(n)(0)(0))) {
          val suboutlist = List()
          for (val m <- ibvlist) {
            if (definiendum(n).contains(definiendum(m))) {
              suboutlist.append(form(m))
              }
            }
          addvars.extend(nblist(form(n), (boundlist + suboutlist)))
          }
         else {
          addvars.extend(nblist(form(n)), boundlist)
          }
        }
      for (val x <- addvars) {
        if (! retlist.contains(x)) {
          retlist.append(x)
          }
        }
      return retlist
      }
    for (val x <- form.substring(1)) {
      if (x(0)(0) == 48) {
        val state = 0
        val addvars = List()
        val accum = List()
        val newbv = ""
        for (val xi <- x.substring(1)) {
          if (type(xi) == list) {
            if (state == 1) {
              val state = 2
              val addvars = (addvars + nblist(xi, (boundlist + accum)))
              }
             else if (state == 2) {
              val addvars = (addvars + nblist(xi, (boundlist + accum)))
              }
            }
           else if (symtype(xi) == 14) {
            if (state == 1) {
              val state = 2
              }
            }
           else if (List(10, 11, 12, 13).contains(symtype(xi))) {
            if (state == 0) {
              if (newbv) {
                accum.append(newbv)
                }
              val newbv = xi
              }
             else if (state == 1) {
              val state = 2
              if (! boundlist.contains(xi)) {
                if (! accum.contains(x)) {
                  addvars.append(xi)
                  }
                }
              }
             else if (state == 2) {
              if (! boundlist.contains(xi)) {
                if (! accum.contains(x)) {
                  addvars.append(xi)
                  }
                }
              }
            }
           else if (xi == ',') {
            if (state == 2) {
              val state = 0
              }
            }
           else if (symtype(xi) == 3) {
            if (state == 0) {
              val state = 1
              }
            }
          }
        }
       else {
        val addvars = nblist(x, (boundlist + indvars))
        }
      for (val t <- addvars) {
        if (! retlist.contains(t)) {
          retlist.append(t)
          }
        }
      }
    return retlist
    }
  
  def getnote(linetail: Any, verbose: Any = False): Any = {
    val linelist = getnotelines(linetail, verbose)
    //	print linelist
    if (! linelist) {
      return List()
      }
    val mathparsed_note = linelist.pop()
    val workparse = deep(mathparsed_note)
    if (List(10, 11).contains(workparse(0)(0))) {
      val workparse = workparse(1)
      }
    val steplist = List(List(List()))
    val tempref = ""
    for (val t <- linelist) {
      if (t(0)) {
        steplist(-1).append(tempref)
        val tempref = t.pop()
        steplist.append(t)
        }
       else {
        if (tempref) {
          println("Lost reference found near" + linetail(2) + ':' + tempref)
          }
        val tempref = t.pop()
        steplist(-1).append(t(1))
        }
      }
    steplist(-1).append(tempref)
    if (len(steplist) == 1) {
      val textfragments = steplist(0).substring(1, -1)
      val thisref = steplist(0)(-1)
      return List(1, varlist(workparse), workparse, textfragments, thisref)
      }
    val chain = buildchain(steplist)
    if (chain == List()) {
      println("Note multi-line note error" + linetail(0))
      println(linetail(2))
      return 0
      }
    chain.append(varlist(workparse))
    return chain
    }
  // The following function returns a list whose
  // last element is just the mathparse of the note
  // and all preceeding elements are triples
  // [number, fetched note text, fetched reference text]
  // The number is positive if a transitive parse
  // is indicated. 
  // An empty return indicates an error.
  
  def getnotelines(linetail: Any, verbose: Any = False): Any = {
    val precedence = mathdb(MD_PRECED)
    val mode = List(2)
    val parsetree = List()
    val linelist = List()
    val lenstepconnector = -1
    val thisref = ""
    val stepcount = 0
    val linecopy = linetail(0).lstrip()
    while (1) {
      val TeXdollars = pattern.TeXdollar.search(linecopy)
      val dollar_spot = linecopy.find('$')
      val by_spot = linecopy.find("""\By""")
      val tokenm = pattern.token.match(linecopy)
      if (mode(0) == 1) {
        if (by_spot != -1) {
          val thisref = linecopy.substring((by_spot + 3)).strip()
          }
         else if (thisref) {
          val thisref = (thisref + linecopy)
          }
         else {
          val thisref = ""
          }
        linelist.append(List(lenstepconnector, newstuff, thisref))
        val parsed_item = parsetree(0)
        linelist.append(parsed_item)
        return linelist
        }
       else if (mode(0) == 2 && ! tokenm) {
        val newtag = List()
        //			print "linelist = ", linelist
        //			print "lenstepconnector = ", lenstepconnector
        //			print "newstuff = ", newstuff
        //			print "thisref = ", thisref
        //			print "linecopy = ", linecopy
        //			print "dollar_spot = ", dollar_spot
        //			print "newtag = ", newtag
        //			print "TeXdollars = ", TeXdollars 
        val linecopy = linecopy.substring((dollar_spot + 1))
        linetail(0) = linecopy
        mode(0) = 3
        }
       else if (mode(0) == 2) {
        //			tokenm = pattern.token.match(linecopy)
        val newtag = optag(linecopy, parsetree)
        val token = tokenm.group(2)
        val nexttokenlen = len(token)
        if (lenstepconnector != -1) {
          linelist.append(List(lenstepconnector, newstuff, thisref))
          }
        val thisref = ""
        if (TeXdollars) {
          val newstuff = linecopy.substring(0, TeXdollars.start(1))
          }
         else {
          val newstuff = linecopy
          }
        linetail(0) = linecopy
        if (newtag == List()) {
          val lenstepconnector = List()
          }
         else if (parsetree == List()) {
          println("Error in multi-line note")
          return List()
          }
         else //Check for preceding op
        if (opcoords(parsetree, precedence(token)) == (0, 0)) {
          if (len(linelist) > 2) {
            val reverse_linelist = linelist.substring(0)
            reverse_linelist.reverse()
            for (val t <- reverse_linelist) {
              if (t(0) != List()) {
                if (t(0)(1) < newtag(1) || t(0)(1) == newtag(1) && t(0)(2) < newtag(2)) {
                  break
                  }
                 else {
                  if (newtag(3) == """\c""") {
                    println("Consider using `True implies ...'")
                    }
                  println("Note parse error line: " + str(linetail(2)) + linetail(0))
                  return List()
                  }
                }
              }
            }
          val lenstepconnector = List()
          }
         else {
          val lenstepconnector = newtag
          }
        mathparse(mode, linetail, parsetree)
        }
       else if (mode(0) == 3) {
        if (! TeXdollars) {
          val by_spot = linecopy.find("""\By""")
          if (by_spot == -1) {
            val thisref = ((thisref + ' ') + linecopy.strip())
            }
           else {
            val thisref = ((thisref + ' ') + linecopy.substring((by_spot + 3)).strip())
            }
          }
        linetail(0) = linecopy
        notemargin(mode, linetail)
        }
       else if (mode(0) == 4) {
        println("Note parse error line: " + str(linetail(2)) + linetail(0))
        return List()
        }
       else if (mode(0) == 5) {
        println("Note fails to terminate: " + str(linetail(2)) + linetail(0))
        return List()
        }
      val linecopy = linetail(0).lstrip()
      if (! linecopy && mode(0) != 1) {
        getline(linetail, verbose)
        val linecopy = linetail(0).lstrip()
        }
      }
    }
  
  def optag(line: Any, node: Any): Any = {
    val precedence = mathdb(MD_PRECED)
    val original_len = len(line)
    val tokenm = pattern.token.match(line)
    val token = tokenm.group(2)
    val tokens = List()
    val precedence_list = List()
    while (symtype(token) < 4) {
      precedence_list.append(precedence(token))
      if (precedence(token) != precedence_list(0)) {
        break
        }
      tokens.append(token)
      val nexttokenlen = len(token)
      val line = line.substring(nexttokenlen)
      val line = line.lstrip()
      val tokenm = pattern.token.match(line)
      if (tokenm) {
        val token = tokenm.group(2)
        }
       else {
        break
        }
      }
    if (len(tokens) == 0) {
      return List()
      }
    val minimum_precedence = min(precedence_list)
    if (! List(2, 4, 6).contains(minimum_precedence)) {
      return List()
      }
    if (len(tokens) == 1) {
      return List(len(tokens(0)), len(node), minimum_precedence, tokens(0))
      }
     else if (len(tokens) > 1) {
      return List((original_len - len(line)), len(node), minimum_precedence, tuple(tokens))
      }
    }
  
  def buildchain(steplist: Any): Any = {
    val firstline = ' '.join(steplist(0).substring(1, -1))
    val treesg = List()
    val mode = List(2)
    mathparse(mode, List(firstline), treesg)
    val chain = List(List(parenclose(treesg), List(), steplist(0).substring(1, -1), steplist(0)(-1)))
    val stack = List(List(subst(List(), List(), treesg), 0, 0))
    val treedl = subst(List(), List(), treesg)
    val num_steps = len(steplist)
    steplist.append(List(List(0, 0, 0, "")))
    for (val k <- range(1, num_steps)) {
      val tran_tag = steplist(k)(0)
      val next_tran_tag = steplist((k + 1))(0)
      (val dpth, val prcd) = List(tran_tag(1), tran_tag(2))
      (val next_dpth, val next_prcd) = List(next_tran_tag(1), next_tran_tag(2))
      val op = tran_tag(3)
      val oplen = tran_tag(0)
      val op = steplist(k)(1).substring(0, oplen)
      val finish = ((steplist(k)(1).substring(oplen) + ' ') + ' '.join(steplist(k).substring(2, -1)))
      if (type(op) != str) {
        println("step = " + steplist(k))
        println("oplen = " + oplen)
        println("op = " + steplist(k)(1).substring(0, oplen))
        }
      mathparse(mode, List(op), treesg)
      mathparse(mode, List(op), treedl)
      deltadelete(treedl, prcd)
      val ok = sigmarevise(treesg, prcd)
      if (ok == 0) {
        return List()
        }
      val depth = len(treedl)
      val splitlist = finish.split(')')
      val len_splitlist = len(splitlist)
      val j = 0
      mathparse(mode, List(splitlist(j)), treedl)
      mathparse(mode, List(splitlist(j)), treesg)
      while ((j + 1) < len_splitlist && len(treedl) > depth) {
        val j = (j + 1)
        mathparse(mode, List((')' + splitlist(j))), treedl)
        mathparse(mode, List((')' + splitlist(j))), treesg)
        }
      if ((j + 1) < len_splitlist) {
        val splitlist = splitlist.substring((j + 1))
        }
       else {
        val splitlist = List()
        }
      val pr = treesg
      val qr = treedl
      val pq = pr
      val link = List(parenclose(treedl), List(), steplist(k).substring(1, -1), steplist(k)(-1))
      chain.append(link)
      while ((dpth, prcd) >= (next_dpth, next_prcd)) {
        val old_dpth = dpth
        val old_prcd = prcd
        if (! stack) {
          break
          }
        List(val last_treesg, val dpth, val prcd) = stack.pop()
        val qr = subst(List(), List(), treedl)
        val pq = subst(List(), List(), last_treesg)
        (val op_spot, val op_len) = opcoords(pr, old_prcd)
        if ((op_spot, op_len) == (0, 0)) {
          break
          }
        val op = pr(-1).substring(op_spot, (op_spot + op_len))
        val finish = pr(-1).substring((op_spot + op_len))
        val tail_prcd = pr(-1)(0)(1)
        paradecrop(last_treesg(-1), old_prcd)
        last_treesg(-1).extend(op)
        val pr = subst(List(), List(), last_treesg)
        sigmarevise(pr, old_prcd)
        pr(-1).extend(finish)
        pr(-1)(0)(1) = tail_prcd
        if (old_dpth > next_dpth) {
          while (len(pr) >= next_dpth && next_dpth > 0 && len(pr) > 1 && len(splitlist) > 0) {
            mathparse(mode, List((')' + splitlist(0))), pr)
            mathparse(mode, List((')' + splitlist(0))), treesg)
            val splitlist = splitlist.substring(1)
            }
          }
        link(1).append(List(parenclose(pr), parenclose(pq), parenclose(qr)))
        val treedl = pr
        }
      if (splitlist) {
        val close_off = (')' + ')'.join(splitlist))
        mathparse(mode, List(close_off), treedl)
        mathparse(mode, List(close_off), treesg)
        }
      stack.append(List(subst(List(), List(), treedl), dpth, prcd))
      }
    return chain
    }
  
  def parenclose(parsetree: Any): Any = {
    val copy_tree = subst(List(), List(), parsetree)
    val mode = List(2)
    while (mode(0) == 2) {
      mathparse(mode, List(')'), copy_tree)
      }
    if (mode(0) == 4) {
      return List()
      }
    return copy_tree(0)
    }
  
  def deltadelete(tree: Any, precedence: Any): Any = {
    val node = tree(-1)
    (val index, val length) = opcoords(tree, precedence)
    for (val n <- range((length + 1))) {
      node -= (index - 1)}
    return
    }
  
  def sigmarevise(tree: Any, precedence: Any): Any = {
    val node = tree(-1)
    (val index, val length) = opcoords(tree, precedence)
    if (length == 1) {
      val nexusop = node(index)(1)
      }
     else {
      val nexus_list = List()
      for (val i <- range(length)) {
        nexus_list.append(node((index + i))(1))
        }
      val nexusop = tuple(nexus_list)
      }
    val resultop = transopswap(nexusop, node(((index + length) + 1))(1))
    if (resultop == 0) {
      if (! mathdb(MD_RSFLG)) {
        /* pass */
        }
      }
     else //			print "Transitivity not established."
    if (type(resultop) == str) {
      node(-1)(1) = resultop
      }
     else {
      val resultnodes = List()
      for (val op <- resultop) {
        resultnodes.append(List(node(-1)(0), op, node(-1)(2)))
        }
      node.substring(-1) = resultnodes
      }
    for (val n <- range((length + 1))) {
      node -= index}
    return
    }
  
  def opcoords(parsetree: Any, precedence: Any): Any = {
    val node = parsetree(-1)
    //	precedence = node[0][1]
    val i = (len(node) - 1)
    while (i > 0) {
      val i = (i - 1)
      if (opnodep(node(i), precedence) && ! i > 0 && opnodep(node((i - 1)), precedence)) {
        break
        }
      }
    if (i == 0) {
      return (0, 0)
      }
    val k = 1
    while ((i + k) < len(node) && opnodep(node((i + k)), precedence)) {
      val k = (k + 1)
      }
    return (i, k)
    }
  
  def opnodep(node: Any, precedence: Any): Any = {
    return type(node) == list && type(node(0)) == list && node(0)(0) < 4 && node(2) == precedence
    }
  
  def opnodep(node: Any, precedence: Any): Any = {
    return type(node) == list && type(node(0)) == list && node(0)(0) < 4 && node(2) == precedence
    }
  
  def transopswap(op1: Any, op2: Any): Any = {
    val transitive_ops = mathdb(MD_TROPS)
    val trans_mult = mathdb(MD_TRMUL)
    val precedence = mathdb(MD_PRECED)
    if (op1 == op2) {
      if (transitive_ops.contains(op1)) {
        return op2
        }
       else {
        return 0
        }
      }
    if (op1 == """\ident""") {
      if (type(op2) == str) {
        if (List(1, 2, 3).contains(symtype(op2)) && precedence(op2) == 6) {
          return op2
          }
         else {
          return 0
          }
        }
       else {
        for (val x <- op2) {
          if (! List(1, 2, 3).contains(symtype(x)) || precedence(x) != 6) {
            return 0
            }
          }
        return op2
        }
      }
    if (op2 == """\ident""") {
      if (type(op1) == str) {
        if (List(1, 2, 3).contains(symtype(op1)) && precedence(op1) == 6) {
          return op1
          }
         else {
          return 0
          }
        }
       else {
        for (val x <- op1) {
          if (! List(1, 2, 3).contains(symtype(x)) || precedence(x) != 6) {
            return 0
            }
          }
        return op1
        }
      }
    if (op1 == '=') {
      if (type(op2) == str) {
        if (symtype(op2) == 3 && precedence(op2) == 6) {
          return op2
          }
         else {
          return 0
          }
        }
       else {
        for (val x <- op2) {
          if (symtype(x) != 3 || precedence(x) != 6) {
            return 0
            }
          }
        return op2
        }
      }
    if (op2 == '=') {
      if (type(op1) == str) {
        if (symtype(op1) == 3 && precedence(op1) == 6) {
          return op1
          }
         else {
          return 0
          }
        }
       else {
        for (val x <- op1) {
          if (symtype(x) != 3 || precedence(x) != 6) {
            return 0
            }
          }
        return op1
        }
      }
    if (op1 == """\Iff""" && op2 == """\c""") {
      return op2
      }
    if (op2 == """\c""" && op2 == """\Iff""") {
      return op1
      }
    if (trans_mult.keys().contains((op1, op2))) {
      return trans_mult((op1, op2))
      }
     else {
      return 0
      }
    }
  
  def definition_check(unparsed_exp: Any): Any = {
    assert(type(unparsed_exp) == str)
    val parsed_exp = stringparse(unparsed_exp)
    if (type(parsed_exp) == list && List(41, 51).contains(parsed_exp(0)(0)) && type(parsed_exp(2)) == list && len(parsed_exp(2)) == 4) {
      //      parsed_exp[2][0][0]  == 45 :
      val definiendum = parsed_exp(2)(1)
      val definor = parsed_exp(2)(2)
      val definiens = parsed_exp(2)(3)
      val left_vars = set(nblist(definiendum))
      val right_vars = set(nblist(definiens))
      //		if definor not in ['\\ident','\\Iff']:
      if (! List(1, 2).contains(symtype(definor))) {
        return """Definitions must have the form (p \Iff q) or (x \ident y)."""
        }
      if (left_vars == right_vars) {
        return
        }
       else if ((right_vars - left_vars)) {
        val error_message = "Dropped variables: "
        for (val x <- (right_vars - left_vars)) {
          val error_message = ((error_message + ' ') + x)
          }
        return error_message
        }
       else if ((left_vars - right_vars)) {
        val error_message = "Useless variables: "
        for (val x <- (left_vars - right_vars)) {
          val error_message = ((error_message + ' ') + x)
          }
        return error_message
        }
      if (parsed_exp(0)(0) == 51) {
        //			print "New form:"
        //			print "definiendum = ", parsed_exp[2][1]
        /* pass */
        }
      }
     else {
      println("parsed_exp[0][0] = " + parsed_exp(0)(0))
      println("len(parsed_exp[2]) = " + len(parsed_exp(2)))
      println("definiendum = " + parsed_exp(2)(1))
      println("definor = " + parsed_exp(2)(2))
      println("definiens = " + parsed_exp(2)(3))
      return """Definitions must have the form (p \Iff q) or (x \ident y)."""
      }
    println(definiendum)
    val ok = raw_input()
    return ok
    }
  
  def translate(linelist: Any, userdict: Any = List()): Any = {
    " Use the translation macros stored in the math data base
	 to translate the list of strings in linelist."
    if (userdict == List()) {
      val userdict = mathdb(MD_MACR)
      }
    // Define dictionary using function 
    
    def usedict(x: Any): Any = {
      if (x.group(2)) {
        return ""
        }
      val y = x.group(3)
      return ('\\' + userdict.get(y, y))
      }
    val newlines = List()
    val indollars = False
    for (val r <- linelist) {
      val comment_match = re.search(pattern.TeXcomment, r)
      if (comment_match) {
        val comment = r.substring(comment_match.start(1))
        val r = r.substring(0, comment_match.start(1))
        }
       else {
        val comment = ""
        }
      val splitlist = re.split(pattern.TeXdollar, r)
      assert((len(splitlist) % 2) == 1)
      val newr = ""
      for (val jj <- range(0, len(splitlist), 2)) {
        if (! indollars) {
          val newr = (newr + splitlist(jj))
          }
         else {
          val newr = (newr + re.sub(pattern.alphacontrolseq_or_skip, usedict, splitlist(jj)))
          }
        if ((jj + 1) == len(splitlist)) {
          continue
          }
        val newr = (newr + splitlist((jj + 1)))
        val indollars = ! indollars
        }
      val newr = (newr + comment)
      newlines.append(newr)
      }
    return newlines
    }
  
  def pprint(exp: Any, depth: Any = 0): Any = {
    if (type(exp) == str) {
      println(((depth * "  ") + exp))
      }
     else if ((len(str(exp)) + (depth * 2)) < 60) {
      println(((depth * "  ") + str(exp)))
      }
     else {
      println(((depth * "  ") + str(exp(0))))
      for (val t <- exp.substring(1)) {
        pprint(t, (depth + 1))
        }
      }
    return
    }
  
  def freshsub(vars_pexp: Any, takenvars: Any, fixedvars: Any = List()): Any = {
    /* global newvarnum */
    val newvlist = List()
    val oldvlist = List()
    for (val t <- vars_pexp) {
      if (fixedvars.contains(t)) {
        /* pass */
        }
       else if (takenvars.contains(t)) {
        oldvlist.append(t)
        val newvar = t
        while (takenvars.contains(newvar)) {
          val newvarnum = (newvarnum + 1)
          val st = symtype(t)
          val arity = mathdb(MD_ARITY)
          val pf = (("_{" + ("%d" % newvarnum)) + '}')
          if (st == 10) {
            val newvar = ('v' + pf)
            }
           else {
            val r = arity(t)
            if (st == 11) {
              val newvar = ("""\q^{0}""" + pf)
              }
             else if (st == 12) {
              val newvar = ((("""\w^{""" + ("%d" % r)) + '}') + pf)
              }
             else if (st == 13) {
              val newvar = ((("""\q^{""" + ("%d" % r)) + '}') + pf)
              }
            }
          }
        takenvars.append(newvar)
        newvlist.append(newvar)
        }
       else {
        takenvars.append(t)
        }
      }
    return List(newvlist, oldvlist)
    }
  //###########################################################
  //
  //  Get properties from file and store in db
  //
  //###########################################################
  
  def readprops(propfilename: Any, db: Any): Any = {
    //	print "len db = ", len(db)
    //	print "propfilename = ", propfilename
    val f = open(propfilename, 'r')
    val line_list = f.readlines()
    f.close()
    val transitive_ops = List()
    val trans_mult = Map()
    val commutative_associative_ops = List()
    val commutative_ops = List()
    val associative_ops = List()
    val chain_triplets = List()
    val linetail = List(line_list(0), 0, 1, line_list)
    val r = linetail(0)
    while (r) {
      val dollarm = pattern.dollar.match(r)
      if (dollarm) {
        val dollar_spot = dollarm.start(1)
        linetail(0) = dollarm.group(1)
        val reffed_item = getformula(linetail)
        if (! reffed_item) {
          println("Error in file" + propfilename + " :" + linetail(1))
          throw new SystemExit()
          }
        val parsed_item = deep(reffed_item(2))
        val op = detect_commutative_op(parsed_item)
        if (op) {
          commutative_ops.append(op)
          }
        val op = detect_associative_op(parsed_item)
        if (op) {
          associative_ops.append(op)
          }
        val optrip = detect_chain(parsed_item)
        if (optrip) {
          chain_triplets.append(optrip)
          }
        }
       else {
        getline(linetail)
        }
      val r = linetail(0)
      }
    //
    val commutative_associative_ops = List()
    val c = List()
    for (val x <- commutative_ops) {
      if (associative_ops.contains(x)) {
        commutative_associative_ops.append(x)
        }
       else {
        c.append(x)
        }
      }
    val commutative_ops = List()
    val transitive_ops = List("""\ident""", """\Iff""", '=')
    val transitive_mult = Map()
    for (val trip <- chain_triplets) {
      if (trip(0) == trip(1) && trip(1) == trip(2)) {
        if (! transitive_ops.contains(trip(0))) {
          transitive_ops.append(trip(0))
          }
        }
      }
    for (val trip <- chain_triplets) {
      if (transitive_ops.contains(trip(0)) && transitive_ops.contains(trip(1)) && transitive_ops.contains(trip(2))) {
        if (trip(0) != trip(2) || trip(1) != trip(2)) {
          if (! transitive_mult.keys().contains((trip(0), trip(1)))) {
            transitive_mult((trip(0), trip(1))) = trip(2)
            }
          }
        }
      }
    //precedence = syntdb[1]
    db(MD_TROPS) = transitive_ops
    db(MD_TRMUL) = transitive_mult
    db(MD_CAOPS) = commutative_associative_ops
    //	print "Commutative Associative Ops", commutative_associative_ops 
    //	print "Commutative Ops", commutative_ops
    //	print "Transitive Ops", transitive_ops
    //###########################################################
    if (len(db) > MD_THMS) {
      db(MD_THMS) = List()
      }
     else {
      //		print "Adding theorems from ", propfilename
      db.append(List())
      db.append(List())
      }
    //###########################################################
    //
    //  Re-parse the properties theorems
    //
    //###########################################################
    val mathdb = db
    val linetail = List(line_list(0), 0, 1, line_list)
    val theorems = List()
    val r = linetail(0)
    // Begin properties file pass
    while (r) {
      if (r(0) == '$') {
        linetail(0) = r.substring(1)
        val next_file_entry = getformula(linetail)
        if (next_file_entry) {
          val wp = deep(next_file_entry(2))
          }
         else {
          println("Error in properties file, line: " + linetail(1))
          throw new SystemExit()
          }
        theorems.append(List(wp, varlist(wp)))
        }
      getline(linetail)
      val r = linetail(0)
      }
    db(MD_THMS) = theorems
    db(MD_PFILE) = propfilename
    println(propfilename)
    return db
    }
  //
  //###############################################################
  //
  //             Read Properties Theorems
  //
  //###########################################################
  
  def binopnp(exp: Any): Any = {
    if (type(exp) == str) {
      return ""
      }
    if (len(exp) < 4) {
      return ""
      }
    if (type(exp(2)) == list) {
      return ""
      }
    for (val k <- range(2, (len(exp) - 1))) {
      if (type(exp(k)) == list) {
        return ""
        }
      if (! List(1, 2, 3).contains(symtype(exp(k)))) {
        return ""
        }
      }
    if (len(exp) == 4) {
      return exp(2)
      }
    return exp.substring(2, -1)
    }
  
  def detect_commutative_op(exp: Any): Any = {
    if (! List("""\ident""", """\Iff""").contains(binopnp(exp))) {
      return ""
      }
    val op1 = binopnp(exp(1))
    if (! "" != op1 && op1 == binopnp(exp(3))) {
      return ""
      }
    if (exp(1)(1) != exp(3)(-1)) {
      return ""
      }
    if (exp(3)(1) != exp(1)(-1)) {
      return ""
      }
    if (exp(1)(1) == exp(1)(-1)) {
      return ""
      }
    if (type(exp(1)(1)) == list || type(exp(1)(-1)) == list) {
      return ""
      }
    if (! symtype(exp(1)(1)) == symtype(exp(1)(-1)) && List(10, 11).contains(symtype(exp(1)(1)))) {
      return ""
      }
    return op1
    }
  
  def detect_chain(exp: Any): Any = {
    if (binopnp(exp) != """\c""") {
      return ""
      }
    if (binopnp(exp(1)) != """\And""") {
      return ""
      }
    val op1 = binopnp(exp(1)(1))
    val op2 = binopnp(exp(1)(3))
    val op3 = binopnp(exp(3))
    if (op1 == "") {
      println("Null" + exp(1)(1))
      }
    if (op2 == "") {
      println("Null" + exp(1)(3))
      }
    if (op3 == "") {
      println("Null" + exp(3))
      }
    if (op1 == "" || op2 == "" || op3 == "") {
      return ""
      }
    if (op3 != op1 && op3 != op2) {
      return ""
      }
    if (exp(1)(1)(3) != exp(1)(3)(1)) {
      return ""
      }
    if (exp(1)(1)(1) != exp(3)(1)) {
      return ""
      }
    if (exp(1)(3)(3) != exp(3)(3)) {
      return ""
      }
    if (exp(3)(1) == exp(1)(1)(3) || exp(3)(1) == exp(3)(3) || exp(1)(3)(3) == exp(3)(1)) {
      return ""
      }
    if (type(exp(3)(1)) != str) {
      return ""
      }
    if (type(exp(1)(3)(1)) != str) {
      return ""
      }
    if (type(exp(3)(3)) != str) {
      return ""
      }
    if (! symtype(exp(1)(1)(1)) == symtype(exp(1)(1)(3)) && List(10, 11).contains(symtype(exp(1)(1)(1)))) {
      return ""
      }
    if (! symtype(exp(1)(1)(1)) == symtype(exp(3)(3)) && List(10, 11).contains(symtype(exp(1)(1)(1)))) {
      return ""
      }
    return List(op1, op2, op3)
    }
  
  def detect_associative_op(exp: Any): Any = {
    if (! List("""\ident""", """\Iff""").contains(binopnp(exp))) {
      return ""
      }
    val op1 = binopnp(exp(1))
    if (! "" != op1 && op1 == binopnp(exp(3))) {
      return ""
      }
    val op11 = binopnp(exp(1)(1))
    if (op11) {
      if (op11 != op1) {
        return ""
        }
      if (op11 != binopnp(exp(3)(-1))) {
        return ""
        }
      if (type(exp(1)(-1)) != str) {
        return ""
        }
      if (type(exp(3)(1)) != str) {
        return ""
        }
      if (type(exp(1)(1)(-1)) != str) {
        return ""
        }
      if (exp(1)(1)(1) != exp(3)(1)) {
        return ""
        }
      if (exp(1)(1)(-1) != exp(3)(-1)(1)) {
        return ""
        }
      if (exp(1)(-1) != exp(3)(-1)(-1)) {
        return ""
        }
      if (! List(10, 11).contains(symtype(exp(1)(1)(1)))) {
        return ""
        }
      if (! List(10, 11).contains(symtype(exp(1)(1)(-1)))) {
        return ""
        }
      if (! List(10, 11).contains(symtype(exp(1)(-1)))) {
        return ""
        }
      return op1
      }
     else {
      val op31 = binopnp(exp(3)(1))
      if (op31 != op1) {
        return ""
        }
      if (op31 != binopnp(exp(1)(-1))) {
        return ""
        }
      if (type(exp(3)(-1)) != str) {
        return ""
        }
      if (type(exp(1)(1)) != str) {
        return ""
        }
      if (type(exp(3)(1)(-1)) != str) {
        return ""
        }
      if (exp(1)(1) != exp(3)(1)(1)) {
        return ""
        }
      if (exp(1)(-1)(1) != exp(3)(1)(-1)) {
        return ""
        }
      if (exp(1)(-1)(-1) != exp(3)(-1)) {
        return ""
        }
      if (! List(10, 11).contains(symtype(exp(1)(1)))) {
        return ""
        }
      if (! List(10, 11).contains(symtype(exp(1)(-1)(1)))) {
        return ""
        }
      if (! List(10, 11).contains(symtype(exp(1)(-1)(-1)))) {
        return ""
        }
      return op1
      }
    }
    * @@@@@@@@@@@@@ */
}
