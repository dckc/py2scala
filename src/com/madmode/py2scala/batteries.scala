package com.madmode.py2scala

/**
 * batteries -- python standard library
 */
object batteries {
  import com.madmode.py2scala.{__builtin__ => b}
  private def TODO = throw new Exception("TODO")

  object ast {
	  sealed abstract class AST {
	    def lineno: Int
	  }
	  abstract class expr extends AST
	  case class Str(s: String) extends expr
	  abstract class stmt extends AST
	  case class Expr(value: expr) extends stmt
	  abstract class Compound extends stmt { // made up name
	    def body: Seq[stmt]
	  }
	  abstract class Decorated extends Compound {
	    def decorator_list: Seq[expr]
	  }
	  type identifier = String
	  type arguments = AST /* TODO */
	  case class FunctionDef(name: identifier, args: arguments, 
                            body: Seq[stmt], decorator_list: Seq[expr]) extends Decorated
	  case class ClassDef(name: identifier, bases: Seq[expr], body: Seq[stmt],
                    decorator_list: Seq[expr]) extends Decorated
	  sealed abstract class operator
	  case class Add(left: expr, right: expr) extends operator
	  case class Sub(left: expr, right: expr) extends operator
	  
	  def parse(code: String, filename: String): AST = TODO
	  
	  class NodeVisitor {
	    def visit(t: AST): Unit = TODO
	  }
  }
  
  object logging {
    object Level extends Enumeration {
      type Level = Value
      val DEBUG, INFO, WARN, ERROR = Value
    }
    import Level._
    def basicConfig(level: Level): Unit = TODO
    def getLogger(which: String): Logger = TODO
    class Logger {
      def debug(msg: String, args: Any*): Unit = TODO
      def info(msg: String, args: Any*): Unit = TODO
    }
  }

  object os {
    def getenv(n: String): String = TODO

    def name: String = TODO

    def stat(path: String): Vector[Int] = TODO
    def popen(cmd: String): __builtin__.File = TODO

    object path {
      import java.nio.file.Files
      import java.nio.file.Paths
      def isdir(path: String): Boolean = TODO
      def isfile(filename: String): Boolean = Files.exists(Paths.get(filename))
      def join(x: String, y: String): String = Paths.get(x).resolve(Paths.get(y)).toString()
      def splitext(path: String): (String, String) = TODO
      def basename(path: String): String = TODO
    }
  }

  object pickle {
    import java.io.BufferedReader
    def load(path: BufferedReader): Any = TODO
  }

  object re {
    import scala.util.matching.Regex
    import java.util.regex.Matcher

    def compile(s: String): RegexObject = {
      new RegexObject(s)
    }

    class RegexObject(regex: String) extends Regex(regex) {
      def match_(s: String): Match = {
        val m = this.pattern matcher s
        runMatcher(m)
        new Match(m)
      }
    }

    class Match(impl: Matcher) {
      def test() = impl.matches()
      def group(i: Int) = impl.group(i)
      def groups() = 1 to impl.groupCount map impl.group
    }
    implicit def test_matcher(m: Match): Boolean = m.test()

  }

  object stat {
    lazy val ST_MTIME: Int = TODO
  }

  object StringIO {
    class StringIO(contents: String) extends b.File
  }
  
  object tokenize {
      type Token = (Int, String, (Int, Int), (Int, Int), String)
      def generate_tokens(readline: (() => String)): Stream[Token] = TODO
      
      
    object TokenType extends Enumeration {
      type TokenType = Value
      val COMMENT = Value
    }
    import TokenType._
  }

  object sys {}

}
