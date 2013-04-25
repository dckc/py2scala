package com.madmode.py2scala


/**
 * batteries -- python standard library
 */
object batteries {
  import com.madmode.py2scala.{ __builtin__ => b }
  import com.madmode.py2scala.__builtin__.TODO

  object ast {
    sealed abstract class AST {
      def lineno: Int
    }
    abstract class expr extends AST {
      override def lineno = TODO
    }
    case class Str(s: String) extends expr
    abstract class stmt extends AST {
      override def lineno = TODO
    }
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
    import java.nio.file.Files
    import java.nio.file.Paths
    def getenv(n: String): String = TODO

    def name: String = TODO

    def stat(path: String): Map[Int, Long] = {
      import com.madmode.py2scala.{ batteries => py }
      val mtime = Files.getLastModifiedTime(Paths.get(path))
      /* TODO fill in other slots */
      Map(py.stat.ST_MTIME -> mtime.toMillis())
    }
    def popen(cmd: String): __builtin__.File = TODO

    object path {
      def isdir(path: String): Boolean = TODO
      def isfile(filename: String): Boolean = Files.exists(Paths.get(filename))
      def join(x: String, y: String): String = Paths.get(x).resolve(Paths.get(y)).toString()
      def splitext(path: String): (String, String) = TODO
      def basename(path: String): String = TODO
    }
  }

  object pickle {
    def load(path: b.File): Object = TODO
    def dump(x: Any, f: b.File): Unit = TODO
  }

  object re {
    import scala.util.matching
    import java.util.regex.Matcher

    def compile(s: String): RegexObject = {
      new RegexObject(s)
    }

    class RegexObject(regex: String) extends matching.Regex(regex) {
      def match_(s: String): Match = {
        val m = this.pattern matcher s
        runMatcher(m)
        new JavaMatch(m)
      }
      def match_(s: String, offset: Int): Match = match_(s.drop(offset))
      
      def search(s: String): Match = {
        new ScalaMatch(this.findFirstMatchIn(s))
      }
    }

    trait Match {
      def test(): Boolean
      def group(i: Int): String
      def start(i: Int): Int
      def end(i: Int): Int
      def groups(): Seq[String]
    }
    implicit def test_matcher(m: Match): Boolean = m != null && m.test()

    class JavaMatch(impl: Matcher) extends Match {
      def test() = impl.matches()
      def start(i: Int) = TODO
      def end(i: Int) = TODO
      def group(i: Int) = impl.group(i)
      def groups() = 1 to impl.groupCount map impl.group
    }

    class ScalaMatch(impl: Option[matching.Regex.Match]) extends Match {
      def test() = !impl.isEmpty
      def group(i: Int) = impl.get.group(i)
      def start(i: Int) = TODO
      def end(i: Int) = TODO
      def groups() = {
        val m = impl.get
        1 to m.groupCount map m.group
      }
    }
  }

  object stat {
    val ST_MTIME = 8
  }

  object StringIO {
    class StringIO(contents: String) extends b.File {
      override def read() = contents
      override def readline() = TODO
      override def readlines() = TODO
      override def write(s: String) = TODO
      override def flush() = {}
      override def close() {}
      override def iterator() = TODO
    }

  }

  object sys {
    def stdout: b.File = TODO
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

}
