package com.madmode.py2scala

/**
 * batteries -- python standard library
 */
object batteries {
  private def TODO = throw new Exception("TODO")

  object os {
    def getenv(n: String): String = TODO

    def name: String = TODO

    def stat(path: String): Vector[Int] = TODO
    def popen(cmd: String): __builtins__.File = TODO

    object path {
      import java.nio.file.Files
      import java.nio.file.Paths
      def isdir(path: String): Boolean = TODO
      def isfile(filename: String): Boolean = Files.exists(Paths.get(filename))
      def join(x: String, y: String): String = Paths.get(x).resolve(Paths.get(y)).toString()
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

  object sys {

  }
}
