package com.madmode.pfmorris


/**
 * python runtime: builtin functions
 */
object __builtin__ {
  def raw_input(prompt: String): String = {
    import java.io.{ BufferedReader, InputStreamReader }
    val b = new BufferedReader(new InputStreamReader(System.in))
    System.out.print(prompt)
    return b.readLine()
  }

  implicit def test_string(s: String): Boolean = !s.isEmpty
}

/**
 * batteries -- python standard library
 */
object batteries {
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
}
