package com.madmode.pfmorris


/**
 * python runtime: builtin functions
 */
object __builtin__ {
  import scala.collection.mutable.{Map, ArrayBuffer}
  
  val True = true
  val False = false

  implicit def test_string(s: String): Boolean = !s.isEmpty
  implicit def test_int(i: Int): Boolean = i != 0
  
  def int(s: String) = s.toInt
  def len(s: String) = s.length()

  /** not sure about this one... */
  implicit def test_option[T](o: Option[T]): Boolean = !o.isEmpty

  class Dict[K, V] extends Map[K, V]{
    def update(x: Dict[K, V]): Unit = { this ++= x }
    def get(k: K, default: V) = this.getOrElse(k, default)
  }
  object Dict {
    def apply[K, V]() = Map[K, V]()
  }
  implicit def test_dict[K, V](d: Dict[K, V]): Boolean = !d.isEmpty

  def raw_input(prompt: String): String = {
    import java.io.{ BufferedReader, InputStreamReader }
    val b = new BufferedReader(new InputStreamReader(System.in))
    System.out.print(prompt)
    return b.readLine()
  }

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
  
  object sys {
    
  }
}
