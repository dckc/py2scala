package com.madmode.pfmorris

import java.nio.charset.Charset
import java.nio.file.Files
import java.nio.file.Paths
import java.io.BufferedReader
import scala.annotation.tailrec
import scala.collection.mutable.{ Map, HashMap }
import scala.collection.immutable.VectorBuilder
import scala.collection.{ Iterable, Iterator }

/**
 * python runtime: builtin functions
 */
object __builtin__ {
  private def TODO = throw new Exception("TODO")

  val True = true
  val False = false

  implicit def test_int(i: Int): Boolean = i != 0

  implicit def test_string(s: String): Boolean = s != null && !s.isEmpty

  def int(s: String) = s.toInt
  def len(s: String) = s.length()
  def len[T](a: Array[T]) = a.length

  implicit def as_string(c: Char): String = new String(c)
  implicit def as_py_char(c: Char): PyChar = new PyChar(c)
  class PyChar(c: Char) {
    def isalpha() = Character.isLetter(c) || Character.isDigit(c)
    def isdigit() = Character.isDigit(c)
  }

  implicit def as_py_string(s: String): PyString = new PyString(s)
  class PyString(s: String) {
    def isalpha(): Boolean = s.forall(_.isalpha())
    def isdigit(): Boolean = s.forall(_.isdigit())
    def strip(): String = s.trim()
    def find(needle: String) = s.indexOf(needle)
    def find(needle: Char) = s.indexOf(needle)
    def startswith(prefix: String) = s.startsWith(prefix)
  }

  class Dict[K, V] extends HashMap[K, V] {
    def update(x: Dict[K, V]): Unit = { this ++= x }
    def get(k: K, default: V) = this.getOrElse(k, default)
  }
  object Dict {
    def apply[K, V]() = Map[K, V]()
  }
  implicit def test_dict[K, V](d: Dict[K, V]): Boolean = d != null && !d.isEmpty

  val ascii = Charset.forName("US-ASCII")
  def open(path: String, mode: String = "r") = Files.newBufferedReader(Paths.get(path), ascii)
  import java.io.BufferedReader
  implicit def as_py_file(fp: BufferedReader): File = new File(fp)
  class File(fp: BufferedReader) extends Iterable[String] {
    def close() = fp.close()
    def read(): String = TODO
    def readline(): String = fp.readLine()
    def readlines(): Vector[String] = {
      val b = new VectorBuilder[String]
      @tailrec
      def mk(): Unit = {
        val line = fp.readLine()
        if (line == null) ()
        else { b += line; mk() }
      }
      b.result()
    }

    override def iterator(): Iterator[String] = new LinesIterator(fp.readLine())
    class LinesIterator(var line: String) extends Iterator[String] {
      override def hasNext = line != null
      override def next() = {
        line = fp.readLine()
        line
      }
    }
  }

  def raw_input(prompt: String): String = {
    import java.io.{ BufferedReader, InputStreamReader }
    val b = new BufferedReader(new InputStreamReader(System.in))
    System.out.print(prompt)
    return b.readLine()
  }

  class SystemExit extends Exception
}

/**
 * batteries -- python standard library
 */
object batteries {
  private def TODO = throw new Exception("TODO")

  object os {
    def getenv(n: String): String = TODO

    def name: String = TODO

    def stat(path: String): Vector[Int] = TODO
    def popen(cmd: String): __builtin__.File = TODO

    object path {
      def isdir(path: String): Boolean = TODO
      def isfile(filename: String): Boolean = Files.exists(Paths.get(filename))
      def join(x: String, y: String): String = TODO
    }
  }

  object pickle {
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
