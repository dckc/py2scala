package com.madmode.py2scala

/**
 * python runtime: builtin functions
 */
object __builtins__ {
  import java.io.BufferedReader
  import java.io.InputStreamReader
  import java.nio.charset.Charset
  import java.nio.file.Files
  import java.nio.file.Paths

  import scala.annotation.tailrec
  import scala.collection.immutable.VectorBuilder
  import scala.collection.{ Iterable, Iterator }
  import scala.collection.mutable.{ Map, HashMap }

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
      mk()
      b.result()
    }

    override def iterator(): Iterator[String] = new LinesIterator(fp.readLine())
    class LinesIterator(var line: String) extends Iterator[String] {
      override def hasNext = line != null
      override def next() = {
        val l = line
        line = fp.readLine()
        l
      }
    }
  }

  def raw_input(prompt: String): String = {
    import java.io.{ BufferedReader, InputStreamReader }
    val b = new BufferedReader(new InputStreamReader(System.in))
    System.out.print(prompt)
    return b.readLine()
  }

  class SystemExit extends Exception("SystemExit")
}
