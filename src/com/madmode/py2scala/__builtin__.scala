package com.madmode.py2scala

/**
 * python runtime: builtin functions
 */
object __builtin__ {
  import java.io.BufferedReader
  import java.io.InputStreamReader
  import java.nio.charset.Charset
  import java.nio.file.Files
  import java.nio.file.Paths

  import scala.annotation.tailrec
  import scala.collection.immutable.VectorBuilder
  import scala.collection.{ Iterable, Iterator }
  import scala.collection.mutable.{ Map, HashMap }

  def TODO = throw new Exception("TODO")

  val True = true
  val False = false

  implicit def test_int(i: Int): Boolean = i != 0
  def range(lo: Int, hi: Int) = lo to hi

  implicit def test_string(s: String): Boolean = s != null && !s.isEmpty

  def int(s: String) = s.toInt
  def len[T](s: Seq[T]) = s.length
  implicit def test_list[T](s: Seq[T]): Boolean = s != null && !s.isEmpty

  implicit def as_string(c: Char): String = new String(c)
  implicit def as_py_char(c: Char): PyChar = new PyChar(c)
  class PyChar(c: Char) {
    def isalpha() = Character.isLetter(c) || Character.isDigit(c)
    def isdigit() = Character.isDigit(c)
  }

  def mod(fmt: String, v: Any*): String = TODO // s % v ; really batteries.operator.mod
  implicit def as_py_string(s: String): PyString = new PyString(s)
  class PyString(s: String) {
    def isalpha(): Boolean = s.forall(_.isalpha())
    def isdigit(): Boolean = s.forall(_.isdigit())
    def strip(): String = s.trim()
    def rstrip(): String = s.replaceAll("\\s+$", "")
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

  def isinstance[T](o: Object, c: java.lang.Class[T]) = c.isInstance(o)
  implicit def as_py_instance(o: Object): Instance = new Instance(o)
  class Instance(o: Object){
	  def __class__ = o.getClass()
  }
  implicit def as_py_class[T](cls: java.lang.Class[T]): Class_ [T] = new Class_ [T] (cls)
  class Class_ [T] (cls: java.lang.Class[T]){
	  def __name__ : String = TODO
  }

  val ascii = Charset.forName("US-ASCII")
  def open(path: String, mode: String = "r") = Files.newBufferedReader(Paths.get(path), ascii)
  import java.io.{ BufferedReader, PrintStream }
  implicit def as_py_file(fp: BufferedReader): File = new BRFile(fp)
  implicit def as_py_file(fp: PrintStream): File = TODO
  
  trait File extends Iterable[String] {
    def read(): String
    def readline(): String
    def readlines(): Vector[String]
    def write(s: String): Unit
    def flush(): Unit
    def close()
  }

  class BRFile(fp: BufferedReader) extends File {
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

    def write(s: String) = TODO
    def flush() = TODO
    
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

  def enumerate[A](x: Iterable[A]) = for ((a, i) <- x.zipWithIndex) yield (i, a)

  def with_ [T](obj: T)(blk: (T => Unit)) = {
    blk(obj)
  }
  
  def raw_input(prompt: String): String = {
    import java.io.{ BufferedReader, InputStreamReader }
    val b = new BufferedReader(new InputStreamReader(System.in))
    System.out.print(prompt)
    return b.readLine()
  }

  class NotImplementedError(msg: String) extends Exception(msg)
  class SystemExit extends Exception("SystemExit")
}
