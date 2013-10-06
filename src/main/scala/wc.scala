import scala.collection.mutable
import com.madmode.py2scala.{batteries => py}
import com.madmode.py2scala.__builtin__._
import wc_fileinfo._

object wc {
  import py.StringIO
  
  /**
  >>> world = Mock()
  >>> _, stdout = world.with_caps(main)
  >>> stdout.getvalue()
  '3 f1\n'
  
  :type argv: IndexedSeq[String]
  :type open_arg: String => Iterable[String]
  :type stdout: File
  */
  def main(argv: IndexedSeq[String], stdout: File, open_arg: String => Iterable[String]) = {
    for (filename <- argv.drop(1)) {
      val stream = open_arg(filename)
      val qty = sum(for (line <- stream) yield 1)
      print(stdout, qty, filename)
      }
    }
  
  class Mock() {
    self =>
    
    /**
    :type f: (IndexedSeq[String], File, String => Iterable[String]) => Unit
    */
    def with_caps(f: (IndexedSeq[String], File, String => Iterable[String]) => Unit) = {
      val argv = mutable.IndexedSeq("prog", "f1")
      
      /**
      :type x: String
      */
      def open_arg(x: String) = {
        if (! argv.contains(x)) {
          throw new IOError()
          }
        mutable.IndexedSeq("line1", "line2", "line3")
        }
      val out = StringIO.StringIO()
      (f(argv.drop(0), out, open_arg _), out)
      }
    }
  if (__name__ == "__main__") {
    
    /**
    :type main: (IndexedSeq[String], File, String => Iterable[String]) => Unit
    */
    def _with_caps(main: (IndexedSeq[String], File, String => Iterable[String]) => Unit) = {
      import py.sys.{argv, stdout}
      
      /**
      :type arg: String
      */
      def open_arg(arg: String) = {
        if (! argv.contains(arg)) {
          throw new IOError("only paths given as arguments can be opened")
          }
        open(arg)
        }
      main(argv.drop(0), stdout, open_arg)
      }
    _with_caps(main _)
    }
  }

object wc_fileinfo {
  val __name__ = "wc"
  }
