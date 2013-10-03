import __fileinfo__._
import com.madmode.py2scala.__builtin__._
import com.madmode.py2scala.{batteries => py}

object wc {
  import py.StringIO
  
  /** 
    >>> caps = Mock.caps()
    >>> main(**caps)
    >>> caps['stdout'].getvalue()
    '3 f1\n'

    :type argv: IndexedSeq[String]
    :type open_arg: String => File
    :type stdout: File
    */
  def main(argv: IndexedSeq[String], open_arg: String => File, stdout: File) = {
    for (filename <- argv.drop(1)) {
      val stream = open_arg(filename)
      val qty = sum(for (line <- stream) yield 1)
      stdout.println(qty + filename)
      }
    }
  
  class Mock {
    
    /*@@ @classmethod */
    def caps(/*cls: Any*/) = {
      val argv = List("prog", "f1")
      
      /** :type x: String*/
      def open_arg(x: String) = {
        if (! argv.contains(x)) {
          throw new IOError("FIXME")
          }
        List("line1", "line2", "line3")
        }
      val out = StringIO.StringIO("KLUDGE")
      dict(argv=argv.clone(), stdout=out, open_arg=open_arg)
      }
    }
  if (__name__ == "__main__") {
    
    def _with_caps() = {
      import py.sys.{argv, stdout}
      
      /** :type arg: String*/
      def open_arg(arg: String) = {
        if (! argv.contains(arg)) {
          throw new IOError("only paths given as arguments can be opened")
          }
        open(arg)
        }
      
      /** 
            :type main: (IndexedSeq[String], File, String => File) => Unit
            */
      def with_caps(main: (IndexedSeq[String], File, String => File) => Unit) = {
        main(argv=argv.clone(), stdout=stdout, open_arg=open_arg)
        }
      with_caps _
      }
    _with_caps(main)
    }
  }

object __fileinfo__ {
  val __name__ = "wc"
}
