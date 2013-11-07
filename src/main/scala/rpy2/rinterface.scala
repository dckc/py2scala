package rpy2

object rinterface {
  import com.madmode.py2scala.__builtin__.TODO

  def set_writeconsole(w: Sexp => Unit) = TODO
  def rternalize(f: Any): Sexp = TODO
  
  class Sexp {
    def apply(args: Sexp*): Sexp = TODO
  }
}

/**
 * cribbed from http://rpy.sourceforge.net/rpy2/doc-2.3/html/robjects.html
 */
object robjects {
  import com.madmode.py2scala.__builtin__.{Dict, TODO}

  import rinterface.Sexp

  class IntVector(xs: IndexedSeq[Int]) extends Sexp
  class StrVector(xs: IndexedSeq[String]) extends Sexp
  class ListVector(items: Dict[String, Sexp]) extends Sexp
  
  class R extends Dict[String, (Sexp*) => Sexp] with Function1[String, Sexp]{
    override def apply(expr: String): Sexp
    def globalenv(): Environment = TODO
  }
  
  val r: R = TODO
  
  class Environment {
    def get(item: String): Sexp
    def get(item: String, wantfun: Boolean): functions.Function
  }
  
  object functions {
    class Function {
      def rcall(args: Sexp*): Sexp = TODO
      def apply(args: Sexp*): Sexp = rcall(args : _*)
    }
  }
}