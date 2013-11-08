package rpy2

object rinterface {
  import com.madmode.py2scala.__builtin__.TODO

  def set_writeconsole(w: Sexp => Unit) = TODO

  class Sexp

  class SexpVector extends Sexp with Seq[Sexp]

  class SexpClosure extends Sexp {
    def apply(args: Sexp*): Sexp = TODO
  }  

  def rternalize(f: Any): SexpClosure = TODO
}

/**
 * cribbed from http://rpy.sourceforge.net/rpy2/doc-2.3/html/robjects.html
 */
object robjects {
  import com.madmode.py2scala.__builtin__.{Dict, TODO}

  import rinterface.Sexp

  class IntVector(xs: Seq[Int]) extends Sexp with Seq[Int]
  class StrVector(xs: Seq[String]) extends Sexp with Seq[String]
  class ListVector(items: Dict[String, Sexp]) extends Sexp  with Iterable[Sexp] {
    def iteritems(): Dict[String, Sexp] = TODO
  }
  
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
    class Function extends rinterface.SexpClosure {
      def rcall(args: Sexp*) = apply(args : _*)
    }
  }

  // simulate dynamic typing a little
  object _dynamic {
    implicit def as_vec[T](x: Sexp): rinterface.SexpVector = TODO
    implicit def as_ints(x: Sexp): IntVector = TODO
    implicit def as_strs(x: Sexp): StrVector = TODO
    implicit def as_items(x: Sexp): ListVector = TODO
  }
}
