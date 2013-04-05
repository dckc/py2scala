package com.madmode.py2scala

import __fileinfo__._
import com.madmode.py2scala.__builtin__._
import com.madmode.py2scala.{batteries => py}

/** p2s -- convert python lexical syntax to scala

see also batteries.scala runtime support

ideas:
 - more general handling of x[y:z]
 - special case for docstrings

*/
object p2s {
  import py.ast
  import py.tokenize
  import py.logging

  import py.os.path.{splitext, basename}

  val log = logging.getLogger(__name__)
  
  def main(argv: Array[String], open: (String => File), stdout: File, level: logging.Level.Level = logging.Level.DEBUG) {
    logging.basicConfig(level=level)
    val (pkg, infn) = if (List("--package") == argv.slice(1, 2)) { (Some(argv(2)), argv(3)) } else { (None, argv(1)) }
    convert(pkg, infn, open(infn).read(), stdout)
    }
  
  def convert(pkg: Option[String], infn: String, src: String, out: File) = {
    val modname = splitext(basename(infn))._1
    val t = ast.parse(src, infn)
    val tl = PyToScala.tokens_per_line(src)
    val p2s = new PyToScala(pkg, modname, out, tl)
    p2s.visit(t)
    }
  
  type LineInfo = (Int, Seq[tokenize.Token])
  type TokenLines = Seq[LineInfo]

  /** 
    http://docs.python.org/2/library/ast.html
    */
  class PyToScala(_pkg: Option[String], _modname: String, _out: File, _lines: TokenLines)
  extends ast.NodeVisitor {
    self =>

    var _col = 0
    var _line_ix = 0
    var _node_row = 0
    
    implicit def test_option[T](o: Option[T]): Boolean = !o.isEmpty

    def _sync(node: ast.AST) = {
      val wr = self._out.write _
      if (isinstance(node, classOf[ast.expr]) || isinstance(node, classOf[ast.stmt])) {
        while (node.lineno >= self._lines(self._line_ix)._1) {
          for (tok <- self._lines(self._line_ix)._2) {
            if (tok._1 == tokenize.TokenType.COMMENT.id) {
              wr(("//" + tok._2.drop(1)))
              self.newline()
              }
            }
          self._line_ix += 1
          }
        }
       wr
      }
    
    //@contextmanager
    def _block(yield_ : => Unit) = {
      val wr = self._out.write _
      self._col += 2
      wr("{\n")
      wr((" " * self._col))
      yield_
      self._col -= 2
      wr("}\n")
      wr((" " * self._col))
      }
    
    def newline() = {
      val wr = self._out.write _
      wr("\n")
      wr((" " * self._col))
      }
    
    def visit_Module(node: ast.Compound, py2scala: String = "com.madmode.py2scala"): Unit = {
      val wr = (self._out.write _)
      if (self._pkg) {
        wr(mod("package %s\n\n", self._pkg.get))
        }
      wr("import __fileinfo__._\n")
      wr(mod("import %s.__builtin__._\n", py2scala))
      wr(mod("import %s.batteries._\n\n", py2scala))
      val (_, body) = self._doc(node)
      wr(mod("object %s ", self._modname))
      self._suite(body)
      wr(mod("""
object __fileinfo__ {
  val __name__ = "%s%s"
}
""", if (self._pkg) { (self._pkg.get + ".") } else { "" }, self._modname))
      }
    
    def _doc(node: ast.Compound) = {
      val body = node.body
      assert(len(body) > 0)
      val wr = self._sync(node)
      val fst = body(0)
      fst match {
        case ast.Expr(ast.Str(doc)) => {
          limitation(! doc.contains("*/"))
          wr((("/** " + doc) + "*/"))
          self.newline()
          (wr, body.drop(1))
        }
        case _ => (wr, body)
      }
    }
    
    def _suite(body: Iterable[ast.stmt]) = {
      with_ (self._block()) {
        case _ => {
          for (stmt <- body) {
            self.visit(stmt)
            }
          }
        }
      }
    
    def _items(wr: (String => Unit), items: Iterable[ast.AST], parens: Boolean = False) = {
      if (parens) {
        wr("(")
        }
      for ((ix, i) <- enumerate(items)) {
        if (ix > 0) {
          wr(", ")
          }
        self.visit(i)
        }
      if (parens) {
        wr(")")
        }
      }
    
    def _opt(wr: (String => Unit), node: Option[ast.AST]) = {
      if (node) {
        wr(", ")
        self.visit(node.get)
        }
      }
    
    /** FunctionDef(identifier name, arguments args, 
                            stmt* body, expr* decorator_list)
        */
    def visit_FunctionDef(node: ast.FunctionDef) = {
      self.newline()
      val (wr, body) = self._doc(node)
      self._decorators(node)
      wr(mod("def %s(", node.name))
      self.visit(node.args)
      wr(") = ")
      self._suite(body)
      }
    
    def _decorators(node: ast.Decorated) = {
      val wr = self._sync(node)
      for (expr <- node.decorator_list) {
        wr("@")
        self.visit(expr)
        self.newline()
        }
      }
    
    /** ClassDef(identifier name, expr* bases, stmt* body,
                    expr* decorator_list)
        */
    def visit_ClassDef(node: ast.ClassDef) = {
      self.newline()
      val (wr, body) = self._doc(node)
      self._decorators(node)
      wr(mod("class %s", node.name))
      if (node.bases) {
        wr(" extends ")
        self._items(wr, node.bases)
        }
      wr(" ")
      self._suite(body)
      }
    
    /* TODO ... you get the idea; nothing novel
     * in the rest of these visit_ methods ...

    /** Return(expr? value)
        */
    def visit_Return(node: Any) = {
      val wr = self._sync(node)
      wr("return")
      if (node.value) {
        wr(" ")
        self.visit(node.value)
        }
      self.newline()
      }
    
    /** Delete(expr* targets)
        */
    def visit_Delete(node: Any) = {
      for (expr <- node.targets) {
        self.visit(expr)
        }
      }
    
    /** Assign(expr* targets, expr value)
        */
    def visit_Assign(node: Any) = {
      val wr = self._sync(node)
      wr("val ")
      if (len(node.targets) > 1) {
        self._items(node.targets, parens=True)
        }
       else {
        self.visit(node.targets(0))
        }
      wr(" = ")
      self.visit(node.value)
      self.newline()
      }
    
    /** AugAssign(expr target, operator op, expr value)
        */
    def visit_AugAssign(node: Any) = {
      self.visit(node.target)
      val wr = self._sync(node)
      wr(" ")
      wr(self._op(node.op))
      wr("= ")
      self.visit(node.value)
      self.newline()
      }
    
    /** Print(expr? dest, expr* values, bool nl)
        */
    def visit_Print(node: Any) = {
      limitation(! node.dest)
      val wr = self._sync(node)
      wr(if (node.nl) { "println" } else { "print" })
      wr("(")
      val sep = ""
      for (expr <- node.values) {
        wr(sep)
        self.visit(expr)
        val sep = " + "
        }
      wr(")")
      self.newline()
      }
    
    /** For(expr target, expr iter, stmt* body, stmt* orelse)
        */
    def visit_For(node: Any) = {
      val wr = self._sync(node)
      if (node.orelse) {
        wr("/* for ... else: */")
        self.newline()
        wr("if (!(")
        self.visit(node.iter)
        wr(").isEmpty) {")
        }
      wr("for (")
      self.visit(node.target)
      wr(" <- ")
      self.visit(node.iter)
      wr(") ")
      self._suite(node.body)
      if (node.orelse) {
        wr("} else ")
        self._suite(node.orelse)
        }
      }
    
    /** While(expr test, stmt* body, stmt* orelse)
        */
    def visit_While(node: Any) = {
      limitation(! node.orelse)
      val wr = self._sync(node)
      wr("while (")
      self.visit(node.test)
      wr(") ")
      self._suite(node.body)
      }
    
    /** If(expr test, stmt* body, stmt* orelse)
        */
    def visit_If(node: Any) = {
      val wr = self._sync(node)
      wr("if (")
      self.visit(node.test)
      wr(") ")
      self._suite(node.body)
      if (node.orelse) {
        val suite = node.orelse
        wr(" else ")
        if (len(suite) == 1 && isinstance(suite(0), ast.If)) {
          self.visit(suite(0))
          }
         else {
          self._suite(suite)
          }
        }
      }
    
    /** With(expr context_expr, expr? optional_vars, stmt* body)
        */
    def visit_With(node: Any) = {
      val wr = self._sync(node)
      wr("with_ (")
      self.visit(node.context_expr)
      wr(") ")
      with (self._block()) {
        case _ => {
          wr("case ")
          if (node.optional_vars) {
            self.visit(node.optional_vars)
            }
           else {
            wr("_")
            }
          wr(" => ")
          self._suite(node.body)
          }
        }
      }
    
    /** Raise(expr? type, expr? inst, expr? tback)
        */
    def visit_Raise(node: Any) = {
      limitation(! node.tback && node.type)
      val wr = self._sync(node)
      wr("throw new ")
      self.visit(node.type)
      wr("(")
      if (node.inst) {
        self.visit(node.inst)
        }
      wr(")")
      self.newline()
      }
    
    /**  TryExcept(stmt* body, excepthandler* handlers, stmt* orelse)
	excepthandler = ExceptHandler(expr? type, expr? name, stmt* body)
                        attributes (int lineno, int col_offset)
        */
    def visit_TryExcept(node: Any) = {
      val wr = self._sync(node)
      wr("try ")
      self._suite(node.body)
      wr("catch ")
      with (self._block()) {
        case _ => {
          for (excepthandler <- node.handlers) {
            wr("case ")
            if (excepthandler.name) {
              self.visit(excepthandler.name)
              }
             else {
              wr("_")
              }
            if (excepthandler.type) {
              wr(": ")
              self.visit(excepthandler.type)
              }
            wr(" => ")
            self._suite(excepthandler.body)
            }
          if (node.orelse) {
            wr("case _ =>")
            self._suite(excepthandler.orelse)
            }
          }
        }
      }
    
    /** Assert(expr test, expr? msg)
        */
    def visit_Assert(node: Any) = {
      val wr = self._sync(node)
      wr("assert(")
      self.visit(node.test)
      self._opt(wr, node.msg)
      wr(")")
      self.newline()
      }
    
    /** Import(alias* names)*/
    def visit_Import(node: Any) = {
      val wr = self._sync(node)
      for (name <- node.names) {
        wr("import ")
        self.visit(name)
        self.newline()
        }
      }
    
    /** ImportFrom(identifier? module, alias* names, int? level)*/
    def visit_ImportFrom(node: Any) = {
      val wr = self._sync(node)
      // what is that, anyway?
      limitation(node.level == 0)
      limitation(node.module)
      wr("import ")
      wr(node.module)
      wr(".{")
      self._items(wr, node.names)
      wr("}")
      self.newline()
      }
    
    /** Global(identifier* names)
        */
    def visit_Global(node: Any) = {
      val wr = self._sync(node)
      wr("/* global ")
      wr(", ".join(node.names))
      wr(" */")
      self.newline()
      }
    
    def visit_Expr(node: Any) = {
      self._sync(node)
      self.visit(node.value)
      self.newline()
      }
    
    def visit_Pass(node: Any) = {
      val wr = self._sync(node)
      wr("/* pass */")
      self.newline()
      }
    
    def visit_Break(node: Any) = {
      val wr = self._sync(node)
      wr("break")
      self.newline()
      }
    
    def visit_Continue(node: Any) = {
      val wr = self._sync(node)
      wr("continue")
      self.newline()
      }
    
    /** BoolOp(boolop op, expr* values)
        boolop = And | Or
        */
    def visit_BoolOp(node: Any) = {
      val wr = self._sync(node)
      val sym = Map(ast.And -> "&&"ast.Or -> "||")(node.op.__class__)
      val sep = ""
      for (expr <- node.values) {
        wr(sep)
        self.visit(expr)
        val sep = ((" " + sym) + " ")
        }
      }
	*/
    
    def operator = Map[Any, String](classOf[ast.Add] -> "+",
        classOf[ast.Sub] -> "-"
        /*@@@,
        classOf[ast.Mult] -> "*",
        classOf[ast.Div] -> "/",
        classOf[ast.Mod] -> "%",
        classOf[ast.Pow] -> "**",
        classOf[ast.LShift] -> "<<",
        classOf[ast.RShift] -> ">>",
        classOf[ast.BitOr] -> "|",
        //ast.BitXor: '@@',
        classOf[ast.BitAnd] -> "&"
            //ast.FloorDiv: '@@'
        */)
    

    def _op(op: ast.operator): String = {
      val cls = op.__class__
      limitation(self.operator.isDefinedAt(cls))
      return self.operator(cls)
      }
    
    /*@@@@@
    /** BinOp(expr left, operator op, expr right)
        operator = Add | Sub | Mult | Div | Mod | Pow | LShift 
                 | RShift | BitOr | BitXor | BitAnd | FloorDiv
        */
    def visit_BinOp(node: Any) = {
      val wr = self._sync(node)
      // parens necessary?
      wr("(")
      self.visit(node.left)
      wr(" ")
      wr(self._op(node.op))
      wr(" ")
      self.visit(node.right)
      wr(")")
      }
    
    /** UnaryOp(unaryop op, expr operand)
        unaryop = Invert | Not | UAdd | USub
        */
    def visit_UnaryOp(node: Any) = {
      limitation(isinstance(node.op, ast.Not))
      val wr = self._sync(node)
      wr("! ")
      self.visit(node.operand)
      }
    
    /** IfExp(expr test, expr body, expr orelse)
        */
    def visit_IfExp(node: Any) = {
      val wr = self._sync(node)
      wr("if (")
      self.visit(node.test)
      wr(") { ")
      self.visit(node.body)
      wr(" } else { ")
      self.visit(node.orelse)
      wr(" }")
      }
    
    def visit_Dict(node: Any) = {
      val wr = self._sync(node)
      wr("Map(")
      for ((k, v) <- zip(node.keys, node.values)) {
        self.visit(k)
        wr(" -> ")
        self.visit(v)
        }
      wr(")")
      }
    
    /** Yield(expr? value)
        */
    def visit_Yield(node: Any) = {
      val wr = self._sync(node)
      wr("/* TODO */ yield_(")
      if (node.value) {
        self.visit(node.value)
        }
      wr(")")
      }
    
    /** Compare(expr left, cmpop* ops, expr* comparators)
        compop = Eq | NotEq | Lt | LtE | Gt | GtE | Is | IsNot | In | NotIn
        */
    def visit_Compare(node: Any) = {
      val wr = self._sync(node)
      val sep = ""
      val left = node.left
      for ((op, expr) <- zip(node.ops, node.comparators)) {
        wr(sep)
        if ((ast.In, ast.NotIn).contains(op.__class__)) {
          if (op.__class__ == ast.NotIn) {
            wr("! ")
            }
          self.visit(expr)
          wr(".contains(")
          self.visit(node.left)
          wr(")")
          }
         else {
          val sym = Map(ast.Eq -> "=="ast.NotEq -> "!="ast.Lt -> "<"ast.LtE -> "<="ast.Gt -> ">"ast.GtE -> ">="ast.Is -> "eq"ast.IsNot -> "!=")(op.__class__)
          self.visit(left)
          wr(((" " + sym) + " "))
          self.visit(expr)
          }
        val left = expr
        val sep = " && "
        }
      }
    
    /** Call(expr func, expr* args, keyword* keywords,
                expr? starargs, expr? kwargs)
           keyword = (identifier arg, expr value)*/
    def visit_Call(node: Any) = {
      limitation(! node.starargs)
      val wr = self._sync(node)
      self.visit(node.func)
      wr("(")
      val ax = len(node.args)
      self._items(wr, node.args)
      val kx = 0
      if (node.keywords) {
        for ((kx, keyword) <- enumerate(node.keywords)) {
          if ((ax + kx) > 0) {
            wr(", ")
            }
          wr(keyword.arg)
          wr("=")
          self.visit(keyword.value)
          }
        }
      if (node.kwargs) {
        if ((ax + kx) > 0) {
          wr(", ")
          }
        wr("/* TODO kwargs */ ")
        self.visit(node.kwargs)
        }
      wr(")")
      }
    
    def visit_Num(node: Any) = {
      val wr = self._sync(node)
      wr(str(node.n))
      }
    
    def visit_Str(node: Any) = {
      val wr = self._sync(node)
      val s = node.s
      //@@limitation('\\u' not in s)
      if (s.contains("""\""") || s.contains("\"") && ! s.endswith("\"")) {
        wr((("\"\"\"" + s) + "\"\"\""))
        }
       else {
        for ((o, n) <- (("""\""", """\\"""), ("\n", """\n"""), ("\t", """\t"""), ("\"", """\""""))) {
          val s = s.replace(o, n)
          }
        wr((("\"" + s) + "\""))
        }
      }
    
    /** Attribute(expr value, identifier attr, expr_context ctx)
        */
    def visit_Attribute(node: Any) = {
      val wr = self._sync(node)
      self.visit(node.value)
      wr(".")
      wr(node.attr)
      }
    
    /** Subscript(expr value, slice slice, expr_context ctx)
        */
    def visit_Subscript(node: Any) = {
      val wr = self._sync(node)
      self.visit(node.value)
      val slice = node.slice
      val sk = slice.__class__
      val ctxk = node.ctx.__class__
      
      def lower0() = {
        if (slice.lower) {
          self.visit(slice.lower)
          }
         else {
          wr("0")
          }
        }
      if (sk == ast.Index && (ast.Load, ast.Store).contains(ctxk)) {
        wr("(")
        self.visit(slice.value)
        wr(")")
        }
       else if (sk == ast.Index && ctxk == ast.Del) {
        wr(" -= ")
        self.visit(slice.value)
        }
       else if (sk == ast.Slice && ctxk == ast.Load) {
        limitation(! slice.step)
        if (slice.upper) {
          wr(".slice(")
          lower0()
          wr(", ")
          self.visit(slice.upper)
          }
         else {
          wr(".drop(")
          self.visit(slice.lower)
          }
        wr(")")
        }
       else {
        limitation(True)
        }
      }
    
    def visit_Name(node: Any) = {
      // hmm... ctx
      val wr = self._sync(node)
      wr(node.id)
      }
    
    def visit_List(node: Any) = {
      val wr = self._sync(node)
      wr("List")
      self._items(wr, node.elts, parens=True)
      }
    
    /** Tuple(expr* elts, expr_context ctx)
        */
    def visit_Tuple(node: Any) = {
      val wr = self._sync(node)
      self._items(wr, node.elts, parens=True)
      }
    
    /** (expr* args, identifier? vararg, 
		     identifier? kwarg, expr* defaults)
        */
    def visit_arguments(node: Any) = {
      val wr = self._sync(node)
      for ((ix, expr) <- enumerate(node.args)) {
        if (ix > 0) {
          wr(", ")
          }
        self.visit(expr)
        val dx = (ix - (len(node.args) - len(node.defaults)))
        if (dx >= 0) {
          wr(" = ")
          self.visit(node.defaults(dx))
          }
         else {
          wr(": Any")
          }
        }
      if (node.vararg) {
        if (ix > 0) {
          wr(", ")
          }
        self.visit(node.vararg)
        wr(" : _*")
        ix += 1
        }
      if (node.kwarg) {
        if (ix > 0) {
          wr(", ")
          }
        wr((("/* TODO kwarg */ " + node.kwarg) + ": Dict[String, Any]"))
        }
      }
    
    def visit_alias(node: Any) = {
      val wr = self._sync(node)
      wr(node.name)
      if (node.asname) {
        wr(" as ")
        wr(node.asname)
        }
      }
    */
    
    def generic_visit(node: ast.AST) = {
      //import pdb
      //pdb.set_trace()
      throw new NotImplementedError(mod("need visitor for: %s %s", (node.__class__.__name__, node)))
      }
    }
  
  def limitation(t: Boolean) = {
    if (! t) {
      //import pdb
      //pdb.set_trace()
      throw new NotImplementedError("limitation")
      }
    }
  
  def main(args: Array[String]): Unit = {
    def _initial_caps() = {
      val out: File = System.out
      def open_f(path: String): File = open(path, "r")
      (args, open_f _, out)
      }
    val ic = _initial_caps()
    main(ic._1, ic._2, ic._3) /* TODO kwargs using runtime reflection */ 
    }
  }

object PyToScala {
  import scala.collection.immutable.VectorBuilder

  import py.tokenize
  import py.StringIO

    def tokens_per_line(src: String) = {
      val readline = new StringIO.StringIO(src).readline _
      val m = tokenize.generate_tokens(readline).groupBy {
        case (toknum, tokval, (tokrow, col), rc, l) => tokrow
      }
      m.toSeq
  }
}

object __fileinfo__ {
  val __name__ = "com.madmode.py2scala.p2s"
}
