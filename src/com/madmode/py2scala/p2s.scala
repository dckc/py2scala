package com.madmode.py2scala

import __fileinfo__._
import com.madmode.py2scala.__builtin__._
import com.madmode.py2scala.batteries._

/** p2s -- convert python lexical syntax to scala

see also batteries.scala runtime support

ideas:
 - more general handling of x[y:z]
 - special case for docstrings

*/
object p2s {
  import StringIO
  import ast
  import logging
  import tokenize
  import os.path.{splitext, basename}
  import contextlib.{contextmanager}
  val log = logging.getLogger(__name__)
  
  def main(argv: Any, open: Any, stdout: Any, level = logging.DEBUG) = {
    logging.basicConfig(level=level)
    val List(pkg, infn) = if (List("--package") == argv.slice(1, 2)) { argv.slice(2, 4) } else { (None, argv(1)) }
    convert(pkg, infn, open(infn).read(), stdout)
    }
  
  def convert(pkg: Any, infn: Any, src: Any, out: Any) = {
    val modname = splitext(basename(infn))(0)
    val t = ast.parse(src, infn)
    val tl = PyToScala.tokens_per_line(src)
    val p2s = PyToScala(pkg, modname, out, tl)
    p2s.visit(t)
    }
  
  /** 
    http://docs.python.org/2/library/ast.html
    */
  class PyToScala extends ast.NodeVisitor {
    
    def __init__(self: Any, pkg: Any, modname: Any, out: Any, token_lines: Any) = {
      val self._pkg = pkg
      val self._modname = modname
      val self._out = out
      val self._col = 0
      val self._lines = list(token_lines)
      val self._line_ix = 0
      val self._node_row = 0
      }
    
    @classmethod
    def tokens_per_line(cls: Any, src: Any) = {
      val row = 1
      val line = List()
      val readline = StringIO.StringIO(src).readline
      for (tok <- tokenize.generate_tokens(readline)) {
        val (toknum, tokval, (tokrow, col), rc, l) = tok
        if (tokrow > row) {
          /* TODO */ yield_((row, line))
          val line = List()
          val row = tokrow
          }
        line.append(tok)
        }
      if (line) {
        /* TODO */ yield_((row, line))
        }
      }
    
    def _sync(self: Any, node: Any) = {
      val wr = self._out.write
      if (isinstance(node, ast.expr) || isinstance(node, ast.stmt)) {
        while (node.lineno >= self._lines(self._line_ix)(0)) {
          for (tok <- self._lines(self._line_ix)(1)) {
            if (tok(0) == tokenize.COMMENT) {
              wr(("//" + tok(1).drop(1)))
              self.newline()
              }
            }
          self._line_ix += 1
          }
        }
      return wr
      }
    
    @contextmanager
    def _block(self: Any) = {
      val wr = self._out.write
      self._col += 2
      wr("{\n")
      wr((" " * self._col))
      /* TODO */ yield_()
      self._col -= 2
      wr("}\n")
      wr((" " * self._col))
      }
    
    def newline(self: Any) = {
      val wr = self._out.write
      wr("\n")
      wr((" " * self._col))
      }
    
    def visit_Module(self: Any, node: Any, py2scala = "com.madmode.py2scala") = {
      val wr = self._out.write
      if (self._pkg) {
        wr(("package %s\n\n" % self._pkg))
        }
      wr("import __fileinfo__._\n")
      wr(("import %s.__builtin__._\n" % py2scala))
      wr(("import %s.batteries._\n\n" % py2scala))
      val (wr, body) = self._doc(node)
      wr(("object %s " % self._modname))
      self._suite(body)
      wr(("""
object __fileinfo__ {
  val __name__ = "%s%s"
}
""" % (if (self._pkg) { (self._pkg + ".") } else { "" }, self._modname)))
      }
    
    def _doc(self: Any, node: Any) = {
      val body = node.body
      assert(len(body) > 0)
      val wr = self._sync(node)
      val fst = body(0)
      if (isinstance(fst, ast.Expr)) {
        val v = fst.value
        if (isinstance(v, ast.Str)) {
          val doc = v.s
          limitation(! doc.contains("*/"))
          wr((("/** " + doc) + "*/"))
          self.newline()
          return (wr, body.drop(1))
          }
        }
      return (wr, body)
      }
    
    def _suite(self: Any, body: Any) = {
      with_ (self._block()) {
        case _ => {
          for (stmt <- body) {
            self.visit(stmt)
            }
          }
        }
      }
    
    def _items(self: Any, wr: Any, items: Any, parens = False) = {
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
    
    def _opt(self: Any, wr: Any, node: Any) = {
      if (node) {
        wr(", ")
        self.visit(node)
        }
      }
    
    /** FunctionDef(identifier name, arguments args, 
                            stmt* body, expr* decorator_list)
        */
    def visit_FunctionDef(self: Any, node: Any) = {
      self.newline()
      val (wr, body) = self._doc(node)
      self._decorators(node)
      wr(("def %s(" % node.name))
      self.visit(node.args)
      wr(") = ")
      self._suite(body)
      }
    
    def _decorators(self: Any, node: Any) = {
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
    def visit_ClassDef(self: Any, node: Any) = {
      self.newline()
      val (wr, body) = self._doc(node)
      self._decorators(node)
      wr(("class %s" % node.name))
      if (node.bases) {
        wr(" extends ")
        self._items(wr, node.bases)
        }
      wr(" ")
      self._suite(body)
      }
    
    /** Return(expr? value)
        */
    def visit_Return(self: Any, node: Any) = {
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
    def visit_Delete(self: Any, node: Any) = {
      for (expr <- node.targets) {
        self.visit(expr)
        }
      }
    
    /** Assign(expr* targets, expr value)
        */
    def visit_Assign(self: Any, node: Any) = {
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
    def visit_AugAssign(self: Any, node: Any) = {
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
    def visit_Print(self: Any, node: Any) = {
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
    def visit_For(self: Any, node: Any) = {
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
    def visit_While(self: Any, node: Any) = {
      limitation(! node.orelse)
      val wr = self._sync(node)
      wr("while (")
      self.visit(node.test)
      wr(") ")
      self._suite(node.body)
      }
    
    /** If(expr test, stmt* body, stmt* orelse)
        */
    def visit_If(self: Any, node: Any) = {
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
    def visit_With(self: Any, node: Any) = {
      val wr = self._sync(node)
      wr("with_ (")
      self.visit(node.context_expr)
      wr(") ")
      with_ (self._block()) {
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
    def visit_Raise(self: Any, node: Any) = {
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
    def visit_TryExcept(self: Any, node: Any) = {
      val wr = self._sync(node)
      wr("try ")
      self._suite(node.body)
      wr("catch ")
      with_ (self._block()) {
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
    def visit_Assert(self: Any, node: Any) = {
      val wr = self._sync(node)
      wr("assert(")
      self.visit(node.test)
      self._opt(wr, node.msg)
      wr(")")
      self.newline()
      }
    
    /** Import(alias* names)*/
    def visit_Import(self: Any, node: Any) = {
      val wr = self._sync(node)
      for (name <- node.names) {
        wr("import ")
        self.visit(name)
        self.newline()
        }
      }
    
    /** ImportFrom(identifier? module, alias* names, int? level)*/
    def visit_ImportFrom(self: Any, node: Any) = {
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
    def visit_Global(self: Any, node: Any) = {
      val wr = self._sync(node)
      wr("/* global ")
      wr(", ".join(node.names))
      wr(" */")
      self.newline()
      }
    
    def visit_Expr(self: Any, node: Any) = {
      self._sync(node)
      self.visit(node.value)
      self.newline()
      }
    
    def visit_Pass(self: Any, node: Any) = {
      val wr = self._sync(node)
      wr("/* pass */")
      self.newline()
      }
    
    def visit_Break(self: Any, node: Any) = {
      val wr = self._sync(node)
      wr("break")
      self.newline()
      }
    
    def visit_Continue(self: Any, node: Any) = {
      val wr = self._sync(node)
      wr("continue")
      self.newline()
      }
    
    /** BoolOp(boolop op, expr* values)
        boolop = And | Or
        */
    def visit_BoolOp(self: Any, node: Any) = {
      val wr = self._sync(node)
      val sym = Dict(ast.And -> "&&", ast.Or -> "||")(node.op.__class__)
      val sep = ""
      for (expr <- node.values) {
        wr(sep)
        self.visit(expr)
        val sep = ((" " + sym) + " ")
        }
      }
    val operator = Dict(ast.Add -> "+", ast.Sub -> "-", ast.Mult -> "*", ast.Div -> "/", ast.Mod -> "%", ast.Pow -> "**", ast.LShift -> "<<", ast.RShift -> ">>", ast.BitOr -> "|", //ast.BitXor: '@@',
    ast.BitAnd -> "&")
    
    //ast.FloorDiv: '@@'
    def _op(self: Any, op: Any) = {
      val cls = op.__class__
      limitation(self.operator.contains(cls))
      return self.operator(cls)
      }
    
    /** BinOp(expr left, operator op, expr right)
        operator = Add | Sub | Mult | Div | Mod | Pow | LShift 
                 | RShift | BitOr | BitXor | BitAnd | FloorDiv
        */
    def visit_BinOp(self: Any, node: Any) = {
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
    def visit_UnaryOp(self: Any, node: Any) = {
      limitation(isinstance(node.op, ast.Not))
      val wr = self._sync(node)
      wr("! ")
      self.visit(node.operand)
      }
    
    /** IfExp(expr test, expr body, expr orelse)
        */
    def visit_IfExp(self: Any, node: Any) = {
      val wr = self._sync(node)
      wr("if (")
      self.visit(node.test)
      wr(") { ")
      self.visit(node.body)
      wr(" } else { ")
      self.visit(node.orelse)
      wr(" }")
      }
    
    def visit_Dict(self: Any, node: Any) = {
      val wr = self._sync(node)
      wr("Dict(")
      for ((ix, (k, v)) <- enumerate(zip(node.keys, node.values))) {
        if (ix > 0) {
          wr(", ")
          }
        self.visit(k)
        wr(" -> ")
        self.visit(v)
        }
      wr(")")
      }
    
    /** Yield(expr? value)
        */
    def visit_Yield(self: Any, node: Any) = {
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
    def visit_Compare(self: Any, node: Any) = {
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
          val sym = Dict(ast.Eq -> "==", ast.NotEq -> "!=", ast.Lt -> "<", ast.LtE -> "<=", ast.Gt -> ">", ast.GtE -> ">=", ast.Is -> "eq", ast.IsNot -> "!=")(op.__class__)
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
    def visit_Call(self: Any, node: Any) = {
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
    
    def visit_Num(self: Any, node: Any) = {
      val wr = self._sync(node)
      wr(str(node.n))
      }
    
    def visit_Str(self: Any, node: Any) = {
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
    def visit_Attribute(self: Any, node: Any) = {
      val wr = self._sync(node)
      self.visit(node.value)
      wr(".")
      wr(node.attr)
      }
    
    /** Subscript(expr value, slice slice, expr_context ctx)
        */
    def visit_Subscript(self: Any, node: Any) = {
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
    
    def visit_Name(self: Any, node: Any) = {
      // hmm... ctx
      val wr = self._sync(node)
      wr(node.id)
      }
    
    def visit_List(self: Any, node: Any) = {
      val wr = self._sync(node)
      wr("List")
      self._items(wr, node.elts, parens=True)
      }
    
    /** Tuple(expr* elts, expr_context ctx)
        */
    def visit_Tuple(self: Any, node: Any) = {
      val wr = self._sync(node)
      self._items(wr, node.elts, parens=True)
      }
    
    /** (expr* args, identifier? vararg, 
		     identifier? kwarg, expr* defaults)
        */
    def visit_arguments(self: Any, node: Any) = {
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
    
    def visit_alias(self: Any, node: Any) = {
      val wr = self._sync(node)
      wr(node.name)
      if (node.asname) {
        wr(" as ")
        wr(node.asname)
        }
      }
    
    def generic_visit(self: Any, node: Any) = {
      import pdb
      pdb.set_trace()
      throw new NotImplementedError(("need visitor for: %s %s" % (node.__class__.__name__, node)))()
      }
    }
  
  def limitation(t: Any) = {
    if (! t) {
      import pdb
      pdb.set_trace()
      throw new NotImplementedError()
      }
    }
  if (__name__ == "__main__") {
    
    def _initial_caps() = {
      import sys
      return dict(argv=sys.argv, stdout=sys.stdout, open=open)
      }
    main(/* TODO kwargs */ _initial_caps())
    }
  }

object __fileinfo__ {
  val __name__ = "com.madmode.py2scala.p2s"
}
