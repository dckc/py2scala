'''p2s -- convert python lexical syntax to scala

see also batteries.scala runtime support

ideas:
 - more general handling of x[y:z]
 - special case for docstrings

'''

import StringIO
import ast
import logging
import tokenize
from os.path import splitext, basename
from contextlib import contextmanager

log = logging.getLogger(__name__)


def main(argv, open, stdout,
         level=logging.DEBUG):
    logging.basicConfig(level=level)
    [pkg, infn] = (argv[2:4] if ['--package'] == argv[1:2]
                   else (None, argv[1]))
    convert(pkg, infn, open(infn).read(), stdout)


def convert(pkg, infn, src, out):
    modname = splitext(basename(infn))[0]
    t = ast.parse(src, infn)
    tl = PyToScala.tokens_per_line(src)
    p2s = PyToScala(pkg, modname, out, tl)
    p2s.visit(t)


class PyToScala(ast.NodeVisitor):
    '''
    http://docs.python.org/2/library/ast.html
    '''
    def __init__(self, pkg, modname, out, token_lines):
        self._pkg = pkg
        self._modname = modname
        self._out = out
        self._col = 0
        self._lines = list(token_lines)
        self._line_ix = 0
        self._node_row = 0

    @classmethod
    def tokens_per_line(cls, src):
        row = 1
        line = []
        readline = StringIO.StringIO(src).readline
        for tok in tokenize.generate_tokens(readline):
            toknum, tokval, (tokrow, col), rc, l = tok
            if tokrow > row:
                yield (row, line)
                line = []
                row = tokrow
            line.append(tok)
        if line:
            yield (row, line)

    def _sync(self, node):
        wr = self._out.write
        if isinstance(node, ast.expr) or isinstance(node, ast.stmt):
            while node.lineno >= self._lines[self._line_ix][0]:
                for tok in self._lines[self._line_ix][1]:
                    if tok[0] == tokenize.COMMENT:
                        wr('//' + tok[1][1:])
                        self.newline()
                self._line_ix += 1
        return wr

    @contextmanager
    def _block(self):
        wr = self._out.write
        self._col += 2
        wr('{\n')
        wr(' ' * self._col)
        yield
        self._col -= 2
        wr('}\n')
        wr(' ' * self._col)

    def newline(self):
        wr = self._out.write
        wr('\n')
        wr(' ' * self._col)

    def visit_Module(self, node,
                     py2scala='com.madmode.py2scala'):
        wr = self._out.write
        if self._pkg:
            wr('package %s\n\n' % self._pkg)
        wr('import __fileinfo__._\n')
        wr('import %s.__builtin__._\n' % py2scala)
        wr('import %s.batteries._\n\n' % py2scala)
        
        wr, body = self._doc(node)
        wr('object %s ' % self._modname)
        self._suite(body)
        wr("""
object __fileinfo__ {
  val __name__ = "%s%s"
}
""" % (self._pkg + '.' if self._pkg else '', self._modname))

    def _doc(self, node):
        body = node.body
        assert(len(body) > 0)
        wr = self._sync(node)
        fst = body[0]
        if isinstance(fst, ast.Expr):
            v = fst.value
            if isinstance(v, ast.Str):
                doc = v.s
                limitation('*/' not in doc)
                wr('/** ' + doc + '*/')
                self.newline()
                return wr, body[1:]
        return wr, body

    def _suite(self, body):
        with self._block():
            for stmt in body:
                self.visit(stmt)

    def _items(self, wr, items, parens=False):
        if parens: wr('(')
        for ix, i in enumerate(items):
            if ix > 0: wr(', ')
            self.visit(i)
        if parens: wr(')')

    def _opt(self, wr, node):
        if node:
            wr(', ')
            self.visit(node)

    def visit_FunctionDef(self, node):
        '''FunctionDef(identifier name, arguments args, 
                            stmt* body, expr* decorator_list)
        '''
        self.newline()
        wr, body = self._doc(node)
        self._decorators(node)
        wr('def %s(' % node.name)
        self.visit(node.args)
        wr(') = ')
        self._suite(body)

    def _decorators(self, node):
        wr = self._sync(node)
        for expr in node.decorator_list:
            wr('@')
            self.visit(expr)
            self.newline()

    def visit_ClassDef(self, node):
        '''ClassDef(identifier name, expr* bases, stmt* body,
                    expr* decorator_list)
        '''
        self.newline()
        wr, body = self._doc(node)
        self._decorators(node)
        wr('class %s' % node.name)
        if node.bases:
            wr(' extends ')
            self._items(wr, node.bases)
        wr(' ')
        self._suite(body)

    def visit_Return(self, node):
        '''Return(expr? value)
        '''
        wr = self._sync(node)
        wr('return')
        if node.value:
            wr(' ')
            self.visit(node.value)
        self.newline()

    def visit_Delete(self, node):
        '''Delete(expr* targets)
        '''
        for expr in node.targets:
            self.visit(expr)

    def visit_Assign(self, node):
        '''Assign(expr* targets, expr value)
        '''
        wr = self._sync(node)
        wr('val ')
        if len(node.targets) > 1:
            self._items(node.targets, parens=True)
        else:
            self.visit(node.targets[0])

        wr(' = ')
        self.visit(node.value)
        self.newline()

    def visit_AugAssign(self, node):
        '''AugAssign(expr target, operator op, expr value)
        '''
        self.visit(node.target)
        wr = self._sync(node)
        wr(' ')
        wr(self._op(node.op))
        wr('= ')
        self.visit(node.value)
        self.newline()

    def visit_Print(self, node):
        '''Print(expr? dest, expr* values, bool nl)
        '''
        limitation(not node.dest)
        wr = self._sync(node)
        wr('println' if node.nl else 'print')
        wr('(')
        sep = ''
        for expr in node.values:
            wr(sep)
            self.visit(expr)
            sep = ' + '
        wr(')')
        self.newline()

    def visit_For(self, node):
        '''For(expr target, expr iter, stmt* body, stmt* orelse)
        '''
        wr = self._sync(node)
        if node.orelse:
            wr('/* for ... else: */')
            self.newline()
            wr('if (!(')
            self.visit(node.iter)
            wr(').isEmpty) {')

        wr('for (')
        self.visit(node.target)
        wr(' <- ')
        self.visit(node.iter)
        wr(') ')
        self._suite(node.body)

        if node.orelse:
            wr('} else ')
            self._suite(node.orelse)

    def visit_While(self, node):
        '''While(expr test, stmt* body, stmt* orelse)
        '''
        limitation(not node.orelse)
        wr = self._sync(node)
        wr('while (')
        self.visit(node.test)
        wr(') ')
        self._suite(node.body)

    def visit_If(self, node):
        '''If(expr test, stmt* body, stmt* orelse)
        '''
        wr = self._sync(node)
        wr('if (')
        self.visit(node.test)
        wr(') ')
        self._suite(node.body)
        if node.orelse:
            suite = node.orelse
            wr(' else ')
            if len(suite) == 1 and isinstance(suite[0], ast.If):
                self.visit(suite[0])
            else:
                self._suite(suite)

    def visit_With(self, node):
        '''With(expr context_expr, expr? optional_vars, stmt* body)
        '''
        wr = self._sync(node)
        wr('with_ (')
        self.visit(node.context_expr)
        wr(') ')
        with self._block():
            wr('case ')
            if node.optional_vars:
                self.visit(node.optional_vars)
            else:
                wr('_')
            wr(' => ')
            self._suite(node.body)

    def visit_Raise(self, node):
        '''Raise(expr? type, expr? inst, expr? tback)
        '''
        limitation(not node.tback and node.type)

        wr = self._sync(node)
        wr('throw new ')
        self.visit(node.type)
        wr('(')
        if node.inst:
            self.visit(node.inst)
        wr(')')
        self.newline()

    def visit_TryExcept(self, node):
        ''' TryExcept(stmt* body, excepthandler* handlers, stmt* orelse)
	excepthandler = ExceptHandler(expr? type, expr? name, stmt* body)
                        attributes (int lineno, int col_offset)
        '''
        wr = self._sync(node)
        wr('try ')
        self._suite(node.body)
        wr('catch ')
        with self._block():
            for excepthandler in node.handlers:
                wr('case ')
                if excepthandler.name:
                    self.visit(excepthandler.name)
                else:
                    wr('_')
                if excepthandler.type:
                    wr(': ')
                    self.visit(excepthandler.type)
                wr(' => ')
                self._suite(excepthandler.body)
            if node.orelse:
                wr('case _ =>')
                self._suite(excepthandler.orelse)
                
    def visit_Assert(self, node):
        '''Assert(expr test, expr? msg)
        '''
        wr = self._sync(node)
        wr('assert(')
        self.visit(node.test)
        self._opt(wr, node.msg)
        wr(')')
        self.newline()

    def visit_Import(self, node):
        """Import(alias* names)"""
        wr = self._sync(node)
        for name in node.names:
            wr('import ')
            self.visit(name)
            self.newline()

    def visit_ImportFrom(self, node):
        """ImportFrom(identifier? module, alias* names, int? level)"""
        wr = self._sync(node)
        limitation(node.level == 0)  # what is that, anyway?
        limitation(node.module)
        wr('import ')
        wr(node.module)
        wr('.{')
        self._items(wr, node.names)
        wr('}')
        self.newline()

    def visit_Global(self, node):
        '''Global(identifier* names)
        '''
        wr = self._sync(node)
        wr('/* global ')
        wr(', '.join(node.names))
        wr(' */')
        self.newline()

    def visit_Expr(self, node):
        self._sync(node)
        self.visit(node.value)
        self.newline()

    def visit_Pass(self, node):
        wr = self._sync(node)
        wr('/* pass */')
        self.newline()

    def visit_Break(self, node):
        wr = self._sync(node)
        wr('break')
        self.newline()

    def visit_Continue(self, node):
        wr = self._sync(node)
        wr('continue')
        self.newline()

    def visit_BoolOp(self, node):
        '''BoolOp(boolop op, expr* values)
        boolop = And | Or
        '''
        wr = self._sync(node)
        sym = {ast.And: '&&', ast.Or: '||'}[node.op.__class__]
        sep = ''
        for expr in node.values:
            wr(sep)
            self.visit(expr)
            sep = ' ' + sym + ' '

    operator = {ast.Add: '+', ast.Sub: '-', ast.Mult: '*', ast.Div: '/',
                ast.Mod: '%', ast.Pow: '**',
                ast.LShift: '<<', ast.RShift: '>>',
                ast.BitOr: '|',
                #ast.BitXor: '@@',
                ast.BitAnd: '&',
                #ast.FloorDiv: '@@'
                }

    def _op(self, op):
        cls = op.__class__
        limitation(cls in self.operator)
        return self.operator[cls]

    def visit_BinOp(self, node):
        '''BinOp(expr left, operator op, expr right)
        operator = Add | Sub | Mult | Div | Mod | Pow | LShift 
                 | RShift | BitOr | BitXor | BitAnd | FloorDiv
        '''
        wr = self._sync(node)
        wr('(')  # parens necessary?
        self.visit(node.left)
        wr(' ')
        wr(self._op(node.op))
        wr(' ')
        self.visit(node.right)
        wr(')')

    def visit_UnaryOp(self, node):
        '''UnaryOp(unaryop op, expr operand)
        unaryop = Invert | Not | UAdd | USub
        '''
        limitation(isinstance(node.op, ast.Not))

        wr = self._sync(node)
        wr('! ')
        self.visit(node.operand)

    def visit_IfExp(self, node):
        '''IfExp(expr test, expr body, expr orelse)
        '''
        wr = self._sync(node)
        wr('if (')
        self.visit(node.test)
        wr(') { ')
        self.visit(node.body)
        wr(' } else { ')
        self.visit(node.orelse)
        wr(' }')

    def visit_Dict(self, node):
        wr = self._sync(node)
        wr('Dict(')
        for ix, (k, v) in enumerate(zip(node.keys, node.values)):
            if ix > 0: wr(', ')
            self.visit(k)
            wr(' -> ')
            self.visit(v)
        wr(')')

    def visit_Yield(self, node):
        '''Yield(expr? value)
        '''
        wr = self._sync(node)
        wr('/* TODO */ yield_(')
        if node.value:
            self.visit(node.value)
        wr(')')

    def visit_Compare(self, node):
        '''Compare(expr left, cmpop* ops, expr* comparators)
        compop = Eq | NotEq | Lt | LtE | Gt | GtE | Is | IsNot | In | NotIn
        '''
        wr = self._sync(node)
        sep = ''
        left = node.left
        for op, expr in zip(node.ops, node.comparators):
            wr(sep)
            if op.__class__ in (ast.In, ast.NotIn):
                if op.__class__ == ast.NotIn:
                    wr('! ')
                self.visit(expr)
                wr('.contains(')
                self.visit(node.left)
                wr(')')
            else:
                sym = {ast.Eq: '==',
                       ast.NotEq: '!=',
                       ast.Lt: '<',
                       ast.LtE: '<=',
                       ast.Gt: '>',
                       ast.GtE: '>=',
                       ast.Is: 'eq',
                       ast.IsNot: '!='}[op.__class__]
                self.visit(left)
                wr(' ' + sym + ' ')
                self.visit(expr)
            left = expr
            sep = ' && '

    def visit_Call(self, node):
        '''Call(expr func, expr* args, keyword* keywords,
                expr? starargs, expr? kwargs)
           keyword = (identifier arg, expr value)'''

        limitation(not node.starargs)
        wr = self._sync(node)
        self.visit(node.func)
        wr('(')
        ax = len(node.args)
        self._items(wr, node.args)
        kx = 0
        if node.keywords:
            for kx, keyword in enumerate(node.keywords):
                if ax + kx > 0: wr(', ')
                wr(keyword.arg)
                wr('=')
                self.visit(keyword.value)
        if node.kwargs:
            if ax + kx > 0: wr(', ')
            wr('/* TODO kwargs */ ')
            self.visit(node.kwargs)
        wr(')')

    def visit_Num(self, node):
        wr = self._sync(node)
        wr(str(node.n))

    def visit_Str(self, node):
        wr = self._sync(node)
        s = node.s
        #@@limitation('\\u' not in s)

        if '\\' in s or '"' in s and not s.endswith('"'):
            wr('"""' + s + '"""')
        else:
            for o, n in (('\\', r'\\'),
                         ('\n', r'\n'), ('\t', r'\t'),
                         ('"', r'\"')):
                s = s.replace(o, n)
            wr('"' + s + '"')

    def visit_Attribute(self, node):
        '''Attribute(expr value, identifier attr, expr_context ctx)
        '''
        wr = self._sync(node)
        self.visit(node.value)
        wr('.')
        wr(node.attr)

    def visit_Subscript(self, node):
        '''Subscript(expr value, slice slice, expr_context ctx)
        '''
        wr = self._sync(node)
        self.visit(node.value)
        slice = node.slice
        sk = slice.__class__
        ctxk = node.ctx.__class__

        def lower0():
            if slice.lower:
                self.visit(slice.lower)
            else:
                wr('0')

        if sk == ast.Index and ctxk in (ast.Load, ast.Store):
            wr('(')
            self.visit(slice.value)
            wr(')')
        elif sk == ast.Index and ctxk == ast.Del:
            wr(' -= ')
            self.visit(slice.value)
        elif sk == ast.Slice and ctxk == ast.Load:
            limitation(not slice.step)
            if slice.upper:
                wr('.slice(')
                lower0()
                wr(', ')
                self.visit(slice.upper)
            else:
                wr('.drop(')
                self.visit(slice.lower)
            wr(')')
        else:
            limitation(True)

    def visit_Name(self, node):
        # hmm... ctx
        wr = self._sync(node)
        wr(node.id)

    def visit_List(self, node):
        wr = self._sync(node)
        wr('List')
        self._items(wr, node.elts, parens=True)

    def visit_Tuple(self, node):
        '''Tuple(expr* elts, expr_context ctx)
        '''
        wr = self._sync(node)
        self._items(wr, node.elts, parens=True)

    def visit_arguments(self, node):
        '''(expr* args, identifier? vararg, 
		     identifier? kwarg, expr* defaults)
        '''
        wr = self._sync(node)

        for ix, expr in enumerate(node.args):
            if ix > 0: wr(', ')
            self.visit(expr)
            dx = ix - (len(node.args) - len(node.defaults))
            if dx >= 0:
                wr(' = ')
                self.visit(node.defaults[dx])
            else:
                wr(': Any')
        if node.vararg:
            if ix > 0: wr(', ')
            self.visit(node.vararg)
            wr(' : _*')
            ix += 1
        if node.kwarg:
            if ix > 0: wr(', ')
            wr('/* TODO kwarg */ ' + node.kwarg + ': Dict[String, Any]')

    def visit_alias(self, node):
        wr = self._sync(node)
        wr(node.name)
        if node.asname:
            wr(' as ')
            wr(node.asname)

    def generic_visit(self, node):
        import pdb; pdb.set_trace()
        raise NotImplementedError('need visitor for: %s %s' %
                                  (node.__class__.__name__, node))


def limitation(t):
    if not t:
        import pdb; pdb.set_trace()
        raise NotImplementedError


if __name__ == '__main__':
    def _initial_caps():
        import sys
        return dict(argv=sys.argv,
                    stdout=sys.stdout,
                    open=open)

    main(**_initial_caps())
