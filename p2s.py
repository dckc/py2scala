'''p2s -- convert python lexical syntax to scala
'''

import ast
import logging

log = logging.getLogger(__name__)


def main(argv, open, stdout,
         level=logging.DEBUG):
    logging.basicConfig(level=level)
    infn = argv[1]
    convert(infn, open(infn).read(), stdout)


def convert(infn, src, out):
    t = ast.parse(src, infn)
    p2s = PyToScala(out)
    p2s.visit(t)


class PyToScala(ast.NodeVisitor):
    def __init__(self, out):
        self._out = out

    def visit_Module(self, node):
        wr = self._out.write
        wr('object __name__ {\n')
        for stmt in node.body:
            self.visit(stmt)
        wr('}\n')

    def visit_FunctionDef(self, node):
        limitation(not node.decorator_list)
        wr = self._out.write
        wr('\n\ndef %s(' % node.name)
        self.visit(node.args)
        wr('): Any = {\n')
        for stmt in node.body:
            self.visit(stmt)
        wr('}')

    def visit_Return(self, node):
        # Return(expr? value)
        wr = self._out.write
        wr('return')
        if node.value:
            wr(' ')
            self.visit(node.value)
        wr('\n')

    def visit_Delete(self, node):
        # Delete(expr* targets)
        for expr in node.targets:
            self.visit(expr)

    def visit_Assign(self, node):
        wr = self._out.write
        if len(node.targets) > 1:
            wr('(')
            for e in node.targets:
                self.visit(e)
            wr(')')
        else:
            self.visit(node.targets[0])

        wr(' = ')
        self.visit(node.value)
        wr('\n')

    def visit_Print(self, node):
        # Print(expr? dest, expr* values, bool nl)
        limitation(not node.dest)
        wr = self._out.write
        wr('println' if node.nl else 'print')
        wr('(')
        sep = ''
        for expr in node.values:
            wr(sep)
            self.visit(expr)
            sep = ' + '
        wr(')\n')

    def visit_For(self, node):
        # For(expr target, expr iter, stmt* body, stmt* orelse)
        wr = self._out.write
        wr('for (')
        self.visit(node.target)
        wr(' <- ')
        self.visit(node.iter)
        wr(') {\n')
        for stmt in node.body:
            self.visit(stmt)
        wr('}\n')

        if node.orelse:
            wr('/* for ... else: */\n')
            wr('if ((')
            self.visit(node.iter)
            wr(').isEmpty) {\n')
            for stmt in node.orelse:
                self.visit(stmt)
            wr('}\n')

    def visit_While(self, node):
        # While(expr test, stmt* body, stmt* orelse)
        limitation(not node.orelse)
        wr = self._out.write
        wr('while (')
        self.visit(node.test)
        wr(') {\n')
        for stmt in node.body:
            self.visit(stmt)
        wr('}\n')

    def visit_If(self, node):
        # If(expr test, stmt* body, stmt* orelse)
        wr = self._out.write
        wr('if (')
        self.visit(node.test)
        wr(') {\n')
        for stmt in node.body:
            self.visit(stmt)
        wr('}')
        if node.orelse:
            wr(' else {\n')
            for stmt in node.orelse:
                self.visit(stmt)
            wr('}')
        wr('\n')

    def visit_Raise(self, node):
        # Raise(expr? type, expr? inst, expr? tback)
        limitation(not node.tback and node.type)

        wr = self._out.write
        wr('throw new')
        self.visit(node.type)
        wr('(')
        if node.inst:
            self.visit(node.inst)
        wr(')\n')

    def visit_Assert(self, node):
        # Assert(expr test, expr? msg)
        wr = self._out.write
        wr('assert(')
        self.visit(node.test)
        if node.msg:
            wr(', ')
            self.visit(node.msg)
        wr(')\n')

    def visit_Import(self, node):
        wr = self._out.write
        for name in node.names:
            wr('import ')
            self.visit(name)
            wr('\n')

    def visit_Global(self, node):
        # Global(identifier* names)
        wr = self._out.write
        wr('/* global ')
        wr(', '.join(node.names))
        wr(' */\n')

    def visit_Expr(self, node):
        wr = self._out.write
        self.visit(node.value)
        wr('\n')

    def visit_Pass(self, node):
        wr = self._out.write
        wr('/* pass */\n')

    def visit_Break(self, node):
        wr = self._out.write
        wr('break\n')

    def visit_Continue(self, node):
        wr = self._out.write
        wr('continue\n')

    def visit_BoolOp(self, node):
        # BoolOp(boolop op, expr* values)
        # boolop = And | Or
        wr = self._out.write
        sym = {ast.And: '&&', ast.Or: '||'}[node.op.__class__]
        sep = ''
        for expr in node.values:
            wr(sep)
            self.visit(expr)
            sep = ' ' + sym + ' '

    def visit_BinOp(self, node):
        # BinOp(expr left, operator op, expr right)
        # operator = Add | Sub | Mult | Div | Mod | Pow | LShift 
        #         | RShift | BitOr | BitXor | BitAnd | FloorDiv
        wr = self._out.write
        wr('(')  # parens?
        self.visit(node.left)
        wr(' ')
        wr({ast.Add: '+', ast.Sub: '-', ast.Mult: '*', ast.Div: '/',
            ast.Mod: '%', ast.Pow: '**',
            ast.LShift: '<<', ast.RShift: '>>',
            ast.BitOr: '|', ast.BitXor: '@@', ast.BitAnd: '&',
            ast.FloorDiv: '@@'}[node.op.__class__])
        wr(' ')
        self.visit(node.right)
        wr(')')

    def visit_UnaryOp(self, node):
        # UnaryOp(unaryop op, expr operand)
        # unaryop = Invert | Not | UAdd | USub
        limitation(isinstance(node.op, ast.Not))

        wr = self._out.write
        wr('! ')
        self.visit(node.operand)

    def visit_Dict(self, node):
        wr = self._out.write
        wr('Map(')
        for k, v in zip(node.keys, node.values):
            self.visit(k)
            wr(' -> ')
            self.visit(v)
        wr(')')

    def visit_Compare(self, node):
        # Compare(expr left, cmpop* ops, expr* comparators)
        # compop = Eq | NotEq | Lt | LtE | Gt | GtE | Is | IsNot | In | NotIn
        wr = self._out.write
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
                sym = {ast.Eq: '=',
                       ast.NotEq: '!=',
                       ast.Lt: '<',
                       ast.LtE: '<=',
                       ast.Gt: '>',
                       ast.GtE: '>=',
                       ast.Is: '==',  # hmm...
                       ast.IsNot: '!='}[op.__class__]
                self.visit(left)
                wr(' ' + sym + ' ')
                self.visit(expr)
            left = expr
            sep = ' && '

    def visit_Call(self, node):
        # Call(expr func, expr* args, keyword* keywords,
	#		 expr? starargs, expr? kwargs)

        # TODO: change x.update(y) to x ++= y
        wr = self._out.write
        self.visit(node.func)
        wr('(')
        sep = ''
        for expr in node.args:
            wr(sep)
            self.visit(expr)
            sep = ', '
        wr(')')
        limitation(not node.keywords and
                   not node.starargs and
                   not node.kwargs)

    def visit_Num(self, node):
        wr = self._out.write
        wr(str(node.n))

    def visit_Str(self, node):
        wr = self._out.write
        s = node.s
        limitation('"""' not in s)

        esc = s.replace('\\', '\\\\')

        if '"' in s:
            wr('"""' + esc + '"""')
        else:
            wr('"' + esc + '"')

    def visit_Attribute(self, node):
        # Attribute(expr value, identifier attr, expr_context ctx)
        wr = self._out.write
        self.visit(node.value)
        wr('.')
        wr(node.attr)

    def visit_Subscript(self, node):
        # Subscript(expr value, slice slice, expr_context ctx)
        wr = self._out.write
        self.visit(node.value)
        slice = node.slice
        limitation(slice.__class__ in (ast.Index, ast.Slice))
        if isinstance(slice, ast.Index):
            limitation(node.ctx.__class__ in (ast.Load, ast.Store, ast.Del))
            if isinstance(node.ctx, ast.Del):
                wr(' -= ')
                self.visit(slice.value)
            else:
                wr('(')
                self.visit(slice.value)
                wr(')')
        else:
            limitation(node.ctx.__class__ in (ast.Load, ast.Store))
            # TODO: move this less-than-fully-general .substring()
            #       to a subclass, along with the .update() to ++= thingy.
            wr('.substring(')
            if slice.lower:
                self.visit(slice.lower)
            else:
                wr('0')
            if slice.upper:
                wr(', ')
                self.visit(slice.upper)

    def visit_Name(self, node):
        # hmm... ctx
        wr = self._out.write
        if isinstance(node.ctx, ast.Store):
            wr('val ')
        wr(node.id)

    def visit_List(self, node):
        self.visit_Tuple(node, o='List')

    def visit_Tuple(self, node, o=None):
        # Tuple(expr* elts, expr_context ctx)
        wr = self._out.write
        if o:
            wr(o)
        wr('(')
        sep = ''
        for expr in node.elts:
            wr(sep)
            self.visit(expr)
            sep = ', '
        wr(')')

    def visit_arguments(self, node):
        wr = self._out.write
        limitation(not node.vararg and
                   not node.kwarg)

        sep = ''
        for ix, expr in enumerate(node.args):
            wr(sep)
            self.visit(expr)
            wr(': Any')
            dx = ix - (len(node.args) - len(node.defaults))
            if dx >= 0:
                wr(' = ')
                self.visit(node.defaults[dx])
            sep = ', '

    def visit_alias(self, node):
        wr = self._out.write
        wr(node.name)
        if node.asname:
            wr(' as ')
            wr(node.asname)

    def generic_visit(self, node):
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
