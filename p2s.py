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

    def visit_Import(self, node):
        wr = self._out.write
        for name in node.names:
            wr('import ')
            self.visit(name)
            wr('\n')

    def visit_alias(self, node):
        wr = self._out.write
        wr(node.name)
        if node.asname:
            wr(' as ')
            wr(node.asname)

    def visit_Name(self, node):
        # hmm... ctx
        wr = self._out.write
        if isinstance(node.ctx, ast.Store):
            wr('val ')
        wr(node.id)

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

    def visit_List(self, node):
        wr = self._out.write
        wr('List(')
        for expr in node.elts:
            self.visit(expr)
            wr(', ')
        wr(')')

    def visit_Dict(self, node):
        wr = self._out.write
        wr('Map(')
        for k, v in zip(node.keys, node.values):
            self.visit(k)
            wr(' -> ')
            self.visit(v)
        wr(')')

    def visit_Num(self, node):
        wr = self._out.write
        wr(str(node.n))

    def visit_Str(self, node):
        wr = self._out.write
        s = node.s
        if '"""' in s:
            raise NotImplementedError('string: %s', s)

        esc = s.replace('\\', '\\\\')

        if '"' in s:
            wr('"""' + esc + '"""')
        else:
            wr('"' + esc + '"')

    def visit_Subscript(self, node):
        # Subscript(expr value, slice slice, expr_context ctx)
        wr = self._out.write
        self.visit(node.value)
        slice = node.slice
        if isinstance(slice, ast.Index):
            wr('(')
            self.visit(slice.value)
            wr(')')
        elif isinstance(slice, ast.Slice):
            wr('.substring(')
            if not slice.lower:
                raise NotImplemented('slice without lower')
            self.visit(slice.lower)
            if slice.upper:
                wr(', ')
                self.visit(slice.upper)
        else:
                raise NotImplemented('slice not Index nor Slice')

    def visit_arguments(self, node):
        wr = self._out.write
        if node.vararg or node.kwarg or node.defaults:
            raise NotImplemented('vararg or kwarg or defaults: %s' % node)

        sep = ''
        for expr in node.args:
            wr(sep)
            self.visit(expr)
            wr(': Any')
            sep = ', '

    def visit_FunctionDef(self, node):
        wr = self._out.write
        wr('def %s(' % node.name)
        self.visit(node.args)
        wr('): Any = {\n')
        for stmt in node.body:
            self.visit(stmt)

        if node.decorator_list:
            raise NotImplementedError(
                'decorator_list: %s' % node.decorator_list)

    def generic_visit(self, node):
        raise NotImplementedError('need visitor for: %s %s' %
                                  (node.__class__.__name__, node))


if __name__ == '__main__':
    def _initial_caps():
        import sys
        return dict(argv=sys.argv,
                    stdout=sys.stdout,
                    open=open)

    main(**_initial_caps())
