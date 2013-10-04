import StringIO
from functools import partial as pf_


def main(argv, stdout, open_arg):
    r'''
    >>> world = Mock()
    >>> _, stdout = world.with_caps(main)
    >>> stdout.getvalue()
    '3 f1\n'

    :type argv: IndexedSeq[String]
    :type open_arg: String => Iterable[String]
    :type stdout: File
    '''
    for filename in argv[1:]:
        stream = open_arg(filename)
        qty = sum([1 for line in stream])
        print >>stdout, qty, filename


class Mock(object):
    def with_caps(self, f):
        '''
        :type f: (IndexedSeq[String], File, String => Iterable[String]) => Unit
        '''
        argv = ['prog', 'f1']

        def open_arg(x):
            ''':type x: String'''
            if x not in argv:
                raise IOError()
            return ['line1', 'line2', 'line3']

        out = StringIO.StringIO()

        return f(argv[:], out, pf_(open_arg)), out


if __name__ == '__main__':
    def _with_caps(main):
        '''
        :type main: (IndexedSeq[String], File, String => Iterable[String]) => Unit
        '''
        from sys import argv, stdout

        def open_arg(arg):
            ''':type arg: String'''
            if arg not in argv:
                raise IOError(
                    'only paths given as arguments can be opened')
            return open(arg)

        return main(argv[:], stdout, open_arg)

    _with_caps(pf_(main))
