import StringIO


def main(argv, open_arg, stdout):
    r'''
    >>> caps = Mock.caps()
    >>> main(**caps)
    >>> caps['stdout'].getvalue()
    '3 f1\n'

    :type argv: IndexedSeq[String]
    :type open_arg: String => File
    :type stdout: File
    '''
    for filename in argv[1:]:
        stream = open_arg(filename)
        qty = sum([1 for line in stream])
        print >>stdout, qty, filename


class Mock(object):
    @classmethod
    def caps(cls):
        argv = ['prog', 'f1']

        def open_arg(x):
            ''':type x: String'''
            if x not in argv:
                raise IOError()
            return ['line1', 'line2', 'line3']

        out = StringIO.StringIO()

        return dict(argv=argv[:], stdout=out, open_arg=open_arg)


if __name__ == '__main__':
    def _with_caps():
        from sys import argv, stdout

        def open_arg(arg):
            ''':type arg: String'''
            if arg not in argv:
                raise IOError(
                    'only paths given as arguments can be opened')
            return open(arg)

        def with_caps(main):
            '''
            :type main: (IndexedSeq[String], File, String => File) => Unit
            '''
            return main(argv=argv[:], stdout=stdout, open_arg=open_arg)

        return with_caps

    _with_caps(main)
