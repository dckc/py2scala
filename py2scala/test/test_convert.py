import pkg_resources as pkg
import unittest

from .. import p2s


def main(argv, exit,
         find_package, save_scala_fp,
         manifest_ok_fn='manifest_ok.txt',
         manifest_err_fn='manifest_err.txt'):

    class TestFunArgs(unittest.TestCase):
        def __init__(self, f, args):
            unittest.TestCase.__init__(self)
            self._f = f
            self._args = args

        def __str__(self):
            return '%s(%s)' % (self.__class__.__name__, self._args)

        def runTest(self):
            self._f(*self._args)

    suite = unittest.TestSuite(
        [TestFunArgs(f_args[0], f_args[1:]) for f_args in
         test_convert_each(
             with_caps=lambda f: f(find_package, save_scala_fp))])
    runner = unittest.TextTestRunner(verbosity=2 if '-v' in argv else 1)
    result = runner.run(suite)
    exit(result.wasSuccessful())


def read_manifest(fn):
    return [item.strip() for item in
            pkg.resource_stream(__name__, fn)
            if item]


def mk_maven_path(os_path, mkdir):
    here = os_path.dirname(__file__)

    def maven_path(*segments):
        there = os_path.join(here, '..', '..')
        there = os_path.relpath(os_path.normpath(there))
        for segment in segments[:-1]:
            there = os_path.join(there, segment)
            if not os_path.exists(there):
                mkdir(there)
        return os_path.join(there, segments[-1])

    return maven_path


def mk_save_scala_fp(open, maven_path):
    def scala_save_fp(fn, err=False):
        src = maven_path('src', 'test',
                         'scala-err' if err else 'scala',
                         fn)
        return open(src, 'w')

    return scala_save_fp


def _with_find_save(f):
    '''Resolve conflict between least-authority design and test discovery.
    KLUDGE.
    '''
    from imp import find_module
    from os import path as os_path
    from os import mkdir
    from sys import path as sys_path

    maven_path = mk_maven_path(os_path, mkdir)
    return f(find_package=p2s.mk_find_package(find_module,
                                              os_path.split, sys_path),
             save_scala_fp=mk_save_scala_fp(open, maven_path))


def test_convert_each(with_caps=_with_find_save,
                      manifest_ok_fn='manifest_ok.txt',
                      manifest_err_fn='manifest_err.txt'):
    '''Nosetest style test generator.
    '''
    find_package, save_scala_fp = with_caps(
        lambda find_package, save_scala_fp: (find_package, save_scala_fp))

    def runTest(res, err):
        scala_fn = res.split('.')[0] + '.scala'
        with save_scala_fp(scala_fn, err) as out:
            fn = pkg.resource_filename(__name__, res)
            src = pkg.resource_string(__name__, res)
            p2s.convert(fn, src, out, find_package)
        return out.name

    ok_filenames = read_manifest(manifest_ok_fn)
    err_filenames = read_manifest(manifest_err_fn)
    cases = ([(fn, False) for fn in ok_filenames] +
             [(fn, True) for fn in err_filenames])

    for res, err in cases:
        yield runTest, res, err


if __name__ == '__main__':
    def _with_caps(main):
        from sys import argv, exit

        def f(find_package, save_scala_fp):
            main(argv, exit, find_package, save_scala_fp)

        _with_find_save(f)

    _with_caps(main)
