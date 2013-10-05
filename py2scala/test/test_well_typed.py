from StringIO import StringIO
import pkg_resources as pkg
import unittest

from .. import p2s


class TestWithCaps(unittest.TestCase):
    def setUp(self,
              scala_version='scala-2.10'):
        from imp import find_module
        from os import path as os_path
        from subprocess import check_call
        from sys import path as sys_path

        self._find_package = p2s.mk_find_package(find_module,
                                                 os_path.split,
                                                 sys_path)

        here = os_path.dirname(__file__)
        target = os_path.join(here, '..', '..', 'target', scala_version)
        scala_src = os_path.join(here, '..', '..',
                                 'src', 'main', 'scala')
        self._run_scalac = mk_run_scalac(check_call, target, scala_src)

        self._save_scala_fp = mk_save_scala_fp(open, os_path)


def mk_run_scalac(check_call, target, scala_src, fsc_path='fsc'):
    def run_scalac(fn):
        args = [fsc_path,
                '-d', target,
                '-sourcepath', scala_src,
                fn]
        print "@@log.info: %s" % ' '.join(args)
        check_call(args)
    return run_scalac


def mk_save_scala_fp(open, os_path):
    def scala_save_fp(fn, err=False):
        here = os_path.dirname(__file__)
        src = os_path.join(here, '..', '..', 'src', 'main',
                           'scala-err' if err else 'scala',
                           fn)
        return open(src, 'w')

    return scala_save_fp


class ConvertTerminates(TestWithCaps):
    def convert_res(self, res, out=None):
        out = out or StringIO()
        fn = pkg.resource_filename(__name__, res)
        src = pkg.resource_string(__name__, res)
        p2s.convert(None, fn, src, out, self._find_package)

    def test_wordcount(self, res='wc.py'):
        self.convert_res(res)

    def test_for_else(self, res='for_else.py'):
        self.convert_res(res)

    def test_distant(self, res='distant_types.py'):
        self.convert_res(res)


class WellTyped(ConvertTerminates):
    def check(self, res, err=False):
        scala_fn = res.split('.')[0] + '.scala'
        with self._save_scala_fp(scala_fn, err) as out:
            self.convert_res(res, out)

        try:
            self._run_scalac(out.name)
        except:
            actual_err = True
        else:
            actual_err = False

        self.assertEqual(err, actual_err)

    def test_wordcount(self, res='wc.py'):
        self.check(res)

    def test_for_else(self, res='for_else.py'):
        self.check(res)

    def test_distant(self, res='distant_types.py'):
        self.check(res, err=True)


if __name__ == '__main__':
    unittest.main()
