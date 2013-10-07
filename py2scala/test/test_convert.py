import pkg_resources as pkg
import unittest

from .. import p2s
from test_caps import TestWithCaps


class ConvertTerminates(TestWithCaps):
    def setUp(self):
        from imp import find_module
        from os import path as os_path
        from sys import path as sys_path

        self._find_package = p2s.mk_find_package(find_module,
                                                 os_path.split,
                                                 sys_path)

        maven_path = mk_maven_path(os_path)
        self._maven_path = maven_path

        self._save_scala_fp = mk_save_scala_fp(open, maven_path)

    def convert_res(self, res, err=False):
        scala_fn = res.split('.')[0] + '.scala'
        with self._save_scala_fp(scala_fn, err) as out:
            fn = pkg.resource_filename(__name__, res)
            src = pkg.resource_string(__name__, res)
            p2s.convert(fn, src, out, self._find_package)
        return out.name

    def test_import_os(self, res='import_os.py'):
        self.convert_res(res)

    def test_instance(self, res='instance_attr.py'):
        self.convert_res(res)

    def test_funval(self, res='funval.py'):
        self.convert_res(res)

    def test_wordcount(self, res='wc.py'):
        self.convert_res(res)

    def test_for_else(self, res='for_else.py'):
        self.convert_res(res)

    def test_distant(self, res='distant_types.py'):
        self.convert_res(res)

    def test_raise(self, res='ex_raise.py'):
        self.convert_res(res)


def mk_maven_path(os_path):
    here = os_path.dirname(__file__)

    def maven_path(*segments):
        there = os_path.join(here, '..', '..', *segments)
        return os_path.relpath(os_path.normpath(there))

    return maven_path


def mk_save_scala_fp(open, maven_path):
    def scala_save_fp(fn, err=False):
        src = maven_path('src', 'test',
                         'scala-err' if err else 'scala',
                         fn)
        return open(src, 'w')

    return scala_save_fp

if __name__ == '__main__':
    unittest.main()
