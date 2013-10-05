from StringIO import StringIO
import pkg_resources as pkg
import unittest


def _fix_path():
    '''Fix sys.path to find the py2scala package (without installing it).

    TODO: study python community norms for organizing sources for packages
    and tests.
    '''
    import sys
    from os import path
    for pth in ['../']:
        sys.path.insert(0, path.join(
            path.dirname(path.abspath(__file__)), pth))

_fix_path()

from py2scala import p2s


class TestWithCaps(unittest.TestCase):
    def setUp(self):
        from imp import find_module
        from os import path as os_path
        from sys import path as sys_path

        self._find_package = p2s.mk_find_package(find_module,
                                                 os_path.split,
                                                 sys_path)


class ConvertTerminates(TestWithCaps):
    def convert_res(self, res):
        fn = pkg.resource_filename(__name__, res)
        src = pkg.resource_string(__name__, res)
        out = StringIO()
        p2s.convert(None, fn, src, out, self._find_package)
        self.assertTrue(len(out.getvalue()) > 0)

    def test_wordcount(self, res='wc.py'):
        self.convert_res(res)

    def test_for_else(self, res='for_else.py'):
        self.convert_res(res)

    def test_distant(self, res='distant_types.py'):
        self.convert_res(res)


class WellTyped(TestWithCaps):
    # TODO
    pass
