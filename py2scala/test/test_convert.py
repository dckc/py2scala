from StringIO import StringIO
import pkg_resources as pkg
import unittest

from .. import p2s
from test_caps import TestWithCaps


class ConvertTerminates(TestWithCaps):
    def convert_res(self, res, err=False):
        scala_fn = res.split('.')[0] + '.scala'
        with self._save_scala_fp(scala_fn, err) as out:
            fn = pkg.resource_filename(__name__, res)
            src = pkg.resource_string(__name__, res)
            p2s.convert(None, fn, src, out, self._find_package)
        return out.name

    def test_import_os(self, res='import_os.py'):
        self.convert_res(res)

    def test_wordcount(self, res='wc.py'):
        self.convert_res(res)

    def test_for_else(self, res='for_else.py'):
        self.convert_res(res)

    def test_distant(self, res='distant_types.py'):
        self.convert_res(res)


if __name__ == '__main__':
    unittest.main()
