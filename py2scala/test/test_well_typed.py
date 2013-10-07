import unittest

from test_convert import ConvertTerminates


class WellTyped(ConvertTerminates):
    def check(self, res, err=False):
        scala_fn = self.convert_res(res, err)

        try:
            self._run_scalac(scala_fn)
        except:
            actual_err = True
        else:
            actual_err = False

        self.assertEqual(err, actual_err)

    def test_import_os(self, res='import_os.py'):
        self.check(res)

    def test_wordcount(self, res='wc.py'):
        self.check(res)

    def test_for_else(self, res='for_else.py'):
        self.check(res)

    def test_distant(self, res='distant_types.py'):
        self.check(res, err=True)

    def test_instance(self, res='instance_attr.py'):
        self.check(res)


if __name__ == '__main__':
    unittest.main()
