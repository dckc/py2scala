import logging
import unittest

from test_convert import ConvertTerminates

log = logging.getLogger(__name__)


class WellTyped(ConvertTerminates):
    def setUp(self,
              scala_version='scala-2.10'):
        from subprocess import check_call

        logging.basicConfig(level=logging.INFO)

        ConvertTerminates.setUp(self)

        maven_path = self._maven_path
        target = maven_path('target', scala_version)
        scala_src = maven_path('src', 'main', 'scala')
        self._run_scalac = mk_run_scalac(check_call, target, scala_src)

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

    def test_funval(self, res='funval.py'):
        self.check(res)

    def test_wordcount(self, res='wc.py'):
        self.check(res)

    def test_for_else(self, res='for_else.py'):
        self.check(res)

    def test_distant(self, res='distant_types.py'):
        self.check(res, err=True)

    def test_instance(self, res='instance_attr.py'):
        self.check(res)

    def test_raise(self, res='ex_raise.py'):
        self.check(res)


def mk_run_scalac(check_call, target, scala_src, fsc_path='fsc'):
    def run_scalac(fn):
        args = [fsc_path,
                '-d', target,
                '-sourcepath', scala_src,
                fn]
        log.info("run scala compiler: %s", ' '.join(args))
        check_call(args)
    return run_scalac


if __name__ == '__main__':
    unittest.main()
