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
