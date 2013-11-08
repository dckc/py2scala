import logging
import unittest

from .. import p2s

import test_convert

log = logging.getLogger(__name__)


def _with_run(f,
              scala_version='scala-2.10'):
    from imp import find_module
    from os import path as os_path
    from os import mkdir
    from subprocess import check_call
    from sys import path as sys_path

    logging.basicConfig(level=logging.INFO)

    maven_path = test_convert.mk_maven_path(os_path, mkdir)
    target = maven_path('target', scala_version)
    scala_src = maven_path('src', 'main', 'scala')

    return f(run_scalac=mk_run_scalac(check_call, target, scala_src),
             find_package=p2s.mk_find_package(find_module,
                                              os_path.split, sys_path),
             save_scala_fp=test_convert.mk_save_scala_fp(open, maven_path))


def test_well_typed(with_run=_with_run):
    run_scalac, find, save = with_run(
        lambda run_scalac, find_package, save_scala_fp: (
            run_scalac, find_package, save_scala_fp))

    def with_caps(f):
        return f(find_package=find, save_scala_fp=save)

    def runTest(runConvert, res, err=False):
        scala_fn = runConvert(res, err)

        try:
            run_scalac(scala_fn)
        except:
            actual_err = True
        else:
            actual_err = False

        assert err == actual_err

    for f_args in test_convert.test_convert_each(
            with_caps=with_caps):
        runConvert, res, err = f_args
        yield runTest, runConvert, res, err


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
