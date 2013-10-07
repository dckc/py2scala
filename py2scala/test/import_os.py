'''
import os from a virtualenv wasn't working because, oddly, the
virtualenv didn't seem to have string.py

Import from datetime wasn't working because it's a C extension.

Exercise dotted import names, aliases while we're at it, since
we have little discipline about testing one thing at a time. ;-)

'''

import sys
import os
import os.path as os_path
from os import path as os_path2
from datetime import timedelta


def main():
    print os.getenv('LOGNAME')
    print os_path.isfile(sys.argv[1])
    print os_path2.isfile(sys.argv[1])
    print timedelta(seconds=2)
