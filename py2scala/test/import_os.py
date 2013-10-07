'''
import os from a virtualenv wasn't working because, oddly, the
virtualenv didn't seem to have string.py
'''

import sys
import os
import os.path as os_path


def main():
    print os.getenv('LOGNAME')
    print os_path.isfile(sys.argv[1])
