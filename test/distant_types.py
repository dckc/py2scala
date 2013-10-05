'''Checking types at a distance.

This program passes its tests and looks good from the view of typical
python static analysis tools such as pyflakes, but when we run it, we get::

    $ python distant_types.py
    Traceback (most recent call last):
      File "distant_types.py", line 28, in <module>
        print f2(2)
      File "distant_types.py", line 24, in f2
        return len(f1(x))
    TypeError: object of type 'int' has no len()
'''


def f1(x):
    return 1 if x % 2 == 0 else 'abc'


def f2(x):
    '''
    >>> f2(1)
    3
    '''
    return len(f1(x))


if __name__ == '__main__':
    print f2(2)
