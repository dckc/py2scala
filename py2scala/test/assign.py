
def concat(a, b):
    '''Simple case: single-assignment.

    :type a: String
    :type b: String
    '''
    c = a + b
    return c


def fact(n):
    '''Local variable.

    :type n: Int
    '''
    var, x, i = None, 1, n
    while i > 1:
        x *= n
        i -= 1
    return x


def colors():
    x = ['red', 'yellow', 'green']
    x[1] = 'blue'
    return x


def sounds():
    s = {'dog': 'bark'}
    s['cow'] = 'moo'
    return s


class Adder(object):
    '''Test updating an instance var outside the constructor.

    >>> a = Adder(10)
    >>> a.add(5)
    15
    >>> a.set_x(3)
    >>> a.add(5)
    8
    '''
    def __init__(self, x):
        ''':type x: Int'''
        self._x = x

    def set_x(self, x):
        ''':type x: Int'''
        self._x = x

    def add(self, y):
        ''':type y: Int'''
        return y + self._x
