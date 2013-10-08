'''
In scala, function f as a value is written f _.

`from functools import partial as pf_` lets us use pf_(f) to signal this usage.

'''
from functools import partial as pf_


def adder(x):
    ''':type x: Int'''

    def add(y):
        ''':type y: Int'''
        return x + y

    return pf_(add)


def multiplyer(x=1):
    '''
    :type x: Int
    :rtype: Int => Int
    '''
    return lambda y: x * y


def bool_default(x=True):
    # TODO: not not x
    return not x


DEFAULT = 1


def name_default(x=DEFAULT):
    ''':type x: Int'''
    return x + 1


def circle_area(r, pi=3.14159):
    ''':type r: Double'''
    # TODO: r ** 2
    return pi * (r * r)


def square():
    return lambda x=2: x * x


if __name__ == '__main__':
    add4 = adder(4)
    print add4(2)
    print multiplyer(3)(9)
    print square()(7)
    print square()()
