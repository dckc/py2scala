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
    ''':type x: Int'''
    return lambda y: x * y


def square():
    return lambda x=2: x * x


if __name__ == '__main__':
    add4 = adder(4)
    print add4(2)
    print multiplyer(3)(9)
    print square()(7)
    print square()()
