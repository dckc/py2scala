class Dummy(object):
    pass


class Adder(object):
    def __init__(self, a):
        ''':type a: Int'''
        self._a = a

    def add(self, x):
        ''':type x: Int'''
        return x + self._a


class Animal(object):
    '''
    :forall: T
    '''
    def __init__(self, eats):
        '''
        :type eats: T
        '''
        self._eats = eats

    def get_eats(self):
        return self._eats
