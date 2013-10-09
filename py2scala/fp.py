
def typed(x, t):
    return x


def classOf(c):
    '''translates to classOf[c], since classes are types, not values.
    '''
    return c


def option_iter(x):
    '''Facilitate well-typed iteration with None.
    '''
    return [] if x is None else [x]


def option_fold(opt_a, f, b):
    return b if opt_a is None else f(opt_a)


def option_orelse(opt_a, a):
    return a if opt_a is None else opt_a


def partition(l, pred):
    which = [(pred(item), item) for item in l]
    return ([item for (test, item) in which if test],
            [item for (test, item) in which if not test])
