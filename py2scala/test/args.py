def f():
    x = [1, 2, 3]
    return g(*x)


def g(*args):
    '''
    :type args: Int
    '''
    return sum(args)


def kw_dict():
    return [dict(x=1, y=2)]
    # TODO: dict(dict(x=1, y=2), x=3),
    #       dict([('x', 1), ('y', 2)]),
    #       dict([('x', 1), ('y', 2)], z=3)]


print kw_dict()
print f()
