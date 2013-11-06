def f():
    x = [1, 2, 3]
    g(*x)


def g(*args):
    '''
    :type args: Int
    '''
    return sum(args)
