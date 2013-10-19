
class CP(object):
    def add_section(self, section):
        # deprecated form of raising exception
        raise ValueError, 'Invalid section name: %s' % section


def raises():
    raise ValueError("oops!")


def try_hard():
    x = CP()
    try:
        x.add_section('abc')
    except:
        print "lose"


def try_it():
    x = CP()
    try:
        x.add_section('abc')
    finally:
        print "lose"
