
class CP(object):
    def add_section(self, section):
        # deprecated form of raising exception
        raise ValueError, 'Invalid section name: %s' % section


def raises():
    raise ValueError("oops!")
