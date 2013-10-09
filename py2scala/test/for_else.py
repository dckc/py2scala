from ..fp import typed

# Note: [] isn't well-typed unless we constrain the parameter type
# TODO: use has_type([], 'Iterable[Int]')
for x in typed([], 'Iterable[Int]'):
    print "some"
else:
    print "none"
