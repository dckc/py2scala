
# Note: [] isn't well-typed unless we constrain the parameter type
# TODO: use has_type([], 'Iterable[Int]')
for x in [1]:
    print "some"
else:
    print "none"
