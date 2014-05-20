
from mbbCore import *

mat = Matrix()
valstr = "1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0"
nm =     "1.0 1.0 1.0 1.0 1.0 1.0"

a = Atom()
for i in range(1,10):
    print "TEST setName %d"%i
    a.setName(nm)

for i in range(1,10):
    print "TEST scale %d"%i
    mat.scale(2.0)

for i in range(1,10):
    print "TEST doNothing %d"%i
    mat.doNothing(valstr)

for i in range(1,10):
    print "TEST setFromString %d"%i
    mat.setFromString(valstr)

mat.dump()

