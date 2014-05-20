
from mbbCore import *


def createHit( score, seq ):
    a = Hit()
    a.setScore(score)
    a.setSequence(seq)
    return a


a = HitCollection(1,3)

a.addHit(0, createHit(3.4, "Hit 3.4" ))
a.addHit(0, createHit(3.5, "Hit 3.5" ))
a.addHit(0, createHit(3.6, "Hit 3.6" ))

a.dump()

print "Adding another two"
a.addHit(0, createHit(5.6, "Hit 5.6" ))
a.addHit(0, createHit(3.55, "Hit 3.55" ))

a.dump()

print "Testing 1.0 hit?=", a.isItAHit(0,1.0)
print "Testing 9.0 hit?=", a.isItAHit(0,9.0)
print "Testing 3.6 hit?=", a.isItAHit(0,3.6)

a.writeToXmlFile("hits.xml");
