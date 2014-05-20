#!/bin/env python

from mbbCore import *

q = QuickDomFromString("<atoms>a b c d e f</atoms>")

print q.getCharacters()

all = StringSet(q)

print "all=",all.asList()


qp = QuickDomFromString("<atoms>a b c</atoms>")
qr = QuickDomFromString("<atoms><rest/></atoms>")


first = StringSet(qp,all)
print "first = ",first.asList()

rest = StringSet(qr,all)
print "rest=", rest.asList()

print "Remaining = ",all.asList()
