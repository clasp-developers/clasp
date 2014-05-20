#!/usr/bin/env python

from mbbComp import *

db = CyclomerDatabase()

#db.readFromFile("test.db")
#db.writeToFileName("out.db")

res = macroModelReadResidue("Mbbs/mbbUpUp.dat")
print res
