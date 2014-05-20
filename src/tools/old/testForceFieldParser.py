#! /bin/env python

from mbbCore import *



#
# First read the structure
#

agg = moeReadAggregateWithAtomTypes("bestRdcRRWithTypes.moe")
olig = newRPOligomer()

xp = Xplor()
xp.setOligomer(
print "PYTHON>>About to read force field"
#ff = ForceField()
ff = moeReadForceField("charmm27.ff")

print "Force field = ", ff

xp = Xplor()
xp.setForceField(ff)
xp.writeParametersToFileName("charmm27.pro")


