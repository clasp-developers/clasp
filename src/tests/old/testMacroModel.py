from mbbCore import *


mmod = MacroModelFile()
mmod.openRead("moedb.mmod")

print "Opened file atEnd=",mmod.atEnd()

mol = mmod.readMolecule()

print "Read one molecule = ", mol


mmod.close()
