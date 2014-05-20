
from mbbCore import *


mmod = MacroModelFile()

mmod.openRead("test.mmod")
mnum = 1
while (not mmod.atEnd() ):
    mol = mmod.readMolecule()
    print "Read molecule: ",mnum
    mnum += 1



