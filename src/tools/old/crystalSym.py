
import sys
from foundation import *

from mbbCore import *


(args,options) = cmdLineParse( "", "iom", sys.argv )


inFile = options["i"]
outFile = options["o"]
transformData = options["m"].split()

transform = Matrix()
vals = []
for v in transformData:
    vals.append(int(v))
transform.set(vals)





a = moeRead(inFile)


l = Loop()
l.loopTopAggregateGoal(a,MOLECULES)
while ( l.advanceLoopAndProcess() ):
    m = l.getMolecule()
    mcopy = m.copy()
    mcopy.applyMatrixToAtoms(transform)
    a.addMolecule(mcopy)

    
moeWrite(a,outFile)
