
#
# Data/pro2s4s.moe 
import sys
from foundation import *

from mbbCore import *


if ( len(sys.argv) <= 1 ):
    inFile = "Data/acetamide.moe"
    databaseFile = "Data/shortPart2.pml"
    outFile = "acetamide.math"
else:
    (args,options) = cmdLineParse( "", "ido", sys.argv )
    inFile = options["i"]
    databaseFile = options["d"]
    outFile = options["o"]


ex = ExecutionContext()
ex.readFromFileName(databaseFile)
db = ex.getTrimerDatabase()
parms = db.parameterSet()

agg = Aggregate()

moeReadAggregateFromFileName(agg,inFile)
moeAssignAmberAtomTypes(parms,agg)

amber = Amber()
amber.useParameterSetAndAggregate(parms,agg)
min = amber.getMinimizer()

min.describeMathematica(outFile)
