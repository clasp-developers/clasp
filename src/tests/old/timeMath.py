#! /bin/env python

from time import *
from mbb import *

tstart = time()
benchmarkMatrixMultiplications(10000000)
tstop = time()
print "Seconds = ",(tstop-tstart)

