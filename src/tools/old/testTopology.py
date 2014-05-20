#! /bin/env python


import sys
from math import *

import  wx

from mbbCore import *



builderXml = QuickDomNode("topologyPhase01.xml")
builderDb = BuilderDatabase()
builderDb.parseFromXml(builderXml)

topDb = builderDb.getTopologyDatabase();



print "TopologyDatabase = ",topDb
sys.stdout.flush()


info = topDb.testConsistancy()
if ( info == "" ):
    print "Topology database is PERFECT"
else:
    print "====================================="
    print "consisteny info ====================="
    print info
