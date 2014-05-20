#! /bin/env python

import sys

print "PATH=",sys.path

from mbbCore import *
#
# Read the phase 1 database
#
#
builderXml = QuickDomNode("topologyPhase01.xml")
builderDb = BuilderDatabase()
builderDb.parseFromXml(builderXml)

topDb = builderDb.getTopologyDatabase();
print "TopologyDatabase = ",topDb
sys.stdout.flush()

print "=================================="
sys.stdout.flush()
topDb.subUnitTypeDatabase().dump()
print "TEST>>1 OK-OK-OK-OK-OK-OK-OK-OK-OK-OK-OK-OK-OK-OK-"
sys.stdout.flush()


subUnits = topDb.allSubUnitMolds()
print "TEST>>2 SubUnitMolds=",subUnits
sys.stdout.flush()

rules = topDb.allCouplingRules()
print "TEST>>3 couplingRules=",rules
sys.stdout.flush()

#
#
# Extend the database here
#
#

topDb.dump()
print "DUMPING NEW DATABASE=============================================="
sys.stdout.flush()


builderDb.setTopologyDatabase(topDb)
builderDbXml = builderDb.asXml()
builderDbXml.writeToFileName("_topologyPhase02.xml")

