#! /usr/bin/env python

from foundation import *
import	sys



def examineMonomer( db, fileName ):
    agg = Aggregate()
    moeReadAggregateFileName(db.parameterSet(), agg, fileName)
    print "============== Atom list"
    sys.stdout.flush()
    l = Loop()
    l.loopTopAggregateGoal(agg,ATOMS)
    while ( l.advanceLoopAndProcess() ):
        a = l.getAtom()
	bonds = a.coordination()
	bondStr = ""
	for i in range(0,bonds):
	    bondStr = "%s %d"%(bondStr,a.bondedNeighbor(i).getMoeIndex())
        print "====  Atom: %d  bonds: %s"%(a.getMoeIndex(),bondStr)
        sys.stdout.flush()
    print "============== Spanning tree"
    sys.stdout.flush()
    l = SpanningLoop()
    l.setTop(a)
    while ( l.advanceLoopAndProcess() ):
        a = l.getAtom()
	bonds = a.coordination()
	bondStr = ""
	for i in range(0,bonds):
	    bondStr = "%s %d"%(bondStr,a.bondedNeighbor(i).getMoeIndex())
        print "  ++++  Atom: %d  bonds: %s"%(a.getMoeIndex(),bondStr)
        sys.stdout.flush()











from mbbCore import *
sys.stdout.flush()

dbFile = sys.argv[1]
monomerFile = sys.argv[2]

print "Loading database from: ",dbFile
print "Loading monomer from: ",monomerFile
sys.stdout.flush()


ec = ExecutionContext()
ec.readFromFileName(sys.argv[1])
db = ec.getCyclomerDatabase()
print "Database = ", db
sys.stdout.flush()

examineMonomer( db, monomerFile )



