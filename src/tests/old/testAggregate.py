
from mbb import *

agg = Aggregate()
agg.readFromFile("trimer.moe")

for m in agg.moleculeIterator:
    print "-Molecule: ",m
    for r in m.residueIterator:
	print "--Residue name: %s"%r.getName()
	for a in r.atomIterator:
	    print "---Atom: ", a.getName()
	    for b in a.bondIterator:
		print "----Bond to : ",b.getTo().getName()
     
