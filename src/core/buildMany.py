#! /usr/bin/env python

#
#
# Search for sequences constructed from subsequences
#
# 
#

from mbbComp import *


global cyclomers
#cyclomers = [["dpex","dpex"],["dpen","dpen"], ["dmen", "dmen"],["dmex","dmex"]]
cyclomers = [["dpex"],["dpen"],["dmex"],["dmen"]]


db = CyclomerDatabase()
db.readFromFile("test.db")

class PCName:
    def __init__(self, value ):
        self.indexes = range(value)
	self.strings = range(value)
	self.done = 0
	i = 0
	while ( i < len(self.indexes) ):
	    self.indexes[i] = 0
	    self.strings[i] = cyclomers[0]
	    i = i + 1

    def increment(self):
	if ( self.done ):
	    return None
	i = len(self.indexes)-1
	while ( i >= 0 ):
	    self.indexes[i] = self.indexes[i]+1
	    if ( self.indexes[i] >= len(cyclomers) ):
		if ( i == 0 ):
		    self.done = 1
		    return None
	        else:
		    self.strings[i] = cyclomers[0]
                    self.indexes[i] = 0;
                    i = i - 1
	    else:
		self.strings[i] = cyclomers[self.indexes[i]]
	        return self
	return None

    def polyCyclomerNames(self):
	poly = ["strt"]
	for i in self.strings:
	    poly = poly + i
	poly = poly + ["end"]
	return poly

a = PCName(8)
i = 0
valid = 1
while ( valid != None  ):
    print a.polyCyclomerNames()
    p = PolyCyclomer( db, a.polyCyclomerNames() )
    print p.getMolecule()
    valid = a.increment()
