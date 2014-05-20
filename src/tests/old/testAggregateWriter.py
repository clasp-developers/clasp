#! /bin/env python

from mbbCore import *

a = Aggregate()
moeReadAggregateFromFileName(a,"trimer.moe")

fout = open("trimerFout.xml","w")
a.writeToXmlStream("  ",fout)
fout.close()
a.writeToXmlFileName("trimer.xml")
