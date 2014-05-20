#! /bin/env python

from mbb import *


xml = QuickDomNode()
xml.parseFileName("../buildDatabase/real/_database.xml")
db = BuilderDatabase()
db.parseFromXml(xml)

gly1 = Monomer("gly")
mm = MutableMonomer("pro4(2S4S) pro4(2R4R)")
gly2 = Monomer("gly")

olig = Oligomer(db)
olig.addMonomer(gly1)
olig.addMonomer(mm)
olig.addMonomer(gly2)
print "Oligomer = ",olig

olig.couple(gly1, "dkp", mm)
olig.couple(mm, "dkp", gly2)


print "Done"
