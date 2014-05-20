#! /bin/env python

import sys
from mbb import *
import sets


#
# Read the phase 1 database
#
#
builderXml = QuickDomNode("extractorDb.xml")
builderDb = BuilderDatabase()
builderDb.parseFromXml(builderXml)

topDb = builderDb.getTopologyDatabase()
print "TopologyDatabase = ",topDb
topDb.dump()
sys.stdout.flush()

agg = moeReadAggregateWithAtomTypes( "bestRdcRRWithTypes.moe" )
print "TEST>>4 Dumping aggregate"
sys.stdout.flush()
mol = agg.firstMolecule()
for i in range(0,mol.residueCount()):
    res = mol.getResidue(i)
    print "Residue name: %s  index: %d"%(res.getName(),i)
    sys.stdout.flush()
#
##get the first OSS residue
print "Removing two hydrogen atoms from the first OSS residue"
sys.stdout.flush()
oss = mol.getResidue(1)
hAtom = oss.atomWithName("H")
hx1Atom = oss.atomWithName("HX1")
print "hAtom = ", hAtom
sys.stdout.flush()
print "hx1Atom = ", hx1Atom
sys.stdout.flush()
oss.removeAtomDontDeleteBonds(hAtom)
oss.removeAtomDontDeleteBonds(hx1Atom)
protons = Residue()
protons.setName("h_2")
protons.addAtom(hAtom)
protons.addAtom(hx1Atom)
mol.addResidue(protons)

#
#
# Now create an oligomer from this molecule
#
#
print "Defining oligomer from molecule"
sys.stdout.flush()
olig = Oligomer(topDb)
olig.defineFromMolecule(mol)
print "========================================"
print "Oligomer = ", olig
sys.stdout.flush()
olig.dump()


mon = olig.getMonomerWithSequenceNumber(3)

print "Dumping the #3 monomer"
sys.stdout.flush()
mon.dump()
sys.stdout.flush()
print "Monomer context = ", monomerContext(mon)

conf = builderDb.getConformationDatabase()
conf.dump()
sys.stdout.flush()

mc = conf.findCoordinates(mon)
mc.dump()
sys.stdout.flush()

