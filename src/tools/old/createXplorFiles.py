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

#print "=================================="
#sys.stdout.flush()
#topDb.subUnitTypeDatabase().dump()
#print "TEST>>1 OK-OK-OK-OK-OK-OK-OK-OK-OK-OK-OK-OK-OK-OK-"
#sys.stdout.flush()
#
#
#subUnits = topDb.allSubUnitMolds()
#print "TEST>>2 SubUnitMolds=",subUnits
#sys.stdout.flush()
#
#rules = topDb.allCouplingRules()
#print "TEST>>3 couplingRules=",rules
#sys.stdout.flush()
#
##
##
## Extend the database from a Molecule
##
##
#
## New way to read aggregate from moe file
##
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
protons.setName("HAM")
protons.addAtom(hAtom)
protons.addAtom(hx1Atom)
mol.addResidue(protons)
#
#for i in range(0,mol.residueCount()):
#    res = mol.getResidue(i)
#    print "Residue name: %s  index: %d"%(res.getName(),i)
#    sys.stdout.flush()
#
#
##
## Remove the two protons from the first OSS residue
## and put them in their own residue
##
#
#
#
#topDb.learnFromAggregate(agg)
#topDb.dump()
#
# Learning the subunits isn't enough, we have to tell each
# SubUnit what sort of couplings it can make
#
#oss = topDb.subUnitMoldWithName("OSS")
#oss.defineCouplingInOut2("resinAmide","dkp","protonateAmine")
#oss.defineCouplingInOut1("dkp","dkp")
#orr = topDb.subUnitMoldWithName("ORR")
#orr.defineCouplingInOut1("dkp","dkp")
#tyr = topDb.subUnitMoldWithName("TYR")
#tyr.defineCouplingIn("dkp")
#tar = topDb.subUnitMoldWithName("resinAmide")
#tar.defineCouplingOut1("resinAmide")

print "DUMPING NEW DATABASE=============================================="
sys.stdout.flush()
builderDb.setTopologyDatabase(topDb)
builderDbXml = builderDb.asXml()
builderDbXml.writeToFileName("_topologyPhase02.xml")

#
#
# Now create an oligomer from this molecule
#
#
print "Defining oligomer from molecule"
sys.stdout.flush()
olig = Oligomer(topDb)
olig.defineFromMolecule(mol)
print "Oligomer = ", olig
sys.stdout.flush()
olig.dump()



print "Reading force field"
sys.stdout.flush()
ff = moeReadForceField("charmm27.ff")

print "Read the force field"
print "stretch terms= ", ff.stretches
sys.stdout.flush()

def dupStretch( ffield, old, new ):
    one = ffield.stretches.find(old[0],old[1])
    print "one="
    sys.stdout.flush()
    one.dump()
    two = one.copy()
    two.setTypes(new[0],new[1])
    print "two="
    sys.stdout.flush()
    two.dump()
    ffield.stretches.add(two)

def dupAngle( ffield, old, new ):
    one = ffield.angles.findExact(old[0],old[1],old[2])
    print "one="
    sys.stdout.flush()
    one.dump()
    two = one.copy()
    two.setTypes(new[0],new[1],new[2])
    print "two="
    sys.stdout.flush()
    two.dump()
    ffield.angles.add(two)

def dupPtor( ffield, new, old ):
    told = old.upper().split(",") 
    tnew = new.upper().split(",") 
    print "dupPtor=", told
    one = ffield.ptors.findExact(told[0],told[1],told[2],told[3])
    print "one="
    sys.stdout.flush()
    one.dump()
    two = one.copy()
    two.setTypes(tnew[0],tnew[1],tnew[2],tnew[3])
    print "two="
    sys.stdout.flush()
    two.dump()
    ffield.ptors.add(two)


#
#
# Create Ptor entries for these missing ones
#
#
#dupPtor( ff, "O,C,CP1,NP", "O,C,CP1,N" )
#dupPtor( ff, "CQ2,CP3,NP,CP1"   , "cp2,cp3,np,cp1")
#dupPtor( ff, "*,CP3,CQ2,*"   	, "*,cp3,cp2,*")
#dupPtor( ff, "*,CQ2,CP2,*"  	, "*,cp2,cp2,*")
#dupPtor( ff, "O,C,CQ2,CP3"	, "O,C,CT1,CT2")
#dupPtor( ff, "O,C,CQ2,CP2"	, "o,c,ct1,ct2")
#dupPtor( ff, "O,C,CQ2,NH1"	, "o,c,ct2,nh1")
#dupPtor( ff, "h,nh1,cq2,cp3"	, "h,nh1,ct2,ct2")
#dupPtor( ff, "h,nh1,cq2,cp2"	, "h,nh1,ct2,ct2")
#dupPtor( ff, "h,nh1,cq2,c"	, "h,nh1,ct2,c")
#dupPtor( ff, "cq2,cp3,n,cp1"	, "cp2,cp3,n,cp1")
#

xplor = Xplor()
xplor.setOligomer(olig)
xplor.setForceField(ff)

xplor.writePsfToFileName("_threeMer.psf")
xplor.writePdbToFileName("_threeMer.pdb")
#xplor.writeParametersToFileName("_threeMer.par")

