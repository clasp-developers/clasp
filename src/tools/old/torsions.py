#! /bin/env python

import sys
from mbbCore import *
import sets

subUnitDihedrals = {}
couplingDihedrals = {}

def treatAsImproper( atoms ):
    inner = [ atoms[1].getTypeString(), atoms[2].getTypeString() ]
    imprSet = sets.Set(['C','NH1','NH2', 'N'])
    for x in inner:
	if (x not in imprSet):
	    return False
    return True

def involvesLightAtom( atoms ):
    for x in atoms:
	if ( x.getTypeString()=="HA" ): return True
	if ( x.getTypeString()=="HC" ): return True
    return False

def orderAtoms(ratoms):
    if (ratoms[0].getName() > ratoms[3].getName() ):
	atoms = (ratoms[3],ratoms[2],ratoms[1],ratoms[0])
    else:
	atoms = ratoms
    return atoms
	

def dihedralInSubUnit( su, ratoms ):
    atoms = orderAtoms(ratoms)
    if ( involvesLightAtom(atoms) ): return
    print "Dihedral in sub unit: ", su.getName()
    s0 = atoms[0].getName()
    s1 = atoms[1].getName()
    s2 = atoms[2].getName()
    s3 = atoms[3].getName()
    list = subUnitDihedrals.get(su.getOligomerSequenceNumber(),{})
    key = (s0,s1,s2,s3)
    list[key] = atoms
    subUnitDihedrals[su.getOligomerSequenceNumber()] = list


def dihedralInCoupling( coup, atoms ):
    if ( involvesLightAtom(atoms) ): return
    rule = coup.getCouplingRule()
    inResId = coup.getIn().getResidue().getId()
    outResId = coup.getOut().getResidue().getId()
    print "Dihedral in coupling: ", rule.getName()
    s0 = atoms[0].getName()
    s1 = atoms[1].getName()
    s2 = atoms[2].getName()
    s3 = atoms[3].getName()
    rid0 = atoms[0].getResidueContainedBy().getId()
    rid1 = atoms[1].getResidueContainedBy().getId()
    rid2 = atoms[2].getResidueContainedBy().getId()
    rid3 = atoms[3].getResidueContainedBy().getId()
    pref0 = "-"
    pref1 = "-"
    pref2 = "-"
    pref3 = "-"
    if (rid0==outResId): pref0="+" 
    if (rid1==outResId): pref1="+" 
    if (rid2==outResId): pref2="+" 
    if (rid3==outResId): pref3="+" 
    list = couplingDihedrals.get(coup.getOligomerSequenceNumber(),{})
    key = (pref0+s0,pref1+s1,pref2+s2,pref3+s3)
    list[key] = atoms
    couplingDihedrals[coup.getOligomerSequenceNumber()] = list


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


print "========================================"
print "Oligomer just before writePsf = ", olig
sys.stdout.flush()
olig.dump()





xplor = Xplor()
xplor.setOligomer(olig)
xplor.setForceField(ff)

xplor.writePsfToFileName("_threeMer.psf")
xplor.writePdbToFileName("_threeMer.pdb")
#xplor.writeParametersToFileName("_threeMer.par")


#
# Generate all of the dihedrals
#
mol = olig.getMolecule()
ld = Loop()
ld.loopTopMoleculeGoal(mol,DIHEDRALS)
while ( ld.advanceLoopAndProcess() ):
    a1 = ld.getAtom1()
    a2 = ld.getAtom2()
    a3 = ld.getAtom3()
    a4 = ld.getAtom4()
    r1 = a1.getResidueContainedBy();
    r2 = a2.getResidueContainedBy();
    r3 = a3.getResidueContainedBy();
    r4 = a4.getResidueContainedBy();
    rid = {}
    rid[r1.getId()] = r1
    rid[r2.getId()] = r2
    rid[r3.getId()] = r3
    rid[r4.getId()] = r4
    ridKeys = rid.keys()
    if ( len(ridKeys) == 1 ):
	su = olig.getSubUnitForResidue(r1)
	dihedralInSubUnit( su, (a1,a2,a3,a4) )
    elif (len(ridKeys)==2 ):
	r1 = rid[ridKeys[0]]
	r2 = rid[ridKeys[1]]
	coup = olig.getCouplingBetweenResidues(r1,r2)
	dihedralInCoupling(coup, (a1,a2,a3,a4) )
    else:
	print "ERROR ERROR: The dihedral must be either in one subunit or one coupling, its: ", ridKeys


print "Subunits"
for x in subUnitDihedrals.iterkeys():
    su = olig.getSubUnitWithSequenceNumber(x)
    print su.getName()
    interactions = subUnitDihedrals[x]
    improperStrings = ""
    dihedralStrings = ""
    for i in interactions.iterkeys():
	atoms = interactions[i]
	if ( treatAsImproper(atoms) ):
	    improperStrings += "IMPRoper %s %s %s %s\n"%(i[0],i[1],i[2],i[3])
        else:
	    dihedralStrings += "DIHEdral %s %s %s %s\n"%(i[0],i[1],i[2],i[3])
    print dihedralStrings,
    print improperStrings

print "Couplings"
for x in couplingDihedrals.iterkeys():
    coup = olig.getCouplingWithSequenceNumber(x)
    rule = coup.getCouplingRule()
    insu = coup.getIn().getName()
    outsu = coup.getOut().getName()
    print "%s in: %s out: %s"%(rule.getName(),insu,outsu)
    interactions = couplingDihedrals[x]
    improperStrings = ""
    dihedralStrings = ""
    for i in interactions.iterkeys():
	atoms = interactions[i]
	if ( treatAsImproper(atoms) ):
	    improperStrings += "add IMPRoper %s %s %s %s\n"%(i[0],i[1],i[2],i[3])
        else:
	    dihedralStrings += "add DIHEdral %s %s %s %s\n"%(i[0],i[1],i[2],i[3])
    print dihedralStrings,
    print improperStrings
