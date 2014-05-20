#! /bin/env python

from math import *
import sys
from mbbCore import *




class MoleculeHandler:
    def __init__(self):
	self.residueIds = {}
	self.molecule = Molecule()
	self.dummyAtoms = {}

    def setDummyPosition(self,dummy,names):
	sumPos = Vector3(0,0,0)
	for x in names:
	    pos = molHandler.findPosition(x)
	    sumPos = sumPos.add(pos)
	avgPos = sumPos.multiplyByScalar(1.0/(len(parts)-1))
	self.dummyAtoms[dummy] = avgPos


    def getMolecule(self):
	return self.molecule

    def createAtom( self, resNum, atomName, elem ):
	if ( self.residueIds.has_key(resNum) ):
	    res = self.molecule.residueWithId(self.residueIds[resNum])
	else:
	    res = Residue()
	    res.setName("R%s"%resNum)
	    self.molecule.addResidue(res)
	    self.residueIds[resNum] = res.getId()
	if ( res.hasAtomWithName(atomName) ):
	    print "ERROR: The atom: %s in resNum: %s already exists"%(atomName,resNum)
	    sys.exit(1)
	a = Atom()
	a.setName(atomName)
	a.setElement(elem)
	a.setHybridization("sp3")
	print "Created atom with name: %s"%atomName
	res.addAtom(a)
	return a

    def formBond( self, a1, a2, bo ):
	if ( bo == "-" ):
	    a1.bondTo(a2,BondOrder.singleBond)
	else:
	    a1.bondTo(a2,BondOrder.doubleBond)
	    a1.setHybridization("sp2")
	    a2.setHybridization("sp2")
	    print "Creating a double bond from: %s to %s"%(a1.getName(),a2.getName())
	

    def findAtom( self, info ):
	print "info=",info
	parts = info.split(":")
        resNum = parts[0]
        atomName = parts[1]
	if ( self.residueIds.has_key(resNum) ):
	    res = self.molecule.residueWithId(self.residueIds[resNum])
	else:
	    print "ERROR: The resNum %d doesn't exist"%(resNum)
	    sys.exit(1)
	if ( not res.hasAtomWithName(atomName) ):
	    print "ERROR: The resNum %s atomName: %s  doesn't exist"%(resNum,atomName)
	    sys.exit(1)
	return res.atomWithName(atomName)

    def findPosition( self, info ):
	if ( info[0] == "@" ):
	    pos = self.dummyAtoms[info]
	else:
	    a = self.findAtom(info)
	    pos = a.getPosition()
	return pos


def value(str):
	global variables
	if ( str[0] == "$" ):
	    val = variables[str]
	else:
	    val = float(str)
	return val



def parseMolecule(parts):
    print len(parts)
    print parts
    molInfo = []
    molInfo.append(parts[1])
    molInfo.append(value(parts[2]))
    molInfo.append(value(parts[3]))
    molInfo.append(value(parts[4]))
    return molInfo


def buildAtOrigin( na, offset ):
	v = Vector3(offset[0],offset[1],offset[2])
	na.setPosition(v)

def buildWithDistance( newAtom, dist, da ):
	vd = Vector3(dist,0.0,0.0)
	vo = da
	vd = vd.add(vo)
	newAtom.setPosition(vd)

def buildWithBondAngle(newAtom,dist,da,angle,aa):
	arad = angle*0.0174533
	vangle = Vector3(-dist*cos(arad),dist*sin(arad),0.0)
	vdist = da
	vangle = vangle.add(vdist)
	newAtom.setPosition(vangle)

def buildWithBondAngleTorsion(d,dist,c,angle,b,torsion,a):
	aPos = a
	bPos = b
	cPos = c
	bcDir = bPos.sub(cPos)
	bcDirNorm = bcDir.normalized()
	dPosDist = bcDirNorm.multiplyByScalar(dist)
		#
		# Now find the axis around which to rotate the bond angle
		#
	abDir = aPos.sub(bPos)
	abDirNorm = abDir.normalized()
	angleAxis = bcDirNorm.crossProduct(abDirNorm)
	angleAxisNorm = angleAxis.normalized()
	angleRotation = Matrix()
	angleRotation.rotationAxis(angle*0.0174533,angleAxisNorm)
	dPosAngle = angleRotation.multiplyByVector3(dPosDist)

		#
		# Now rotate around the dihedral bond
		#
	dihedralRotation = Matrix()
	dihedralRotation.rotationAxis(-torsion*0.0174533,bcDirNorm)
	dPosDihedral = dihedralRotation.multiplyByVector3(dPosAngle)
		#
		# Now translate it to atom C
		#
	dPos = dPosDihedral.add(cPos)
	d.setPosition(dPos)



def parseAtom( mol, offset, parts ):
	newAtomInfo = parts[0].split(":")
	print "Creating atom: ", newAtomInfo
	resNum = newAtomInfo[0]
	atomName = newAtomInfo[1]
	if ( len(newAtomInfo)>2 ):
	    atomElement = newAtomInfo[2]
	else:
	    atomElement = newAtomInfo[1][0]
	newAtom = mol.createAtom(resNum,atomName,atomElement)
	
	newName = parts[0]
	if (len(parts)>1): bondOrder = parts[1]
	if (len(parts)>2): bondValue = value(parts[2])
	if (len(parts)>3): bondName = parts[3]
	if (len(parts)>4): angleValue = value(parts[4])
	if (len(parts)>5): angleName = parts[5]
	if (len(parts)>6): dihedralValue = value(parts[6])
	if (len(parts)>7): dihedralName = parts[7]

	if (len(parts)==1):
	    buildAtOrigin( newAtom, offset )
	elif (len(parts)==4):
	    dist = bondValue
	    da = mol.findPosition(bondName)
	    buildWithDistance( newAtom, dist, da )
	elif (len(parts)==6):
	    dist = bondValue
	    da = mol.findPosition(bondName)
	    angle = angleValue
	    aa = mol.findPosition(angleName)
	    buildWithBondAngle(newAtom,dist,da,angle,aa)
	elif (len(parts)==8):
	    dist = bondValue
	    da = mol.findPosition(bondName)
	    angle = angleValue
	    aa = mol.findPosition(angleName)
	    torsion = dihedralValue
	    ta = mol.findPosition(dihedralName)
	    buildWithBondAngleTorsion(newAtom,dist,da,angle,aa,torsion,ta)
	else:
	    print "ERROR: Illegal number of fields in line: ", parts
	if ( len(parts)>1 ):
	    da = mol.findAtom(bondName)
	    mol.formBond(newAtom,da,bondOrder)





#########################################################
#########################################################
#########################################################
#########################################################



inFile = sys.argv[1]
print "Reading Z-matrix from file: ", inFile

fin = open(inFile,"r")

agg = Aggregate()
variables = {}


for x in fin.readlines():
    comment = x.find("#")
    if ( comment >= 0 ):
	line = x[0:comment]
    else:
	line = x
		# split at whitespace
    parts = line.split()
		# skip blank lines
    if (len(parts) == 0 ):
	continue
		# handle new molecules
    if ( parts[0][0] == "!" ):
	molInfo = parseMolecule(parts)
	molHandler = MoleculeHandler()
	agg.addMolecule(molHandler.getMolecule())
	print "molInfo=",molInfo
	molHandler.getMolecule().setName(molInfo[0])
	offset = (molInfo[1],molInfo[2],molInfo[3])
	continue
		# handle defining variables
    if ( parts[0][0] == "$" ):
	varName = parts[0]
	val = float(parts[1])
	variables[varName] = val
	print "Setting variable: %s with: %lf"%(varName,val)
	continue
		# handle forming bonds
    if ( parts[0] == "bond" ):
	print "Creating bond"
	a1 = molHandler.findAtom(parts[1])
	bo = parts[2]
	a2 = molHandler.findAtom(parts[3])
	molHandler.formBond(a1,a2,bo)
	continue
		# handle creation of dummy atoms
    if ( parts[0][0] == "@" ):
	print "Creating dummy position"
	molHandler.setDummyPosition(parts[0], parts[1:])
	continue

		# otherwise parse it as an atom
    print "Parsint atom: ", parts
    parseAtom(molHandler,offset,parts)

moeWriteAggregateFileName( agg, "out.moe" )
 



