#! /bin/env python


from mbb import *
import math
import random
import copy


# ###########################################################
# ###########################################################
# ###########################################################
# ###########################################################
# ###########################################################

class CGOligomer:
    def __init__(self,aSize,aSegment):
	self._sequence = []
	for mon in range(0,aSize):
	    self._sequence.append(copy.copy(aSegment))

    def perturb(self,scale):
	for mon in self._sequence:
	    mon.perturb(scale)

    def forget(self):
	for mon in self._sequence:
	    mon.forget()

    def energy(self):
	e = 0
	for mon in self._sequence:
	    e = e + mon.energy()
	return e

    def buildSegments(self):
	segs = []
	coord = CoordinateSystem()
	segs.append(coord)
	for mon in self._sequence:
	    segCoord = mon.build(coord)
	    segs.append(segCoord)
	    coord = segCoord
	return segs

    def endToEndLength(self):
	segs = self.buildSegments()
	len = segs[-1].getOrigin().length()
	return len

    def renderToXml(self):
	node = QuickDomNode("frame")
	strip = xmlBeginLineStrip("FFFF00")
	segs = self.buildSegments()
	for m in segs:
	    child = xmlVertexVector3(strip,m.getOrigin())
	node.addChild(strip)
	return node  

  

def renderRay(gf,col,vo,dir):
    vf = vo.add(dir)
    print >> gf, '<line x1="%lf" y1="%lf" z1="%lf" x2="%lf" y2="%lf" z2="%lf" color="%s"/>'\
		%(vo.getX(),vo.getY(),vo.getZ(),vf.getX(),vf.getY(),vf.getZ(),col)


class SixParameterModel:
    def __init__(self,gf):
	self._dir = Vector3(0,0,0)
	self._trans = 0
	self._rotx = 0
	self._roty = 0
	self._rotz = 0
	self._gf = gf 
	self._delta = (0,0,0,0)
	self._save = (0,0,0,0)
	self._force_rotx = 10
	self._force_roty = 1
	self._force_rotz = 10
	self._force_trans = 10


    def description(self):
	s = """
Direction = (%lf,%lf,%lf)
Length = %lf
RotX = %lf
RotY = %lf
RotZ = %lf"""%(
self._dir.getX(),
self._dir.getY(),
self._dir.getZ(),
self._trans,
self._rotx/0.0174533,
self._roty/0.0174533,
self._rotz/0.0174533 )
	return s


    def extract(self,c1,c2):
    #    renderRay(self._gf,MAGENTA,c1.getOrigin(),c2.getX())
	trans = c2.getOrigin().sub(c1.getOrigin())
	trans=Vector3(trans.dotProduct(c1.getX()),trans.dotProduct(c1.getY()),trans.dotProduct(c1.getZ()))
	c2xo = c2.getX()
	xycomp1 = c1.getX().multiplyByScalar(c2xo.dotProduct(c1.getX()))
	xycomp2 = c1.getY().multiplyByScalar(c2xo.dotProduct(c1.getY()))
	xycomp = (xycomp1.add(xycomp2)).normalized()
	if ( xycomp.dotProduct(c1.getY())>0 ):
	    sgn = 1.0
	else:
	    sgn = -1.0
	rotz = math.acos(xycomp.dotProduct(c1.getX()))*sgn
	    # See if it's where it should be
    #    renderRay(self._gf,CYAN,c1.getOrigin(),xycomp)
	    #
	    # Now undo the effect of rotz on c2.getX()
	    #
	mrotz = Matrix()
	mrotz.rotationAxis(-rotz,c1.getZ())
	c2x = mrotz.multiplyByVector3(c2xo)
    #    renderRay(self._gf,YELLOW,c1.getOrigin(),c2x)
	if ( c2x.dotProduct(c1.getZ()) > 0 ):
	    sgn = -1.0
	else:
	    sgn = 1.0
	roty = math.acos(c2x.dotProduct(c1.getX()))*sgn
	mroty = Matrix()
	mroty.rotationAxis(-roty,c1.getY())
	c3x = mroty.multiplyByVector3(c2x)
    #    renderRay(self._gf,YELLOW,c1.getOrigin(),c3x)
	c2y = mrotz.multiplyByVector3(c2.getY())
	c3y = mroty.multiplyByVector3(c2y)
    #    renderRay(self._gf,YELLOW,c1.getOrigin(),c3y)
	if ( c3y.dotProduct(c1.getZ()) > 0 ):
	    sgn = 1.0
	else:
	    sgn = -1.0
	rotx = math.acos(c3y.dotProduct(c1.getY()))*sgn
    #    mrotx = Matrix()
    #    mrotx.rotationAxis(-rotx,c1.getX())
    #    c4y = mrotx.multiplyByVector3(c3y)
    #    renderRay(self._gf,YELLOW,c1.getOrigin(),c4y)
    # ########################
    #  TEST by applying rotations and translation to C2 and it should
    # put it right on C1
    #
	self._trans = trans.length()
	self._dir = trans.normalized()
	self._rotx = rotx
	self._roty = roty
	self._rotz = rotz

    def transform(self):
	delta_rotx = self._delta[0]
	delta_roty = self._delta[1]
	delta_rotz = self._delta[2]
	delta_trans = self._delta[3]
	rotx = self._rotx+delta_rotx
	roty = self._roty+delta_roty
	rotz = self._rotz+delta_rotz
	trans = self._dir.multiplyByScalar(self._trans+delta_trans)
	mto = c1.matrixToCanonical()
	mfrom = c1.matrixFromCanonical()
	mrotx = Matrix()
	mrotx.rotationX(-rotx)
	mroty = Matrix()
	mroty.rotationY(-roty)
	mrotz = Matrix()
	mrotz.rotationZ(-rotz)
	mtrans = Matrix()
	mtrans.translate(trans)
	mtransform = Matrix()
	mtransform = mrotx.multiplyByMatrix(mtransform)
	mtransform = mroty.multiplyByMatrix(mtransform)
	mtransform = mrotz.multiplyByMatrix(mtransform)
	mtransform = mtrans.multiplyByMatrix(mtransform)
	return mtransform

    def build(self,coord):
	mfrom = coord.matrixFromCanonical()
	cgen = CoordinateSystem()
	cgen.canonical()
    #    mtrans = Matrix()
    #    mtrans.translate(Vector3(1.0,0,0))
	mtrans = self.transform()
	cgen.transformWithMatrix(mtrans)
	cgen.transformWithMatrix(mfrom)
	return cgen 

    def energy(self):
	delta_rotx = self._delta[0]
	delta_roty = self._delta[1]
	delta_rotz = self._delta[2]
	delta_trans = self._delta[3]
	en = 0
	en = en + delta_rotx*delta_rotx*self._force_rotx
	en = en + delta_roty*delta_roty*self._force_roty
	en = en + delta_rotz*delta_rotz*self._force_rotz
	en = en + delta_trans*delta_trans*self._force_trans
	return en

    def perturb(self,scale):
	self._save = self._delta
	rotx = self._delta[0]+(random.random()-0.5)*scale*2
	roty = self._delta[1]+(random.random()-0.5)*scale*2
	rotz = self._delta[2]+(random.random()-0.5)*scale*2
	trans = self._delta[3]+(random.random()-0.5)*scale*2
	self._delta = (rotx,roty,rotz,trans)

    def forget(self):
	self._delta = self._save



class FourParameterModel:

    def __init__(self,gf):
	self._trans = 0
	self._rotx = 0
	self._roty = 0
	self._rotz = 0
	self._gf = gf 

    def description(self):
	s = """
Distance = %lf
RotX = %lf
RotY = %lf
RotZ = %lf"""%(self._trans,
self._rotx/0.0174533,
self._roty/0.0174533,
self._rotz/0.0174533 )
	return s


    def extract(self,c1,c2):
    #    renderRay(self._gf,MAGENTA,c1.getOrigin(),c2.getX())
	l = (c2.getOrigin().sub(c1.getOrigin())).length()
	c2xo = ((c2.getX().add(c2.getOrigin())).sub(c1.getOrigin())).normalized()
	xycomp1 = c1.getX().multiplyByScalar(c2xo.dotProduct(c1.getX()))
	xycomp2 = c1.getY().multiplyByScalar(c2xo.dotProduct(c1.getY()))
	xycomp = (xycomp1.add(xycomp2)).normalized()
	if ( xycomp.dotProduct(c1.getY())>0 ):
	    sgn = 1.0
	else:
	    sgn = -1.0
	rotz = math.acos(xycomp.dotProduct(c1.getX()))*sgn
	    # See if it's where it should be
    #    renderRay(self._gf,CYAN,c1.getOrigin(),xycomp)
	    #
	    # Now undo the effect of rotz on c2.getX()
	    #
	mrotz = Matrix()
	mrotz.rotationAxis(-rotz,c1.getZ())
	c2x = mrotz.multiplyByVector3(c2xo)
    #    renderRay(self._gf,YELLOW,c1.getOrigin(),c2x)
	if ( c2x.dotProduct(c1.getZ()) > 0 ):
	    sgn = -1.0
	else:
	    sgn = 1.0
	roty = math.acos(c2x.dotProduct(c1.getX()))*sgn
	mroty = Matrix()
	mroty.rotationAxis(-roty,c1.getY())
	c3x = mroty.multiplyByVector3(c2x)
    #    renderRay(self._gf,YELLOW,c1.getOrigin(),c3x)
	c2y = mrotz.multiplyByVector3(c2.getY())
	c3y = mroty.multiplyByVector3(c2y)
    #    renderRay(self._gf,YELLOW,c1.getOrigin(),c3y)
	if ( c3y.dotProduct(c1.getZ()) > 0 ):
	    sgn = 1.0
	else:
	    sgn = -1.0
	rotx = math.acos(c3y.dotProduct(c1.getY()))*sgn
    #    mrotx = Matrix()
    #    mrotx.rotationAxis(-rotx,c1.getX())
    #    c4y = mrotx.multiplyByVector3(c3y)
    #    renderRay(self._gf,YELLOW,c1.getOrigin(),c4y)
    # ########################
    #  TEST by applying rotations and translation to C2 and it should
    # put it right on C1
    #
	self._trans = l
	self._rotx = rotx
	self._roty = roty
	self._rotz = rotz


    def transform(self):
	rotx = self._rotx
	roty = self._roty
	rotz = self._rotz
	trans = self._trans
	mto = c1.matrixToCanonical()
	mfrom = c1.matrixFromCanonical()
	mrotx = Matrix()
	mrotx.rotationX(-rotx)
	mroty = Matrix()
	mroty.rotationY(-roty)
	mrotz = Matrix()
	mrotz.rotationZ(-rotz)
	v = Vector3(trans,0,0)
	mtrans = Matrix()
	mtrans.translate(v)
	mtransform = mtrans
	mtransform = mrotx.multiplyByMatrix(mtransform)
	mtransform = mroty.multiplyByMatrix(mtransform)
	mtransform = mrotz.multiplyByMatrix(mtransform)
	return mtransform

    def build(self,coord):
	mfrom = coord.matrixFromCanonical()
	cgen = CoordinateSystem()
	cgen.canonical()
    #    mtrans = Matrix()
    #    mtrans.translate(Vector3(1.0,0,0))
	mtrans = self.transform()
	cgen.transformWithMatrix(mtrans)
	cgen.transformWithMatrix(mfrom)
	return cgen 


# ###########################################################
#
# Load the database
#
#

PI = 3.1415
PIdiv2 = PI/2

builderXml = QuickDomNode()
builderXml.parseFileName("../buildDatabase/newDb.xml")
builderDb = BuilderDatabase()
builderDb.parseFromXml(builderXml)


agg = Aggregate()
mol2ReadAggregateFromFileName(agg,"fourMer.mol2")

mol = agg.firstMolecule()
olig = Oligomer(builderDb)
olig.defineFromMolecule(mol)

YELLOW = "FFFF00"
MAGENTA = "FF00FF"
CYAN = "00FFFF"

xmlAgg = xmlAggregateCreate(agg)

gfFour = open("_gFour.xml", "w")
gfSix = open("_gSix.xml", "w")
print >> gfFour, "<graphics>"
print >> gfFour, xmlAgg.asString()

print >> gfSix, "<graphics>"
print >> gfSix, xmlAgg.asString()

pro4s = {}
prevMon = None
index = 0
mon = olig.rootMonomer()
while (mon != None):
    res = mon.getResidue()
    if ( res.getMold().getName() == "Pro4" ):
	aO = res.atomWithName("CG")
	aX1 = res.atomWithName("CB")
	aX2 = res.atomWithName("CD")
	vO = aO.getPosition()
	vX1 = aX1.getPosition().sub(vO)
	vX2 = aX2.getPosition().sub(vO)
	vX = ((vX1.add(vX2)).multiplyByScalar(-1.0)).normalized()
	vY = (vX1.sub(vX2)).normalized()
	cs = CoordinateSystem()
	cs.defineForVectorsOriginXDirXYPlane(vO,vX,vY)
	seqNum = mon.getOligomerSequenceNumber()
	pro4s[seqNum] = (mon,cs)
	xml = cs.renderXml(2,1)
	print >> gfFour, xml.asString()
	print >> gfSix, xml.asString()
    if ( mon.hasOutCouplingWithName("dkp") ):
        coup = mon.getOutCouplingWithName("dkp")
	prevMon = mon
	mon = coup.getOut()
    else:
	mon = None
    index = index + 1


pairs = {}
for (mon,cs) in pro4s.values():
    seqNum = mon.getOligomerSequenceNumber()
    if ( mon.hasOutCouplingWithName("dkp") ):
	coup = mon.getOutCouplingWithName("dkp")
	nextMon = coup.getOut()
	if ( nextMon.getResidue().getMold().getName() != "Pro4" ):
	    continue
	nextSeqNum = nextMon.getOligomerSequenceNumber()
	(tmon,nextCs) = pro4s[nextSeqNum]
	pairs[(seqNum,nextSeqNum)] = (cs,nextCs)



print "======================================="
print "Four parameter model"
for (c1,c2) in pairs.values():
    parms = FourParameterModel(gfFour)
    parms.extract(c1,c2)
    print parms.description()
    c2Predicted = parms.build(c1)
    xml = c2Predicted.renderXml(8,0.5)
    print >> gfFour, xml.asString()

model = None
print "======================================="
print "Six parameter model"
for (c1,c2) in pairs.values():
    parms = SixParameterModel(gfSix)
    parms.extract(c1,c2)
    print parms.description()
    c2Predicted = parms.build(c1)
    if ( model == None ):
	model = parms
    xml = c2Predicted.renderXml(8,0.5)
    print >> gfSix, xml.asString()



print "======================================"
print "Starting MonteCarlo with Six parameter model"

#
# First build a model containing eight segments
#
structure = CGOligomer(8,model)

graphicsXml = xmlGraphics("main")
animationXml = QuickDomNode("animation")
graphicsXml.addChild(animationXml)

fenergy = open("_mc.txt","w")
beta = 1.0   # The larger beta, the lower the temperature
lastEnergy = structure.energy()
for step in range(0,1000):
    structure.perturb(0.1)
    energy = structure.energy()
    deltaE = energy-lastEnergy
    if ( deltaE > 0 ):
	print "Energy up:  ",
	boltz = math.exp(-deltaE*beta)
	rnd = random.random()
	print "Boltzman (%lf)  rnd(%lf) "%(boltz,rnd),
	if ( rnd > boltz ):
	    structure.forget()
	    print "Ignore step"
	    continue
	else:
	    print "keep step"
	    lastEnergy = energy
    else:
	print "Energy down"
	lastEnergy = energy
    frameXml = structure.renderToXml()
    animationXml.addChild(frameXml)
    print >> fenergy, "%d %lf %lf"%(step,lastEnergy,structure.endToEndLength())
fenergy.close()
graphicsXml.writeToFileName("_animation.xml")
    
    




print >> gfFour, "</graphics>"
gfFour.close()

print >> gfSix, "</graphics>"
gfSix.close()

print "SUCCESS!!!!!!!!!!!!!!!!"
