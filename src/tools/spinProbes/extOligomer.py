#! /bin/env python


from mbb import *
import math
import random
import copy

PI = 3.1415
PIdiv2 = PI/2


# ###########################################################
# ###########################################################
# ###########################################################
# ###########################################################
# ###########################################################

#
#class HistogramArray:
#    def __init__(self,dataNames,bins):
#	self._histograms = []
#	for z in range(0,len(dataNames)):
#	    self._histograms.append(Histogram(bins))
#	self._names = dataNames
#
#    def addValues(self,vals):
#	for z in range(0,len(vals)):
#	    self._histograms[z].addValue(vals[z])
#
#    def dumpAll(self,prefix,ext):
#	for z in range(0,len(self._histograms)):
#	    self._histograms[z].dump("%s%s.%s"%(prefix,self._names[z],ext))
#
#
#
#class Histogram:
#    def __init__(self,bins):
#	self._bins = bins
#	self._data = []
#	self._sum = 0.0
#	self._dmin = 99999999.9
#	self._dmax = -99999999.9
#
#
#    def addValue(self,v):
#	self._data.append(v)
#	self._sum = self._sum+v
#	if ( v < self._dmin ): self._dmin = v
#	if ( v > self._dmax ): self._dmax = v
#
#    def getAverage(self):
#	return self._sum/len(self._data)
#
#    def getInterval(self):
#	return (self._dmin,self._dmax)
#
#    def calculate(self):
#	binWidth = (self._dmax-self._dmin)/self._bins
#	bins = []
#	for x in range(0,self._bins):
#	    bins.append(0)
#	for v in self._data:
#	    ibin = int((v-self._dmin)/binWidth)
#	    if ( ibin >= len(bins) ): ibin = len(bins)-1
#	    bins[ibin] = bins[ibin]+1
#	for z in range(0,len(bins)):
#	    bins[z] = (self._dmin+binWidth*z,bins[z])
#	return bins
#
#    def getDescription(self):
#	return str(self.calculate())
#
#    def dump(self,fnout):
#	fout = open(fnout,"w")
#	bins = self.calculate()
#	for x,y in bins:
#	    print >> fout,  "%f %f"%(x,y)
#	fout.close()
#
#

class ExtMonomer:
    def __init__(self):
	self._monomer = None
#	self._coords = None
	self._rawCoords = None
	self._couplings = {}
	self._in = None
	self._outs = {}
	pass

    def getName(self):
	return self._monomer.getName()

    def getPdbName(self):
	return self._monomer.getResidue().getPdbName()

    def getSequenceNumber(self):
	return self._monomer.getOligomerSequenceNumber()

	# return a transform True if you can build this monomer from the residue
	# otherwise False
    def buildFromMonomer(self,mon):
#,transform):
	self._monomer = mon
	res = mon.getResidue()
	if ( res.getConstitution().getName() == "Pro4" ):
	    aO = res.atomWithName("CG")
	    aLeft = res.atomWithName("CGC")
	    aRight = res.atomWithName("NG")
	    vO = aO.getPosition()
	    vRight = aRight.getPosition().sub(vO)
	    vLeft = aLeft.getPosition().sub(vO)
	    vX = (vRight.add(vLeft)).normalized()
	    vXY = (vRight.sub(vLeft)).normalized()
	    ucs = CoordinateSystem()
	    ucs.defineForVectorsOriginXDirXYPlane(vO,vX,vXY)
	    self._rawCoords = ucs
#	    if ( transform != None ):
#		cs = ucs.copy()
#	        cs.transformWithMatrix(transform)
#		self._coords = cs
#	    else:
#		cs = CoordinateSystem()
#		self._coords = cs
	    return True
	if ( res.getConstitution().getName() == "Poac" ):
	    aO = res.atomWithName("ND")
	    aX = res.atomWithName("OD")
	    aXY = res.atomWithName("CE")
	    vO = aO.getPosition()
	    vX = (aX.getPosition().sub(vO)).normalized()
	    vXY = (aXY.getPosition().sub(vO)).normalized()
	    ucs = CoordinateSystem()
	    ucs.defineForVectorsOriginXDirXYPlane(vO,vX,vXY)
	    self._rawCoords = ucs
#	    if ( transform != None ):
#		cs = ucs.copy()
#	        cs.transformWithMatrix(transform)
#		self._coords = cs
#	    else:
#		cs = CoordinateSystem()
#		self._coords = cs
	    return True
	# deal with other monomers here
	return False

    def getRawCoordinateSystem(self):
	return self._rawCoords

#    def getTransformedCoordinateSystem(self):
#	return self._coords

    def getMonomer(self):
	return self._monomer

    def renderAsXml(self):
	xml = xmlBlock()
	vo = self._coords.getOrigin()
	vx = vo.add(self._coords.getX())
	vy = vo.add(self._coords.getY())
	vz = vo.add(self._coords.getZ())
	line = xmlLine("FF0000",2,vo,vx)
	xml.addChild(line)
	line = xmlLine("00FF00",2,vo,vy)
	xml.addChild(line)
	line = xmlLine("0000FF",2,vo,vz)
	xml.addChild(line)
	return xml

    def renderRelativeToAsXml(self,coord):
	trans = coord.matrixToCanonical()
	xml = xmlBlock()
	vo = self._rawCoords.getOrigin()
	sys.stdout.flush()
	vx = vo.add(self._rawCoords.getX())
	vy = vo.add(self._rawCoords.getY())
	vz = vo.add(self._rawCoords.getZ())
	vo = trans.multiplyByVector3(vo)
	vx = trans.multiplyByVector3(vx)
	vy = trans.multiplyByVector3(vy)
	vz = trans.multiplyByVector3(vz)
	line = xmlLine("FF0000",2,vo,vx)
	xml.addChild(line)
	line = xmlLine("00FF00",2,vo,vy)
	xml.addChild(line)
	line = xmlLine("0000FF",2,vo,vz)
	xml.addChild(line)
	return xml


    def addOut(self,c):
	self._outs[c.getCoupling().getName()] = c

    def getOuts(self):
	return self._outs.values()

    def setIn(self,c):
	self._in = c

    def getIn(self):
	return self._in



class ExtCoupling:
    def __init__(self):
	self._coupling = None
	self._in = None
	self._out = None
	pass

    def getName(self):
	return self._coupling.getName()

    def setCoupling(self,c):
	self._coupling = c

    def getCoupling(self):
	return self._coupling

    def setIn(self,cgMon):
	self._in = cgMon

    def getIn(self):
	return self._in

    def setOut(self,cgMon):
	self._out = cgMon

    def getOut(self):
	return self._out


    def renderAsXml(self):
	vi = self._in.getTransformedCoordinateSystem().getOrigin()
	vo = self._out.getTransformedCoordinateSystem().getOrigin()
	line = xmlLine("FFFF00",3,vi,vo)
	return line

    def renderRelativeToAsXml(self,coord):
	trans = coord.matrixToCanonical()
	vi = self._in.getRawCoordinateSystem().getOrigin()
	vo = self._out.getRawCoordinateSystem().getOrigin()
	vi = trans.multiplyByVector3(vi)
	vo = trans.multiplyByVector3(vo)
	line = xmlLine("FF00FF",3,vi,vo)
	return line


	


class ExtOligomer:
    def __init__(self,aDb):
	self._db = aDb
	self._monomers = []
	self._couplings = []
	self._aggregate = None
	self._oligomer = None
#	self._transform = None

    def getOligomer(self):
	return self._oligomer

    def getAggregate(self):
	return self._aggregate

#    def getTransform(self):
#	return self._transform

    def findExtMonomerForMonomer(self,mon):
	for cgm in self._monomers:
	    if ( cgm.getMonomer() == mon ):
		return cgm
	return None

    def buildFromAggregate(self,agg):
	self._aggregate = agg
	mol = agg.firstMolecule()
	self._oligomer = Oligomer(self._db)
	self._oligomer.defineFromMolecule(mol)
        for mon in self._oligomer.getMonomers():
	    cgmon = ExtMonomer()
	    success = cgmon.buildFromMonomer(mon)
	    if ( success ):
		self._monomers.append(cgmon)
	for mon in self._monomers:
	    outCouplings = mon.getMonomer().getOutCouplings()
	    VP0("monomer(%s) outCouplings=%s"%(mon.getMonomer().description(),str(outCouplings)))
	    for oc in outCouplings:
		if ( oc == None ):
		    raise "Outcoupling was NONE"
		else:
		    VP0( "oc name = %s"%oc.getName())
	    	om = oc.getOut()
		cgom = self.findExtMonomerForMonomer(om)
		if ( cgom != None ):
		    coup = ExtCoupling()
		    coup.setIn(mon)
		    coup.setOut(cgom)
		    coup.setCoupling(oc)
		    mon.addOut(coup)
		    cgom.setIn(coup)
		    self._couplings.append(coup)

    def buildFromAggregateInFile(self,fn):
	agg = Aggregate()
        mol2ReadAggregateFromFileName(agg,fn)
#	print "DEBUG DEBUG DEBUG"
#	print agg.asXml().asString()
	self.buildFromAggregate(agg)

    def renderAsXml(self):
	xml = xmlBlock()
	for z in self._monomers:
	    one = z.renderAsXml()
	    xml.addChild(one)
	for z in self._couplings:
	    one = z.renderAsXml()
	    xml.addChild(one)
	return xml

    def getSegments(self):
	segs = []
	for z in self._couplings:
	    cgs = ExtSegment(self._aggregate)
	    cgs.defineFromExtCoupling(z)
	    segs.append(cgs)
	return segs


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

#    def buildSegments(self):
#	segs = []
#	coord = CoordinateSystem()
#	segs.append(coord)
#	for mon in self._sequence:
#	    segCoord = mon.build(coord)
#	    segs.append(segCoord)
#	    coord = segCoord
#	return segs

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


class ExtSegment:
    def __init__(self,agg=None):
	self._aggregate = agg
	self._Gimbal = None
	self._cgCoupling = None

    def getEnvironment(self):
	cgc = self._cgCoupling
	min = cgc.getIn()
	mout = cgc.getOut()
	k = "%s-%s%d+%s%d"%(cgc.getName(),
				min.getPdbName(),min.getSequenceNumber(),
				mout.getPdbName(),mout.getSequenceNumber())
	return k

    def parametersAsXml(self):
	xml = self._Gimbal.asXml()
	xml.setLocalName("frame")
#	ch = QuickDomNode("x")
#	ch.setCharacters(str(self._dir.getX()))
#	xml.addChild(ch)
#	ch = QuickDomNode("y")
#	ch.setCharacters(str(self._dir.getY()))
#	xml.addChild(ch)
#	ch = QuickDomNode("z")
#	ch.setCharacters(str(self._dir.getZ()))
#	xml.addChild(ch)
#	ch = QuickDomNode("r")
#	ch.setCharacters(str(self._trans))
#	xml.addChild(ch)
#	ch = QuickDomNode("rotx")
#	ch.setCharacters(str(self._rotx))
#	xml.addChild(ch)
#	ch = QuickDomNode("roty")
#	ch.setCharacters(str(self._roty))
#	xml.addChild(ch)
#	ch = QuickDomNode("rotz")
#	ch.setCharacters(str(self._rotz))
#	xml.addChild(ch)
	return xml

    def getTransformedAggregate(self):
	agg = self._aggregate.deepCopy()
	cs = self._cgCoupling.getIn().getRawCoordinateSystem()
	transform = cs.matrixToCanonical()
	agg.applyTransformToAtoms(transform)
	return agg

    def renderAsXml(self):
	xml = xmlBlock()
	min = self._cgCoupling.getIn()
	mout = self._cgCoupling.getOut()
	coord = min.getRawCoordinateSystem()
	xml.addChild(min.renderRelativeToAsXml(coord))
	xml.addChild(mout.renderRelativeToAsXml(coord))
	xml.addChild(self._cgCoupling.renderRelativeToAsXml(coord))
	return xml

    def renderDirectionAsXml(self,sc):
	xml = xmlPoint("202020",self._Gimbal.getDirection().multiplyByScalar(sc))
	return xml
	
    def getValueNames(self):
	return self._Gimbal.asLine(False)
	return ( "rotx","roty","rotz","rotyy","rotzz", "distance" )

    def getValues(self):
	return (
self._Gimbal.getRotX()/0.0174533,
self._Gimbal.getRotY()/0.0174533,
self._Gimbal.getRotZ()/0.0174533,
self._Gimbal.getRotYY()/0.0174533,
self._Gimbal.getRotZZ()/0.0174533,
self._Gimbal.getDistance() )
	
    def descriptionHeader(self):
	return self._Gimbal.asLine(False)

    def getDescription(self):
	return self._Gimbal.asLine(True)


    def defineFromExtCoupling(self,cgc):
	self._cgCoupling = cgc
	c1 = cgc.getIn().getRawCoordinateSystem()
	c2 = cgc.getOut().getRawCoordinateSystem()
	gimbal = GimbalTransform()
	gimbal.defineForCoordinateSystemToCoordinateSystem(c1,c2)
	self._Gimbal = gimbal
	return

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


    def getGimbal(self):
	return self._Gimbal
