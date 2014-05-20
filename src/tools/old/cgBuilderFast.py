#! /bin/env python

import hotshot
import getopt

from mbb import *
import sys
import random
import math

#profiling = True
profiling = False
parm = True

class Histogram:
    def __init__(self,bins):
	self._bins = bins
	self._data = []
	self._sum = 0.0
	self._dmin = 99999999.9
	self._dmax = -99999999.9


    def addValue(self,v):
	self._data.append(v)
	self._sum = self._sum+v
	if ( v < self._dmin ): self._dmin = v
	if ( v > self._dmax ): self._dmax = v

    def getAverage(self):
	return self._sum/len(self._data)

    def getInterval(self):
	return (self._dmin,self._dmax)

    def calculate(self):
	binWidth = (self._dmax-self._dmin)/self._bins
	bins = []
	for x in range(0,self._bins):
	    bins.append(0)
	for v in self._data:
	    ibin = int((v-self._dmin)/binWidth)
	    if ( ibin >= len(bins) ): ibin = len(bins)-1
	    bins[ibin] = bins[ibin]+1
	for z in range(0,len(bins)):
	    bins[z] = (self._dmin+binWidth*z,float(bins[z])/float(len(self._data)))
	return bins

    def getDescription(self):
	return str(self.calculate())

    def dump(self,fnout):
	fout = open(fnout,"w")
	bins = self.calculate()
	for x,y in bins:
	    print >> fout,  "%f %f"%(x,y)
	fout.close()

    def totalCounts(self):
	c = 0.0
	bins = self.calculate()
	for x,y in bins:
	    c = c + y
	return c

    def getNumericalFunction(self):
	nf = NumericalFunction()
	for x,y in bins:
	    nf.addValues(x,y)
	return nf


#
# A CGTableBuilder uses lookup tables to lookup matrices
# to build structures
#
class	CGTableBuilder:
    def __init__(self):
	self._Matrices = []
	self._OriginalMatrices = []
	self._Values = []
	self._Name = ""

    def transform(self,rotx,roty,rotz,tx,ty,tz,tr):
	dir = Vector3(tx,ty,tz)
	trans = dir.multiplyByScalar(tr)
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
	
    def appendSegmentMatrices(self,aNode):
	frames = 0
	for c in aNode.getChildren():
	    if ( c.isNamed("frame") ):
		rotx = float(c.getAttribute("rotx"))
		roty = float(c.getAttribute("roty"))
		rotz = float(c.getAttribute("rotz"))
		tx = float(c.getAttribute("x"))
		ty = float(c.getAttribute("y"))
		tz = float(c.getAttribute("z"))
		tr = float(c.getAttribute("r"))
		self._Values.append((rotx,roty,rotz,tx,ty,tz,tr))
		mtransform = self.transform(rotx,roty,rotz,tx,ty,tz,tr)
		self._OriginalMatrices.append(mtransform)
		self._Matrices.append(mtransform)
		frames = frames + 1

    def getName(self):
	return self._Name

    def appendMatrices(self,aFile,aSegmentName):
	self._Name = aSegmentName
	xml = QuickDomNode()
	xml.parseFileName(aFile)
	for c in xml.getChildren():
	    if ( c.isNamed("segment") ):
		if ( c.getAttribute("name")==aSegmentName ):
		    self.appendSegmentMatrices(c)

    def getStartingVariables(self):
	return self.randomStep(0)

    def numberOfMatrices(self):
	return len(self._Matrices)

    def getMatrix(self,vi):
	return self._Matrices[vi]

    def getEquilibriumMatrix(self):
	return self._Matrices[0]

    def randomStep(self,vi):
	nvi = random.randint(0,len(self._Matrices)-1)
	return nvi

    def getEnergy(self,vi):
	return 0.0

    def outputValues(self,fn):
	fout = open(fn,"w")
	fout.write("# rotx roty rotz dx dy dz dist\n" )
	for z in self._Values:
	    fout.write("%f %f %f %f %f %f %f\n"%z)
	fout.close()



#
# A CGParameterizedBuilder uses parameters to construct
# matrices to build structures
#
class	CGParameterizedPro4Builder:
    def __init__(self):
	self._Name = ""
	self._RotXParms = None
	self._RotYParms = None
	self._RotZParms = None
	self._RotYYParms = None
	self._RotZZParms = None
	self._DirectionTransform = None
				# rotx,roty,rotz,rotyy,rotzz,dist
	self._VariableTemplate = (0.0,0.0,0.0,0.0,0.0,0.0)
	self._Transform = None 

    def modifyRotX(self,mx0,mk):
	a = mx0*0.0174533
	self._RotXParms = (self._RotXParms[0]+a,self._RotXParms[1]*mk)

    def modifyRotY(self,mx0,mk):
	a = mx0*0.0174533
	self._RotYParms = (self._RotYParms[0]+a,self._RotYParms[1]*mk)

    def modifyRotZ(self,mx0,mk):
	a = mx0*0.0174533
	self._RotZParms = (self._RotZParms[0]+a,self._RotZParms[1]*mk)

    def modifyRotYY(self,mx0,mk):
	a = mx0*0.0174533
	self._RotYYParms = (self._RotYYParms[0]+a,self._RotYYParms[1]*mk)

    def modifyRotZZ(self,mx0,mk):
	a = mx0*0.0174533
	self._RotZZParms = (self._RotZZParms[0]+a,self._RotZZParms[1]*mk)

    def modifyDist(self,mx0,mk):
	a = mx0
	self._DistParms = (self._DistParms[0]+a,self._DistParms[1]*mk)

    def extractParameters(self,node):
	x0 = float(node.getAttribute("x0"))
	k = float(node.getAttribute("k"))
	return (x0,k)
	
    def defineFromFile(self,aFileName):
	xml = QuickDomNode()
	xml.parseFileName(aFileName)
	self._Name = xml.getAttribute("name")
	for c in xml.getChildren():
	    if ( c.isNamed("rotx") ):
		self._RotXParms = self.extractParameters(c)
	    if ( c.isNamed("roty") ):
		self._RotYParms = self.extractParameters(c)
	    if ( c.isNamed("rotz") ):
		self._RotZParms = self.extractParameters(c)
	    if ( c.isNamed("rotyy") ):
		self._RotYYParms = self.extractParameters(c)
	    if ( c.isNamed("rotzz") ):
		self._RotZZParms = self.extractParameters(c)
	    if ( c.isNamed("dist") ):
		self._DistParms = self.extractParameters(c)
	    if ( c.isNamed("directionTransform") ):
		self._DirectionTransform = Matrix()
		self._DirectionTransform.setFromString(c.getCharacters())

    def outputParameters(self,fout,nm,parms):
	fout.write("%s  x0=%f  k=%f\n"%(nm,parms[0],parms[1]))

    def outputValues(self,fn):
	fout = open(fn,"w")
	self.outputParameters(fout, "rotx",self._RotXParms )
	self.outputParameters(fout, "roty",self._RotYParms )
	self.outputParameters(fout, "rotz",self._RotZParms )
	self.outputParameters(fout, "rotyy",self._RotYYParms )
	self.outputParameters(fout, "rotzz",self._RotZZParms )
	self.outputParameters(fout, "dist",self._DistParms )
	fout.write("directionTransform\n")
	fout.write("%s"%(self._DirectionTransform.asString()))
	fout.close()

    def monomerDirection(self,tyy,tzz):
	dd = Vector3(math.cos(tyy)+math.cos(tzz),math.sin(tzz),-math.sin(tyy))
	dd = dd.normalized()
	vv = self._DirectionTransform.multiplyByVector3(dd)
	return vv


    def transform(self,rotx,roty,rotz,tx,ty,tz,tr):
	dir = Vector3(tx,ty,tz)
	trans = dir.multiplyByScalar(tr)
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
	
	    
    def getName(self):
	return self._Name

    def numberOfMatrices(self):
	return len(self._Matrices)

    def getStartingVariables(self):
	return self.randomStep(self._VariableTemplate)

    def getMatrix(self,vi):
	drotx,droty,drotz,drotyy,drotzz,ddist = vi
	rotx = drotx + self._RotXParms[0]
	roty = droty + self._RotYParms[0]
	rotz = drotz + self._RotZParms[0]
	rotyy = drotyy + self._RotYYParms[0]
	rotzz = drotzz + self._RotZZParms[0]
	dist = ddist + self._DistParms[0]
	dir = self.monomerDirection(rotyy,rotzz)
	tm = self.transform(rotx,roty,rotz,dir.getX(),dir.getY(),dir.getZ(),dist)
	return tm
	
    def getEquilibriumMatrix(self):
	rotx = self._RotXParms[0]
	roty = self._RotYParms[0]
	rotz = self._RotZParms[0]
	rotyy = self._RotYYParms[0]
	rotzz = self._RotZZParms[0]
	dist = self._DistParms[0]
	dir = self.monomerDirection(rotyy,rotzz)
	tm = self.transform(rotx,roty,rotz,dir.getX(),dir.getY(),dir.getZ(),dist)
	return tm
	
    def randomStep(self,vi):
	nvi = []
	for i in range(0,len(vi)):
	    nvi.append(random.random()*0.2-0.1+vi[i])
	return nvi
	    

    def getEnergy(self,vi):
	de = 0.0
	de = de + vi[0]*vi[0]*self._RotXParms[1] 
	de = de + vi[1]*vi[1]*self._RotYParms[1] 
	de = de + vi[2]*vi[2]*self._RotZParms[1] 
	de = de + vi[3]*vi[3]*self._RotYYParms[1] 
	de = de + vi[4]*vi[4]*self._RotZZParms[1] 
	de = de + vi[5]*vi[5]*self._DistParms[1] 
	return de



class	CGBMonomer:
    def __init__(self,aName):
	self._Name = aName
	self._Coords = CoordinateSystem()
	self._In = None
	self._Outs = []
	self._Oligomer = None
	self._TransformCascadeIndex = None

    def getTransformCascadeIndex(self):
	return self._TransformCascadeIndex

    def setTransformCascadeIndex(self,i):
	self._TransformCascadeIndex = i

    def setOligomer(self,anOligomer):
	self._Oligomer = anOligomer


    def getName(self):
	return self._Name

    def setIn(self,anIn):
	self._In = anIn

    def addOut(self,anOut):
	self._Outs.append(anOut)

    def getIn(self):
	return self._In

    def getOut(self):
	return self._Outs

    def getCoordinates(self):
	return self._Oligomer.getTransformCascade().getCoordinateSystem(self._TransformCascadeIndex)
#	return self._Coords

#    def setCoordinates(self,aCoord):
#	self._Coords = aCoord






class	CGBCoupling:
    def __init__(self,aBuilder):
	self._Builder = aBuilder
	self._In = None
	self._Out = None
	self._Variables = aBuilder.getStartingVariables()
	self._Matrix = None
	self._Oligomer = None
	self._TransformCascadeIndex = None

    def getTransformCascadeIndex(self):
	return self._TransformCascadeIndex

    def setTransformCascadeIndex(self,i):
	print "Setting coupling TCindex to ",i
	self._TransformCascadeIndex = i

    def setOligomer(self,anOligomer):
	self._Oligomer = anOligomer

    def dump(self):
	print "Coupling TCindex=",self._TransformCascadeIndex

    def setIn(self,anIn):
	self._In = anIn

    def getIn(self):
	return self._In

    def getOut(self):
	return self._Out

    def setOut(self,anOut):
	self._Out = anOut

    def setBetween(self,mon1,mon2):
	self.setIn(mon1)
	self.setOut(mon2)
	mon2.setIn(self)
	mon1.addOut(self)

    def renderDirectionAsXml(self):
	vin = self.getIn().getCoordinates().getOrigin()
	vout = self.getOut().getCoordinates().getOrigin()
	vdiff = vout.sub(vin)
	vdiff = vdiff.normalized().multiplyByScalar(4.5)
	pnt = xmlPoint("0000FF",vdiff)
	return pnt


    def renderCoordinateSystem(self,xml,coord):
	vo = coord.getOrigin()
	vx = vo.add(coord.getX())
	vy = vo.add(coord.getY())
	vz = vo.add(coord.getZ())
	line = xmlLine("FF0000",4,vo,vx)
	xml.addChild(line)
	line = xmlLine("00FF00",4,vo,vy)
	xml.addChild(line)
	line = xmlLine("0000FF",4,vo,vz)
	xml.addChild(line)
	
    def renderEquilibriumToXml(self,xml):
	coord = CoordinateSystem()
	self.renderCoordinateSystem(xml,coord)
	mat = self._Builder.getEquilibriumMatrix()
	coord.transformWithMatrix(mat)
	self.renderCoordinateSystem(xml,coord)

    def getMatrix(self):
	if ( self._Matrix != None ):
	    return self._Matrix
	mat = self._Builder.getMatrix(self._Variables)
	self._Matrix = mat
	return mat


    def randomStep(self):
	self._Matrix = None
	self._SavedVariables = self._Variables
	self._Variables = self._Builder.randomStep(self._Variables)
	mat = self.getMatrix()
	tc = self._Oligomer.getTransformCascade()
	tc.setMatrix(self._TransformCascadeIndex,mat)
	tc.build()
#	print "DEBUG: ", tc.asXml().asString()

		#
		# If we have SavedVariables then we can forget the
		# current ones
		#
    def forgetStep(self):
	self._Matrix = None
	if ( self._SavedVariables != None ):
	    self._Variables = self._SavedVariables
	    self._SavedVariables = None
	    mat = self.getMatrix()
	    tc = self._Oligomer.getTransformCascade()
	    tc.setMatrix(self._TransformCascadeIndex,mat)
	return

    def getEnergy(self):
	return self._Builder.getEnergy(self._Variables)

		# render coupling
    def renderAsXml(self):
	xml = xmlGraphics("coupling")
	vin = self.getIn().getCoordinates().getOrigin()
	vout = self.getOut().getCoordinates().getOrigin()
	vhalf = (vin.add(vout).multiplyByScalar(0.5))
	vdiff = vout.sub(vin)
	clen = vdiff.length()
	vdiffn = vdiff.normalized()
	vy = self.getIn().getCoordinates().getY()
	vplane = (vy.crossProduct(vdiff)).normalized()
	vooplane = vplane.crossProduct(vdiff).normalized()
	vleft = vplane.multiplyByScalar(clen/4.0)
	vright = vplane.multiplyByScalar(-clen/4.0)
	vplusOffset = vooplane.multiplyByScalar(0.2)
	vminusOffset = vooplane.multiplyByScalar(-0.2)
	sideColor = "000000"
	poly = xmlBeginPolygonNoNormal("FF0000") #,vooplane)
	xmlVertexVector3(poly,vin.add(vplusOffset))
	xmlVertexVector3(poly,vright.add(vhalf).add(vplusOffset))
	xmlVertexVector3(poly,vout.add(vplusOffset))
	xmlVertexVector3(poly,vleft.add(vhalf).add(vplusOffset))
	xml.addChild(poly)
	poly = xmlBeginPolygonNoNormal("0000FF") #,vooplane)
	xmlVertexVector3(poly,vin.add(vminusOffset))
	xmlVertexVector3(poly,vright.add(vhalf).add(vminusOffset))
	xmlVertexVector3(poly,vout.add(vminusOffset))
	xmlVertexVector3(poly,vleft.add(vhalf).add(vminusOffset))
	xml.addChild(poly)
	poly = xmlBeginPolygonNoNormal(sideColor) #,vooplane)
	xmlVertexVector3(poly,vin.add(vminusOffset))
	xmlVertexVector3(poly,vright.add(vhalf).add(vminusOffset))
	xmlVertexVector3(poly,vright.add(vhalf).add(vplusOffset))
	xmlVertexVector3(poly,vin.add(vplusOffset))
	xml.addChild(poly)
	poly = xmlBeginPolygonNoNormal(sideColor) #,vooplane)
	xmlVertexVector3(poly,vright.add(vhalf).add(vplusOffset))
	xmlVertexVector3(poly,vright.add(vhalf).add(vminusOffset))
	xmlVertexVector3(poly,vout.add(vminusOffset))
	xmlVertexVector3(poly,vout.add(vplusOffset))
	xml.addChild(poly)
	poly = xmlBeginPolygonNoNormal(sideColor) #,vooplane)
	xmlVertexVector3(poly,vleft.add(vhalf).add(vplusOffset))
	xmlVertexVector3(poly,vout.add(vplusOffset))
	xmlVertexVector3(poly,vout.add(vminusOffset))
	xmlVertexVector3(poly,vleft.add(vhalf).add(vminusOffset))
	xml.addChild(poly)
	poly = xmlBeginPolygonNoNormal(sideColor) #,vooplane)
	xmlVertexVector3(poly,vleft.add(vhalf).add(vplusOffset))
	xmlVertexVector3(poly,vin.add(vplusOffset))
	xmlVertexVector3(poly,vin.add(vminusOffset))
	xmlVertexVector3(poly,vleft.add(vhalf).add(vminusOffset))
	xml.addChild(poly)
        sph = xmlSphere( "FF00FF",vin,0.5)
	xml.addChild(sph)
	return xml




	
#	vdiff = vout.sub(vin)
#	l = vdiff.length()
#	if ( l > 10 ):
#	    print self.getMatrix().asString()
#	print "Segment: %s len=%s"%(self._Builder.getName(),l)
	xml = xmlLine("FF00FF",2, vin, vout )
	return xml



class	CGBOligomer:
    def __init__(self):
	self._Monomers = []
	self._Couplings = []
	self._TransformCascade = None

    def getTransformCascade(self):
	return self._TransformCascade

    def addMonomer(self,m):
	if ( self._TransformCascade != None ):
	    raise "You cannot add monomers once you have defined the TransformCascade"
	self._Monomers.append(m)
	m.setOligomer(self)

    def addCoupling(self,c):
	if ( self._TransformCascade != None ):
	    raise "You cannot add couplings once you have defined the TransformCascade"
	self._Couplings.append(c)
	c.setOligomer(self)

    def getRoot(self):
	mon = self._Monomers[0]
	inc = mon.getIn()
	while ( inc != None ):
	    mon = inc.getIn()
	    inc = mon.getIn()
	return mon

    def dump(self):
	for x in self._Couplings:
	    x.dump()

#    def buildFromChild(self,ch):
#	for coup in ch.getOut():
#	    coords = coup.getOut().getCoordinates()
#	    coords.canonical()
#	    matrix = coup.getMatrix()
#	    coords.transformWithMatrix(matrix)
#	    coords.transformWithMatrix(coup.getIn().getCoordinates().matrixFromCanonical())
#	    coup.getOut().setCoordinates(coords)
#	    self.buildFromChild(coup.getOut())
  
    def buildFromChild(self,mon):
	bfIndex = mon.getTransformCascadeIndex()
	print "bfIndex = ",bfIndex
	for coup in mon.getOut():
	    cascadeIndex = self._TransformCascade.addEntry(bfIndex)
	    self._TransformCascade.setMatrix(cascadeIndex,coup.getMatrix())
	    coup.setTransformCascadeIndex(cascadeIndex)
	    monOut = coup.getOut()
	    monOut.setTransformCascadeIndex(cascadeIndex)
	    self.buildFromChild(monOut)
  
    def buildTransformCascade(self):
	if ( self._TransformCascade == None ):
	    self._TransformCascade = TransformCascade()
	    mon = self.getRoot()
	    mon.setTransformCascadeIndex(0)
	    self.buildFromChild(mon)

    def build(self):
	if ( self._TransformCascade == None ):
	    self.buildTransformCascade()
	self._TransformCascade.build()

#    def build(self):
#	r = self.getRoot()
#	self.buildFromChild(r)


    def randomStep(self):
	if ( self._TransformCascade == None ):
	    self.buildTransformCascade()
	i = random.randint(0,len(self._Couplings)-1)
	self._CouplingStepped = i
	self._Couplings[i].randomStep()
#	print self._TransformCascade.asXml().asString() # DEBUG

    def forgetStep(self):
	self._Couplings[self._CouplingStepped].forgetStep()


    def renderAsXml(self):
	if ( self._TransformCascade == None ):
	    self.buildTransformCascade()
	xml = xmlBlock()
	for z in self._Couplings:
	    c = z.renderAsXml()
	    xml.addChild(c)
	return xml

    def getEnergy(self):
	if ( self._TransformCascade == None ):
	    self.buildTransformCascade()
	e = 0.0
	for z in self._Couplings:
	    e = e + z.getEnergy()
	return e


def monteCarlo(maxSteps,olig,ends,histo,animationXml,directionXml,watchCoup):
    rejectedSteps = 0
    acceptedSteps = 0
    beta = 0.002*300
    prevEnergy = None
    cnt = 0
    if ( watchCoup != None ):
	if ( directionXml != None ):
	    watchCoup.renderEquilibriumToXml(directionXml)
    while ( cnt < maxSteps ):
	olig.randomStep()
	energy = olig.getEnergy()
	if ( prevEnergy == None ):
	    prevEnergy = energy
	deltaE = energy - prevEnergy
	if ( deltaE > 0.0 ):
	    boltz = math.exp(-deltaE*beta)
	    rnd = random.random()
	    if ( rnd>boltz ):
		olig.forgetStep()
		rejectedSteps = rejectedSteps + 1
	        continue	
	prevEnergy = energy
	acceptedSteps = acceptedSteps + 1
	olig.build()
	vs = ends[0].getCoordinates().getOrigin()
	ve = ends[1].getCoordinates().getOrigin()
	vDiff = ve.sub(vs)
	histo.addValue(vDiff.length())
	if ( cnt%50000 == 0 ):
	    sys.stdout.write("\nStep: %6d "%cnt) 
	    sys.stdout.flush()
	if ( cnt%1000 == 0 ):
	    sys.stdout.write(".")
	    sys.stdout.flush()
	if ( cnt%100==0 ):
	    if ( animationXml != None ):
	        animationXml.addChild(olig.renderAsXml())
	    if ( directionXml != None ):
		directionXml.addChild(watchCoup.renderDirectionAsXml())
	cnt = cnt + 1
    print "\nRan %d steps"%cnt
    print "Steps rejected: %6d"%rejectedSteps
    print "Steps accepted: %6d"%acceptedSteps
    print "Bin integral:   %6f"%histo.totalCounts()



def buildNMer(n,trailPoac,middle,lead,leadPoac):
    olig = CGBOligomer()
    monTail = None
    monPrev = None
    monLead = None
    firstMiddleCoup = None
    for i in range(0,n-1):
	mon = CGBMonomer("SS")
	olig.addMonomer(mon)
	if ( monTail == None ):
	    monTail = mon
	else:
	    coup = CGBCoupling(middle)
	    coup.setBetween(monPrev,mon)
	    olig.addCoupling(coup)
	    if ( firstMiddleCoup == None ):
		firstMiddleCoup = coup
	monLead = mon
	monPrev = mon
    mon = CGBMonomer("Poac")
    olig.addMonomer(mon)
    coup = CGBCoupling(trailPoac)
    coup.setBetween(monTail,mon)
    olig.addCoupling(coup)
    monTail = mon

    mon = CGBMonomer("SSl")
    olig.addMonomer(mon)
    coup = CGBCoupling(lead)
    coup.setBetween(monLead,mon)
    olig.addCoupling(coup)
    monLead = mon
    
    mon = CGBMonomer("Poac")
    olig.addMonomer(mon)
    coup = CGBCoupling(leadPoac)
    coup.setBetween(monLead,mon)
    olig.addCoupling(coup)
    monLead = mon
    return (olig,monTail,monLead,firstMiddleCoup)


#def doAll(parmFile,nmer,maxSteps,outputFile,animationXml):




def main():
    optlist, args = getopt.getopt(sys.argv[1:],"ha:s:n:o:p:d:",
			["rotx0=","rotxk=",
			"roty0=","rotyk=",
			"rotz0=","rotzk=",
			"rotyy0=","rotyyk=",
			"rotzz0=","rotzzk=",
			"dist0=","distk="])
    maxSteps = 100000
    nmer = 4
    outputFileTemplate = "_histo%dmer.txt"
    parmFile = "dkp-pro4SS+pro4SS.xml"
    animationFileName = None
    directionFileName = None
    rotx0 = 0.0
    rotxk = 1.0
    roty0 = 0.0
    rotyk = 1.0
    rotz0 = 0.0
    rotzk = 1.0
    rotyy0 = 0.0
    rotyyk = 1.0
    rotzz0 = 0.0
    rotzzk = 1.0
    dist0 = 0.0
    distk = 1.0
    for opt,arg in optlist:
	if ( opt == "--rotx0" ):
	    rotx0 = float(arg)
	    print "Modified rotx0 to: ",rotx0
	if ( opt == "--rotxk" ):
	    rotxk = float(arg)
	    print "Modified rotxk to: ",rotxk
	if ( opt == "--roty0" ):
	    roty0 = float(arg)
	    print "Modified roty0 to: ",roty0
	if ( opt == "--rotyk" ):
	    rotyk = float(arg)
	    print "Modified rotyk to: ",rotyk
	if ( opt == "--rotz0" ):
	    rotz0 = float(arg)
	    print "Modified rotz0 to: ",rotz0
	if ( opt == "--rotzk" ):
	    rotzk = float(arg)
	    print "Modified rotzk to: ",rotzk
	if ( opt == "--rotyy0" ):
	    rotyy0 = float(arg)
	    print "Modified rotyy0 to: ",rotyy0
	if ( opt == "--rotyyk" ):
	    rotyyk = float(arg)
	    print "Modified rotyyk to: ",rotyyk
	if ( opt == "--rotzz0" ):
	    rotzz0 = float(arg)
	    print "Modified rotzz0 to: ",rotzz0
	if ( opt == "--rotzzk" ):
	    rotzzk = float(arg)
	    print "Modified rotzzk to: ",rotzzk
	if ( opt == "--dist0" ):
	    dist0 = float(arg)
	    print "Modified dist0 to: ",dist0
	if ( opt == "--distk" ):
	    distk = float(arg)
	    print "Modified distk to: ",distk
	if ( opt == "-d" ):
	    directionFileName = arg
	if ( opt == "-a" ):
	    animationFileName = arg
	if ( opt == "-s" ):
	    maxSteps= int(arg)
	if ( opt == "-p" ):
	    parmFile = arg
	if ( opt == "-n" ):
	    nmer = int(arg)
	if ( opt == "-o" ):
	    outputFileTemplate = arg
	if ( opt == "-h" ):
	    print "Usage: %s [options] frameDir frameRoot"
	    print "  -h           = Help"
	    print "  -p [file]    = Parameter file, default %s"%parmFile
	    print "  -s [steps]   = Number of Monte Carlo steps, default 100000"
	    print "  -n [nmer]    = Number of SS monomers, default 4"
	    print "  -o [output]  = Output file name; default _histo\%dmer.txt"
	    sys.exit(0)
    outputFile = outputFileTemplate%nmer
    animationXml = None
    if ( animationFileName != None ):
	animationXml = xmlAnimation()
    directionXml = None
    if ( directionFileName != None ):
	directionXml = xmlGraphics("dir")
    
    ssparm = CGParameterizedPro4Builder()
    ssparm.defineFromFile(parmFile)
    ssparm.outputValues("out/_middle_parameters.txt")
    ssparm.modifyRotX(rotx0,rotxk)
    ssparm.modifyRotY(roty0,rotyk)
    ssparm.modifyRotZ(rotz0,rotzk)
    ssparm.modifyRotYY(rotyy0,rotyyk)
    ssparm.modifyRotZZ(rotzz0,rotzzk)
    ssparm.modifyDist(dist0,distk)

    builderSS_dkp_SS = CGTableBuilder()
    builderSS_dkp_SS.appendMatrices("out/_middle_parameters.xml",
						"dkp-OSS2+OSS3")
    builderSS_dkp_SSend = CGTableBuilder()
    builderSS_dkp_SSend.appendMatrices("out/_lead_parameters.xml",
						"dkp-OSS5+OSS2")
    builderSS_dkp_SSend.outputValues("out/_table_SS_dkp_SSlead.txt")

    builderSS_main_Poac = CGTableBuilder()
    builderSS_main_Poac.appendMatrices("out/_lead_parameters.xml",
						"main-OSS2+POA3")
    builderSS_dkp_SSend.outputValues("out/_table_lead_poac.txt")

    builderSS_back_Poac = CGTableBuilder()
    builderSS_back_Poac.appendMatrices("out/_tail_parameters.xml",
						"back-OSS4+POA5")
    builderSS_back_Poac.outputValues("out/_table_back_poac.txt")

    olig,tail,lead,watchCoup = buildNMer(nmer, builderSS_back_Poac, ssparm,
		builderSS_dkp_SSend,builderSS_main_Poac)


    olig.buildTransformCascade()
    olig.dump()


    histo = Histogram(30)
    animationXml = None
    if ( animationFileName != None ):
        animationXml = xmlAnimation()
    print "Running %dmer for %d steps"%(nmer,maxSteps)
    sys.stdout.flush()
    monteCarlo(maxSteps,olig,(tail,lead),
		histo,animationXml,directionXml,watchCoup)
    if ( animationXml != None ):
        xml = xmlGraphics("test")
        xml.addChild(animationXml)
        xml.writeToFileName(animationFileName)
    if ( directionXml != None ):
        xml = xmlGraphics("test")
        xml.addChild(directionXml)
        xml.writeToFileName(directionFileName)
    histo.dump(outputFile)

if (__name__ == "__main__" ):
    if ( profiling ):
        prof = hotshot.Profile("hotshot_edi_stats_new")
        prof.runcall(main)
        prof.close()
    else:
        main()

