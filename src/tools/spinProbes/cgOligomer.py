
import random

from mbb import *

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
	cs = CoordinateSystem();
	mat = self._Oligomer.getTransformCascade().getResultTransform(self._TransformCascadeIndex)
	cs.transformWithMatrix(mat)
	return cs
#	return self._Oligomer.getTransformCascade().getCoordinateSystem(self._TransformCascadeIndex)
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

    def asXml(self):
	node = QuickDomNode("CGCoupling")
	node.addAttributeInt("transformCascadeIndex",self._TransformCascadeIndex )
	node.addAttributeString("builder",self._Builder.getName())
	child = self._Builder.asXmlVar(self._Variables)
	node.addChild(child)
	return node

    def getTransformCascadeIndex(self):
	return self._TransformCascadeIndex

    def setTransformCascadeIndex(self,i):
	self._TransformCascadeIndex = i

    def setOligomer(self,anOligomer):
	self._Oligomer = anOligomer

    def dump(self):
	pass
#	print "Coupling TCindex=",self._TransformCascadeIndex

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


    def originStep(self):
	self._Matrix = None
	self._SavedVariables = self._Variables
	self._Variables = self._Builder.originStep(self._Variables)
	mat = self.getMatrix()
	tc = self._Oligomer.getTransformCascade()
	tc.setMatrix(self._TransformCascadeIndex,mat)


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
    def renderAsXml(self,frac):
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
	poly = xmlBeginPolygonNoNormal("%2x0000"%(200*frac+55)) #,vooplane)
	xmlVertexVector3(poly,vin.add(vplusOffset))
	xmlVertexVector3(poly,vright.add(vhalf).add(vplusOffset))
	xmlVertexVector3(poly,vout.add(vplusOffset))
	xmlVertexVector3(poly,vleft.add(vhalf).add(vplusOffset))
	xml.addChild(poly)
	poly = xmlBeginPolygonNoNormal("0000%2x"%(200*frac+55)) #,vooplane)
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


    def describe(self):
	print "CGCoupling ======"
	print " monomer(%s) --> monomer(%s)"%(self._In.getName(),self._Out.getName())
	print " parameters---"
	print self._Builder.getParameterDescriptionAtPoint(self._Variables)
	print

class	CGBOligomer:
    def __init__(self,aName):
	self._Name = aName
	self._Monomers = []
	self._Couplings = []
	self._TransformCascade = None

    def getName(self):
	return self._Name

    def getTransformCascade(self):
	return self._TransformCascade

    def describe(self):
	print "Oligomer=============================="
	for c in self._Couplings:
	    c.describe()


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
#	print "bfIndex = ",bfIndex
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


    def originStep(self):
	for c in self._Couplings:
	    c.originStep()
	self._TransformCascade.build()


    def randomStep(self):
	if ( self._TransformCascade == None ):
	    self.buildTransformCascade()
	i = random.randint(0,len(self._Couplings)-1)
	self._CouplingStepped = i
	self._Couplings[i].randomStep()
#	print self._TransformCascade.asXml().asString() # DEBUG

    def forgetStep(self):
	self._Couplings[self._CouplingStepped].forgetStep()


    def asXml(self):
	node = QuickDomNode("CGOligomer")
	child = self._TransformCascade.asXml()
	node.addChild(child)
	couplings = QuickDomNode("Couplings")
	for c in self._Couplings:
	    child = c.asXml()
	    couplings.addChild(child)
	node.addChild(couplings)
	return node

    def renderAsXml(self,frac):
	if ( self._TransformCascade == None ):
	    self.buildTransformCascade()
	xml = xmlBlock()
	for z in self._Couplings:
	    c = z.renderAsXml(frac)
	    xml.addChild(c)
	return xml

    def getEnergy(self):
	if ( self._TransformCascade == None ):
	    self.buildTransformCascade()
	e = 0.0
	for z in self._Couplings:
	    e = e + z.getEnergy()
	return e






def buildNMer(n,trailPoac,middle,lead,leadPoac):
    olig = CGBOligomer()
    monTail = None
    monPrev = None
    monLead = None
    firstMiddleCoup = None
    for i in range(0,n-1):
	mon = CGBMonomer("SS%d"%(i+1))
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
    mon = CGBMonomer("Poac_Trail")
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
    
    mon = CGBMonomer("Poac_Lead")
    olig.addMonomer(mon)
    coup = CGBCoupling(leadPoac)
    coup.setBetween(monLead,mon)
    olig.addCoupling(coup)
    monLead = mon
    return (olig,monTail,monLead,firstMiddleCoup)

