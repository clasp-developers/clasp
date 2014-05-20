import math
import random

from mbb import *


#
# A CGTableBuilder uses lookup tables to lookup matrices
# to build structures
#
class	CGTableBuilder:
    def __init__(self):
	self._Matrices = []
	self._OriginalMatrices = []
	self._Gimbals = []
	self._Name = ""

    def asXmlVar(self,parms):
	node = QuickDomNode("TableBuilder")
	child = self._Gimbals[parms].asXml()
	node.addChild(child)
	return node


    def transform(self,gimbal):
	coordSys = gimbal.getCoordinateSystem()
	return coordSys.matrixFromCanonical()
	
    def appendSegmentMatrices(self,aNode):
	frames = 0
	for c in aNode.getChildren():
	    if ( c.isNamed("frame") ):
		gimbal = GimbalTransform()
		gimbal.parseFromXml(c)
		self._Gimbals.append(gimbal)
		mtransform = self.transform(gimbal)
		self._OriginalMatrices.append(mtransform)
		self._Matrices.append(mtransform)
		frames = frames + 1
	VP0( "Appended %d frames"%frames)
	return frames

    def getName(self):
	return self._Name

    def appendMatrices(self,aFile,aSegmentName):
	self._Name = aSegmentName
	xml = QuickDomNode()
	xml.parseFileName(aFile)
	entries = 0
	VP0("Loading entries from file(%s) looking for segment(%s)"%(aFile,aSegmentName))
	for c in xml.getChildren():
	    if ( c.isNamed("segment") ):
		segName = c.getAttribute("name")
		VP0("  looking at segment(%s)"%segName)
		if ( c.getAttribute("name")==aSegmentName ):
		    entries += self.appendSegmentMatrices(c)
		else:
		    VP0("  Skipped segment")

    def getStartingVariables(self):
	return self.randomStep(0)

    def numberOfMatrices(self):
	return len(self._Matrices)

    def getVariablesAsString(self,vi):
	gimbal = self._Gimbals[vi]
	str = gimbal.asString("// ")
	matStr = "// matrix = %s"%self.getMatrix(vi).asString()
	return "%s%s"%(str,matStr)

    def getMatrix(self,vi):
	return self._Matrices[vi]

    def getParameterDescriptionAtPoint(self,vi):
	str = self._Gimbals[vi].asString("//builder  ")
	return str

    def getEquilibriumMatrix(self):
	return self._Matrices[0]

    def randomStep(self,vi):
	nvi = random.randint(0,len(self._Matrices)-1)
	return nvi

    def originStep(self,vi):
	nvi = 0
	return nvi

    def getEnergy(self,vi):
	return 0.0


#
# A CGParameterizedBuilder uses parameters to construct
# matrices to build structures
#
CGParameterizedPro4Builder_XML_Name = "CGParameterizedPro4Builder"
class	CGParameterizedPro4Builder:
    def __init__(self):
	self._Name = ""
	self._BaseRotXParms = None
	self._BaseRotYParms = None
	self._BaseRotZParms = None
	self._BaseRotYYParms = None
	self._BaseRotZZParms = None
	self._BaseDistParms = None
	self._RotXParms = None
	self._RotYParms = None
	self._RotZParms = None
	self._RotYYParms = None
	self._RotZZParms = None
	self._DistParms = None
	self._Gimbal = GimbalTransform()
	self._DirectionTransform = None
				# rotx,roty,rotz,rotyy,rotzz,dist
	self._VariableTemplate = (0.0,0.0,0.0,0.0,0.0,0.0)
	self._Transform = None 


    def setFromConstrainedVector(self,constVector):
	degToRad = 0.0174533
	arotx0 = constVector.getRawPositionValue("rotx0")*degToRad
	aroty0 = constVector.getRawPositionValue("roty0")*degToRad
	arotz0 = constVector.getRawPositionValue("rotz0")*degToRad
	arotyy0 = constVector.getRawPositionValue("rotyy0")*degToRad
	arotzz0 = constVector.getRawPositionValue("rotzz0")*degToRad
	adist0 = constVector.getRawPositionValue("dist0")
	arotxk = pow(10.0,constVector.getRawPositionValue("logrotxk"))
	arotyk = pow(10.0,constVector.getRawPositionValue("logrotyk"))
	arotzk = pow(10.0,constVector.getRawPositionValue("logrotzk"))
	arotyyk = pow(10.0,constVector.getRawPositionValue("logrotyyk"))
	arotzzk = pow(10.0,constVector.getRawPositionValue("logrotzzk"))
	adistk = pow(10.0,constVector.getRawPositionValue("logdistk"))
	self._RotXParms = (self._BaseRotXParms[0]+arotx0,
				self._BaseRotXParms[1]*arotxk)
	self._RotYParms = (self._BaseRotYParms[0]+aroty0,
				self._BaseRotYParms[1]*arotyk)
	self._RotZParms = (self._BaseRotZParms[0]+arotz0,
				self._BaseRotZParms[1]*arotzk)
	self._RotYYParms = (self._BaseRotYYParms[0]+arotyy0,
				self._BaseRotYYParms[1]*arotyyk)
	self._RotZZParms = (self._BaseRotZZParms[0]+arotzz0,
				self._BaseRotZZParms[1]*arotzzk)
	self._DistParms = (self._BaseDistParms[0]+adist0,
				self._BaseDistParms[1]*adistk)


    def getParameterDescription(self):
	str = ""
	degToRad = 0.0174533
	rotX0 = self._RotXParms[0]
	rotXk = self._RotXParms[1]
	rotY0 = self._RotYParms[0]
	rotYk = self._RotYParms[1]
	rotZ0 = self._RotZParms[0]
	rotZk = self._RotZParms[1]
	rotYY0 = self._RotYYParms[0]
	rotYYk = self._RotYYParms[1]
	rotZZ0 = self._RotZZParms[0]
	rotZZk = self._RotZZParms[1]
	str += "//builder   rotX0 = %9.6lf rad (%9.4lf deg)  rotXk = %9.4lf\n"%(rotX0,rotX0/degToRad,rotXk)
	str += "//builder   rotY0 = %9.6lf rad (%9.4lf deg)  rotYk = %9.4lf\n"%(rotY0,rotY0/degToRad,rotYk)
	str += "//builder   rotZ0 = %9.6lf rad (%9.4lf deg)  rotZk = %9.4lf\n"%(rotZ0,rotZ0/degToRad,rotZk)
	str += "//builder  rotYY0 = %9.6lf rad (%9.4lf deg) rotYYk = %9.4lf\n"%(rotYY0,rotYY0/degToRad,rotYYk)
	str += "//builder  rotZZ0 = %9.6lf rad (%9.4lf deg) rotZZk = %9.4lf\n"%(rotZZ0,rotZZ0/degToRad,rotZZk)
	str += "//builder   dist0 = %9.6lf Angstroms            distk = %9.4lf\n"%(self._DistParms)
	return str


    def asXml(self):
	node = QuickDomNode(CGParameterizedPro4Builder_XML_Name)
	node.addAttributeDouble("rotx0",self._RotXParms[0],6,6)
	node.addAttributeDouble("rotxk",self._RotXParms[1],6,6)
	node.addAttributeDouble("roty0",self._RotYParms[0],6,6)
	node.addAttributeDouble("rotyk",self._RotYParms[1],6,6)
	node.addAttributeDouble("rotz0",self._RotZParms[0],6,6)
	node.addAttributeDouble("rotzk",self._RotZParms[1],6,6)
	node.addAttributeDouble("rotyy0",self._RotYYParms[0],6,6)
	node.addAttributeDouble("rotyyk",self._RotYYParms[1],6,6)
	node.addAttributeDouble("rotzz0",self._RotZZParms[0],6,6)
	node.addAttributeDouble("rotzzk",self._RotZZParms[1],6,6)
	node.addAttributeDouble("dist0",self._DistParms[0],6,6)
	node.addAttributeDouble("distk",self._DistParms[1],6,6)
	return node

    def asXmlVar(self,var):
	node = QuickDomNode(CGParameterizedPro4Builder_XML_Name)
	child = QuickDomNode("RotX0")
	child.addAttributeDouble("value",self._RotXParms[0],6,6)
	node.addChild(child)
	child = QuickDomNode("RotXK")
	child.addAttributeDouble("value",self._RotXParms[1],6,6)
	node.addChild(child)
	child = QuickDomNode("RotY0")
	child.addAttributeDouble("value",self._RotYParms[0],6,6)
	node.addChild(child)
	child = QuickDomNode("RotYK")
	child.addAttributeDouble("value",self._RotYParms[1],6,6)
	node.addChild(child)
	child = QuickDomNode("RotZ0")
	child.addAttributeDouble("value",self._RotZParms[0],6,6)
	node.addChild(child)
	child = QuickDomNode("RotZK")
	child.addAttributeDouble("value",self._RotZParms[1],6,6)
	node.addChild(child)
	child = QuickDomNode("RotYY0")
	child.addAttributeDouble("value",self._RotYYParms[0],6,6)
	node.addChild(child)
	child = QuickDomNode("RotYYK")
	child.addAttributeDouble("value",self._RotYYParms[1],6,6)
	node.addChild(child)
	child = QuickDomNode("RotZZ0")
	child.addAttributeDouble("value",self._RotZZParms[0],6,6)
	node.addChild(child)
	child = QuickDomNode("RotZZK")
	child.addAttributeDouble("value",self._RotZZParms[1],6,6)
	node.addChild(child)
	child = QuickDomNode("Dist0")
	child.addAttributeDouble("value",self._DistParms[0],6,6)
	node.addChild(child)
	child = QuickDomNode("DistK")
	child.addAttributeDouble("value",self._DistParms[1],6,6)
	node.addChild(child)
	return node


    def parseFromXml(self,node):
	rotX0 = float(node.getAttribute("rotx0"))
	rotXk = float(node.getAttribute("rotxk"))
	rotY0 = float(node.getAttribute("roty0"))
	rotYk = float(node.getAttribute("rotyk"))
	rotZ0 = float(node.getAttribute("rotz0"))
	rotZk = float(node.getAttribute("rotzk"))
	rotYY0 = float(node.getAttribute("rotyy0"))
	rotYYk = float(node.getAttribute("rotyyk"))
	rotZZ0 = float(node.getAttribute("rotzz0"))
	rotZZk = float(node.getAttribute("rotzzk"))
	dist0 = float(node.getAttribute("dist0"))
	distk = float(node.getAttribute("distk"))
	self._RotXParms = ( rotX0, rotXk )
	self._RotYParms = ( rotY0, rotYk )
	self._RotZParms = ( rotZ0, rotZk )
	self._RotYYParms = ( rotYY0, rotYYk )
	self._RotZZParms = ( rotZZ0, rotZZk )
	self._DistParms = ( dist0, distk )


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
		self._BaseRotXParms = self.extractParameters(c)
	    if ( c.isNamed("roty") ):
		self._BaseRotYParms = self.extractParameters(c)
	    if ( c.isNamed("rotz") ):
		self._BaseRotZParms = self.extractParameters(c)
	    if ( c.isNamed("rotyy") ):
		self._BaseRotYYParms = self.extractParameters(c)
	    if ( c.isNamed("rotzz") ):
		self._BaseRotZZParms = self.extractParameters(c)
	    if ( c.isNamed("dist") ):
		self._BaseDistParms = self.extractParameters(c)
	    if ( c.isNamed("directionTransform") ):
		self._DirectionTransform = Matrix()
		self._DirectionTransform.setFromString(c.getCharacters())
	self._RotXParms  = self._BaseRotXParms
	self._RotYParms  = self._BaseRotYParms
	self._RotZParms  = self._BaseRotZParms
	self._RotYYParms = self._BaseRotYYParms
	self._RotZZParms = self._BaseRotZZParms
	self._DistParms  = self._BaseDistParms

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


#    def transform(self,rotx,roty,rotz,tx,ty,tz,tr):
#	dir = Vector3(tx,ty,tz)
#	trans = dir.multiplyByScalar(tr)
#	mrotx = Matrix()
#	mrotx.rotationX(-rotx)
#	mroty = Matrix()
#	mroty.rotationY(-roty)
#	mrotz = Matrix()
#	mrotz.rotationZ(-rotz)
#	mtrans = Matrix()
#	mtrans.translate(trans)
#	mtransform = Matrix()
#	mtransform = mrotx.multiplyByMatrix(mtransform)
#	mtransform = mroty.multiplyByMatrix(mtransform)
#	mtransform = mrotz.multiplyByMatrix(mtransform)
#	mtransform = mtrans.multiplyByMatrix(mtransform)
#	return mtransform

    def getName(self):
	return self._Name

    def numberOfMatrices(self):
	return len(self._Matrices)

    def getStartingVariables(self):
	return self.randomStep(self._VariableTemplate)


    def getVariablesAsString(self,vi):
	drotx,droty,drotz,drotyy,drotzz,ddist = vi
	rotx = drotx + self._RotXParms[0]
	roty = droty + self._RotYParms[0]
	rotz = drotz + self._RotZParms[0]
	rotyy = drotyy + self._RotYYParms[0]
	rotzz = drotzz + self._RotZZParms[0]
	dist = ddist + self._DistParms[0]
	dir = self.monomerDirection(rotyy,rotzz)
	str = """rotx = %lf
roty = %lf
rotz = %lf
tx   = %lf
ty   = %lf
tz   = %lf
tr   = %lf"""%(rotx,roty,rotz,dir.getX(),dir.getY(),dir.getZ(),dist)
	matStr = "matrix = %s"%self.getMatrix(vi).asString()
	return "%s%s"%(str,matStr)

    def getMatrix(self,vi):
	drotx,droty,drotz,drotyy,drotzz,ddist = vi
	rotx = drotx + self._RotXParms[0]
	roty = droty + self._RotYParms[0]
	rotz = drotz + self._RotZParms[0]
	rotyy = drotyy + self._RotYYParms[0]
	rotzz = drotzz + self._RotZZParms[0]
	dist = ddist + self._DistParms[0]
	self._Gimbal.setRotX(rotx)
	self._Gimbal.setRotY(roty)
	self._Gimbal.setRotZ(rotz)
	self._Gimbal.setRotYY(rotyy)
	self._Gimbal.setRotZZ(rotzz)
	self._Gimbal.setDistance(dist)
	return self._Gimbal.matrixFromCanonical()

    def getParameterDescriptionAtPoint(self,vi):
	drotx,droty,drotz,drotyy,drotzz,ddist = vi
	degToRad = 0.0174533
	rotX = drotx + self._RotXParms[0]
	rotY = droty + self._RotYParms[0]
	rotZ = drotz + self._RotZParms[0]
	rotYY = drotyy + self._RotYYParms[0]
	rotZZ = drotzz + self._RotZZParms[0]
	dist = ddist + self._DistParms[0]
	str = ""
	str += "//builder   rotX = %9.6lf rad (%9.4lf deg)\n"%(rotX,rotX/degToRad)
	str += "//builder   rotY = %9.6lf rad (%9.4lf deg)\n"%(rotY,rotY/degToRad)
	str += "//builder   rotZ = %9.6lf rad (%9.4lf deg)\n"%(rotZ,rotZ/degToRad)
	str += "//builder  rotYY = %9.6lf rad (%9.4lf deg)\n"%(rotYY,rotYY/degToRad)
	str += "//builder  rotZZ = %9.6lf rad (%9.4lf deg)\n"%(rotZZ,rotZZ/degToRad)
	dir = self.monomerDirection(rotYY,rotZZ)
	str += "//builder     tx = %9.6lf Angstroms\n"%dir.getX()
	str += "//builder     ty = %9.6lf Angstroms\n"%dir.getY()
	str += "//builder     tz = %9.6lf Angstroms\n"%dir.getZ()
	
	str += "//builder   dist = %9.6lf Angstroms\n"%dist
	return str

	
	
	    
    def getEquilibriumMatrix(self):
	rotx = self._RotXParms[0]
	roty = self._RotYParms[0]
	rotz = self._RotZParms[0]
	rotyy = self._RotYYParms[0]
	rotzz = self._RotZZParms[0]
	dist = self._DistParms[0]
	self._Gimbal.setRotX(rotx)
	self._Gimbal.setRotY(roty)
	self._Gimbal.setRotZ(rotz)
	self._Gimbal.setRotYY(rotyy)
	self._Gimbal.setRotZZ(rotzz)
	self._Gimbal.setDistance(dist)
	return self._Gimbal.matrixFromCanonical()
	
    def renderAsXml(self):
	mat = self.getEquilibriumMatrix()
	coords = CoordinateSystem()
	xml = xmlGraphics("Builder")
	xml.addChild(coords.renderXml(2.0,1.0))
	coords.transformWithMatrix(mat)
	xml.addChild(coords.renderXml(2.0,1.0))
	return xml

    def originStep(self,vi):
	nvi = []
	for i in range(0,len(vi)):
	    nvi.append(0.0)
	return nvi


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

