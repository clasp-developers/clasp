
from mbb import *

import sys

if (len(sys.argv) < 2):
    fn = "out.xml"
else:
    fn = sys.argv[1]



def	testRandom():
    xml = xmlGraphics("all")
    can = CoordinateSystem()
    xml.addChild(can.renderXml(4.0,4.0))
    allGood = True
    for x in range(1,100):
	print "-------------"
	can = CoordinateSystem()
	can.defineRandom()
	print can.asXml().asString()
	e = XConventionEulerTransform()
	e.defineForCoordinateSystem(can)
	print e.asString()
	xml.addChild(can.renderXml(1.0,1.0))
	canew = e.getCoordinateSystem()
	print canew.asXml().asString()
	print canew.sameAs(can)
	allGood &= canew.sameAs(can)
    print "All same = ", allGood
    xml.writeToFileName("all.xml")

def	testRandomOrientation():
    for x in range(1,10):
	print "-------------"
	can = CoordinateSystem()
	can.defineRandom()
	can.setOrigin(Vector3(0.0,0.0,0.0))
	print can.asXml().asString()
	e = XConventionEulerTransform()
	e.defineForCoordinateSystem(can)
	canew = e.getCoordinateSystem()
	print canew.asXml().asString()
	print canew.sameAs(can)

def testSingle():
    xml = xmlGraphics("top")

    can = CoordinateSystem()
    xml.addChild(can.renderXml(4.0,4.0))

    e = XConventionEulerTransform()
    e.setAlpha(0*0.0174533)
    e.setBeta(20.0*0.0174533)
    e.setGamma(0.0*0.0174533)
    e.setDistance(5.0)
    e.setPhi(0*0.0174533)
    e.setTheta(90.0*0.0174533)
    print "e=\n%s"%e.asString()
    ce = e.getCoordinateSystem()
    print "e matrix=\n%s",ce.asXml().asString()
    xml.addChild(ce.renderXml(1.0,1.0))

    enew = XConventionEulerTransform()
    enew.defineForCoordinateSystem(ce)

    print "enew=\n%s"%enew.asString()
    print "enew matrix=\n%s",enew.getCoordinateSystem().asXml().asString()

    xml.writeToFileName(fn)



testSingle()


#testRandom()
