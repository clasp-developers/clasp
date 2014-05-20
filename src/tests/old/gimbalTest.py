
from mbb import *

import sys


def testSingle():
    xml = xmlGraphics("top")
    can = CoordinateSystem()
    xml.addChild(can.renderXml(4.0,4.0))
    e = GimbalTransform()
    e.setRotXDegrees(10.0)
    e.setRotYDegrees(20.0)
    e.setRotZDegrees(30.0)
    e.setRotYYDegrees(45.0)
    e.setRotZZDegrees(55.0)
    e.setDistance(1.0)
    print "e=\n%s"%e.asString("// ")
    ce = e.getCoordinateSystem()
    print "e matrix=\n%s"%ce.matrixFromCanonical().asXml().asString()
    xml.addChild(ce.renderXml(1.0,1.0))

    enew = GimbalTransform()
    enew.defineForCoordinateSystem(ce)

    print "enew=\n%s"%enew.asString("// ")
    print "enew matrix=\n%s"%enew.getCoordinateSystem().asXml().asString()


    return xml


def testRandom():
    xml = xmlGraphics("all")
    can = CoordinateSystem()
    xml.addChild(can.renderXml(4.0,4.0))
    allGood = True
    for x in range(1,100):
	print "-------------"
	can = CoordinateSystem()
	can.defineRandom()
	print can.asXml().asString()
	e = GimbalTransform()
	e.defineForCoordinateSystem(can)
	print e.asString()
	xml.addChild(can.renderXml(1.0,1.0))
	canew = e.getCoordinateSystem()
	print canew.asXml().asString()
	print canew.sameAs(can)
	allGood &= canew.sameAs(can)
    print "All same = ", allGood
    return xml


def testOneRandom():
    xml = xmlGraphics("all")
    can = CoordinateSystem()
    xml.addChild(can.renderXml(4.0,4.0))
    allGood = True
    for x in range(0,1):
	print "-------------"
	can = CoordinateSystem()
	can.defineRandom()
	print can.asXml().asString()
	e = GimbalTransform()
	e.defineForCoordinateSystem(can)
	print e.asString()
	xml.addChild(can.renderXml(1.0,1.0))
	canew = e.getCoordinateSystem()
	print canew.asXml().asString()
	print canew.sameAs(can)
	allGood &= canew.sameAs(can)
    print "All same = ", allGood
    return xml



def	testRandomOrientation():
    for x in range(1,10):
	print "-------------"
	can = CoordinateSystem()
	can.defineRandom()
	can.setOrigin(Vector3(0.0,0.0,0.0))
	print can.asXml().asString()
	e = GimbalTransform()
	e.defineForCoordinateSystem(can)
	canew = e.getCoordinateSystem()
	print canew.asXml().asString()
	print canew.sameAs(can)


def	testRandomOrientation():
    for x in range(1,10):
	print "-------------"
	can = CoordinateSystem()
	can.defineRandom()
	can.setOrigin(Vector3(0.0,0.0,0.0))
	print can.asXml().asString()
	e = GimbalTransform()
	e.defineForCoordinateSystem(can)
	canew = e.getCoordinateSystem()
	print canew.asXml().asString()
	print canew.sameAs(can)


if ( len(sys.argv)>1 ):
    fn = sys.argv[1]
else:
    fn = "out.xml"


xml = xmlGraphics("main")

xmlOne = testSingle()
#xmlOne = testOneRandom()
xml.addChild(xmlOne)


xml.writeToFileName(fn)
