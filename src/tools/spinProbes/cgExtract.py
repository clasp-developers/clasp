#! /bin/env python

import sys

from mbb import *
import getopt


from extOligomer import *


#debugSuppressMessages(True)

def renderAxes(sc):
    xml = xmlBlock()
    vO = Vector3(0,0,0)
    vX = Vector3(sc,0,0)
    vY = Vector3(0,sc,0)
    vZ = Vector3(0,0,sc)
    xml.addChild(xmlLine("FF0000",2,vO,vX))
    xml.addChild(xmlLine("00FF00",2,vO,vY))
    xml.addChild(xmlLine("0000FF",2,vO,vZ))
    return xml



# ###########################################################
#
# Load the database
#
#

optlist, args = getopt.getopt(sys.argv[1:],"f:hd:o:")

databaseName = "../../buildDatabase/small_database.xml"
outputPrefix = "_"
renderStructure = False

maxFrames = -1

for opt,arg in optlist:
    if ( opt == "-f" ):
	maxFrames = int(arg)
    if ( opt == "-d" ):
	databaseName = arg
    if ( opt == "-o" ):
	outputPrefix = arg
    if ( opt == "-h" ):
	print "Usage: %s [options] frameDir frameRoot"
	print "  -h                = Help"
	print "  -f [frames]       = Number of frames to process; default all"
	print "  -d [database]     = Database name; default (%s)"%databaseName
	print "  -o [outputPrefix] = Output prefix; default (%s)"%outputPrefix
	sys.exit(0)

if ( len(args) != 2 ):
    print "You must supply a frame directory and a frame root name"
    sys.exit(0)

frameDir = args[0]
frameRoot = args[1]

print "Monomer database: %s"%databaseName
print "Frame directory:  %s"%frameDir
print "Frame root:       %s"%frameRoot


builderXml = QuickDomNode()
builderXml.parseFileName(databaseName)
builderDb = BuilderDatabase()
builderDb.parseFromXml(builderXml)


#
# Lets put the fileDict 
files = os.listdir(frameDir)
fileDict = {}
for x in files:
    offset = x.rfind(frameRoot)
    indexOffset = offset+len(frameRoot)+1
    val = int(x[indexOffset:])
    fileDict[val] = x


directionXml = xmlGraphics("direction")
animationDict = {}
directionDict = {}
#histoDict = {}
parameterDict = {}
parameterXml = QuickDomNode("all")
entryDict = {}
structureAnimate = xmlAnimation()

# HistogramArray(CGSegment().getValueNames(),20)

iframe = 0
for x in fileDict.values():
    fileName = "%s/%s"%(frameDir,x)
    cgo = ExtOligomer(builderDb)
    cgo.buildFromAggregateInFile(fileName)
    VP0("Loaded aggregate from file(%s)"%fileName)
    xml = cgo.getOligomer().asXml()
    VP0("\n%s"%xml.asString())



#    frame = cgo.renderAsXml()
#    agg = cgo.getAggregate().copy()
#    agg.applyTransformToAtoms(cgo.getTransform())
#    aggXml = xmlAggregateCreate(agg)
#    frame.addChild(aggXml)
#    animation.addChild(frame)
		#
		# Now initialize output accumulators 
		#
    if (1==1):
	if ( iframe == 0 ):	# initialize accumulators
	    for seg in cgo.getSegments():
		env = seg.getEnvironment()
		print "Initializing segment statistics for: %s"%env
#		histoDict[env] = HistogramArray(CGSegment().getValueNames(),19)
		dirXml = xmlBlock()
		dirXml.addChild(renderAxes(4))
		segAggregate = seg.getTransformedAggregate()
#		dirXml.addChild(xmlAggregateCreate(segAggregate))
		dirXml.addChild(segAggregate.asXmlWithCoordinates())
		directionDict[env] = dirXml
		directionXml.addChild(dirXml)
		animationDict[env] = xmlAnimation()
		parameterDict[env] = QuickDomNode("segment")
		parameterDict[env].addAttributeString("name",env)
		parameterXml.addChild(parameterDict[env])
		entryDict[env] = []

		#
		# Now get save the animation in the 
		# frame of the first segment
		#	
    seg = cgo.getSegments()[0]
#    structureAnimate.addChild(xmlAggregateCreate(seg.getTransformedAggregate()))
    structureAnimate.addChild(seg.getTransformedAggregate().asXmlWithCoordinates())


    if (1):
	for seg in cgo.getSegments():
	    env = seg.getEnvironment()
#	    histos = histoDict[env]
#	    histos.addValues(seg.getValues())
	    dirXml = directionDict[env]
	    dirXml.addChild(seg.renderDirectionAsXml(5))
	    animation = animationDict[env]
	    block = xmlBlock()
	    animation.addChild(block)
	    block.addChild(seg.renderAsXml())
#	    block.addChild(xmlAggregateCreate(seg.getTransformedAggregate()))
	    block.addChild(seg.getTransformedAggregate().asXmlWithCoordinates())
	    parameterDict[env].addChild(seg.parametersAsXml())
	    entryDict[env].append(seg.getGimbal())
	if ( iframe % 20 == 0 ):
	    print "\nFrame %6d "%(iframe+1),
	else:
	    print ".",
    sys.stdout.flush()
    iframe = iframe + 1
    if ( maxFrames > 0 ):
	if ( iframe >= maxFrames ):
	    break


#
# Write out the entire animation
#
xml = xmlGraphics("animate")
xml.addChild(structureAnimate)
xml.writeToFileName("%s_animation.xml"%outputPrefix)

#
# Write out the parameter frames
#
parameterXml.writeToFileName("%s_parameters.xml"%outputPrefix)


#
# Write out the entries as straight numbers
#

for k,entries in entryDict.items():
    fn = "%s_entries_%s.txt"%(outputPrefix,k)
    fout = open(fn,"w")
    fout.write("%s\n"%entries[0].asLine(False))
    for e in entries:
	fout.write("%s\n"%e.asLine(True))
    fout.close()



#
# Write out the histograms
#
#for k,d in histoDict.items():
#    d.dumpAll("%s_histo_%s_"%(outputPrefix,k),"txt")

#
# write out individual segment animations
#
for k,a in animationDict.items():
    xml = xmlGraphics(k)
    xml.addChild(a)
    xml.writeToFileName("%s_segmentAnimation_%s.xml"%(outputPrefix,k))

for k,d in directionDict.items():
    d.writeToFileName("%s_direction_%s.xml"%(outputPrefix,k))
    
print
print "Extraction complete."
