
import sys

from mbb import *

db = BuilderDatabase()

xml = QuickDomNode()
xml.parseFileName("seedDb.xml")
db.parseFromXml(xml)


def getRepresentedGroups(db,groupName):
    group = db.getGroup(groupName)
    if ( group == None ):
	raise "Could not find group with name: ",groupName
    if ( group.getRepresentative() != "" ):
	return [group]
    allGroups = []
    for g in group.groupNamesIterator:
	allGroups.extend(getRepresentedGroups(db,g))
    return allGroups


class TopologyBranch:
    def __init__(self,aPlug):
	self._Plug = aPlug
	self.reset()

    def getPlug(self):
	return self._Plug

    def reset(self):
	self._ReceptacleIndex = 0
	self._Receptacles = list(self._Plug.receptacleIterator)
	if ( len(self._Receptacles)==0 ):
	    raise "There must be at least one receptacle"
	self.initGroups()
	self._Overflow = False

    def initGroups(self):
	receptacle = self._Receptacles[self._ReceptacleIndex]
	groups = list(receptacle.groupNamesIterator)
	db = self._Plug.getBuilderDatabase()
	allReps = []
	for g in groups:
	    reps = getRepresentedGroups(db,g)
	    allReps.extend(reps)
	self._GroupIndex = 0
	self._Groups = allReps


    def increment(self):
	if ( self._Overflow ):
	    return None
	self._GroupIndex += 1
	if ( self._GroupIndex >= len(self._Groups) ):
	    self._ReceptacleIndex += 1
	    if ( self._ReceptacleIndex >= len(self._Receptacles) ):
		self._Overflow = True
	    self._GroupIndex = 0

    def getBranch(self):
	if ( self._GroupIndex >= len(self._Groups) ):
	    raise "Ran out of representations"
	return self._Groups[self._GroupIndex]

    def overflowed(self):
	return self._Overflow



class OligomerIterator:
    def __init__(self,db,aMold,aStereoisomer,aTopology):
	self._Database = db
	self._Mold = aMold
	self._Stereoisomer = aStereoisomer
	self._Topology = aTopology
	self._Branches = []
	if ( aTopology.getIn() != "" ):
	    plug = aMold.getInPlugNamed(aTopology.getIn())
	    self._Branches.append(TopologyBranch(plug))
	for out in aTopology.outIterator:
	    plug = aMold.getOutPlugNamed(out)
	    self._Branches.append(TopologyBranch(plug))
	self._Overflow = False

    def completeBranchWithOutCoupling(self,oligomer, neighborMon, outCoupling ):
	print "Looking for moldForNameOrPdb: ", neighborMon.getName()
	mold = self._Database.moldForNameOrPdb(neighborMon.getName())
	topology = mold.simplestTopologyWithOutPlugNamed(outCoupling.getName())
	if ( topology == None ):
	    raise "Could not find topology in mold(%s) with OutPlugNamed(%s)"%(mold.getName(),outCoupling.getName())
		# make the in connection and the remaining out connections	
	for op in topology.outIterator:
			# if the outIterator is the same as outCoupling
			# then don't connect it
	    if ( op == outCoupling.getName() ):
		continue
	    plug = mold.getOutPlugNamed(op)
			# find the first receptacle of the plug
	    capMonomerName = list(plug.receptacleIterator)[0].getCap()
	    capCoupling = Coupling(self._Database)
	    capMonomer = Monomer(self._Database)
	    capMonomer.setName(capMonomerName)
	    capMonomer.setInCoupling(capCoupling)
	    neighborMon.addOutCoupling(capCoupling)
	    capCoupling.setIn(neighborMon)
	    capCoupling.setOut(capMonomer)
	    capCoupling.setName(plug.getName())
	    oligomer.addMonomer(capMonomer)
	    oligomer.addCoupling(capCoupling)
	if ( topology.getIn() != "" ):
			#
			# fix this to deal with in 
	    plug = mold.getInPlugNamed(topology.getIn())
			# find the first receptacle of the plug
	    capMonomerName = list(plug.receptacleIterator)[0].getCap()
	    capCoupling = Coupling(self._Database)
	    capMonomer = Monomer(self._Database)
	    capMonomer.setName(capMonomerName)
	    capMonomer.addOutCoupling(capCoupling)
	    capCoupling.setIn(capMonomer)
	    capCoupling.setOut(neighborMon)
	    capCoupling.setName(plug.getName())
	    neighborMon.setInCoupling(capCoupling)
	    oligomer.addMonomer(capMonomer)
	    oligomer.addCoupling(capCoupling)



    def completeBranchWithInCoupling(self,oligomer, neighborMon, inCoupling ):
	print "Looking for moldForNameOrPdb: ", neighborMon.getName()
	mold = self._Database.moldForNameOrPdb(neighborMon.getName())
	if ( mold == None ):
	    raise "Could not find mold for monomer named(%s)"%neighborMon.getName()
	topology = mold.simplestTopologyWithInPlugNamed(inCoupling.getName())
	if ( topology == None ):
	    raise "Could not find topology in mold(%s) with InPlugNamed(%s)"%(mold.getName(),inCoupling.getName())
		# make the remaining out connections
	for op in topology.outIterator:
	    plug = mold.getOutPlugNamed(op)
			# find the first receptacle of the plug
	    capMonomerName = list(plug.receptacleIterator)[0].getCap()
	    capCoupling = Coupling(self._Database)
	    capMonomer = Monomer(self._Database)
	    capMonomer.setName(capMonomerName)
	    capMonomer.setInCoupling(capCoupling)
	    neighborMon.addOutCoupling(capCoupling)
	    capCoupling.setIn(neighborMon)
	    capCoupling.setOut(capMonomer)
	    capCoupling.setName(plug.getName())
	    oligomer.addMonomer(capMonomer)
	    oligomer.addCoupling(capCoupling)



    def buildOligomer(self):
	olig = Oligomer(self._Database)
	centralMon = Monomer(self._Database)
	centralMon.setName(self._Stereoisomer.getName())
	olig.addMonomer(centralMon)
	print "central monomer: ", self._Stereoisomer.getName()
	for b in self._Branches:
	    plug = b.getPlug()
	    coupling = Coupling(self._Database)
	    coupling.setName(plug.getName())
	    neighborMon = Monomer(self._Database)
	    neighborMon.setName(b.getBranch().getRepresentative())
	    if ( plug.getIsIn() ):
		dir = "IN "
		coupling.setIn(neighborMon)
		coupling.setOut(centralMon)
		centralMon.setInCoupling(coupling)
		neighborMon.addOutCoupling(coupling)
		self.completeBranchWithOutCoupling(olig,neighborMon,coupling)	
	    else:
		dir = "OUT"
		coupling.setIn(centralMon)
		coupling.setOut(neighborMon)
		centralMon.addOutCoupling(coupling)
		neighborMon.setInCoupling(coupling)
		self.completeBranchWithInCoupling(olig,neighborMon,coupling)	
	    olig.addMonomer(neighborMon)
	    olig.addCoupling(coupling)
	    print "monomer(%s)>>coupling(%s)>>monomer(%s)"%(coupling.getIn().getName(),coupling.getName(),coupling.getOut().getName())
	return olig

    def increment(self):
	if ( self._Overflow ):
	    return
	idx = 0
	for br in self._Branches:
	    br.increment()
	    if ( not br.overflowed() ):
		return
#	    print "         ====Overflowed: ", idx
	    idx += 1
	    br.reset()
	self._Overflow = True

    def overflowed(self):
	return self._Overflow


    






for m in db.moldIterator:
    print "Mold: ", m.getName()
    for s in m.stereoisomerIterator:
	print "-Stereoisomer: ",s.getName(),s.getPdb()
    for t in m.topologyIterator:
        print "-Topology: ", t
	outs = list(t.outIterator)
	print "     in/out = %s/%s"%(t.getIn(),str(outs))
    for p in m.plugIterator:
	print "-Plug: ", p.getName()
	print "as xml=", p.asXml().asString()


structures = 0
for m in db.moldIterator:
    for s in m.stereoisomerIterator:
	print "monomer name: ",s.getName()
	for t in m.topologyIterator:
	    print "------- OLIGOMER_ITERATOR"
	    oi = OligomerIterator(db,m,s,t)
	    while ( not oi.overflowed() ):
		structures += 1
		print "---------------Oligomer #", structures
		sys.stdout.flush()
		olig = oi.buildOligomer()
		print olig.asXml().asString()
		oi.increment()

print "There will be #%d oligomers."%structures
