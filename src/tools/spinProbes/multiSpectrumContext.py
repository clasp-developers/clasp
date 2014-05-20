

from mbb import *
#from cgBuilders import *
from report import *

from gimbalModel import *



PopulationDistributionDict_XML_Name = "PopulationDistributionDict"
class	PopulationDistributionDict(dict):
    def __init__( self, aListOfOligomerNames ):
	for oligomerName in aListOfOligomerNames:
	    self[oligomerName] = NumericalFunction()
	    self[oligomerName].setXAxisName("Distance (A)")

    def __add__( self, data ):
	newPopDists = PopulationDistributionDict(self.keys())
	for name,func in self.items():
	    func = self[name]
	    func2 = data[name]
	    func.setXInc(func2.getXInc())
	    result = func.add(func2)
	    newPopDists[name] = result
	return newPopDists

    def asXml(self):
	node = QuickDomNode(PopulationDistributionDict_XML_Name)
	for name,func in self.items():
	    child = func.asXml()
	    child.addAttributeString("name",name)
	    node.addChild(child)
	return node

    def parseFromXml(self,node):
	self.clear()
	for child in node.getChildren():
	    name = child.getAttribute("name")
	    func = NumericalFunction()
	    func.parseFromXml(child)
	    self[name] = func

    def asMessage(self):
	parts = []
	for name,func in self.items():
	    funcString = func.asXml().asString()
	    part = (name,funcString)
	    parts.append(part)
	return parts

    def parseFromMessage(self,message,cp):
	self.clear()
	for name,funcString in message:
	    func = NumericalFunction()
	    xml = QuickDomFromString(funcString)
	    func.parseFromXml(xml)
	    self[name] = func








MultiSpectrumContext_XML_Name = "MultiSpectrumContext"
class MultiSpectrumContext:
    MutliSpectrumContext.GaussianSampling = 1
    MutliSpectrumContext.MonteCarloSampling = 2

    def __init__( self ):
	self._MPIRank = 0
	self._MPISize = 1
	self._ParmeterizedBuilder = None
	self._ConstrainedVector = None 
	self._PreSamplingSteps = None 
	self._SamplingSteps = None
	self._OutputFilePrefix = None
	self._OptimizeStep = 0
	self._ExperimentalSpectra = {}
	self._CalculatedSpectra = {}
	self._SpectrumPlotters = None
	self._PopulationDistributionPlotters = None
	self._Initialized = False
	self._CurrentComment = ""
	self._Results = QuickDomNode("Results")
	self._Results.addAttributeInt("mpiSize",self._MPISize)
	self._DebugFile = None
	self._Oligomers = []
	self._SamplingMethod = None 
	self.initialize()

    def initialize(self):
	if ( self._MPIRank == 0 ):
	    self._Plot = NumericalFunction()
	    self._Plot.setXStart(0)
	    self._Plot.setXInc(1)
	    self._Plot.setXAxisName("Iteration")
	else:
	    self._Plot = None
	self._Initialized = True
	

    def setup( self, mpiRank, mpiSize, builder, constrainedVector, \
			samplingMethod, \
			preSamplingSteps, samplingSteps ):
	self._MPIRank = mpiRank
	self._MPISize = mpiSize
	self._ParameterizedBuilder = builder
	self._ConstrainedVector = constrainedVector
	self._SamplingMethod = samplingMethod
	self._PreSamplingSteps = preSamplingSteps
	self._SamplingSteps = samplingSteps


    def setOutputPrefix(self,of):
	self._OutputFilePrefix = of


    def createDebugFile(self,fn):
	self._DebugFile = open(fn,"w")

    def addToComment(self,cstr):
	self._CurrentComment += "\n%s"%cstr

    def getComment(self):
	return self._CurrentComment


    def getMPIRank(self):
	return self._MPIRank

    def getMPISize(self):
	return self._MPISize

    def getOptimizeStep(self):
	return self._OptimizeStep

    def getPreSamplingSteps(self):
	return self._PreSamplingSteps

    def getSamplingSteps(self):
	return self._SamplingSteps

    def setPlotIntermediateResults(self,turnOn):
	if ( turnOn and len(self._Oligomers) != 0 ):
	    raise "You can't turn on plotting of intermediate spectra after you added oligomers, you have to do it before"
	if ( not turnOn ):
	    self._SpectrumPlotters = None
	    self._PopulationDistributionPlotters = None
	else:
	    if ( self._MPIRank == 0 ):
	        self._SpectrumPlotters = {}
	        self._PopulationDistributionPlotters = {}

    def getOligomers(self):
	return self._Oligomers

    def addOligomer(self,o):
	self._Oligomers.append(o)

    def addExperimentalSpectrum(self,nmer,spec):
	self._ExperimentalSpectra[nmer] = spec
#	if ( self._MPIRank == 0 ):
#	    if ( self._SpectrumPlotters != None ):
#		pl = Ploticus("%s-spec-%s.pl"%(self._OutputFilePrefix,
#							o.getName()),
#			    (spec.getLowX(),spec.getHighX()),
#			    (spec.getLowY(),spec.getHighY()),
#			    "time(ns)","Intensity")
#		self._SpectrumPlotters[o.getName()] = pl
#		pl.pushData(spec,"Experimental", "#Experimental")
#		pl.protectData()
#	    if ( self._PopulationDistributionPlotters != None ):
#		pl = Ploticus("%s-dist-%s.pl"%(self._OutputFilePrefix,
#							o.getName()),
#			    None, None,
#			    "distance(A)","P(distance)")
#		self._PopulationDistributionPlotters[o.getName()] = pl


    def getConstrainedVector(self):
	return self._ConstrainedVector

    def getOligomerNames(self):
	names = []
	for x in self._Oligomers:
	    names.append(x.getName())
	return names

    def getParameterizedBuilder(self):
	return self._ParameterizedBuilder


    def calculatePopulationDistributions(self,valueVector):
	if ( not self._Initialized ):
	    self.initialize()
	self._OptimizeStep += 1
	self._ConstrainedVector.setFromValueVector(valueVector)
	self._ParameterizedBuilder.setFromConstrainedVector(self._ConstrainedVector)
	if ( len(self._Oligomers)==0 ):
	    raise("There are no SingleSpectrumOligomers defined")
	place = self._ConstrainedVector.getPositionSummary(self._OptimizeStep)
	self._CurrentComment = place
	preSamplingSteps = self._PreSamplingSteps
	samplingSteps = self._SamplingSteps
	compSum = 0.0
	populationDistributions = \
		PopulationDistributionDict(self.getOligomerNames())
	for oligomer in self._Oligomers:
#	    res = oligomer.calculatePopulationDistribution( \
#					preSamplingSteps, samplingSteps, \
#					self._ConstrainedVector, \
#					self._OptimizeStep )
	    place = self._ConstrainedVector.getPositionSummary(self._OptimizeStep)


	#
	# This is where the distribution is generated
	#

	    if ( self._SamplingMethod==MultiSpectrumContext.GaussianSampling ):
		sampler = GaussianSampler()
	    elif ( self._SamplingMethod == MultiSpectrumContext.MonteCarloSampling ):
		sampler = MonteCarloSampler()
	    else:
		raise "You must define a sampling method"
	    sampler.setPreSamplingSteps(preSamplingSteps)
	    sampler.setSamplingSteps(int(samplingSteps))
	    sampler.setBuilder(oligomer.getRootBuilder())
	    sampler.run()
	    res = oligomer.getMeter().getHistogram()   

	    populationDistributions[oligomer.getName()] = res
#	    if ( self._PopulationDistributionPlotters != None ):
#		pl = self._PopulationDistributionPlotters[oligomer.getName()]
#		pl.popData()
#		pl.pushData( res, \
#			"Distribution opt_step(%d)"%self._OptimizeStep, \
#			place )
	return populationDistributions

    def setIndividualPopulationDistributions(self,ind):
	self._IndividualDistributions = ind

    def setSummedPopulationDistributions(self,summedPopDists):
	self._SummedDistributions = summedPopDists

    def compareSpectra(self):
	rmsSum = 0.0
	for oligomerName,popDist in self._SummedDistributions.items():
	    experimentalSpectrum = self._ExperimentalSpectra[oligomerName]
	    tp = experimentalSpectrum.getXStart()
	    stepSize = experimentalSpectrum.getXInc()
	    number = experimentalSpectrum.getNumberOfValues()
	    calcSpectrum =  deer(tp, stepSize, number, popDist )
	    calcSpectrum.scaleYValues( 1.0/calcSpectrum.getYValueAtIndex(0))
	    self._CalculatedSpectra[oligomerName] = calcSpectrum
	    self.VP0( "calculated spectrum xInc = %f"%calcSpectrum.getXInc() )
	    self.VP0( "experimental spectrum xInc = %f"%experimentalSpectrum.getXInc() )
	    rms = experimentalSpectrum.rmsDifference(calcSpectrum)
	    rmsSum += rms
	if ( self._MPIRank == 0 ):
	    self._Plot.appendValue(rmsSum)
	self._SpectralError = rmsSum
	return rmsSum

    def generateReport(self):
	optReport = OptimizeReport()
	optReport.setHeader("Optimizer report: cycle(%d)"%self._OptimizeStep)
	optReport.setComment("%s"%(self._CurrentComment))
	optReport.setFirstPageGraph("Spectral_error function",self._Plot)
	oligomers = self._ExperimentalSpectra.keys()
	oligomers.sort()
	for oligomer in oligomers:
	    page = ReportPage()
	    page.setTitle(oligomer)
	    graph = ReportGraph()
	    graph.setTitle("Population distributions")
	    graph.addData(self._SummedDistributions[oligomer])
	    rank = 0
	    for z in self._IndividualDistributions:
		graph.addData(z[oligomer])
		rank += 1
	    page.addGraph(graph)
	    graph = ReportGraph()
	    graph.setTitle("EPR Spectrum")
	    graph.addData(self._ExperimentalSpectra[oligomer])
	    graph.addData(self._CalculatedSpectra[oligomer])
	    page.addGraph(graph)
	    optReport.addPageData(page)
	page = ReportPage()
	graph = ReportGraph()
	graph.setTitle("All distance distributions")
	for oligomer in oligomers:
	    graph.addData(self._SummedDistributions[oligomer])
	page.addGraph(graph)
	optReport.addPageData(page)
	optReport.generateReport("%s-step_%05d.pdf"%(self._OutputFilePrefix,self._OptimizeStep-1))


    def VP0(self,message):
	if ( self._DebugFile != None ):
	    self._DebugFile.write("DEBUG_%d: %s\n"%(self._MPIRank,message))
	    sys._DebugFile.flush()


    def getConstraintError(self):
	self._ConstraintError = self.getConstrainedVector().rangeError()
	return self._ConstraintError

    def getSpectralError(self):
	return self._SpectralError


    def asXml(self):
	node = QuickDomNode(MultiSpectrumContext_XML_Name)
	node.addAttributeDouble("spectralError",self._SpectralError,5,5)
	node.addAttributeDouble("constraintError",self._ConstraintError,5,5)
	node.addAttributeInt("step",self._OptimizeStep)
	comNode = QuickDomNode("Comment")
	comNode.setCharacters(self._CurrentComment)
	node.addChild(comNode)
	child = self._SummedDistributions.asXml()
	child.setLocalName("SummedDistributions")
	node.addChild(child)
	child = QuickDomNode("PerProcessPopulationDistributions")
	idx = 0
	for x in self._IndividualDistributions:
	    ix = x.asXml()
	    ix.addAttributeInt("mpiRank", idx)
	    idx += 1
	    child.addChild(ix)
	node.addChild(child)
	return node


    def parseFromXml(self,node):
	print "MultiSpectrumContext::parseFromXml"
	xmlName = CGParameterizedPro4Builder_XML_Name
	if ( node.hasChildrenWithName(xmlName) ):
	    self._ParmeterizedBuilder = ParameterizedGimbal()
	    self._ParmeterizedBuilder.parseFromXml(node.childWithName(xmlName))
	if ( node.hasChildrenWithName(PopulationDistributionDict_XML_Name) ):
	    child = node.childWithName(PopulationDistributionDict_XML_Name)
	    self._SummedDistributions = PopulationDistributionDict([])
	    self._SummedDistributions.parseFromXml(child)
	if ( node.hasChildrenWithName("Comment") ):
	    child = node.childWithName("Comment")
	    self._Comment = child.getCharacters()
	self._SpectralError = float(node.getAttribute("spectralError"))
	self._ConstraintError = float(node.getAttribute("constraintError"))
	self._OptimizeStep = int(node.getAttribute("step"))


    def saveContext(self):
	node = self.asXml()
	self._Results.addChild(node)


    def writeAllContextsToXmlFile(self,fileName):
	self._Results.writeToFileName(fileName)


	# Scan through the XML file and look for the context with the desired step
	# Return True if the step is found
	# Return False if the step is not found
    def readContextFromXmlFileAtStep(self,fileName,step):
	node = QuickDomNode()
	node.parseFileName(fileName)
	children = node.getChildren()
	for child in children:
	    if ( child.getLocalName() == MultiSpectrumContext_XML_Name ):
		childStep = int(child.getAttribute("step"))
	        if ( childStep == step ):
		    self.parseFromXml(child)
		    return True
	return False
