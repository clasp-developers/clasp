
from cgOligomer import *
from histogram import *
from ploticus import *
from monteCarlo import *


class EprOligomer(CGBOligomer):
    def __init__(self,aName):
	CGBOligomer.__init__(self,aName)
	self._Histogram = None
	self._NMer = None
	self._AnimationXml = None

    def turnOnAnimation(self):
	self._AnimationXml = xmlAnimation()

    def saveAnimation(self,fnprefix):
	if ( self._AnimationXml != None ):
            xml = xmlGraphics("test")
            xml.addChild(self._AnimationXml)
            xml.writeToFileName("%s_%danim.xml"%(fnprefix,self._NMer))
	    

    def createNMer(self,n,trailPoacBuilder,middleBuilder,leadBuilder,leadPoacBuilder,outputPrefix):
	self.__init__("%dmer"%n)
	self._NMer = n
#	self._SpectrumPlot = Ploticus("%s_spec%d.pl"%(outputPrefix,self._NMer),
#				"0 2100", "-0.2 1.0",
#				"time(ns)","signal")
#	self._DistPlot = Ploticus("%s_dist%d.pl"%(outputPrefix,self._NMer),
#				"0 2100", "-0.2 1.0",
#				"r (nm)","p(r)")
	monTail = None
	monPrev = None
	monLead = None
	for i in range(0,n-1):
	    mon = CGBMonomer("SS%d"%(i+1))
	    self.addMonomer(mon)
	    if ( monTail == None ):
		monTail = mon
	    else:
		coup = CGBCoupling(middleBuilder)
		coup.setBetween(monPrev,mon)
		self.addCoupling(coup)
	    monLead = mon
	    monPrev = mon
	mon = CGBMonomer("Poac_Trail")
	self.addMonomer(mon)
	coup = CGBCoupling(trailPoacBuilder)
	coup.setBetween(monTail,mon)
	self.addCoupling(coup)
	monTail = mon

	mon = CGBMonomer("SSl")
	self.addMonomer(mon)
	coup = CGBCoupling(leadBuilder)
	coup.setBetween(monLead,mon)
	self.addCoupling(coup)
	monLead = mon
	
	mon = CGBMonomer("Poac_Lead")
	self.addMonomer(mon)
	coup = CGBCoupling(leadPoacBuilder)
	coup.setBetween(monLead,mon)
	self.addCoupling(coup)
	monLead = mon
	self._Tail = monTail
	self._Lead = monLead
	self.buildTransformCascade()



    def getEndToEndDistance(self):
	self.build()
	m1 = self._TransformCascade.getResultTransform( \
			self._Lead.getTransformCascadeIndex())
	m2 = self._TransformCascade.getResultTransform( \
			self._Tail.getTransformCascadeIndex())
	v1 = m1.getTranslation()
	v2 = m2.getTranslation()
	vDiff = v2.sub(v1)
	return vDiff.length()
	



    def monteCarloCallback(self,olig,step):
	if ( step < 0 ): 
		return
	dist = self.getEndToEndDistance()
	self._Histogram.addValueToHistogram(dist)
	if ( step%50000 == 0 ):
	    sys.stdout.write("\n%d-mer step: %6d\n"%(self._NMer,step)) 
	    sys.stdout.flush()
#	if ( step%100 == 0 ):
#	    if ( self._AnimationXml != None ):
#		self._AnimationXml.addChild(self.renderAsXml(self._FractionDone))


    def calculatePopulationDistribution(self, preMcSteps, mcSteps, \
					constrainedVector, optimizeStep ):
	place = constrainedVector.getPositionSummary(optimizeStep)
#	if ( not accumulatePlots ):
#	    if ( self._GeneratedSpectrum ):
#	        self._DistPlot.popData()
#	        self._SpectrumPlot.popData()
	self._GeneratedSpectrum = True
	self._Histogram = NumericalFunction()
	self._Histogram.setXInc(0.5)
#	if ( maxpoints != None ):
#	    self._FractionDone = float(point)/maxpoints
#	else:
#	    self._FractionDone = 0.0
	mc = MonteCarlo()
        mc.setOver(self)
	mc.setCallbackEvery1Step(self.monteCarloCallback)
	mc.run( preMcSteps, mcSteps )
	rejected = mc.getRejectedSteps()
	accepted = mc.getAcceptedSteps()
#	print "\nRejected steps = %d"%rejected
#	print "Accepted steps = %d"%accepted
	hnf = self._Histogram
	return hnf
	
#    def generatePredictedSpectrum(self,mcsteps,constrainedVector,point,
#					maxpoints=None,accumulatePlots=True):
#	place = constrainedVector.getPositionSummary(point)
#	if ( not accumulatePlots ):
#	    if ( self._GeneratedSpectrum ):
#	        self._DistPlot.popData()
#	        self._SpectrumPlot.popData()
#	self._GeneratedSpectrum = True
#	self._Histogram = Histogram(30)
#	if ( maxpoints != None ):
#	    self._FractionDone = float(point)/maxpoints
#	else:
#	    self._FractionDone = 0.0
#	mc = MonteCarlo()
#       mc.setOver(self)
#	mc.setCallbackEvery1Step(self.monteCarloCallback)
#	mc.run(mcsteps)
#	rejected = mc.getRejectedSteps()
#	accepted = mc.getAcceptedSteps()
#	print "\nRejected steps = %d"%rejected
#	print "Accepted steps = %d"%accepted
#	hnf = self._Histogram.getNumericalFunction()

#
#	self._DistPlot.pushData(hnf,"p(r) %d-mer point: %d"%(self._NMer,point),
#					place)
#	nf = self._ExperimentalSpectrum
#	pnt0 = nf.getXYPair(0)
#	pnt1 = nf.getXYPair(1)
#	tp = int(pnt0[0])
#	stepSize = int(pnt1[0]-pnt0[0])
#	number = nf.getNumberOfValues()
#	cnf = deer(tp, stepSize, number, self._Histogram.getNumericalFunction())
#	cnf.scaleXValues(1000.0)
##	yscale = pow(10.0,constrainedVector.getPositionValue("logyscale") )
##	yoffset = constrainedVector.getPositionValue("yoffset")
#	yscale = 1.0
#	yoffset = 0.0
#	cnf.scaleYValues(yscale/cnf.getXYPair(0)[1])
#	cnf.offsetYValues(yoffset)
#	self._CalculatedEprSpectrum = cnf
#	self._SpectrumPlot.pushData(cnf,
#		"calculated %d-mer point: %d"%(self._NMer,point),place)
#
#    def getRmsDifference(self):
#	rms = self._ExperimentalSpectrum.rmsDifference(self._CalculatedEprSpectrum)
#	return rms

#    def finishPlot(self):
#	self._SpectrumPlot.writeFinish()
#	self._DistPlot.writeFinish(loc="2 max")
