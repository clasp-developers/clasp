
import math
import random


class MonteCarlo:
    def __init__(self):
	self._Over = None
	self._Beta = 0.002*300
	self._Steps = None
	self._CallbackEvery1Step = None

    def setSteps(self,st):
	self._Steps = st

    def setOver(self,ov):
	self._Over = ov

    def setCallbackEvery1Step(self,rep):
	self._CallbackEvery1Step = rep


    def run(self, preSteps, maxSteps):
#def monteCarlo(maxSteps,olig,ends,histo,animationXml,directionXml,watchCoup):
	self._RejectedSteps = 0
	self._AcceptedSteps = 0
	beta = self._Beta
	prevEnergy = None
	cnt = -preSteps
	while ( cnt < maxSteps ):
	    self._Over.randomStep()
	    energy = self._Over.getEnergy()
	    if ( prevEnergy == None ):
		prevEnergy = energy
	    deltaE = energy - prevEnergy
	    if ( deltaE > 0.0 ):
		boltz = math.exp(-deltaE*beta)
		rnd = random.random()
		if ( rnd>boltz ):
		    self._Over.forgetStep()
		    if ( cnt > 0 ):
		        self._RejectedSteps = self._RejectedSteps + 1
		    continue	
	    prevEnergy = energy
	    if ( cnt > 0 ):
	        self._AcceptedSteps = self._AcceptedSteps + 1
	        if ( self._CallbackEvery1Step != None ):
		    self._CallbackEvery1Step(self._Over,cnt)
	    cnt = cnt + 1


    def getRejectedSteps(self):
	return self._RejectedSteps

    def getAcceptedSteps(self):
	return self._AcceptedSteps

