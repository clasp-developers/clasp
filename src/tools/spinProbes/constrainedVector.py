
import random


class ConstrainedVector:
		# variables is an ordered list of variables to optimize
		# they can either be a string or a tuple with the structure
		#  (varname, scale)
		#  (varname, scale, startValue )
		#  (varname, scale, startValue, rangeMin, rangeMax )
    def __init__(self,variables):
	self._Range = {}
	self._Position = {}
	self._Optimize = {}
	self._Index = {}
	self._Scale = {}
	self._Order = []
	self._ScaleLocally = True
	for v in variables:
	    if ( isinstance(v,tuple) ):
		var = v[0] 
		scale = float(v[1])
		val = None
		rangeMin = None
		rangeMax = None
		optimize = True
		if ( len(v) > 2 ):
		    val = v[2]
		    if ( len(v) > 3 ):
			range = v[3]
			if ( isinstance(range,tuple) ):
			    rangeMin = range[0]
			    rangeMax = range[1]
			if ( len(v) > 4 ):
			    optimize = v[4]
	    else:
		scale = 1.0
		var = v
		val = None
		rangeMin = None
		rangeMax = None
		optimize = True
	    self._Order.append(var)
	    self._Range[var] = [rangeMin,rangeMax]
	    self._Scale[var] = scale
	    self._Position[var] = val
	    self._Optimize[var] = optimize

    def setScaleLocally(self,b):
	self._ScaleLocally = b


    def getOptionsForGetOpt(self):
	ops = []
	for v in self.getVariables():
	    ops.append("min_%s="%v)   # min constraint
	    ops.append("max_%s="%v)   # max constraint
	    ops.append("optimize_%s="%v)   # turn on/off optimize
	    ops.append("start_%s="%v)   # starting value
	return ops

    def getOptions(self):
	ops = set()
	for v in self.getVariables():
	    ops.add("--min_%s"%v)   # min
	    ops.add("--max_%s"%v)   # max
	    ops.add("--optimize_%s"%v)   # optimize
	    ops.add("--start_%s"%v)   # starting value
	return ops

    def setOption(self,opt,arg):
	parts = opt.split("_")
	parmType = parts[0]
	variable = parts[1]
	if ( parmType == "--min" ):
	    rangeMin,rangeMax = self._Range[variable]
	    rangeMin = float(arg)
	    self._Range[variable] = (rangeMin,rangeMax)
	if ( parmType == "--max" ):
	    rangeMin,rangeMax = self._Range[variable]
	    rangeMax = float(arg)
	    self._Range[variable] = (rangeMin,rangeMax)
	if ( parmType == "--optimize" ):
	    if ( arg=="false" or arg=="False" or arg=="0" or arg=="off" or arg=="Off" ):
		self._Optimize[variable] = False
	    else:
		self._Optimize[variable] = True
	if ( parmType == "--start" ):
	    self._Position[variable] = float(arg)
  
    def getVariables(self):
	return self._Range.keys()

#    def getPosition(self):
#	return self._Position
#
    def getPositionSummary(self,point):
	desc = "//summary point: %d\n"%point
	for key in self._Order:
	    pos = self.getRawPositionValue(key)
	    range = self._Range[key]
	    if ( range[0] == None ):
		rangeMin = "%6s"%"-Inf"
	    else:
		rangeMin = "%6.2f"%range[0]
	    if ( range[1] == None ):
		rangeMax = "%6s"%"+Inf"
	    else:
		rangeMax = "%6.2f"%range[1]
	    desc = desc + "//var %10s = %7.2f scale: %5.2f range: %s %s optimize: %s\n"%(key,pos,self._Scale[key],rangeMin,rangeMax,str(self._Optimize[key]))
	desc = desc+"//done"
	return desc	

    def parsePositionSummaryFromStream(self,summaryStream):
	variables = []
	foundVariables = False
	while (1):
	    l = summaryStream.readline()
	    if ( l == "" ): break
	    parts = l.split()
	    if ( parts[0] == "//done" ):
	        break;
	    if ( parts[0] == "//var" ):
	 	foundVariables = True
		var = parts[1]
		valStr = parts[3]
		scaleStr = parts[5]
		rangeMinStr = parts[7]
		rangeMaxStr = parts[8]
		optimizeStr = parts[10].lower
		val = float(valStr)
		scale = float(scaleStr)
		if ( rangeMinStr == "-Inf" ):
		    rangeMin = None
		else:
		    rangeMin = float(rangeMinStr)
		if ( rangeMaxStr == "+Inf" ):
		    rangeMax = None
		else:
		    rangeMax = float(rangeMaxStr)
		if ( optimizeStr == "false" or optimizeStr == "off" ):
		    optimize = False
		else:
		    optimize = True
		variables.append((var,scale,val,(rangeMin,rangeMax),optimize))
	if ( foundVariables ):
	    self.__init__(variables)
	else:
	    raise "Could not find variables"

    def initializeFromRestartFile(self,fileName):
	fin = open(fileName,"r")
	self.parsePositionSummaryFromStream(fin)
	fin.close()


    def initializePositionAtZero(self):
	self._Index = {}
	idx = 0
	for v in self._Order:
	    if ( self._Position[v] == None ):
		self._Position[v] = 0.0
	    if ( self._Optimize[v] ):
		self._Index[v] = idx
		idx += 1

    def initializePositionAtRandom(self):
	self._Index = {}
	idx = 0
	for v in self._Order:
	    if ( self._Optimize[v] ):
		self._Index[v] = idx
		idx += 1
		rangeMin = -100.0
		if ( self._Range[v][0] != None ):
		    rangeMin = self._Range[v][0]
		rangeMax = 100.0
		if ( self._Range[v][1] != None ):
		    rangeMax = self._Range[v][1]
		self._Position[v] = random.random()*(rangeMax-rangeMin)+rangeMin
	
    def getRawPositionValue(self,var):
	return self._Position[var]


    def moveInRange(self,val,range):
	if ( range[0] != None ):
	    if ( val < range[0] ):
		val = range[0]
	if ( range[1] != None ):
	    if ( val > range[1] ):
		val = range[1]
	return val


#
# All position values that come in or out of this object through valueVectors are scaled
#
    def getValueVector(self):
	valueVector = [0.0]*len(self._Index.keys())
	for v,i in self._Index.items():
	    valueVector[i] = self._Position[v]/self._Scale[v]
	return valueVector

    def setFromValueVector(self,valueVector):
	for v,i in self._Index.items():
	    val = valueVector[i]*self._Scale[v]
#	    val = self.moveInRange(val,self._Range[v])
	    self._Position[v] = val


    def getBoundsVector(self):
	rangeVector = [(None,None)]*len(self._Index.keys())
	for v,i in self._Index.items():
	    range = self._Range[v]
	    minRange = range[0]
	    maxRange = range[1]
	    if ( minRange != None ):
		minRange /= self._Scale[v]
	    if ( maxRange != None ):
		maxRange /= self._Scale[v]
	    rangeVector[i] = (minRange,maxRange)
	return rangeVector

    def getScaleVector(self):
	scaleVector = [0.0]*len(self._Index.keys())
	for v,i in self._Index.items():
	    scaleVector[i] = self._Scale[v]
	return scaleVector


	#
	# Return 0.0 if all positions are in range
	# otherwise return a parabolic function for each variable out of range
	#
    def rangeError(self):
	err = 0.0
	for v,i in self._Index.items():
	    pos = self._Position[v]
	    range = self._Range[v]
	    rangeMin = range[0]
	    rangeMax = range[1]
	    if ( rangeMin != None ):
		if ( pos < rangeMin ):
		    dev = pos-rangeMin
		    err += dev*dev/self._Scale[v]
	    if ( rangeMax != None ):
		if ( pos > rangeMax ):
		    dev = pos-rangeMax
		    err += dev*dev/self._Scale[v]
	return err

