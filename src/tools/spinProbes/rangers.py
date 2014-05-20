


class LinearRanger:
    def __init__(self,variables):
	self._Range = {}
	self._Position = {}
	self._Inc = {}
	self._Index = {}
	idx = 0
	for v in variables:
	    self._Range[v] = [None,None]
	    self._Position[v] = 0.0
	    self._Inc[v] = 0.0
	    self._Index[v] = idx
	    idx += 1

    def setNumberOfPoints(self,points):
	intervals = points-1
	for v in self.getVariables():
	    if ( intervals != 0 ):
	        self._Inc[v] = (self._Range[v][1]-self._Range[v][0])/intervals
	    else:
		self._Inc[v] = 0.0

    def getRangeOptionsForGetOpt(self):
	ops = []
	for v in self.getVariables():
	    ops.append("s%s="%v)   # start
	    ops.append("e%s="%v)   # end
	return ops

    def getRangeOptions(self):
	ops = set()
	for v in self.getVariables():
	    ops.add("--s%s"%v)   # start
	    ops.add("--e%s"%v)   # end
	return ops

    def setRange(self,opt,arg):
	send = opt[:3]
	var = opt[3:]
	val = float(arg)
	range = self._Range[var]
	print "Starting range: %s"%str(range)
	if ( send == '--s' ):
	    erange = range[1]
	    if ( erange == None ):
		erange = val
	    range = [val,erange]
	if ( send == '--e' ):
	    srange = range[0]
	    if ( srange == None ):
		srange = val
	    range = [srange,val]
	print "opt(%s) arg(%s)"%(opt,arg)
	print "Setting range option(%s) to range: %s"%(var,str(range))
	self._Range[var] = range
  
    def isRangeSet(self,var):
	range = self._Range[var]
	return range[0] != None 
  
    def getVariables(self):
	return self._Range.keys()

    def getPosition(self):
	return self._Position

    def getPositionDescription(self,point):
	keys = self.getVariables()
	keys.sort()
	desc = "# point: %d\n"%point
	for key in keys:
	    desc = desc + "# %7s = %5.2f range(%5.2f,%5.2f) inc(%5.2f)\n"%(key,self.getPositionVariable(key),self._Range[key][0],self._Range[key][1],self._Inc[key])
	return desc	

    def getPositionVariable(self,var):
	return self._Position[var]

    def initializePosition(self,points):
	for v in self.getVariables():
	    srange = self._Range[v][0]
	    erange = self._Range[v][1]
	    if ( srange == None ):
		srange = 0.0
	    if ( erange == None ):
		erange = 0.0
	    self._Range[v] = [srange,erange]
	    self._Position[v] += self._Range[v][0]
	self.setNumberOfPoints(points)
	
    def advancePosition(self):
	for v in self.getVariables():
	    self._Position[v] += self._Inc[v]

    def getValueVector(self):
	varvec = [0]*len(self._Index)
	for var,idx in self._Index.entries():
	    varvec[idx] = self._Position[var]
	return varvec

    def setValueVector(self,varvec):
	for var,idx in self._Index.entries():
	    self._Position[var] = varvec[idx]




class RandomRanger:
    def __init__(self,variables):
	self._Range = {}
	self._Position = {}
	self._Inc = {}
	self._Index = {}
	idx = 0
	for v in variables:
	    self._Range[v] = [None,None]
	    self._Position[v] = 0.0
	    self._Inc[v] = 0.0
	    self._Index[v] = idx
	    idx += 1

    def getRangeOptionsForGetOpt(self):
	ops = []
	for v in self.getVariables():
	    ops.append("%s="%v)
	return ops

    def getRangeOptions(self):
	ops = set()
	for v in self.getVariables():
	    ops.add("--%s"%v)   # start
	return ops

    def setRange(self,opt,arg):
	var = opt[2:]
	val = float(arg)
	range = self._Range[var]
	print "Starting range: %s"%str(range)
	    srange = range[0]
	    if ( srange == None ):
		srange = val
	    range = [srange,val]
	print "opt(%s) arg(%s)"%(opt,arg)
	print "Setting range option(%s) to range: %s"%(var,str(range))
	self._Range[var] = range
  
    def isRangeSet(self,var):
	range = self._Range[var]
	return range[0] != None 
  
    def getVariables(self):
	return self._Range.keys()

    def getPosition(self):
	return self._Position

    def getPositionDescription(self,point):
	keys = self.getVariables()
	keys.sort()
	desc = "# point: %d\n"%point
	for key in keys:
	    desc = desc + "# %7s = %5.2f range(%5.2f,%5.2f) inc(%5.2f)\n"%(key,self.getPositionVariable(key),self._Range[key][0],self._Range[key][1],self._Inc[key])
	return desc	

    def getPositionVariable(self,var):
	return self._Position[var]

    def initializePosition(self,points):
	for v in self.getVariables():
	    srange = self._Range[v][0]
	    erange = self._Range[v][1]
	    if ( srange == None ):
		srange = 0.0
	    if ( erange == None ):
		erange = 0.0
	    self._Range[v] = [srange,erange]
	    self._Position[v] += self._Range[v][0]
	self.setNumberOfPoints(points)
	
    def advancePosition(self):
	for v in self.getVariables():
	    self._Position[v] += self._Inc[v]

    def getValueVector(self):
	varvec = [0]*len(self._Index)
	for var,idx in self._Index.entries():
	    varvec[idx] = self._Position[var]
	return varvec

    def setValueVector(self,varvec):
	for var,idx in self._Index.entries():
	    self._Position[var] = varvec[idx]

