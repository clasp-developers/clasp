

PlDetails = [ 
"color=black 		width=1.0 style=0",
"color=red 		width=1.0 style=0",
"color=green 		width=1.0 style=0",
"color=brightblue 	width=1.0 style=0",
"color=purple 		width=1.0 style=0",
"color=claret 		width=1.0 style=0",
"color=yelloworange 	width=1.0 style=0",
"color=powderblue 	width=1.0 style=0",
"color=red 		width=1.0 style=1",
"color=green 		width=1.0 style=1",
"color=brightblue 	width=1.0 style=1",
"color=purple 		width=1.0 style=1",
"color=claret 		width=1.0 style=1",
"color=yelloworange 	width=1.0 style=1",
"color=powderblue 	width=1.0 style=1",
"color=red 		width=1.0 style=2",
"color=green 		width=1.0 style=2",
"color=brightblue 	width=1.0 style=2",
"color=purple 		width=1.0 style=2",
"color=claret 		width=1.0 style=2",
"color=yelloworange 	width=1.0 style=2",
"color=powderblue 	width=1.0 style=2",
 ]

class	Ploticus:
    def __init__(self,fileName,xrange,yrange,xlabel,ylabel):
	self._FileName = fileName
	self._LineDetail = 0
	self._Lines = []
	self._XLabel = xlabel
	self._YLabel = ylabel
	self._NextWriteStack = [ self.tell() ]
	self._PopBarrier = 1
	self._XRange = xrange
	self._YRange = yrange

    def getLineDetails(self):
	p = self._LineDetail
	if ( p >= len(PlDetails) ):
	    p = 0
	return PlDetails[p]

    def write(self,str):
	self._Lines.append(str)

    def seek(self,ln):
	self._Lines = self._Lines[0:ln]

    def tell(self):
	return len(self._Lines)


    def flush(self):
	fOut = open(self._FileName,"w")
	for l in self._Lines:
	    fOut.write(l)
	fOut.close()

    def writeData(self,nf,label,inFileComment,areadef):
	self.write("%s\n"%inFileComment)
	self.write("#proc getdata\n")
	self.write(" data:\n")
	for i in range(0,nf.getNumberOfValues()):
	    x = nf.getXValueAtIndex(i)
	    y = nf.getYValueAtIndex(i)
	    self.write("%lf %lf\n"%(x,y))
	self.write("\n")
	if ( areadef ):
	    self.write("#proc areadef\n")
	    self.write(" areaname: square\n")
	    if ( self._XRange != None ):
		self.write(" xrange: %f %f\n"%self._XRange)
	    else:
		self.write(" xautorange: datafield=1\n")
	    if ( self._YRange != None ):
		self.write(" yrange: %f %f\n"%self._YRange)
	    else:
		self.write(" yautorange: datafield=2\n")
	    self.write("\n")
	self.write("#proc legendentry\n")
	self.write(" sampletype: line\n")
	self.write(" details: %s\n"%self.getLineDetails())
	self.write(" label: %s\n"%label)
	self.write("\n")
	self.write("#proc lineplot\n")
	self.write(" linedetails: %s\n"%self.getLineDetails())
	self._LineDetail += 1
	self.write(" xfield: 1\n")
	self.write(" yfield: 2\n")
	self.write("\n")
	self.flush()

    def writeFinish(self,loc="max-3.0 max"):
	self.seek(self._NextWriteStack[-1])
	self.write("#proc xaxis\n")
	self.write(" label: %s\n"%self._XLabel )
	self.write(" tics: yes\n" )
	self.write(" stubs: incremental\n" )
	self.write("\n")
	self.write("#proc yaxis\n")
	self.write(" label: %s\n"%self._YLabel );
	self.write(" stubs: incremental\n" )
	self.write(" tics: yes\n" )
	self.write("\n")
	self.write("#proc legend\n")
	self.write(" format: multiline\n")
	self.write(" location: %s\n"%loc)
	self.write("\n")
	self.flush()


    def pushData(self,nf,label,inFileComment,areadef=True):
	if ( len(self._NextWriteStack) != 0 ):
	    self.seek(self._NextWriteStack[-1])
	self.writeData(nf,label,inFileComment,areadef);
	self._NextWriteStack.append(self.tell())
	self.writeFinish()
	self.flush()

    def popData(self):
	if ( len(self._NextWriteStack) > self._PopBarrier ):
	    self._NextWriteStack.pop()
	    self._LineDetail -= 1
	    if ( self._LineDetail < 0 ):
	        self._LineDetail = 0

	#
	# protectData will prevent data that has already been pushed
	# from being popped
	#
    def protectData(self):
	self._PopBarrier = len(self._NextWriteStack)


class	PointPloticus:
    def __init__(self,fileName,xlabel,ylabel):
	self._FileName = fileName
	self._Lines = []
	self.write("#proc getdata\n")
	self.write(" data:\n")
	self._LineDetail = 0
	self._XLabel = xlabel
	self._YLabel = ylabel
	self._NextWrite = None

    def write(self,str):
	self._Lines.append(str)

    def seek(self,ln):
	self._Lines = self._Lines[0:ln]

    def tell(self):
	return len(self._Lines)


    def flush(self):
	fOut = open(self._FileName,"w")
	for l in self._Lines:
	    fOut.write(l)
	fOut.close()

    def getLineDetails(self):
	p = self._LineDetail
	if ( p >= len(PlDetails) ):
	    p = 0
	return PlDetails[p]

    def addPoint(self,x,y):
	if ( self._NextWrite != None ):
	    self.seek(self._NextWrite)
	self.write("%lf %lf\n"%(x,y))
	self._NextWrite = self.tell()
	self.write("\n")
	self.write("#proc areadef\n")
	self.write(" areaname: square\n")
	self.write(" xautorange: datafield=1\n")
	self.write(" yautorange: datafield=2\n")
	self.write("\n")
	self.write("#proc lineplot\n")
	self.write(" linedetails: %s\n"%self.getLineDetails())
	self.write(" xfield: 1\n")
	self.write(" yfield: 2\n")
	self.write("\n")
	self.flush()
	self.write("#proc xaxis\n")
	self.write(" label: %s\n"%self._XLabel )
	self.write(" tics: yes\n" )
	self.write(" stubs: incremental\n" )
	self.write("\n")
	self.write("#proc yaxis\n")
	self.write(" label: %s\n"%self._YLabel );
	self.write(" stubs: incremental\n" )
	self.write(" tics: yes\n" )
	self.write("\n")
	self.flush()

