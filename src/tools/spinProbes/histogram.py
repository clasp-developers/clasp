from mbb import *

class Histogram:
    def __init__(self,binWidth):
	self._binWidth= binWidth
	self._data = []
	self._sum = 0.0
	self._dmin = 99999999.9
	self._dmax = -99999999.9


    def addValue(self,v):
	self._data.append(v)
	self._sum = self._sum+v
	if ( v < self._dmin ): self._dmin = v
	if ( v > self._dmax ): self._dmax = v

    def getAverage(self):
	return self._sum/len(self._data)

    def getInterval(self):
	return (self._dmin,self._dmax)

    def calculate(self):
	self._numFunc = NumericalFunction()
	self._numFunc.setXInc(1.0)
	for v in self._data:
	    self._numFunc.addValueToHistogram(v);

    def getDescription(self):
	return str(self.calculate())

    def dump(self,fnout):
	fout = open(fnout,"w")
	bins = self.calculate()
	for x,y in bins:
	    print >> fout,  "%f %f"%(x,y)
	fout.close()

    def totalCounts(self):
	c = 0.0
	bins = self.calculate()
	for x,y in bins:
	    c = c + y
	return c

    def getNumericalFunction(self):
	self.calculate()
	return self._numFunc




if ( __name__ == "__main__" ):
    a = Histogram(1.0)
    a.addValue(1.1)
    a.addValue(2.1)
    a.addValue(2.4)
    a.addValue(2.9)
    a.addValue(0.4)
    a.getNumericalFunction().dump()

