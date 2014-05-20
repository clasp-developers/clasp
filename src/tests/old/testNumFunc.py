#! /usr/bin/env python

from mbb import *


a = NumericalFunction()
a.setXYData([(1,2),(2,4),(3,8)])

a.setXStart(2.0)
a.setXInc(0.5)
b = NumericalFunction()
b.setXStart(1.0)
b.setXInc(0.5)
b.appendValue(4.0)
b.appendValue(5.0)

a.dump()
b.dump()
c = a.add(b)
c.dump()

d = NumericalFunction()
d.setXInc(0.5)
e = d.add(b)
e.dump()


z = NumericalFunction()
z.setXInc(0.5)
z.addValueToHistogram(1.0)
z.addValueToHistogram(2.0)
z.addValueToHistogram(2.0)
z.addValueToHistogram(3.0)
z.addValueToHistogram(4.0)

z.dump()
