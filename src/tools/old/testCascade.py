#! /bin/env python

from mbb import *

m = Matrix()
m.translate(Vector3(1,0,0))

a = TransformCascade()
i = a.addEntry(0)
a.setMatrix(i,m)
i = a.addEntry(i)
a.setMatrix(i,m)
i = a.addEntry(i)
a.setMatrix(i,m)
i = a.addEntry(i)
a.setMatrix(i,m)
i = a.addEntry(i)
a.setMatrix(i,m)

a.build()
a.dump()
