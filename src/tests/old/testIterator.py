from mbb import *

db = BuilderDatabase()
m = MonomerMold()
t = Topology(m)
p = Plug(db)

t.addOutPlug("tz",p)

b = t.getOutPlugs()
print b

for z in t.outPlugIterator:
    print "key=",z.key()
    print "   data=", z.data()

