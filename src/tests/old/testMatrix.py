
from mbbCore import *

vals = [-3.33333, 3.0, 2.0, -1.33333, 3.0, 0.0, \
1.33333, 2.0, 2.0, 1.33333, -3.33333, -3.0, -1.33333, 2.0, -3.0, 0.0]

mat = Matrix()

mat.set(vals)


valstr = "1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0"
for i in range(1,10000):
    mat.setFromString(valstr)




mat.dump()

evals = Vector4()
evecs = Matrix()

mat.eigenSystem( evals, evecs )

print "evals = ", evals.getW(),",", evals.getX(),",", evals.getY(),",", evals.getZ()

iLargest = evals.indexOfLargestElement()


print "Evecs = "
evecs.dump()

print "index of largest element=",iLargest
lev = evecs.getCol(iLargest)

print "lev = ", lev.getW(),",", lev.getX(),",", lev.getY(),",", lev.getZ()





fixed = VectorVector3s()
v = Vector3()
v.set(1.0,0.0,0.0)
fixed.append(v)
v.set(2.0,0.0,0.0)
fixed.append(v)
v.set(3.0,1.0,0.0)
fixed.append(v)
v.set(4.0,5.0,0.0)
fixed.append(v)

movable = VectorVector3s()
v = Vector3()
v.set(0.0,1.0,0.0)
movable.append(v)
v.set(0.0,2.0,0.0)
movable.append(v)
v.set(0.0,3.0,1.0)
movable.append(v)
v.set(0.0,4.0,8.0)
movable.append(v)

mat = superpose(fixed,movable)
rms = rootMeanSquareDifference(fixed,movable,mat)

print "rms = ",rms
print "mat = "
mat.dump()

for i in range(0,len(movable)):
    mv = movable[i]
    md = mat.multiplyByVector3(mv)
    print "Transformed: ", md.getX(),",", md.getY(),",", md.getZ()
