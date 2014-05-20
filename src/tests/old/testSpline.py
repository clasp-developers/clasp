

from mbbCore import *

pnts = ( 
(1.581,42.001,25.096),
(4.359,37.061,23.344),
(4.233,31.735,21.885),
(1.688,26.262,20.776),
(-3.050,23.495,19.397),
(-8.687,24.105,17.007),
(-11.875,27.017,12.918),
(-12.637,30.458,8.542),
(-10.615,33.287,4.160),
(-7.588,34.017,-1.063),
(-3.206,33.113,-3.570) )

#
# Construct the VectorVector3s
#

vpnts = VectorVector3s()
for i in pnts:
    v = Vector3()
    v.set(i[0],i[1],i[2])
    vpnts.append(v)



sp = Spline( SPLINE_CATMULLROM, vpnts )

print "Description of spline = "
sp.describe()

v1 = sp.evaluateAtIndex(0.5)
print "evaluate at 1 = ",v1.getX(), v1.getY(), v1.getZ()
