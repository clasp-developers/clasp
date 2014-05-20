#!/bin/env python

from mbbCore import *


a = buildOrigin()
print "Origin = ", a.asString()
b = buildUsingBond(1.06,a)
print "Bond = ", b.asString()
c = buildUsingBondAngle(1.56,b,120*0.0174533,a)
print "Angle = ", c.asString()

vb = 1.06
va = 120.0*0.0174533
vd = -50*0.0174533

print "vb=",vb
print "va=",va/0.0174533
print "vd=",vd/0.0174533
d = buildUsingBondAngleDihedral(vb,c,va,b,vd,a)
print "Dihedral = ", d.asString()

print "Extracted bond = ", calculateDistance(d,c)
print "Extracted angle = ", calculateAngle(d,c,b)/0.0174533
print "Extracted dihedral = ", calculateDihedral(d,c,b,a)/0.0174533
