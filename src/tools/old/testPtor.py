#! /bin/env python
from mbbCore import *

ff = moeReadForceField("charmm27.ff")
ff.ptors.list("","","C","O")
ff.ptors.list("O","C","","")

def lm(a):
    t = a.upper().split(",");
    ff.ptors.list(t[0],t[1],t[2],t[3])

