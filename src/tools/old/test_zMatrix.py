#! /bin/env python
import sys
from mbbCore import *

print "Testing zMatrix stuff"

a = ZMatrix_ElementMold("=A B C ~aa D")

print a
sys.stdout.flush()
a.dump()


xml = QuickDomFromString("""
   <zMatrixExtractor name=\"sideChain\" follows=\"backbone\">
	=C1 CB CA ~X1 CAC   <!-- X1 dihedral-->
	=HB1 CB C1 CA
	=HB2 CB C1 CA
	=C2 C1 CB ~X2 CA    <!-- relative to X2 dihedral-->
	=C3 C2 C1 CB
	=C4 C3 C2 C1
	=C5 C4 C3 C2
	=C6 C5 C4 C3
	=H2 C2 C1 C3
	=H3 C3 C2 C4
	=O4 C3 C2 C4
	=H4 O4 C4 C3
   </zMatrixExtractor>
""")
print "XML="
xml.dump()
sys.stdout.flush()

extractor = ZMatrix_Extractor()
extractor.parseFromXml(xml)
extractor.dump()
sys.stdout.flush()

