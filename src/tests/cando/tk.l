

(defvar *at* (kin:AtomTree))
(kin:resizeMolecules *at* 1)
(kin:resizeResidues *at* 0 1)
(kin:resizeAtoms *at* 0 0 10)

(print "About to create newBondedAtom --------------")
;(defvar *a1* (kin:newBondedAtom *at* (list 0 0 0)))
;(defvar *a2* (kin:newBondedAtom *at* (list 0 0 1)))

(print "done creation -----")
;(print (kin:asString *a1*))
;(print (kin:asString *a2*))

(print "About to create bad atom")
(defvar *a3* (kin:newBondedAtom *at* (list 0 0 100)))

(print "About to shutdown ---------")
