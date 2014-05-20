
(defvar *m* (loadMol2 "trimer.mol2"))

(defvar *rm* (GrPickableMatter))
(setFromMatter *rm* *m*)
(saveArchive *rm* "_render.cxml")
