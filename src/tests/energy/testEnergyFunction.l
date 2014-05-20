
(defvar *gaff* (loadArchive (% "%s/gaff.cxml" (databaseDir))))

(defvar *mol* (loadMol2 "small.mol2"))

(defvar *energyFunction* (EnergyFunction))
(defineForMatter *energyFunction* *mol* *gaff*)


(dumpTerms *energyFunction*)