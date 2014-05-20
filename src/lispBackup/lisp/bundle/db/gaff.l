


(defparameter *gaff-forcefield-database* (loadArchive (% "%s/gaff.cxml" (databaseDir))))


(defun gaff-forcefield ()
  *gaff-forcefield-database*)