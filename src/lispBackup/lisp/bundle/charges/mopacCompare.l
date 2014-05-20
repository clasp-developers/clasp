
(include "CANDO/charges/mopacInterface.l")

(defun compare-am1-charges-across-entries (trainer)
  (let ((pathString (asString (getPath (getHeader trainer)))))
    (print (% "Reading charges for trainer: %s" pathString)))
  (let* ((mopacInterface (trainer-mopac-interface trainer))
	 (conformations (getConformationExplorer trainer))
	 (zmatrix (mopac-interface-zmatrix mopacInterface)))
    (for a (entriesAsCons zmatrix)
	 (let ((atomName (getTargetAtomZMatrixName a)))
	   (print (% "%s " atomName))
	   (for entry (entriesAsCons conformations)
		(let* ((charges (get-am1-charges mopacInterface entry))
		       (charge (if (contains charges atomName)
				   (get charges atomName)
				 (raise (LispError (% "Could not find key[%s] in dictionary with keys[%s]" atomName (allKeysAsString charges)))))))
		  (prin1 (% "%f " charge))))))))



(when (isTopLevelScript)
  (progn
    (print "Extracting mopac files")
    (include "CANDO/db/jobSetup.l")
    (database (seedDatabaseName) true)
    (let ((jobs (gatherTrainers *ARGS*)))
      (for j jobs
	   (let ((trainer (readTrainer j)))
	     (compare-am1-charges-across-entries trainer))
	   )
      )))

