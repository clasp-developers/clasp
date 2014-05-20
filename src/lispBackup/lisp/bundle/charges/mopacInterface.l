;(include "CANDO/db/organizer.l")
(include "CANDO/db/trainer.l")



(defClass MopacAbandoned ConditionWithStackTrace ())
(defClass MopacError ConditionWithStackTrace () )




(defvar *mopacExec* "~/Local/bin/mopac.exe")


(defun deleteIfExists (fileName)
  (print (% "Checking if file[%s] exists" fileName ))
  (let ((p (Path fileName)))
    (when (exists p)
      (print (% "Removing file: %s" fileName)
	     (remove p)))
    ))



;;
;; Read and extract the AM1 charges and coordinates from the mopac output file
;;
;; If the calculation was successful -
;;  - return a cons of two ObjectDictionarys.
;;    The first maps atoms to AM1 charges and the
;;    second one maps atoms to AM1 optimized coordinates
;; If the mopac calculation failed raise an error
;;

(defun parse-mopac-output (outputFileName zMatrix)
  (let* ((fin (FileInStream outputFileName))
	 (calculationAbandonedRe (Regex "[[:space:]]*CALCULATION ABANDONED.*"))
	 (startChargeRe (Regex "[[:space:]]*NET ATOMIC CHARGES.*"))
	 (chargeRe (Regex "[[:space:]]*([[:digit:]]+)[[:space:]]*([[:word:]]+)[[:space:]]*(-?[[:digit:]]*\.[[:digit:]]+).*"))
	 (stopChargeRe (Regex "[[:space:]]*"))
	 (startCartesianRe (Regex "[[:space:]]*CARTESIAN.*"))
	 (cartesianRe (Regex "[[:space:]]*([[:digit:]]+)[[:space:]]*([[:word:]]+)[[:space:]]*(-?[[:digit:]]*\.[[:digit:]]+)[[:space:]]*(-?[[:digit:]]*\.[[:digit:]]+)[[:space:]]*(-?[[:digit:]]*\.[[:digit:]]+).*"))
	 (stopCartesianRe (Regex "[[:space:]]*"))
	 (lines (linesAsCons fin))
	 (charges (ObjectMap))
	 (coordinates (ObjectMap))
	 (calculationAbandoned false))
    (let ((mode :idle))
      (for l lines
	   (cond
	    ((regex-matches calculationAbandonedRe l)
	     (raise (MopacAbandoned "Mopac calculation abandoned")))
	    ((regex-matches startChargeRe l)    (setq mode :waitingForCharges))
	    ((and (== mode :parseCharges) (regex-matches stopChargeRe l))
	     (setq mode :idle))
	    ((regex-matches startCartesianRe l) (setq mode :waitingForCartesian))
	    ((and (== mode :parseCartesian) (regex-matches stopCartesianRe l))
	     (setq mode :idle))
	    (true
	     (cond
	      ((or (== mode :waitingForCharges) (== mode :parseCharges ))
	       (let ((chargeMatch (regex-match chargeRe l)))
		 (when (matched chargeMatch 0)
		   (print (% "Parsed new charge line: %s" l))
		   (setq mode :parseCharges)
		   (let* ((atomName (% "%s%s" (part chargeMatch 2) (part chargeMatch 1)))
			  (charge (asReal (part chargeMatch 3)))
			  (atom (chem:getAtom (zmatrix-entry-with-name zMatrix atomName))))
		     (extend charges atom charge)))))
	      ((or (== mode :waitingForCartesian) (== mode :parseCartesian ))
	       (let ((cartesianMatch (regex-match cartesianRe l)))
		 (when (matched cartesianMatch 0)
		   (setq mode :parseCartesian)
		   (print (% "Parsed cartesian line: %s" l))
		   (let* ((atomName (% "%s%s" (part cartesianMatch 2) (part cartesianMatch 1)))
			  (pos (OVector3 (asReal (part cartesianMatch 3))
					 (asReal (part cartesianMatch 4))
					 (asReal (part cartesianMatch 5))))
			  (atom (chem:getAtom (zmatrix-entry-with-name zMatrix atomName))))
		     (extend coordinates atom pos)))))
	      (true () ))))))
    (list charges coordinates)))


;;
;; Print a Mopac ZMatrix entry
;;
(defun formatZMatrixEntry (entry &key (optBonds true) (optAngles false) (optDihedrals false))
  (let ((targetAtomName (getElementAsString (getAtom entry)))
	(bondIndex -1)
	(bondValue 0.0)
	(angleIndex -1)
	(angleValue 0.0)
	(dihedralIndex -1)
	(dihedralValue 0.0)
	(optimizeBond (if optBonds 1 0))
	(optimizeAngle (if optAngles 1 0))
	(optimizeDihedral (if optDihedrals 1 0))
	)
    (when (hasBond entry)
      (setq bondIndex (getBondAtomIndex (getBond entry)))
      (setq bondValue (getValue (getBond entry)))
      (when (hasAngle entry)
	(assert (eql (getBondAtomIndex (getAngle entry)) bondIndex)
		"angle - bond index doesn't match")
	(setq angleIndex (getAngleAtomIndex (getAngle entry)))
	(setq angleValue (getValue (getAngle entry)))
	(when ( hasDihedral entry)
	  (assert (eql (getBondAtomIndex (getDihedral entry)) bondIndex)
		  "dihedral - bond index doesn't match")
	  (assert (eql (getAngleAtomIndex (getDihedral entry)) angleIndex)
		  "dihedral - angle index doesn't match")
	  (setq dihedralIndex (getDihedralAtomIndex (getDihedral entry)))
	  (setq dihedralValue (getValue (getDihedral entry))))))
    (% "%s   %f %d %f %d %f %d   %d %d %d"
       targetAtomName
       bondValue optimizeBond
       angleValue optimizeAngle
       dihedralValue optimizeDihedral
       (+ bondIndex 1)
       (+ angleIndex 1)
       (+ dihedralIndex 1) )
    ))

(defun formatDebugZMatrixEntry (zmatrix entry &key (optimize true))
  (let ((targetAtomName (getElementAsString (getAtom entry)))
	(bondIndex -1)
	(bondValue 0.0)
	(angleIndex -1)
	(angleValue 0.0)
	(dihedralIndex -1)
	(dihedralValue 0.0)
	(optimizeBond (if optimize 1 0))
	(optimizeAngle (if optimize 1 0))
	(optimizeDihedral (if optimize 1 0))
	)
    (when (hasBond entry)
      (setq bondIndex (getBondAtomIndex (getBond entry)))
      (setq bondValue (getValue (getBond entry)))
      (when (hasAngle entry)
	(assert (eql (getBondAtomIndex (getAngle entry)) bondIndex)
		"angle - bond index doesn't match")
	(setq angleIndex (getAngleAtomIndex (getAngle entry)))
	(setq angleValue (getValue (getAngle entry)))
	(when ( hasDihedral entry)
	  (assert (eql (getBondAtomIndex (getDihedral entry)) bondIndex)
		  "dihedral - bond index doesn't match")
	  (assert (eql (getAngleAtomIndex (getDihedral entry)) angleIndex)
		  "dihedral - angle index doesn't match")
	  (setq dihedralIndex (getDihedralAtomIndex (getDihedral entry)))
	  (setq dihedralValue (getValue (getDihedral entry))))))
    (% "%s   %f %d %f %d %f %d   %d %d %d ; %s %s %s %s"
       targetAtomName
       bondValue optimizeBond
       angleValue optimizeAngle
       dihedralValue optimizeDihedral
       (+ bondIndex 1)
       (+ angleIndex 1)
       (+ dihedralIndex 1)
       (getTargetAtomZMatrixName entry)
       (if (<= 0 bondIndex)
	   (get-zmatrix-atom-name-at-index zmatrix bondIndex)
	 "-")
       (if (<= 0 angleIndex)
	   (get-zmatrix-atom-name-at-index zmatrix angleIndex)
	 "-")
       (if (<= 0 dihedralIndex)
	   (get-zmatrix-atom-name-at-index zmatrix dihedralIndex)
	 "-")
       )
    ))



(defun writeMopacZMatrix (mopacInterface fileName zMatrix &key (optBonds true) (optAngles false) (optDihedrals false))
  (let ((fout (FileOutStream (asString fileName))))
    (write fout (% "%s\n" (mopac-calculation mopacInterface)))
    (write fout "comment1\n")
    (write fout "comment2\n")
    (for e (entriesAsCons zMatrix)
       (write fout (% "%s\n" (formatZMatrixEntry e
						 :optBonds optBonds
						 :optAngles optAngles
						 :optDihedrals optDihedrals ))))
    (close fout)))

(defun writeMopacDebugZMatrix (fileName zMatrix)
  (let ((fout (FileOutStream (asString fileName))))
    (write fout "am1 dfp mmok\n")
    (write fout "comment1\n")
    (write fout "comment2\n")
    (for e (entriesAsCons zMatrix)
       (write fout (% "%s\n" (formatDebugZMatrixEntry zMatrix e))))
    (close fout)))



(defClass MopacInterface () ( _weakTrainer _zMatrix _mopacCalculation ))

(defMethod __init__ MopacInterface (self trainer)
  "Create a MopacInterface for the given trainer using the current selected stage"
  (let* ((atomIndex 0)
	 (conformations (getConformationExplorer trainer))
	 (matter (getMatter conformations)))
    (setq (slot self _weakTrainer) (WeakReference trainer))
    (setq (slot self _zMatrix) (ZMatrix matter))
    ))


(defMethod zmatrix MopacInterface (self)
  (slot self _zMatrix))


(defMethod mopac-calculation MopacInterface (self &optional calc)
  "Set the mopac calculation if calc is defined, otherwise return it"
  (if calc
      (setq (slot self _mopacCalculation) calc)
    (slot self _mopacCalculation)))



(defMethod cleanup-mopac-for-entry MopacInterface (self trainer entry)
  (let ((pathString (asString (entry-mopac-directory self trainer entry)))
	(job (entry-mopac-job-name self trainer entry)))
    (for ext (list "out" "res" "den" "log" "arc" "gpt" "syb" "ump" "end")
	 (deleteIfExists (% "%s/%s.%s" pathString job ext)))))
  

(defMethod run-mopac-for-entry MopacInterface (self trainer entry)
  (let ((pathString (asString (entry-mopac-directory self trainer entry)))
	(job (entry-mopac-job-name self trainer entry)))
    (print (% "Running mopac on directory[%s]" pathString))
    (setenv "FOR005" (% "%s.dat" job))
    (setenv "FOR006" (% "%s.out" job))
    (setenv "FOR009" (% "%s.res" job))
    (setenv "FOR010" (% "%s.den" job))
    (setenv "FOR011" (% "%s.log" job))
    (setenv "FOR012" (% "%s.arc" job))
    (setenv "FOR013" (% "%s.gpt" job))
    (setenv "FOR016" (% "%s.syb" job))
    (setenv "FOR020" (% "%s.ump" job))
    (setenv "SETUP" (% "SETUP.DAT"))
    (setenv "SHUTDOWN" (% "%s.end" job))
    (print (% "Running mopac job[%s] in directory[%s]" job pathString))
    (let ((cmd (% "(cd %s; %s %s <%s.dat)" pathString *mopacExec* job job)))
      (print cmd)
      (let ((result (system cmd)))
	(print (% "Result[%s]" result))
	(when (!= result 0)
	  (cleanup-mopac-for-entry self trainer entry)
	  (raise (MopacError "Mopac was terminated"))))
      )))












(defMethod mopac-interface-zmatrix MopacInterface (self)
  (slot self _zMatrix))


(defMethod get-am1-entry-stage MopacInterface (self entry)
  "Get the Mopac EntryStage for this entry"
  (if (hasEntryStageWithName entry "Am1")
      (getEntryStage entry "Am1")
    ()))


(defMethod get-am1-entry-stage-status MopacInterface (self entry)
  "Returns the current status of the am1 calculation.
   Can be 'start, 'extracted, 'error, 'prerun"
  (if (hasEntryStageWithName entry "Am1")
      (let ((stage (getEntryStage entry "Am1")))
	(if (contains (getBinder stage) 'Am1Status)
	    (lookup (getBinder stage) 'Am1Status)
	  'none))
    'none
    )
  )

(defMethod set-am1-entry-stage-status MopacInterface (self entry status)
  "Returns the current status of the am1 calculation.
   Can be 'none, 'extracted, 'error, 'prerun"
  (when (not (or (== status 'none)
		 (== status 'ran)
		 (== status 'extracted)
		 (== status 'error)
		 (== status 'prerun)))
    (raise (LispError (% "Illegal am1-entry-stage-status[%s]" status))))
  (let ((am1Stage (if (hasEntryStageWithName entry "Am1")
		      (getEntryStage entry "Am1")
		    (createEntryStage entry "Am1"))))
    (extend (getBinder am1Stage) 'Am1Status status))
  )


(defMethod has-am1-charges MopacInterface (self entry)
  (let ((am1EntryStage (get-am1-entry-stage self entry)))
    (if am1EntryStage
	(contains (getBinder am1EntryStage) 'am1Charges)
      false)))

(defMethod get-am1-charges MopacInterface (self entry)
  (let ((am1EntryStage (get-am1-entry-stage self entry)))
    (if (and am1EntryStage (contains (getBinder am1EntryStage) 'am1Charges))
	(lookup (getBinder am1EntryStage) 'am1Charges)
      ()
)))


(defMethod write-am1-coordinates-into-matter MopacInterface (self coordinates)
  "Write the coordinates into the atoms of the zMatrix"
  (for atom (keys-as-cons coordinates)
       (let ((pos (lookup coordinates atom)))
	 (chem:setPosition atom pos))))


(defMethod create-am1-entry-stage MopacInterface (self trainer entry coordinates charges)
  "Write the coordinates and charges into a new Mopac stage for the entry."
  (let* ((am1Stage (if (hasEntryStageWithName entry "Am1")
		       (getEntryStage entry "Am1")
		     (raise (LispError "There must be an Am1 stage"))))
	 (matter (getMatter (getConformationExplorer trainer))))
    (write-am1-coordinates-into-matter self coordinates)
    (extractCoordinatesFromMatter am1Stage matter)
    (extend (getBinder am1Stage) 'am1Charges charges)))
    




(defMethod entry-mopac-directory MopacInterface ( self trainer entry )
  (let* ((trainerPath (getPath (getHeader trainer)))
	 (entryIndex (getUniqueEntryIndex entry))
	 (mopacPath (append (copyPath trainerPath) (% "conformation%04d" entryIndex))))
    mopacPath))

(defMethod entry-mopac-job-name MopacInterface (self trainer entry)
  "Return the name of the jobs"
  "entry")

(defMethod entry-mopac-file MopacInterface ( self trainer entry extension )
  (let* ((mopacPath (entry-mopac-directory self trainer entry ))
	 (mopacFilePath (append (copyPath mopacPath)
				(% "%s.%s" (entry-mopac-job-name self trainer entry) extension ))))
    mopacFilePath))



(defMethod createMopacDirectories MopacInterface (self &key (optBonds true) (optAngles false) (optDihedrals false))
  "Create all of the mopac directories and data files.
Return a list of mopac data files that need to be run through mopac."
  (assert (valid (slot self _weakTrainer)) "The trainer has become invalid")
  (let* ((trainer (lock (slot self _weakTrainer)))
	 (conformations (getConformationExplorer trainer))
	 (trainerPath (getPath (getHeader trainer)))
	 (changedTrainer false))
    (for entry (entriesAsCons conformations)
	 (when (== (get-am1-entry-stage-status self entry) 'none)
	   (setq changedTrainer true)
	   (let* ((mopacDirectory (entry-mopac-directory self trainer entry ))
		  (mopacJobName (entry-mopac-job-name self trainer entry))
		  (mopacFilePath (entry-mopac-file self trainer entry "dat"))
		  (selectedStage (getSelectedStage entry))
		  (matter (getMatter conformations)))
	     (createDirectory mopacDirectory)
	     (set-am1-entry-stage-status self entry 'prerun)
	     (writeCoordinatesToMatter selectedStage matter)
	     (extractInternals (slot self _zMatrix))
	     (writeMopacZMatrix self mopacFilePath (slot self _zMatrix)
				:optBonds optBonds
				:optAngles optAngles
				:optDihedrals optDihedrals )
	     (writeMopacDebugZMatrix (entry-mopac-file self trainer entry "zmd") (slot self _zMatrix))
	     (print (% "Created directory[%s] and wrote mopac file[%s]" (asString mopacDirectory) (asString mopacFilePath)))
	     )))
    (when changedTrainer
      (chem:write trainer))
    ))











;;
;;
;; Creates an interface for the trainer 


(defun create-mopac-interface-for-trainer (trainer &key (mopacCalculation "am1 precise dft mmok") (optBonds true) (optAngles false) (optDihedrals false))
  (let ((mopacInterface (if (trainer-has-mopac-interface trainer)
			    (trainer-mopac-interface trainer)
			  (let ((mi (MopacInterface trainer)))
			    (trainer-mopac-interface trainer (MopacInterface trainer))
			    mi))))
    (mopac-calculation mopacInterface mopacCalculation)
    (createMopacDirectories mopacInterface
			    :optBonds optBonds
			    :optAngles optAngles
			    :optDihedrals optDihedrals )
    (print (% "Writing trainer %s" (getPath (getHeader trainer))))
    (chem:write trainer)
    mopacInterface))




;;
;; Reset the MopacInterface for a trainer
;;

(defun mopac-reset (trainer)
  (let* ((mopacInterface (trainer-mopac-interface trainer))
	 (conformations (getConformationExplorer trainer))
	 (matter (getMatter conformations)))
    (for entry (entriesAsCons conformations)
	 (let ((mopacOutputFile (entry-mopac-file mopacInterface trainer entry "out")))
	   (print (% "Resetting %s" mopacOutputFile)))
	 (cleanup-mopac-for-entry mopacInterface trainer entry)
	 (set-am1-entry-stage-status mopacInterface entry 'none))
    (chem:write trainer)))







(defun run-mopac-for-trainer (trainer)
  "Run the mopac calculations for one trainer"
  (let* ((mopacInterface (trainer-mopac-interface trainer))
	 (conformations (getConformationExplorer trainer))
	 (modifiedTrainer false))
    (for entry (entriesAsCons conformations)
	 (let ((status (get-am1-entry-stage-status mopacInterface entry)))
	   (if (== status 'prerun)
	       (progn
		 (run-mopac-for-entry mopacInterface trainer entry)
		 (set-am1-entry-stage-status mopacInterface entry 'ran)
		 (chem:write trainer)
		 )
	     (print (% "ignoring [%s] status[%s]"
		       (entry-mopac-directory mopacInterface trainer entry) status  )))))
))    



(defun extract-and-save-mopac-data (trainer)
  (let* ((mopacInterface (trainer-mopac-interface trainer))
	 (conformations (getConformationExplorer trainer))
	 (matter (getMatter conformations)))
    (for entry (entriesAsCons conformations)
	 (let ((mopacOutputFile (entry-mopac-file mopacInterface trainer entry "out")))
	   (when (exists mopacOutputFile)
	     (handler-case
	      (let* ((chargesAndCoordinates (parse-mopac-output (asString mopacOutputFile)
								(zmatrix mopacInterface)))
		     (charges (car chargesAndCoordinates))
		     (coordinates (cadr chargesAndCoordinates)))
		(print (% "Parsing %s" mopacOutputFile))
		(create-am1-entry-stage mopacInterface trainer entry coordinates charges)
		(when (== (size coordinates) 0)
		  (raise (MopacError "There are no coordinates")))
		(when (== (size charges) 0)
		  (raise (MopacError "There are no charges")))
		(print (% "The keys for the coordinates are[%s]" (summary-of-contents coordinates)))
		(print (% "The keys for the charges are[%s]" (summary-of-contents charges)))
		(print (% "MOPAC-STATUS-SUCCESS %s" (asString mopacOutputFile)))
		(set-am1-entry-stage-status mopacInterface entry 'extracted)
		)
	      (MopacAbandoned (e)
			      (progn
				(set-am1-entry-stage-status mopacInterface entry 'error)
				(print (% "MOPAC-STATUS-ABANDONED %s" (asString mopacOutputFile)))))
	      (MopacError (e)
			  (progn
			    (set-am1-entry-stage-status mopacInterface entry 'error)
			    (print (% "MOPAC-STATUS-ERROR %s" (asString mopacOutputFile)))))
	      )
	     (chem:write trainer)
	     )))))






(defun apply-bcc-corrections-to-trainer (trainer bccCorrections)
  (trainer-bcc-corrections trainer bccCorrections)
  (chem:write trainer))
  


