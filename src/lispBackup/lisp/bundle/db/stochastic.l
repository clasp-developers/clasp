(include "CANDO/db/trainer.l")

					; fixed code
(include "CANDO/db/geometryChecker.l" )	;require "CANDO/db/amideBondTester.l"
					;include "CANDO/db/stereochemistryTester.l"

(defparameter *SaveDurationInSeconds* (* 4 60 ) ; 14 minutes = 14*60 seconds
	)				  ;
					; :options
					;	[ maxRestartFails = [ [self slot _Options] lookupDefault :maxRestartFails 60 ] ]
					;	[ maxMinimizations = [ [self slot _Options] lookupDefault :maxMinimizations 10000 ] ]

(defparameter *StochasticSearch.StartStageName* "cando_gaff" )
(defparameter *StochasticSearch.StartPackage* "cando" )
(defparameter *StochasticSearch.StartModel* "gaff" )




(defun alignAggregate (trainer agg )
  (let* ((focusMonomer (trainer-focus-monomer trainer))
	 (focusConstitution (getConstitution focusMonomer ))
	 (focusTopology (getTopology focusMonomer ))
	 (alignTo (progn
		    (assert (notNil focusTopology )"focusTopology is nil - it should never be nil" )
		    (let ((at (getAlignTo focusTopology)))
		      (when (isNil at)
			(print (% "ERROR: FocusMonomer(%s) " (getName focusMonomer )))
			(print (% "ERROR: FocusTopology(%s) " (getName focusTopology )))
			(raise  "alignTo is nil - it should never be nil" ))
		      at)))
	 (dbg (debugAsString alignTo focusMonomer ))
	 (coordSys (getCoordinateSystem alignTo focusMonomer ))
	 (transform (matrixToCanonical coordSys )))
    (applyTransformToAtoms agg transform )))




(defClass StochasticSearch ()(_ForceField _MolTrainer _FileName _StartStageName _StartPackage _StartModel _TwisterDriver _Options _KeepEverything )
  (method __init__ (self forceField trainer &key (options (list )))
	  (setq (slot self _ForceField )forceField )
	  (setq (slot self _MolTrainer )trainer )
	  (setq (slot self _FileName ) (asString (getPath (getHeader trainer ))))
	  (setq (slot self _TwisterDriver )())
	  (setq (slot self _Options )options )
	  (setq (slot self _StartStageName) *StochasticSearch.StartStageName*)
	  (setq (slot self _KeepEverything )false )
	  (setupRotatableTorsions self )
	  self )
  (method setKeepEverything (self b )
	  (setq (slot self _KeepEverything )b ))
  (method defineTwistableAtoms (self agg focusResidue focusMonomer )
	  (let ((atomsAdded 0))
	    (for a (atoms agg )
		 (setTempInt a 0 ))
	    (for a (atoms focusResidue )
		 (ifTrue (!= (chem:getElement a ) :H )
			 (setTempInt a 1 )
			 (setq atomsAdded (+ atomsAdded 1 ))))
	    (ifTrue (== atomsAdded 0 )
		    (for a (atoms focusResidue )
			 (setTempInt a 1 )
			 (setq atomsAdded (+ atomsAdded 1 ))))
	    (print (% "Atoms with tempInt set to 1 = %d" atomsAdded ))
	    (log (% "Atoms with tempInt set to 1 = %d" atomsAdded ))
	    (print "------")
	    (print "------")
	    (print "------")
	    (print (% "At %s ---- We need to replace the following code with something that uses" (repr (sourceFileLine)) ))
	    (print "           the atom tree and the stub atoms for the root")
	    (print "------")
	    (print "------")
	    (print "------")
;;	    (ifTrue (hasInCoupling focusMonomer )
;;		    (let ((boundFrame (getAtomBoundFrameForInCoupling focusMonomer ))
;;			  (originAtom (get_oAtom boundFrame ))
;;			  (xAtom (get_pAtomOrNil boundFrame ))
;;			  (xyAtom (get_qAtomOrNil boundFrame )))
;;		      (print (% "boundFrame = %s" (repr boundFrame )))
;;		      (setTempInt originAtom 1 )
;;		      (ifTrue (notNil xAtom )
;;			      (setTempInt xAtom 1 ))
;;		      (ifTrue (notNil xyAtom )
;;			      (setTempInt xyAtom 1 ))
;;		      (print (% "boundFrame atoms (%s,%s,%s)" (description originAtom )(description xAtom )(description xyAtom )))))
	    (prognDebug 
	     (setq atomsAdded 0 )
	     (for a (atoms agg )
		  (ifTrue (== (getTempInt a ) 1 )
			  (setq atomsAdded (+ atomsAdded 1 )))))
	    (prognDebug 
	     (setq atomsAdded 0 )
	     (for a (atoms agg )
		  (ifTrue (== (getTempInt a )1 )
			  (setq atomsAdded (+ atomsAdded 1 ))))
	     (print (% "Number of selected atoms = %d" atomsAdded ))
	     (log (% "Number of selected atoms = %d" atomsAdded )))))
  (method bondsToBreakAndTwist (self agg )
	  (let (bondsToBreak bondsToTwist a1 a2 order bond )
	    (setq bondsToBreak ())
	    (setq bondsToTwist ())
	    (for bondLoop (bonds agg )
		 (setq a1 (getAtom1 bondLoop ))
		 (setq a2 (getAtom2 bondLoop ))
		 (setq order (getBondOrder bondLoop ))
		 (if (not (or (== (getBackSpan a1 )a2 )(== (getBackSpan a2 )a1 )))
		     (then 
		      (prognLog "Break bond not in spanning tree" 
				(print (% "Found bond that is not in spanning tree %s-%s" (description a1 )(description a2 )))
				(setq bond (list a1 a2 order ))
				(setq bondsToBreak (cons bond bondsToBreak ))
				(removeBondTo a1 a2 )))
		   (else 
		    (prognLog "Twist bond if both atoms have tempInt == 1 " 
			      (ifTrue (and (== (getTempInt a1 )1 )(== (getTempInt a2 )1 ))
				      (ifTrue (and (> (numberOfBonds a1 )1 )(> (numberOfBonds a2 )1 ))
					      (ifTrue (and (== (getTempInt a1 )1 )(== (getTempInt a2 )1 ))
						      (prognLog "Twisting around bond" 
								(setq bondsToTwist (cons (list a1 a2 order )bondsToTwist ))))))))))
	    (list bondsToBreak bondsToTwist )))
  (method setupRotatableTorsions (self )
	  (let (header body structures agg focusMonomer focusResidue firstAtom span atomCount a tempBondsToBreakAndTwist bondsToBreak bondsToTwist )
	    (log "StochasticSearch.setupRotatableTorsions" )
	    (setq header (getHeader (slot self _MolTrainer )))
	    (setq body (slot self _MolTrainer ))
	    (setq structures (getConformationExplorer body ))
	    (setq agg (getMatter structures ))
	    (setq focusMonomer (trainer-focus-monomer body))
	    (setq focusResidue (trainer-focus-residue body))
	    (setq firstAtom (firstAtom focusResidue ))
	    (setq span (SpanningLoop ))
	    (setTop span firstAtom )
	    (setq atomCount 0 )		; Build the spanning tree

	    (while (advanceLoopAndProcess span )
	      (setq a (getAtom span ))
	      (setq atomCount (+ atomCount 1 )))
	    (log (% "Number of atoms = %d" atomCount ))	;
					; Define the atoms that will be used for superposition and twisting
					; Set TempInt of twistable atoms to 1
					;

	    (defineTwistableAtoms self agg focusResidue focusMonomer )
	    (clearSuperposeAtoms structures )
	    (log "Adding superpose atoms" )
	    (for a (atoms agg )
		 (log (% "Looking at atom: %s" (description a )))
		 (if (== (getTempInt a )1 )
		     (then 
		      (addSuperposeAtom structures a )
		      (log (% "Added atom: %s" (description a ))))
		   (else 
		    (log (% "NOT adding atom: %s" (description a ))))))
	    (log "Done adding superpose atoms" ) ;
					; Loop through all bonds
					; Save and break the bonds that are not edges in the spanning tree
					; Create twisters for bonds between atoms with more than one bond to them 
					;

	    (log "loop through bonds" )
	    (setq tempBondsToBreakAndTwist (bondsToBreakAndTwist self agg ))
	    (setq bondsToBreak (car tempBondsToBreakAndTwist ))
	    (setq bondsToTwist (cadr tempBondsToBreakAndTwist ))
	    (log "creating twister driver" )
	    (setq (slot self _TwisterDriver )(setupTwisterDriver self bondsToTwist ))
	    (log "Reforming bonds that we broke" )
	    (reformBrokenBonds self bondsToBreak )))
  (method setupTwisterDriver (self bondsToTwist )
	  (let (twisterDriver twistA1 twistA2 order a1Atoms span a2Atoms twistAtoms twistFixed twistMovable twister )
	    (setq twisterDriver (TwisterDriver ))
	    (for bond bondsToTwist 
		 (setq twistA1 (car bond ))
		 (setq twistA2 (cadr bond ))
		 (setq order (caddr bond ))
		 (log "Breaking bond between the atoms to twist" )
		 (removeBondTo twistA1 twistA2 ) ; Accumulate the atoms on the A1 side

		 (setq a1Atoms ())
		 (setq span (SpanningLoop ))
		 (setTop span twistA1 )
		 (while (advance span )
		   (setq a1Atoms (cons (getAtom span )a1Atoms )) ; Accumulate the atoms on the A2 side
		   )
		 (setq a2Atoms ())
		 (setq span (SpanningLoop ))
		 (setTop span twistA2 )
		 (while (advance span )
		   (setq a2Atoms (cons (getAtom span )a1Atoms )))
		 (if (< (length a1Atoms )(length a2Atoms ))
		     (then 
		      (setq twistAtoms a1Atoms )
		      (setq twistFixed twistA2 )
		      (setq twistMovable twistA1 ))
		   (else 
		    (setq twistAtoms a2Atoms )
		    (setq twistFixed twistA1 )
		    (setq twistMovable twistA2 )))
		 (bondTo twistA1 twistA2 order )
		 (setq twister (Twister ))
		 (setFixedAndMovable twister twistFixed twistMovable )
		 (for a twistAtoms 
		      (addAtom twister a ))
		 (addTwister twisterDriver twister ))
	    twisterDriver ))
  (method reformBrokenBonds (self bondsBroken )
	  (let (a1 a2 order )
	    (for b bondsBroken 
		 (setq a1 (car b ))
		 (setq a2 (cadr b ))
		 (setq order (caddr b ))
		 (log (% "Reforming bond %s - %s" (description a1 )(description a2 ))) ;
					; Reform the bonds that we broke
					;

		 (bondTo a1 a2 order ))))
  (method createInitializeAndAddEntry (self structures minimizer matter )
	  (let* ((entry (createEntry structures ))
		 (entryStage (createEntryStage entry (slot self _StartStageName))))
	    (setExternalInterfaceName entryStage (slot self _StartPackage ))
	    (setModel entryStage (slot self _StartModel ))
	    (setEnergyKCal entryStage (getTotalEnergy (getEnergyFunction minimizer )))
	    (setSelectedStage entry entryStage )
	    (extractCoordinatesFromMatter entryStage matter )
	    (setComplete entryStage true )
	    (appendEntry structures entry )))



  (method runOneStochasticSearch (self trainer structures energyFunction minimizer
				       agg fails restartFails successes geometryChecker problems )
	  (prognLog
	   "In runOneStochasticSearch"
	   (if (> (numberOfEntries structures ) 0 )
	       (let* ((totalNumberOfStructures (numberOfEntries structures))
		      (entry (let ((startIndex (int (* (randomNumber01 ) totalNumberOfStructures))))
			       (log (% "There are %d structures already" totalNumberOfStructures))
			       (log (% "Randomly picked structure #[%d] as a starting point for stochastic search" startIndex))
			       (getEntry structures startIndex)))
		      (entryStage (getEntryStage entry (slot self _StartStageName))))
		 (writeCoordinatesToMatter entryStage agg )
		 )
	     (else
					; If there are no entries then just use the coordinates in the molecule
					;
	      (log "There are no structures yet using given coordinates for stochastic search") ))
	   (log "Perturbing conformation" ) ;; Perturb the conformation
	   (perturbConformation (slot self _TwisterDriver )) ;; Restart the minimizer and minimize
	   (restart minimizer )
	   (log "Running minimizer")
	   (minimize minimizer )
	   ;;		      (alignAggregate trainer agg ) ;; If there is a minimizer error then retrn False
	   (when (== (status minimizer ) :minimizerError )
	     (log (% "STOCHASTIC-FAIL;%3d: !!! minimizer failed - returning" fails ))
	     (writeln problems (% "STOCHASTIC-FAIL;%3d: !!! minimizer failed" fails ))
	     (return-from runOneStochasticSearch false ))
	   (ifFalse (hasGoodGeometry geometryChecker energyFunction agg problems fails )
		    (log (% "Bad geometry - returning"))
		    (return-from runOneStochasticSearch false ))
					; Check if the conformation has already been found
					; if it has then retrn False
	   (let ((match (ConformationExplorerMatch )))
	     (findClosestMatchingConformation structures match agg (slot self _StartStageName )  0.1 )
	     (log (% "Looked for closest match found -> (getMatches match) = %d" (getMatches match) ))
	     (log (% "Am I keeping everything?  (slot self _KeepEverything) == %d" (slot self _KeepEverything)))
	     (ifTrue (and (getMatches match ) (not (slot self _KeepEverything )))
		     (log (% "This conformation has already been found at index[%d] and we aren't keeping it - returning it" (getClosestMatchIndex match) ))
		     (writeln problems (% "STOCHASTIC-FAIL;%3d: !!! The conformation has already been found at :index %d" fails (getClosestMatchIndex match )))
		     (return-from runOneStochasticSearch false ))
	     )
	   (log "Adding new conformation")
	   (createInitializeAndAddEntry self structures minimizer agg )
	   true ))



  (method run (self )
	  (let* ((structures (getConformationExplorer (slot self _MolTrainer )))
		 (jobHead (getHeader (slot self _MolTrainer )))
		 (geometryChecker (GeometryChecker (getMatter structures )))
		 (agg (getMatter structures ))
		 (minimizer (Minimizer :matter agg :forceField (slot self _ForceField )))
		 (energyFunction (getEnergyFunction minimizer ))
		 (fails 0 )
		 (maxRestartFails (getMaxConformationalSearchFailsRequired jobHead ))
		 (maxMinimizations (lookupDefault (slot self _Options ):maxMinimizations 10000 ))
		 (restartFails 0 )
		 (successes (numberOfEntries structures ))
		 (startTime (PosixTime ))
		 (setToLocalTime startTime )
		 (timeOfLastSave startTime )
		 (maxMinimizations 50)
		 )
	    (print (% "NumberOfTwisters = %d" (numberOfTwisters (slot self _TwisterDriver ))))
	    (chem:setName energyFunction (slot self _FileName ))
	    (disablePrintIntermediateResults minimizer )
	    (prognDebug 
	     (enablePrintIntermediateResults minimizer ))
	    (if (== successes 0 )
		(then 
		 (print "At start of stochastic search there are NO entries" ))
	      (else 
	       (print (% "At start of stochastic search there are already[%d] entries" successes ))
	       ))
					; if there is no bonds to rotate around then we don't
					; need to do many minimizations
	    (ifTrue (== (numberOfTwisters (slot self _TwisterDriver )) 0)
		    (print "There are no twisters, running only 2 minimizations" )
		    (setq maxMinimizations 2 ))
	    (when (slot self _KeepEverything )
	      (print "Running in KeepEverything mode, only 50 structures will be generated" )
	      (setq maxMinimizations 50 ))
	    (prognLog (% "Running up to %d minimizations" maxMinimizations)
		      (for i (Range 0 maxMinimizations )
			   (let ((problems (StringOutStream ))
				 res)
			     (handler-case
			      (setq res (runOneStochasticSearch self
								(slot self _MolTrainer )
								structures energyFunction
								minimizer agg fails
								restartFails successes geometryChecker problems ))
			      (MinimizerError (e) (print "Ignoring minimizer errors")))
			     (if (== res true )
				 (then 
				  (setq successes (+ successes 1 ))
				  (log (% "The conformation was kept so there are now %d successes" successes))
				  (setq restartFails 0 ))
			       (else 
				(print (% "%s" (str problems )))
				(setq fails (+ fails 1 ))
				(setq restartFails (+ restartFails 1 ))
				(log (% "The conformation was NOT kept so there are %d fails and %d restartFails" fails restartFails ))))
			     (updateMostConformationalSearchFailsSeen jobHead restartFails )
			     (let* ((currentTime (setToLocalTime (PosixTime )))
				    (duration (sub currentTime timeOfLastSave ))
				    (secondsLeft (- *SaveDurationInSeconds* (totalSeconds duration ))))
			       (print (% "SEARCH: SaveTimer[%4ld] Fails[%4d] Restarts[%d/%d] Hits[%4d] :job %s"
					 secondsLeft fails restartFails maxRestartFails successes (slot self _FileName )))
			       (ifTrue (< secondsLeft 0 )
				       (print  "+++++++++" )
				       (print  "+++++++++ Saving intermediate results of stochastic search" )
				       (print  "+++++++++ Restarting Timer" )
				       (print  "+++++++++" )
				       (setState jobHead "searching" )
				       (let ((cursec (getSecondsSpentConformationalSearching jobHead )))
					 (setSecondsSpentConformationalSearching jobHead (+ cursec *SaveDurationInSeconds* )))
				       (chem:write (slot self _MolTrainer ))
				       (setq timeOfLastSave (PosixTime ))
				       (setToLocalTime timeOfLastSave ))))
			   (ifTrue (not (conformationalSearchesRequired jobHead ))
				   (return-from () ))))
	    (print (% "End of search there are %d structures." (numberOfEntries structures )))
	    (ifTrue (== (numberOfEntries structures )0 )
		    (setState jobHead "searchFailed" )
		    (chem:write (slot self _MolTrainer ))
		    (return-from run ))
	    (log "Writing result" )
	    (let* ((cursec (getSecondsSpentConformationalSearching jobHead ))
		   (currentTime (setToLocalTime (PosixTime)))
		   (duration (sub currentTime timeOfLastSave)))
	      (setSecondsSpentConformationalSearching jobHead (+ cursec (totalSeconds duration ))))
	    (setState jobHead "searched" )
	    (chem:write (slot self _MolTrainer ))))



  (method summary (self )
	  (let (structures numStructures )
	    (setq structures (getConformationExplorer (slot self _MolTrainer )))
	    (setq numStructures (numberOfEntries structures ))
	    (when (< numStructures 1 )
	      (return-from summary "Stochastic search error, there are no structures" ))
	    (% "Stochastic Search OK - %d structures" numStructures ))))


