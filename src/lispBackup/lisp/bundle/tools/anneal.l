
(defClass Anneal () (_Scorer
		     _Callback
		     _TookInitialStep
		     _ShowProgress
		     _InitialTemperatureBuilds
		     _InitialTemperatureRejects
		     _NumberOfTemperatureEstimationSteps
		     _ScaleInitialTemperatureEstimate
		     _HitList _Accepts
		     _NumberOfTemperatureSteps
		     _ScoreLogger
		     _NumberOfConformationsBuilt
		     _TemperatureStepScale
		     _EstimateInitialTemperature
		     _NumberOfStepsPerTemperature )
  (method __init__ (self scorer &key (showProgress true) hitList callback )
	  (setq (slot self _Scorer) scorer)
	  (setq (slot self _Callback) callback)
	  (setq (slot self _HitList) hitList)
	  (setq (slot self _TookInitialStep) false)
	  (setq (slot self _ShowProgress) showProgress)
	  (setq (slot self _InitialTemperatureBuilds) 100)
	  (setq (slot self _InitialTemperatureRejects) 10)
	  (setq (slot self _EstimateInitialTemperature) true)
	  (setq (slot self _NumberOfTemperatureSteps) 100)
	  (setq (slot self _ScaleInitialTemperatureEstimate) 10.0)
	  (setq (slot self _NumberOfStepsPerTemperature) 100)
	  (setq (slot self _TemperatureStepScale) 0.9)
	  (setq (slot self _NumberOfTemperatureEstimationSteps) 100))

  (method estimateInitialTemperature (self &optional stats)
	  (prognLog "estimateInitialTemperature"
		   (let ((bestScorerState (ScorerState))
			 (scorer (slot self _Scorer))
			 (previousScore 9.0e99)	(largestDelta 0.0) (absDelta 0.0) (delta 0.0) (curScore 0.0))
		     (setq previousScore (moveAndScore (slot self _Scorer) bestScorerState))
		     (foreach i (Range 0 (slot self _NumberOfTemperatureEstimationSteps))
			      (setq (slot self _InitialTemperatureBuilds) (+ (slot self _InitialTemperatureBuilds) 1 ))
			      (moveAndScore scorer bestScorerState)
			      (when stats 
				(incrementBuilds stats)
				(incrementHits stats))
			      (setq curScore (getScore bestScorerState))
			      (assert curScore "curScore cannot be nil")
			      (assert previousScore "previousScore cannot be nil")
			      (setq delta (- curScore previousScore))
			      (setq previousScore curScore)
			      (setq absDelta (abs delta))
			      (setq largestDelta (max absDelta largestDelta))
			      (when (slot self _ShowProgress)
				(when (eqn (mod i 10) 0)
				  (print (% "%5s %8s %8s %8s" "Step" "Score" "Delta" "MaxDelta" )))
				(print (% "%5d %8.3lf %8.3lf %8.3lf" i curScore absDelta largestDelta))))
		     (when (slot self _ShowProgress)
		       (let ((initTemp (* largestDelta (slot self _ScaleInitialTemperatureEstimate))))
			 (print "Initial temperature estimation report")
			 (print (% "Initial temperature = %lf" initTemp ))
			 (print (% "There were %d builds and %d rejects"
				   (slot self _InitialTemperatureBuilds)
				   (slot self _InitialTemperatureRejects)))))
		     (* largestDelta (slot self _ScaleInitialTemperatureEstimate)))))


  (method checkAccept (self scorerState temperature currentScore)
	  (let ((acceptStep false)
		(delta (getScore scorerState) currentScore)
		)
	    (when (slot self _ShowProgress) (print (% "delta = %lf" delta )))
	    (if ( < delta 0.0) 
		true
	      (let (exponent a rand)
		(setq exponent (/ (- delta) temperature))
		(setq a (exp exponent))
		(setq rand (randomNumber01))
		(setq acceptStep (< rand a))
		(when (slot self _ShowProgress) (print (% "rand(%lf) < a(%lf) = %d" rand a acceptStep)))
		acceptStep))))



  (method run (self )
	  (prognLog
	   "---- running search"
	   (let ( stepCount (scorer (slot self _Scorer))
			    (hitList (slot self _HitList))
			    (scorerState (ScorerState))
			    (currentScore 9.9e99)
			    (currentScorerState (ScorerState))
			    temperature (stats (getSearchStatistics (slot self _HitList)))
			    (maxInTempAccepts (min 10 (int (* 0.1 (slot self _NumberOfStepsPerTemperature)))))
			    inTempAccepts hit prevScore acceptStep (stepCount 0)
			    )
					;	RPAlchemistState currentState = O_AlchemistState::create(this->lisp());
					;	RPAlchemistState alchemistState = O_AlchemistState::create(this->lisp());
	     (setq temperature (if (slot self _EstimateInitialTemperature)
				   (estimateInitialTemperature self stats)
				 (slot self _InitialTemperature)))
	     (assert (> temperature 0.0) "The temperature was zero - is there something wrong with the mover/scorer?")
	     (setq (slot self _NumberOfConformationsBuilt) 0)
	     (when (slot self _ShowProgress)
	       (print (% "Initial temperature = %d" temperature)))
	     (setq currentScore (moveAndScore scorer scorerState))
	     (setq (slot self _Accepts) 0)
	     (block outerLoop
	       (foreach tempStep (Range 0 (slot self _NumberOfTemperatureSteps))
			(prognLog
			 (% "Temperature step loop of sequence %d" tempStep)
			 (when (slot self _ScoreLogger)
			   (newTemperature (slot self _ScoreLogger) temperature))
			 (setq inTempAccepts 0)
			 (block innerLoop
			   (foreach inTempStep (Range 0 (slot self _NumberOfStepsPerTemperature))
				    (prognLog
				     (% "Top temperature(%lf) step(%d)" temperature inTempStep)
				     (when (slot self _ShowProgress)
				       (print "------------------ step start -----------------")
				       (print (% "Current state=%s currentScore=%lf"
						 (fingerprintAsString scorer) currentScore )))
				     (moveAndScore scorer scorerState)
				     (setq (slot self _NumberOfConformationsBuilt) (+ (slot self _NumberOfConformationsBuilt) 1))
				     (when (slot self _ShowProgress)
				       (print (% "    Perturbed state %s    score = %lf"
						 (fingerprintAsString scorerState) (getScore scorerState))))
				     (when (isAHit hitList scorerState)
				       (log (% "Adding hit with scorerState = %s" (summary scorerState)))
				       (print "++++++++ Created a hit")
				       (log "+++++ Created a hit" )
				       (setq hit (createHitWithStates hitList scorerState))
				       
				       )
				     (setq acceptStep (checkAccept self scorerState temperature currentScore))
				     (setq prevScore currentScore)
				     (when (slot self _ScoreLogger)
				       (appendScore (slot self _ScoreLogger) (getScore scorerState) acceptStep))
				     (if acceptStep
					 (progn
					   (setq (slot self _Accepts) (+ (slot self _Accepts) 1))
					   (setq inTempAccepts (+ inTempAccepts 1))
					   (setq currentScore (getScore scorerState))
					   (when (slot self _Callback)
					     (apply (slot self _Callback) scorer scorerState)))
				       (restoreAllSavedStates scorer))
				     (when (> inTempAccepts maxInTempAccepts)
				       (print "Encountered too many temperature accepts, advancing temperature --- THIS BETTER GO TO THE TOP OF THE LOOP")
				       (return-from innerLoop)
				       (raise (LispError "Should never get here")))
				     (when (slot self _ShowProgress)
				       (let ((acceptStr (if acceptStep "Yes" "No"))
					     (stepsLeft (- (slot self _NumberOfStepsPerTemperature) inTempStep)))
					 (print (% "  Step[%5d] Temp[%5.3lf] Prev.Score[%10.3lf] Score[%10.3lf] Accept[%3s] StepsLeft[%3d]"
						   stepCount temperature prevScore (getScore scorerState) acceptStr stepsLeft ))))
				     (setq stepCount (+ stepCount 1))))))
			(when (== inTempAccepts 0) 
			  (print "inTempAccepts are zero!!!!!  BREAKING")
			  (return-from outerLoop))
			(setq temperature (* temperature (slot self _TemperatureStepScale)))))
	     (print (% "Carried out %d simulated annealing steps" stepCount))
	     (print (% "Built %d TOTAL structures" (slot self _NumberOfConformationsBuilt))))))
  )


(defun anneal (packagedSearch &key callback )
  (let* ((svm (slot packagedSearch _Scorer))
	 (hitList (slot packagedSearch _HitList))
	 (annealer (Anneal svm :hitList hitList :callback callback )))
    (run annealer)
    hitList))

(print "Loaded anneal")
