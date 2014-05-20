;; fixed code
;;
;; A Search stores everything needed to search/score/accumulate hits of a Scorer
;;

(defClass PackagedSearch () (_Scorer
			     _IgnoreDifferencesInConformation
			     _HitList _CompressedCandoDatabase
			     _ResultsFileName )
  (method __init__ (self &key scorer hitList
			 (ignoreDifferencesInConformation false )
			 candoDatabase searchType resultsFileName )
	  (assert scorer "You must provide a scorer" )
	  (assert searchType "You must specify searchType - either " )
	  (setq (slot self _Scorer )scorer )
	  (when (isNil hitList )
	    (setq hitList (HitList :maxHits 500 :scorer scorer )))
	  (setq (slot self _HitList )hitList )
	  (setq (slot self _CompressedCandoDatabase )candoDatabase )
	  (setq (slot self _IgnoreDifferencesInConformation )ignoreDifferencesInConformation )
	  (setq (slot self _ResultsFileName )resultsFileName ))
  (method getCandoDatabase (self )	  (slot self _CompressedCandoDatabase ))
  (method getScorer (self )	  (slot self _Scorer ))
  (method getHitList (self )	  (slot self _HitList ))
  (method getIgnoreDifferencesInConformation (self )
	  (slot self _IgnoreDifferencesInConformation ))
  (method setResultsFileName (self fn )  (setq (slot self _ResultsFileName )fn ))
  (method getResultsFileName (self )  (slot self _ResultsFileName ))
  (method loadIntermediateResults (self )
	  (when (not (exists (Path (slot self _ResultsFileName ))))
	    (return-from loadIntermediateResults ()))
	  (loadArchive (slot self _ResultsFileName )))

  (method saveIntermediateResults (self results )
	  (saveArchive results (slot self _ResultsFileName ))))