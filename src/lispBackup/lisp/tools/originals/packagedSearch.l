
;
; A Search stores everything needed to search/score/accumulate hits of a BuilderScorer
;
;
; Search type is one of: 
;      m:SimulatedAnnealingSearchType.sequenceWithBestConformation
;      m:SimulatedAnnealingSearchType.bestConformationOfOneSequence

defClass PackagedSearch () (_BuilderScorer _IgnoreDifferencesInConformation _HitList _CompressedBuilderDatabase _SearchType _ResultsFileName )
  method __init__ (self &key builderScorer hitList (ignoreDifferencesInConformation false) builderDatabase searchType resultsFileName )
    assert [ builderScorer notNil ] "You must provide a builderScorer"
    assert [ searchType notNil ] "You must specify searchType - either "
    setq [ self slot _BuilderScorer ] builderScorer
    when [ hitList isNil ]
      setq hitList (HitList :maxHits 500 :builderScorer builderScorer )
    setq [ self slot _HitList ] hitList
    setq [ self slot _CompressedBuilderDatabase ] builderDatabase
    setq [ self slot _IgnoreDifferencesInConformation ] ignoreDifferencesInConformation
    setq [ self slot _SearchType ] searchType
    setq [ self slot _ResultsFileName ] resultsFileName

  method getBuilderDatabase (self)
    return [ self slot _CompressedBuilderDatabase ]

  method getBuilderScorer (self)
    return [ self slot _BuilderScorer ]

  method getSearchType (self)
    return [ self slot _SearchType ]

  method getHitList ( self )
    return [ self slot _HitList ]

  method getIgnoreDifferencesInConformation ( self )
    return [ self slot _IgnoreDifferencesInConformation ]

  method setResultsFileName ( self fn )
    setq [ self slot _ResultsFileName ] fn

  method getResultsFileName ( self )
    return [ self slot _ResultsFileName ]

  method loadIntermediateResults ( self )
    when (not [ (Path :path [self slot _ResultsFileName]) exists ] )
        return ()
    local a (loadArchive [self slot _ResultsFileName] )
    return a

  method saveIntermediateResults ( self results )
    saveArchive results [ self slot _ResultsFileName ]
