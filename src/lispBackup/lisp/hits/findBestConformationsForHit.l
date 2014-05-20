;
; WORKING WORKING WORKING

;
; This provides the function findBestConformationsForHits for picking one hit out
; of a HitList and building every conformation for it
;


require "CANDO/hits/job.l"



;
;require "scorers.l"
;set allScore (createSuperposeScorer superposeCarbons: true)

defun findBestConformationsForHits_usage ( )
    println "--hitList (name) --hitIndex (idx)"



defun findBestConformationsForHits ( &key hitList hitIndex  scorer 
			( maxConfs 1000 ) ( maxDuplicatesBeforeQuit 9999999 )
			( stageName "start" ) ( rmsDistance 0.1 ) )
    ifTrue [ hitList isNil ]
        let hitList ( getline "File name containing hits: ")
    ifTrue [ hitIndex isNil ]
        let hitIndex [ ( getline "Hit index: " ) asInt ]
    ASSERT [ scorer notNil ] "You must provide a scorer"
    println [ "maxConfs = %d" % maxConfs ]
    println [ "maxDuplicatesBeforeQuit = %d" % maxDuplicatesBeforeQuit ]
    println [ "rmsDistance = %lf" % rmsDistance ]
    let hitsToConsider (list hitIndex )
    let idx 0
    let scorerState [ allScore createState ]
    let hitsStem [ ( Path path: hitList) stem ]
    println [ "Loading hits from file(%s) stem(%s)" % hitList hitsStem ]
    let hits ( load hitList)
;
; Now loop through the hits we want to extract
; 
    foreach hitIdx hitsToConsider
	let hit [hits getHit hitIdx]
	let builderState [hit getBuilderState]
	let builder [hit getBuilder]
	[ builderState applyToBuilder builder ]
	let oneJobHitList (HitList maxHits: maxConfs)
	let options ()
	exhaustiveSearchConformationsOfCurrentSequence builder allScore oneJobHitList options: options
;
; Load the job if there is one or create a new one
;
	let jobName (createJobName hitsStem hitIdx)
	let oneJob (readJobOrNil jobName)
	if [ oneJob isNil ]
	  then
	    let oneJob (new OneJob)
	    [oneJob setBuilderDatabase *DATABASE*]
	    let confExplorer (ConformationExplorer )
	    [oneJob setFileNamePrefix jobName ]
	    [oneJob setConformationExplorer confExplorer ]
	    let mol [ builder getMolecule ]
	    [confExplorer setMatter mol]
	    [confExplorer superposeAllHeavyAtoms ]
	  else
	    let confExplorer [ oneJob getConformationExplorer ]
;
; Now the oneJob has been loaded or created and the ConformationExplorer
; that we want to fill has been set in (confExplorer)
;
; Now integrate the hits from that search into the jobs ConformationExplorer
;	
	let confExplorerArrayedMol (ArrayedMolecule)
	let confExplorerMol [confExplorer getMatter ]
	[confExplorerArrayedMol setMolecule confExplorerMol ]
	let coordinates [ confExplorerArrayedMol createCoordinateArray ]
;
; Now the hitBuilder can build coordinates for its molecule 
; and the coordinates can be put in (coordinates) and then
; put into the ConformationExplorer (confExplorer)'s molecule
; using the confExplorerArrayedMol
;
	let match ( ConformationExplorerMatch )
	println "Starting to add hits to ConformationExplorer"
	let hitConfIdx 0
	let newConfs 0
	let alreadyFound 0
	foreach oneHit (entries oneJobHitList)
	    let hitMol [oneHit getBuiltMolecule]
	    let scorerState [oneHit getScorerState ]
	    let hitArrayedMol (ArrayedMolecule)
	    [ hitArrayedMol setMolecule hitMol ]
;
; Here we do the transfer of the coordinates from the hit molecule to the
; ConformationExplorer molecule
;
	    [ hitArrayedMol writeToCoordinateArray coordinates ]
	    [ confExplorerArrayedMol extractFromCoordinateArray coordinates ]
;
; Now evaluate if we need to create a new entry for this conformation
;
	    [ confExplorer findClosestMatchingConformation match confExplorerMol stageName rmsDistance ]
	    if [ match getMatches ]
	      then
		println [ "      Conformation was already found: index(%d)" % [ match getClosestMatchIndex ] ]
		let alreadyFound [ alreadyFound + 1 ]
		let entryStage [confExplorer getClosestMatchingEntryStage match ]
		[ [ entryStage getData ] put [allScore getName] [scorerState getScore ] ]
	      else
		println [ "--------     !!!!!!Adding entry %d to ConformationExplorer it has the score(%lf)" % hitConfIdx[ scorerState getScore ] ]
		addEntry confExplorer stageName [allScore getName] [scorerState getScore] confExplorerMol
		[ newConfs = [ newConfs + 1 ] ]
	    ; nothing
	    let hitConfIdx [ hitConfIdx + 1 ]
	println "Writing out ConformationExplorer"
	[ oneJob write ]




defun addEntry (confExplorer stageName scorerName score matter )
    [ entry = [ confExplorer createEntry ] ]
    [ entryStage = [ entry createEntryStage ] ]
    [ entryStage setStageName stageName ]
    [ entryStage setExternalInterfaceName "cando" ]
    [ entryStage setModel "gaff" ]
    [ entryStage setEnergyKCal 0.0 ]
    [ [ entryStage getData ] put scorerName score ]
    [ entry setSelectedStage entryStage ]
    [ entryStage extractCoordinatesFromMatter matter ]
    [ entryStage setComplete true ]
    [ entry appendEntryStage entryStage ]
    [ confExplorer appendEntry entry ]
