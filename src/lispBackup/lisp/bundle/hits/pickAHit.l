;
; This provides the function pickAHit for picking one hit out
; of a HitList and building every conformation for it
;


require "CANDO/hits/job.l"



;
;require "CANDO/scorers.l"
;set allScore (createSuperposeScorer superposeCarbons: true)

defun pickAHit_usage ( )
    println "--hitList (name) ---hitIndices (idx)"
    println "--maxConfs: 9999999"
    println "--maxDuplicatesBeforeQuit: 9999999"
    println "--rmsDistance: 0.1"



defun pickAHit ( &key hitList hitIndices  scorer ( maxConfs 9999999 ) ( maxDuplicatesBeforeQuit 9999999 )
			( rmsDistance 0.1 ) )
    ASSERT [ hitList notNil ] "You must provide a hitList"
    ASSERT [ hitIndices notNil ] "You must provide hitIndices"
    ASSERT [ scorer notNil ] "You must provide a scorer"
    println [ "maxConfs = %d" % maxConfs ]
    println [ "maxDuplicatesBeforeQuit = %d" % maxDuplicatesBeforeQuit ]
    println [ "rmsDistance = %lf" % rmsDistance ]
    let idx 0
    let scorerState [ allScore createState ]
    let hits ( load hitList)
    let hitsStem [ ( Path path: hitList) stem ]
    let stageBuild "start"
    println [ "Loading hits from file(%s) stem(%s)" % hitList hitsStem ]
    foreach hitIdx hitIndices
	let hit [hits getHit hitIdx]
	let oneJob (new OneJob )
	[ oneJob setFileNamePrefix [ "%s%03d" % hitsStem hitIdx ] ]
	setBuilderDatabase oneJob *DATABASE*
	println [ "---------   Working on hit(%d)" % hitIdx ]
	let confExplorer ( ConformationExplorer )
	setConformationExplorer oneJob confExplorer
	let builder [hit getBuilder]
	[ allScore builderChanged builder ]
	let selectAll (BuildAllSelectAll)
	let mol [ builder getMolecule ]
	[confExplorer setMatter mol ]
	[confExplorer superposeAllHeavyAtoms ]
;	[ min = ( Minimizer matter: mol forceField: gaff ) ]
;	[ min enablePrintIntermediateResults ]
	firstConformation builder selectAll
	[ newConfs = 0 ]
	[ alreadyFound = 0 ]
	[ totalConfs = [ builder numberOfConformations selectAll ] ]
	[ confIdx = 0 ]
	[ match = ( ConformationExplorerMatch ) ]
	foreach conf (range 0 [ builder numberOfConformations selectAll ])
	    println [ "======= Looking at conformation %d out of %d" % confIdx totalConfs ]
	    [ builder buildAllAtomsConformation ]
	    [ allScore evaluate builder scorerState ]
	    [ builder writeCoordinatesToAtoms ]
	    [ mol calculateVirtualAtomPositions ]
; Put the conformation into the ConformationExplorer
	    [ confExplorer findClosestMatchingConformation match mol stageBuild rmsDistance ]
	    if [ match getMatches ]
	      then
		println [ "   Conformation was already found: index(%d)" % [ match getClosestMatchIndex ] ]
		[ alreadyFound = [ alreadyFound + 1 ] ]
		[ confsSinceLastAdd = [ confsSinceLastAdd + 1 ] ]
	      else
		println [ "   Adding entry %d to ConformationExplorer it has the score(%lf)" % newConfs [ scorerState getScore ] ]
		addEntry confExplorer stageBuild 0.0 mol
		[ newConfs = [ newConfs + 1 ] ]
		[ confsSinceLastAdd = 0 ]
; advance to next conformation
	    incrementConformation builder selectAll
	    [ confIdx = [ confIdx + 1 ] ]
	    println [ "   Running totals new conformations = %d   duplicates = %d" % newConfs alreadyFound ]
	    ifTrue [ confsSinceLastAdd > maxDuplicatesBeforeQuit ]
		break
	    ifTrue [ confIdx > maxConfs ]
		break
	[ oneJob write ]




defun addEntry (confExplorer stageName energy matter )
    [ entry = [ confExplorer createEntry ] ]
    [ entryStage = [ entry createEntryStage ] ]
    [ entryStage setStageName stageName ]
    [ entryStage setExternalInterfaceName "cando" ]
    [ entryStage setModel "gaff" ]
    [ entryStage setEnergyKCal energy ]
    [ entry setSelectedStage entryStage ]
    [ entryStage extractCoordinatesFromMatter matter ]
    [ entryStage setComplete true ]
    [ entry appendEntryStage entryStage ]
    [ confExplorer appendEntry entry ]
