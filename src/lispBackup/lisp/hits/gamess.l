
require "CANDO/hits/job.l"

;
; Read the hit list, minimize the energy of every conformation
; in the selected stage
; sort the ConformationExplorer by the score
; Write the conformation explorer to a new file for viewing
; write the hit list back to the file
;

set gaff ( load [ "%s/gaff.oml" % (databaseDir) ] )
set gaffTypes [ gaff getTypes ]


defun generateGamess_usage ( )
    println "--job {jobName} "

defun generateGamess ( &key job markName )
    ASSERT [ job notNil ] "You must provide job"
    ASSERT [ markName notNil ] "You must provide markName"
    [ idx = 0 ]
    let oneJob ( readJobOrError job )
    let confExplorer [ oneJob getConformationExplorer ]
    let gamessInterface ( GamessInterface conformationExplorer: confExplorer )
    let mol [ confExplorer getMatter ]
    [ entryIdx = 0 ]
    foreach entry [ confExplorer entriesAsCons ]
	let stage [ entry getSelectedStage ]
	let stageName [ stage getStageName ]
	if [ [stage getData] asdfasdfasdfasdfasdfasdf
        if [ entry hasEntryStageWithName stageMin ]
	  then
	    println [ "Entry;(%d) has already been minimized, skipping" % entryIdx ]
	  else
	    ifTrue [ entryIdx >= maxEntries ]
		break
	    let stage [ entry getEntryStage stageBuild ]
	    println [ "minimizing entry;(%d)  stageName(%s)" % entryIdx stageBuild ]
	    [stage writeCoordinatesToMatter mol ]
;	( save mol [ "_mol%03d.cxml" % entryIdx ] )
;	debugLogOn
	    [ min resetAndMinimize ]
	    if [ entry hasEntryStageWithName stageMin ]
	      then
		[ newStage = [ entry getEntryStage stageMin ] ]
	      else
		[ newStage = [ entry createEntryStage ] ]
		[ entry appendEntryStage newStage ]
		[ newStage setStageName stageMin ]
		[ entry setSelectedStage newStage ]
	    [ newStage extractCoordinatesFromMatter mol ]
	    ifTrue [ [ entryIdx mod 50 ] == 0 ]
		print "Saving checkpoint......"
		[ oneJob write ]
		println "done"
	; write results back to hit file
        [ entryIdx = [ entryIdx + 1 ] ]
    println "Saving fully minimized job"
    [ oneJob write ]


