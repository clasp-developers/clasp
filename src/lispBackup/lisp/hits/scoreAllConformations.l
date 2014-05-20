

require "CANDO/hits/job.l"


;
; Read the hit list, calculate the score for every conformation
; in the selected stage
; sort the ConformationExplorer by the score
; Write the conformation explorer to a new file for viewing
; write the hit list back to the file
;
defun scoreAllConformations ( &key job scorer scoreResultName )
    ASSERT [job notNil] "You must provide jobName"
    ASSERT [scorer notNil] "You must provide scorer"
    ASSERT [scoreResultName notNil] "You must provide scoreResultName"
    let jobName job
    [ idx = 0 ]
    [ scorerState = [ scorer createState ] ]
    let oneJob ( readJobOrError jobName )
    let confExplorer [ oneJob getConformationExplorer ]
    [ [confExplorer getData] put "backgroundRender" [scorer getBackgroundRender] ]
    let moleculeReferencer ( MoleculeReferencer )
    let mol [ confExplorer getMatter ]
    ( setAtomAliasesForBackboneHBondAcceptors mol )
    [ moleculeReferencer setMolecule mol scorer ]
    [ entryIdx = 0 ]
    foreach entry [ confExplorer entriesAsCons ]
	let stage [ entry getSelectedStage ]
	let stageName [ stage getStageName ]
	println [ "entry;(%d)  stageName(%s)" % entryIdx stageName ]
	[stage writeCoordinatesToMatter mol ]
;	( save mol [ "_mol%03d.cxml" % entryIdx ] )
	[ mol calculateVirtualAtomPositions ]
	debugLogOn
	[ scorer evaluate moleculeReferencer scorerState ]
	println [ "scorerState %s" % [ scorerState summary ] ]
	ifTrue [ scorerState isTransformSet ]
	    [ mol applyTransformToAtoms [scorerState getTransform] ]
	    [ stage extractCoordinatesFromMatter mol ]
	[ stageData = [ stage getData ] ]
	[ stageData put scoreResultName ( getScore scorerState ) ]
	[ stageData put [ "%sInfo" % scoreResultName ] (GrInformation text: ["%s score=%lf" % scoreResultName ( getScore scorerState ) ] ) ]
	println [ "Entry(%d) Score = %lf" % entryIdx ( getScore scorerState ) ]
	[ entryIdx = [ entryIdx + 1 ] ]
    println [ "sorting on stageName(%s) scoreResultName(%s)" % stageName scoreResultName ]
    [ confExplorer sort stageName scoreResultName ]
    let hitFilePrefix [ oneJob getFileNamePrefix ]
    let saveFileName [ "%s_sortedBy%s.cxml" % hitFilePrefix scoreResultName ]
    save confExplorer saveFileName
;    foreach entry [ confExplorer entriesAsCons ]
;	let stage [ entry getEntryStage stageName ]
;	[ stageData = [ stage getData ] ]
;	println [ "%s = %lf" % scoreResultName [stageData get scoreResultName ] ]
; write results back to hit file
    write oneJob



