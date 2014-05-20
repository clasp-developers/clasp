debugLogOff

require "CANDO/hits/job.l"


;
; Read the hit list, calculate the score for every conformation
; in the selected stage
; sort the ConformationExplorer by the score
; Write the conformation explorer to a new file for viewing
; write the hit list back to the file
;
defun removeDuplicateConformations ( &key job outputJob ( rmsDistance 0.1 ) )
    ASSERT [job notNil] "You must provide job"
    let outputJobName outputJob
    ifTrue [ outputJob isNil ]
        let outputJobName job
    let jobName job
    [ idx = 0 ]
    let oneJob (readJobOrError jobName)
    let confExplorer [ oneJob getConformationExplorer ]
    debugLogOn
    let newConfExplorer ( ConformationExplorer )
    let moleculeReferencer ( MoleculeReferencer )
    let mol [ confExplorer getMatter ]
    [ newConfExplorer setMatter mol ]
    [ newConfExplorer superposeAllHeavyAtoms ]
    let entryIdx 0
    let uniqueConfs 0
    let alreadyFound 0
    [ match = ( ConformationExplorerMatch ) ]
    foreach entry [ confExplorer entriesAsCons ]
	let stage [ entry getSelectedStage ]
	let stageName [ stage getStageName ]
	println [ "entry;(%d)  stageName(%s)" % entryIdx stageName ]
	[stage writeCoordinatesToMatter mol ]
        [ newConfExplorer findClosestMatchingConformation match mol stageName rmsDistance ]
	if [ match getMatches ]
	  then
	    println [ "      Conformation was already found: index(%d)" % [ match getClosestMatchIndex ] ]
	    [ alreadyFound = [ alreadyFound + 1 ] ]
	    [ confsSinceLastAdd = [ confsSinceLastAdd + 1 ] ]
	  else
	    println [ "--------     !!!!!!Adding entry %d to ConformationExplorer. Closest match had rms: %lf " % entryIdx [ match getClosestMatchRms ] ]
    	    [ newConfExplorer appendEntry entry ]
	    let uniqueConfs [ uniqueConfs + 1 ]
	    let confsSinceLastAdd 0
	let entryIdx [ entryIdx + 1 ]
    println [ "Unique conformations(%d) Duplicate conformations(%d)" % uniqueConfs alreadyFound ]
    ; write results back to hit file
    [ oneJob setFileNamePrefix outputJobName ]
    [ oneJob setConformationExplorer newConfExplorer ]
    write oneJob



defun removeDuplicateConformations_usage ( )
    println "Usage:  --job [ --outputJob {jobName} ]  [ --rmsDistance 0.1 ]"
