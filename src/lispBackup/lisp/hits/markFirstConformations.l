

require "CANDO/hits/job.l"

defun markFirstConformations_usage ( )
    println "Arguments: --job --markName --last"

;
; Read the hit list, calculate the score for every conformation
; in the selected stage
; sort the ConformationExplorer by the score
; Write the conformation explorer to a new file for viewing
; write the hit list back to the file
;
defun markFirstConformations ( &key job markName last ( accumulate true ) )
    ASSERT [job notNil] "You must provide job"
    ASSERT [markName notNil] "You must provide markName"
    ASSERT [last notNil] "You must provide last"
    let jobName job
    let oneJob ( readJobOrError job )
    let confExplorer [ oneJob getConformationExplorer ]
    let entryIdx 0
    foreach entry [ confExplorer entriesAsCons ]
	let stage [ entry getSelectedStage ]
	let stageName [ stage getStageName ]
	if [ entryIdx <= last ]
	  then
	    if accumulate
	      then
	        let val [ [ stage getData ] getOrDefault markName 0 ]
	      else
		let val 0
	    let val [ val + 1 ]
	    [ [ stage getData ] put markName val ]
	  else
	    let val [ [ stage getData ] getOrDefault markName 0 ]
	    [ [ stage getData ] put markName val ]
	println [ "entry;(%d)  stageName(%s)  markName(%s) = %d" % entryIdx stageName markName val ]
	[ entryIdx = [ entryIdx + 1 ] ]
    ; write results back to hit file
    write oneJob



