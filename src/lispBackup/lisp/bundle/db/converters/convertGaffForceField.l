					; fixed code
					;
					; Convert Antechamber type rules and
					;  AMBER formatted parameter files
					; into CANDO formated parameter file
(when (!= (length *ARGS* ) 4 )
  (bformat t "You must provide 3 arguments: typeRules parmFile output\n" )
  (exit))
(let ((typeFileName (nth 1 *ARGS* ))
      (parameterFileName (nth 2 *ARGS* ))
      (outputFileName (nth 3 *ARGS* ))
      (reader (chem:ReadAmberParameters)))
  (bformat t "Read antechamber type rules from: %s\n" typeFileName )
  (bformat t "Reading parameters from: %s\n" parameterFileName )
  (chem:readTypes reader typeFileName )
  (chem:readParameters reader parameterFileName )
  (let ((forceField (chem:getForceField reader )))
    (bformat t "Writing output to: %s\n" outputFileName )
    (saveArchive forceField outputFileName ))
  )