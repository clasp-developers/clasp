; fixed code
(ifTrue (!=   (length   l:*ARGS*  ) 4   ) ;END1
(println  "You must provide 3 arguments: typeRules parmFile output"    )

(exit )
)

(let ((typeFileName  (@   l:*ARGS*   1   ) )(parameterFileName  (@   l:*ARGS*   2   ) )(outputFileName  (@   l:*ARGS*   3   ) )reader  forceField   ) ;END1
(println (%   "Reading antechamber type rules from: %s"   typeFileName   ) )

(=   reader   (ReadAmberParameters   ) )

(println (%   "Reading parameters from: %s"   parameterFileName   ) )

(readTypes   reader   typeFileName   )

(readParameters   reader   parameterFileName   )

(=   forceField   (getForceField   reader  ) )

(println (%   "Writing output to: %s"   outputFileName   ) )

(saveArchive forceField  outputFileName   )
)
