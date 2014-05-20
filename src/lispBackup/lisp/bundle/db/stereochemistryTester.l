					; fixed code


(defun configurationAsString (conf )
  (cond 
   ((== conf Configuration.S_Configuration ) return-from configurationAsString "S")
   ((== conf Configuration.R_Configuration ) return-from configurationAsString "R")
   (true "undefined" )))



(defun countBadStereocenters (aggregate problems )
  (let (badCenters setConf actualConf )
    (setq badCenters 0 )
    (foreach atom (asCons (allAtoms aggregate ))
	     (block continue
	       (cond 
		((== (getStereochemistryType atom ) :chiralCenter )
		 (setq setConf (getConfiguration atom ))
		 (setq actualConf (calculateStereochemicalConfiguration atom ))
		 (ifTrue (!= setConf actualConf )
			 (setq badCenters (+ badCenters 1 ))
			 (writeln problems (% "Chiral %s has (%s) configuration but it should be %s"
					      (description atom )
					      (configurationAsString actualConf )
					      (configurationAsString setConf )))))
		((== (getStereochemistryType atom ) :prochiralCenter )
		 (setq setConf Configuration.R_Configuration) ; all prochirals set to R
		 (setq actualConf (calculateStereochemicalConfiguration atom ))
		 (ifTrue (!= setConf actualConf )
			 (setq badCenters (+ badCenters 1 ))
			 (writeln problems (% "Prochiral %s has (%s) configuration but it should be %s"
					      (description atom )
					      (configurationAsString actualConf )
					      (configurationAsString setConf )))))
		(true 
		 (return-from continue )))))
    badCenters ))