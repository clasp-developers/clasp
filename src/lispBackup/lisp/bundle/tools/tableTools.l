					; fixed code
					;
					;
					; Tools for manipulating tables
					;
					;
(defun optimizeGeometryOfAllEntries (table minimizeField &key (writeEnergyToField ()))"Minimize the energy of the (minimizeField) of every entry" 
  (assert (hasField table minimizeField )(% "The table is missing the field(%s)" (fullName minimizeField )))
  (assert (isSubClassOf (fieldClass table minimizeField )Matter )(% "The table field(%s) is not a Matter - its of class(%s)" (fullName minimizeField )(className (fieldClass table minimizeField ))))
  (local gaff (loadArchive (% "%s/gaff.cxml" (databaseDir ))))
  (when (notNil writeEnergyToField )
    (assert (not (hasField table writeEnergyToField ))(% "The table already has the field(%s)" (fullName writeEnergyToField )))
    (appendField table writeEnergyToField Real ))
  (local idx 0 )
  (foreach entry (asCons table )
	   (println (% "Optimizing entry %d" idx ))
	   (local mol (read entry minimizeField ))
	   (local minimizer (Minimizer :matter mol :forceField gaff ))
	   (minimize minimizer )
	   (local energyFunction (getEnergyFunction minimizer ))
	   (local energy (getTotalEnergy energyFunction ))
	   (when (notNil writeEnergyToField )
	     (write entry writeEnergyToField energy ))
	   (setq idx (+ idx 1 ))))