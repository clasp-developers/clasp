; fixed code
(defun allMetaConstitutionNamesForPlugMates (plug )
  (let ((mates (getMates plug )))
    mates ))


(defun mateMetaConstitutionHasTopologyWithPlugName (metaDatabase metaConstitutionName mateMetaConstitutionName matePlugName )
  (let ((mateConstitution (getMetaConstitution metaDatabase mateMetaConstitutionName )))
    (hasTopologyWithPlugNamed mateConstitution matePlugName )))

(defun aggregateMateNames (allMates )
  (let ((names (SymbolSet ))
	monomerNames )
    (foreach mate allMates 
	     (setq monomerNames (getUnExpandedNames mate ))
	     (log (% "aggregateMateNames mate monomer names: %s" (repr monomerNames )))
	     (insertSymbolSet names monomerNames ))
    names))

(defun mateMetaConstitutionHasPlugWithName (metaDatabase mateMetaConstitutionName matePlugName )
  (raise (LispError "implement me" ))
  (setq constitution (monomerConstitutionForName *DATABASE* mateMetaConstitutionName ))
  (hasPlugNamed constitution matePlugName ))

(defun mateMetaConstitutionHasPlugWithMatesThatContainFocus (metaDatabase focusMetaConstitutionName mateMetaConstitutionName matePlugName )
  (let (constitution matePlug allMateNames )
    (setq constitution (getMetaConstitution metaDatabase mateMetaConstitutionName ))
    (setq matePlug (getMetaPlugNamed constitution matePlugName ))
    (setq allMateNames (aggregateMateNames (getMates matePlug )))
    (contains allMateNames focusMetaConstitutionName )))

(defun checkMates (metaDatabase focusMetaConstitution focusTopology errorStream idx )
  (let (metaConstitutionName plugs problem allMateNames otherSidePlugName problem )
    (setq metaConstitutionName (getGroupName focusMetaConstitution ))
    (log (% "Checking mates for constitution(%s)  trainer(%d)" metaConstitutionName idx ))
    (setq plugs (getMetaPlugs focusTopology ))
    (setq problem false )
    (foreach plug plugs 
	     (setq allMateNames (aggregateMateNames (getMates plug )))
	     (log (% "Mates for plug(%s) = |%s|" (getName plug )(repr allMateNames )))
	     (foreach mateMetaConstitutionName (asCons allMateNames )
		      (block inner
			(setq otherSidePlugName (otherSidePlugName plug ))
			(log (% "    otherSidePlugName(%s)" otherSidePlugName ))
			(log (% "    mateMetaConstitutionName = %s" (repr mateMetaConstitutionName )))
			(ifFalse (mateMetaConstitutionHasTopologyWithPlugName metaDatabase metaConstitutionName mateMetaConstitutionName otherSidePlugName )
				 (writeln errorStream (% "MetaConstitution(%s) has plug(%s) but its mate constititution(%s) doesn't have a single Topology with plug(%s)"
							 metaConstitutionName (getName plug ) mateMetaConstitutionName (otherSidePlugName plug )))
				 (writeln errorStream (% "    solutions 1: Add a topology to %s with plug(%s) with Mate(%s)"
							 mateMetaConstitutionName (otherSidePlugName plug ) metaConstitutionName ))
				 (writeln errorStream (% "              2: From %s plug(%s) remove Mate %s" metaConstitutionName (getName plug )mateMetaConstitutionName ))
				 (setq problem true )
				 (return-from inner))
			(ifFalse (mateMetaConstitutionHasPlugWithMatesThatContainFocus metaDatabase metaConstitutionName mateMetaConstitutionName (otherSidePlugName plug ))
				 (writeln errorStream (% "Group(%s) has plug(%s) but its mate constititution(%s) plug(%s) doesn't have a mate that matches us"
							 metaConstitutionName (getName plug )mateMetaConstitutionName (otherSidePlugName plug )))
				 (writeln errorStream (% "    solutions 1: To %s plug(%s) add the Mate %s"
							 mateMetaConstitutionName (otherSidePlugName plug )metaConstitutionName ))
				 (writeln errorStream (% "              2: Remove all topologies with plug(%s) from %s" (getName plug )metaConstitutionName ))
				 (setq problem true ) ))))
    (ifTrue problem 
	    (log "   ---- Found a problem see below for list of problems" ))))



(defun checkAllMates (metaDatabase )
  (let (errorStream idx m )
    (setq errorStream (StringOutStream ))
    (setq idx 0 )
    (foreach entry (keysAsCons (getMetaConstitutions metaDatabase ))
	     (setq m (lookup (getMetaConstitutions metaDatabase) entry ))
	     (prognLog (% "Iterating over MetaConstitution[%s]" (getGroupName m ))
		       (foreach tt (getMetaTopologies m )
				(prognLog (% "Iterating over MetaTopology[%s]" (getName tt )))
				(checkMates metaDatabase m tt errorStream idx )))
	     (setq idx (+ idx 1 )))
    (ifTrue (> (size (str errorStream ))0 )
	    (log (% "%s" (str errorStream )))
	    (exit 1 ))))