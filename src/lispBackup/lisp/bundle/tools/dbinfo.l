
(defun describePlug (plug)
  (let ((ss (StringStream)))
    (if (getIsRingClosing plug)
	(write ss (% " RingClosing[%s]" (getName plug)))
      (write ss (% " %s" (getName plug))))
    (str ss)))
  

(defun organizePlugs (plugs) 
  (let (ins outs rings)
    (foreach plug plugs
	     (cond
	      ((getIsIn plug)
	       (setq ins (cons plug ins)))
	      ((getIsRingClosing plug)
	       (setq rings (cons plug rings)))
	      (true
	       (setq outs (cons plug outs)))))
    (list :in ins :out outs :ring rings)))


(defun plugsForEntity (entityName)
  "Return the plugs for the entity"
  (let* ((entity (getEntity *DATABASE* entityName))
	 (econst (constitution entity))
	 (plugs (plugsWithMatesAsCons (constitution entity))))
    (print (% "entity = %s" entity ))
    (print (% "entity constitution = %s" econst ))
    (organizePlugs plugs)))

(defun couplingNamesAsSet (plugs)
  (let ((nameSet (StringSet)))
    (foreach p plugs
	     (insert nameSet (DirectionalCoupling_couplingName (getName p))))
    nameSet))


(defun describeTopology (top)
  (let ((ss (StringStream)))
    (write ss (% "Topology name[%s] plugs:" (getName top)))
    (let ((plugs (plugsAsCons top)))
      (foreach plug plugs
	       (if (getIsRingClosing plug)
		   (write ss (% " RingClosing[%s]" (getName plug)))
		 (write ss (% " %s" (getName plug)))))
      )
    (str ss)))


(defun dumpPlugs (plugs)
  (foreach p plugs
	   (print (getName p))))


(defun dumpConstitutions ()
  (let ((constitutions (constitutionsAsCons *DATABASE*)))
    (foreach ct constitutions
	     (prin1 (% " %s" (getName ct))))
    (print "")))






;;
;; Start here
;;

;;
;; Parse arguments
;;


(if (not *DATABASE*)
    (progn
      (standardDatabase "core" true)
      (print "Loaded \"core\" database"))
  (progn
    (print "Using provided database")))

(defvar *posArgs* *ARGS*)
(defvar *cmd* (if (> (length *posArgs*) 1) (asKeywordSymbol (@ *posArgs* 1)) :describe))
(defvar *first* (if (> (length *posArgs*) 2) (@ *posArgs* 2) ""))
(defvar *second* (if (> (length *posArgs*) 3) (@ *posArgs* 3) ""))
(let ((orig *second*))
  (while (== (substr *second* 0 1) "\\")
    (setq *second* (substr *second* 1 99999)))
  (when (!= orig *second*)
    (print (% "Second argument had leading backslashes stripped [%s]-->[%s]" orig *second* ))))

(print (% "Command = %s" *cmd* ))
(cond
 ((== *cmd* :help)
  (print "describe                  - Print the current Cando database")
  (print "in _entity_               - List the inPlugs for the entity")
  (print "out _entity_              - List the outPlugs for the entity")
  (print "ring _entity_             - List the ringPlugs for the entity")
  (print "constitutions             - List all Constitutions")
  (print "between _e1_ _e2_         - List the valid couplings between the entities _e1_ and _e2_")
  (print "topologiesWithPlug _p_    - List the topologies that contain the plug")
  (print "matesWithPlug _e1_ _p_    - List the mates of entity _e1_ with plug _p_")
  (print "ringClosingMates _e1_ _p_ - List the ring closing mates of entity _e1_ with plug _p_")
  (print "topologies _e_            - List the Topologies of entity _e_")
  (print "atomNames _e_             - List the atom names for the entity _e_")
  (print "Note: If you are passing an argument that starts with '-' escape it with \\\\")
  (exit 0))
 ((== *cmd* :describe)
  (print "core database"))
 ((== *cmd* :constitutions)
  (dumpConstitutions))
 ((or (== *cmd* :in) (== *cmd* :ring))
  (print (% "%s plug names" *cmd*))
  (dumpPlugs (lookup (plugsForEntity *first*) *cmd*)))
 ((== *cmd* :out)
  (print (% "%s plug names - including :ring" *cmd*))
  (dumpPlugs (lookup (plugsForEntity *first*) :out))
  (dumpPlugs (lookup (plugsForEntity *first*) :ring)))
 ((== *cmd* :between)
  (assert *second* "You must provide a second entity")
  (let* ((outCouplings (couplingNamesAsSet (lookup (plugsForEntity *first*) :out)))
	 (inCouplings (couplingNamesAsSet (lookup (plugsForEntity *second*) :in)))
	 (shared (intersection outCouplings inCouplings))
	 )
    (print (% "Shared couplings: %s" shared))))
 ((== *cmd* :topologiesWithPlug)
  ;
  ; Dump the topologies for the entity that have the coupling provided
  ; in the second argument
  ;
  (assert *second* "You must provide a plug name as the second entity")
  (let* ((econst (constitution (getEntity *DATABASE* *first*)))
	 (econstTops (topologiesAsCons econst))
	 topsWithPlug)
    (print (% "Found constitution[%s]" (getName econst)))
    (foreach top econstTops
	     (when (hasPlugNamed top *second*)
	       (print (describeTopology top))))))

 ((== *cmd* :matesWithPlug)
  ;
  ; Dump the topologies for the entity that have the coupling provided
  ; in the second argument
  ;
  (assert *second* "You must provide a plug name as the second entity")
  (let* ((econst (constitution (getEntity *DATABASE* *first*)))
	 (econstTops (topologiesAsCons econst))
	 topsWithPlug
	 (uniqueNames (StringSet)))
    (print (% "Found constitution[%s]" (getName econst)))
    (foreach top econstTops
	     (when (hasPlugNamed top *second*)
	       (let* ((plug (plugNamed top *second*))
		      (mates (matesAsCons plug))
		      (names (StringSet)))
		 (foreach mate mates
			  (insertStringSet uniqueNames (getMonomerNames mate))))))
    (print (% "Mate names: %s" (asString uniqueNames)))))

 ((== *cmd* :ringClosingMates)
  ;
  ; Dump the topologies for the entity that have the coupling provided
  ; in the second argument
  ;
  (assert *second* "You must provide a plug name as the second entity")
  (let* ((econst (constitution (getEntity *DATABASE* *first*)))
	 (econstTops (topologiesAsCons econst))
	 topsWithPlug
	 (uniqueNames (StringSet)))
    (print (% "Found constitution[%s]" (getName econst)))
    (foreach top econstTops
	     (when (hasPlugNamed top *second*)
	       (let* ((plug (plugNamed top *second*))
		      (mates (ringClosingMatesAsCons plug))
		      (names (StringSet)))
		 (foreach mate mates
			  (insertStringSet uniqueNames (getMonomerNames mate))))))
    (print (% "Ring closing mate names: %s" (asString uniqueNames)))))

  ;
  ; Dump the topologies for the entity that have the coupling provided
  ; in the second argument
  ;

 ((== *cmd* :topologies)
  (let* ((econst (constitution (getEntity *DATABASE* *first*)))
	 (econstTops (topologiesAsCons econst)))
    (print (% "Found constitution[%s]" (getName econst)))
    (foreach top econstTops
	       (print (describeTopology top)))))
 ((== *cmd* :atomNames)
  (let* ((econst (constitution (getEntity *DATABASE* *first*)))
	 (fragments (fragmentsAsCons econst))
	 (atomSet (StringSet)))
    (foreach frag fragments
	     (insertStringSet atomSet (getAtomNames frag)))
    (print (% "Found constitution[%s]" (getName econst)))
    (print (% "Unique atom names: %s" (asString atomSet)))))
 (true
   (print (% "Unrecognized command[%s]" *cmd*))))



