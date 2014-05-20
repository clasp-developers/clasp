					; fixed code
					;
;;
;; This module makes it easier to define oligomers
;; You can define them using code like the following
;;
					;setq oligChain
					;    chain
					;	branch () "gly"
					;	    branch "nside" "nh"
					;	branch "dkp" "pro4(2S4S)"
					;	    branch "nside" "nAla"
					;	branch "dkp" "dab(S)"
					;	    branch "nside" "nAla"
					;	branch "sidePeptide" "dab(S)"
					;	    branch "nside" "nAla"
					;	    branch "sideAmideC" "ace"
					;	branch "peptide" "ace"
					; setq oligomer (assemble oligChain)
;;
;; (chain) creates a linear chain of oligomers and (br) creates branches
;;
;; To convert the OligomerHolder into an oligomer use
;;
					; setq oligomer ( oligomerFromChain oligChain )
					;



(defClass Assembler () (_oligomer _nextMonomerId _allMonomerIds )
  (method __init__ (self )
	  (setq (slot self _oligomer )())
	  (setq (slot self _nextMonomerId ) 1 )
	  (setq (slot self _allMonomerIds ) (Binder )))

  (method nextMonomerId (self )
	  (let ((s (% "mon%03d" (slot self _nextMonomerId ))))
	    (setq (slot self _nextMonomerId ) (+ (slot self _nextMonomerId ) 1 ))
	    (print (% "-- in nextMonomerId monomerId: %s" s))
	    (asSymbol s) ))
  (method setMonomerId (self id monomer )
	  (when (contains (slot self _allMonomerIds) id )
	    (raise (LispError (% "There is a non-unique monomer id(%s) - existing monomer ids: %s" id (repr (slot self _allMonomerIds)) ))))
	  (extend (slot self _allMonomerIds ) id monomer ))
  (method recognizesMonomerId (self id )
	  (contains (slot self _allMonomerIds ) id ))
  (method getMonomerId (self id )
	  (when (not (contains (slot self _allMonomerIds )id ))
	    (raise (LispError (% "There is no unique monomer id(%s)" id ))))
	  (lookup (slot self _allMonomerIds ) id ))
  (method addMonomer (self monomer )
	  (addMonomer (slot self _oligomer )monomer ))
  (method realize (self chain )
	  (setq (slot self _oligomer )(Oligomer ))
	  (realize chain self ))
  (method couple (self mon1 coupling mon2 )
	  (couple (slot self _oligomer ) mon1 coupling mon2 ))
  (method ringCouple (self id1 id2 )
	  (let ((mon1 (getMonomerId self id1 ))
		(mon2 (getMonomerId self id2 )))
	    (print  "About to call oligomer.ringCouple" )
	    (ringCouple (slot self _oligomer ) mon1 mon2 )
	    (print  "Came out of call oligomer.ringCouple" )))
  (method getOligomer (self )
	  (slot self _oligomer ))
)


(defClass RingCoupler ()(_id1 _id2 )
  (method __init__ (self id1 id2 )
	  (setq (slot self _id1 )id1 )
	  (setq (slot self _id2 )id2 ))
  (method realize (self assembler )
	  (ringCouple assembler (slot self _id1 )(slot self _id2 ))))



(defClass ChainSegment ()(_coupling _topMonomer _bottomMonomer )
  (method __init__ (self coupling topMonomer bottomMonomer )
	  (setq (slot self _coupling )coupling )
	  (setq (slot self _topMonomer )topMonomer )
	  (setq (slot self _bottomMonomer )bottomMonomer ))
  (method repr (self )
	  (if (== topMonomer bottomMonomer )
	      (print (% "ChainSegment coupling: %s topMonomer==bottomMonomer: %s"
			(slot self _coupling ) (repr topMonomer )))
	    (print (% "ChainSegment coupling: %s topMonomer: %s bottomMonomer: %s"
		      (slot self _coupling ) (repr topMonomer ) (repr bottomMonomer )))))
  (method getCoupling (self )
	  (slot self _coupling ))
  (method getTopMonomer (self )
	  (slot self _topMonomer ))
  (method getBottomMonomer (self )
	  (slot self _bottomMonomer )))





(defClass ChainHolder ()(_chain _rings )
  (method __init__ (self chain rings )
	  (setq (slot self _chain )chain )
	  (setq (slot self _rings )rings ))
  (method desc (self )
	  (foreach ch (slot self _chain )
		   (desc ch )))
  (method realize (self assembler )
	  (let ( previous topSegment segment )
	    (foreach one (slot self _chain )
		     (setq segment (realize one assembler ))
		     (when (isNil topSegment )
		       (setq topSegment segment ))
		     (when (notNil previous )
		       (couple assembler (getBottomMonomer previous )(getCoupling segment )(getTopMonomer segment )))
		     (setq previous segment ))
	    (foreach one (slot self _rings )
		     (realize one assembler ))
	    (ChainSegment (getCoupling topSegment )(getTopMonomer topSegment )(getBottomMonomer segment ))))
)


(defClass BranchHolder () (_coupling _groupName _id _aliases _children _backTrace )
  (method __init__ (self coupling groupName id aliases children backTrace )
	  (setq (slot self _coupling )coupling )
	  (setq (slot self _groupName )groupName )
;;	  (assert (isA id Symbol ) (% "id must be Symbol - instead it is a %s[%s]" (className id) (repr id)))
	  (setq (slot self _id ) id)
	  (setq (slot self _aliases) aliases)
	  (setq (slot self _children) (filterOutNil children))
	  (setq (slot self _backTrace) (backtrace)))


  (method desc (self )
	  (let ((idDesc ""))
	    (when (!= (slot self _id ) () )
	      (setq idDesc (% "id(%s)" (repr (slot self _id )))))
	    (println (% "BranchHolder coupling(%s) groupName(%s) %s" (repr (slot self _coupling ))(repr (slot self _groupName ))idDesc ))
	    (printPushPrefix ":- " )
	    (foreach ch (slot self _children )
		     (desc ch ))
	    (printPopPrefix )))

  (method getCoupling (self )
	  (slot self _coupling ))


  (method realize (self assembler )
	  (let ((monomer (MultiMonomer :name (slot self _groupName ) :comment "Created by code" )))
;;	    (debug)
	    (if (isNil (slot self _id ))
	      (let ((mid (nextMonomerId assembler)))
		(print (% "Setting monomer without id to id: %s" mid))
		(setq (slot self _id ) mid ))
	      (print (% "The id is: %s" (slot self _id))))
	    (setId monomer (slot self _id ))
	    (log (% "BranchHolder::realize - Setting aliases from cons: %s" (repr (slot self _aliases))))
	    (setAliasesFromCons monomer (slot self _aliases ))
	    (setMonomerId assembler (slot self _id ) monomer )
	    (addMonomer assembler monomer )
	    (foreach neighbor (slot self _children )
		     (log (% "About to realize neighbor class(%s) value(%s)" (className neighbor )(repr neighbor )))
		     (let ((realizedNeighbor (realize neighbor assembler )))
		       (couple assembler monomer
			       (getCoupling realizedNeighbor )
			       (getTopMonomer realizedNeighbor ))))
	    (ChainSegment (slot self _coupling )monomer monomer )))
)



(defun branch (inCoupling partName aliasNames &rest children )
  "Define a branch of an oligomer" 
  (log (% "branch children[%s]" (repr children )))
  (let ((bt (backtrace))
	(id ())
	aliases firstName branch)
    (when (notNil aliasNames )
      (cond 
       ((isA aliasNames Text )
	(setq id aliasNames )
	(setq aliases (list aliasNames )))
       ((isA aliasNames Cons )
	(setq firstName (car aliasNames ))
	(setq aliases (cdr aliasNames ))
;;	(assert (isA firstName Symbol )(% "You provided a list for the name/aliases - the first element is the name of the monomer and must be a Symbol object followed by the aliases in the same list - instead it was(%s)" (repr aliasNames )))
	(setq id firstName ))
       (true 
	(raise (LispError "Illegal argument type for aliasNames" )))))
    (setq branch (BranchHolder inCoupling partName id aliases children bt ))
    branch ))



(defun chain (&rest allArgs )
  (log (% "chain allArgs[%s]" (repr allArgs )))
  (let (reversedChainLinks rings (cur allArgs) obj chainLinks chainResult )
    (while (notNil cur )
      (setq obj (car cur ))
      (cond 
       ((isA obj ChainHolder )					;E-A
	(setq reversedChainLinks (Cons obj reversedChainLinks ))) ;E-B

       ((isA obj BranchHolder )					;E-A
	(setq reversedChainLinks (Cons obj reversedChainLinks ))) ;E-B

       ((isA obj RingCoupler )		;E-A
	(setq rings (Cons obj rings )))	;E-B

       (true 
	(raise (LispError (% "Unknown element of chain: %s" (className obj ))))))
      (setq cur (cdr cur )))
    (setq chainLinks (reverse reversedChainLinks ))
    (setq chainResult (ChainHolder chainLinks rings ))
    chainResult ))



(defun ring (id1 id2 )
  (let ((r (RingCoupler id1 id2 )))
    r))



(defun assemble (ch &optional (name 'olig ))
  (let ( (assembler (Assembler)) oligomer )
    (log (% "About to realize assember -- its class(%s) value(%s)" (className assembler )(repr assembler )))
    (realize assembler ch )
    (setq oligomer (getOligomer assembler ))
    (chem:setName oligomer name )
    oligomer ))
