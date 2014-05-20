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

defClass Assembler () ( _oligomer _nextMonomerId _allMonomerIds )
    method __init__ ( self )
        setq [ self slot _oligomer ] ()
	setq [ self slot _nextMonomerId ] 1
	setq [ self slot _allMonomerIds ] (ObjectDictionary)

    method nextMonomerId ( self )
        setq s [ "mon%03d" % [ self slot _nextMonomerId ] ]
	setq [ self slot _nextMonomerId ] [ [ self slot _nextMonomerId ] + 1 ]
	return s

    method setMonomerId ( self id monomer )
        when [ [ self slot _allMonomerIds ] contains id ]
	    raise ( LispError ["There is a non-unique monomer id(%s)" % id] )
	[ [ self slot _allMonomerIds ] put id monomer ]

    method recognizesMonomerId ( self id )
        return [[ self slot _allMonomerIds ] contains id ]

    method getMonomerId ( self id )
        when (not [[ self slot _allMonomerIds ] contains id ] )
	    raise ( LispError [ "There is no unique monomer id(%s)" % id ] )
	return [ [ self slot _allMonomerIds ] get id ]
        
    method addMonomer ( self monomer )
        [ [ self slot _oligomer ] addMonomer monomer ]        

    method realize ( self chain )
        setq [ self slot _oligomer ] (Oligomer)
        [ chain realize self ]

    method couple ( self mon1 coupling mon2 )
        [ [ self slot _oligomer ] couple mon1 coupling mon2 ]

    method ringCouple ( self id1 id2 )
	setq mon1 [ self getMonomerId id1 ]
	setq mon2 [ self getMonomerId id2 ]
	println [ "About to call oligomer.ringCouple" ]
	[[ self slot _oligomer ] ringCouple mon1 mon2 ]
	println [ "Came out of call oligomer.ringCouple" ]

    method getOligomer ( self )
        return [ self slot _oligomer ]
	 

defClass RingCoupler () ( _id1 _id2 )
    method __init__ ( self id1 id2 )
	setq [ self slot _id1 ] id1
	setq [ self slot _id2 ] id2

    method realize ( self assembler )
        [ assembler ringCouple [ self slot _id1 ] [ self slot _id2 ] ]


defClass ChainSegment () ( _coupling _topMonomer _bottomMonomer )
    method __init__ ( self coupling topMonomer bottomMonomer )
        setq [ self slot _coupling ] coupling
        setq [ self slot _topMonomer ] topMonomer
        setq [ self slot _bottomMonomer ] bottomMonomer

    method repr ( self )
        if [ topMonomer == bottomMonomer ]
	  then
            println [ "ChainSegment coupling: %s topMonomer==bottomMonomer: %s" % [ self slot _coupling ] (repr topMonomer) ]
	  else
            println [ "ChainSegment coupling: %s topMonomer: %s bottomMonomer: %s" % [ self slot _coupling ] (repr topMonomer) (repr bottomMonomer) ]

    method getCoupling ( self )
        return [ self slot _coupling ]

    method getTopMonomer ( self )
        return [ self slot _topMonomer ]

    method getBottomMonomer ( self )
        return [ self slot _bottomMonomer ]






defClass ChainHolder () ( _chain _rings )
    method __init__ ( self chain rings )
        setq [ self slot _chain ] chain
	setq [ self slot _rings ] rings

    method desc (self )
        foreach ch [ self slot _chain ]
	    [ch desc ]

    method realize ( self assembler )
        setq previous ()
	setq topSegment ()
        foreach one [ self slot _chain ]
	    setq segment [ one realize assembler ]
	    when [ topSegment isNil ]
	        setq topSegment segment
	    when [ previous notNil ]
	        [ assembler couple [ previous getBottomMonomer ] [ segment getCoupling ] [ segment getTopMonomer ] ]
	    setq previous segment
        foreach one [ self slot _rings ]
	    [ one realize assembler ]
	return ( ChainSegment [ topSegment getCoupling] [ topSegment getTopMonomer ] [ segment getBottomMonomer ] )



defClass BranchHolder () ( _coupling _groupName _id _aliases _children _backTrace)
    method __init__ ( self coupling groupName id aliases children backTrace )
        setq [ self slot _coupling ] coupling
        setq [ self slot _groupName ] groupName
	assert [ id isA Text ] "id must be Text"
        setq [ self slot _id ] id
        setq [ self slot _aliases ] aliases
        setq [ self slot _children ] [children filterOutNil]
	setq [ self slot _backTrace ] (backtrace)

    method desc (self )
	local idDesc ""
        when [ [self slot _id] != "" ]
	    setq idDesc [ "id(%s)" % (repr [self slot _id]) ]
	println [ "BranchHolder coupling(%s) groupName(%s) %s" % (repr [ self slot _coupling ]) (repr [ self slot _groupName ]) idDesc ]
	printPushPrefix ":- "
        foreach ch [ self slot _children ]
	    [ ch desc ]
	printPopPrefix

    method getCoupling ( self )
	return [ self slot _coupling ]

    method realize ( self assembler )
        setq monomer (MultiMonomer :name [ self slot _groupName ] :comment "Created by code" )
	when [ [ self slot _id ] == "" ]
	    setq [ self slot _id ] [ assembler nextMonomerId ]
	[ monomer setId [ self slot _id ] ]
	[ monomer setAliasesFromCons [ self slot _aliases ] ]
	[ assembler setMonomerId [ self slot _id ] monomer ]
	[ assembler addMonomer monomer ]
        foreach neighbor [ self slot _children ]
	    log [ "About to realize neighbor class(%s) value(%s)" % (className neighbor) (repr neighbor) ]
	    setq realizedNeighbor [ neighbor realize assembler ]
	    [ assembler couple monomer [ realizedNeighbor getCoupling ] [ realizedNeighbor getTopMonomer ] ]
        return (ChainSegment [ self slot _coupling ] monomer monomer )



defun branch ( inCoupling partName aliasNames &rest children ) \
	"Define a branch of an oligomer"
    log [ "branch children[%s]" % (repr children) ]
    setq bt (backtrace)
    setq id ""
    setq aliases ()
    when [ aliasNames notNil ]
        cond
	    [ aliasNames isA Text ]
	        setq id aliasNames
	        setq aliases (list aliasNames)
	    [ aliasNames isA Cons ]
	        setq firstName (car aliasNames)
		local aliases ( cdr aliasNames )
	        assert [ firstName isA Text ] [ "You provided a list for the name/aliases - the first element is the name of the monomer and must be a Text object followed by the aliases in the same list - instead it was(%s)" % (repr aliasNames ) ]
		setq id firstName
	    true
	        raise (LispError "Illegal argument type for aliasNames")
    setq branch (BranchHolder inCoupling partName id aliases children bt)
    return branch
 

defun chain ( &rest allArgs )
    log [ "chain allArgs[%s]" % (repr allArgs) ]
    setq reversedChainLinks ()
    setq rings ()
    setq cur allArgs
    while [ cur notNil ]
	setq obj (car cur)
	cond
	    [ obj isA ChainHolder ]
	        setq reversedChainLinks (Cons obj reversedChainLinks)
	    [ obj isA BranchHolder ]
	        setq reversedChainLinks (Cons obj reversedChainLinks)
	    [ obj isA RingCoupler ]
	        setq rings (Cons obj rings)
	    true
	        raise ( LispError [ "Unknown element of chain: %s" % (className obj) ] )
	setq cur (cdr cur)
    setq chainLinks [reversedChainLinks reverse]
    setq chainResult (ChainHolder chainLinks rings )
    return chainResult


defun ring ( id1 id2 )
    setq r (RingCoupler id1 id2)
    return r



defun assemble ( ch &optional (name "olig") )
    local assembler (Assembler )
    log [ "About to realize assember -- its class(%s) value(%s)" % (className assembler) (repr assembler) ]
    [ assembler realize ch ]
    setq oligomer [ assembler getOligomer ]
    [oligomer setName name ]
    return oligomer
    


