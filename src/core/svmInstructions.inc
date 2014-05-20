






	/* Each instruction can have the form (Operation, Id, Index)
	The instruction macro defines the operation as the first argument and whether the
	operation uses either or both of the Id and Index arguments.
	The Operation is stored in a single byte.
	The Id is stored in a single unsigned byte.
	The Index is stored in an unsigned word. 
	Each instruction fits into a four byte word.
	The Id argument is used to select between different arrays.
	The Index argument is most commonly used to index into an array but sometimes is used
	to select between arrays.
	*/

	



	/*! Generate a random number between 0 and 1 and push onto real stack */
	



	


	


	


	



	


	


	


	


	


	


	



	


	


	



	


	


	



        /*! Push an untransformed vector referenced by an alias onto the stack */
	



        /*! Push a transformed vector referenced by an alias onto the stack */
	



	/*! 1)popY 2)popX 3)push(X+Y) */
	



	/*! 1)popY 2)popX 3)push(X*Y) */
	



	/*! 1)popY 2)popX 3)push(X-Y) */
	



	/*! 1)popY 2)popX 3)push(X/Y) */
	



	/*! Duplicate the top real on the stack */
	



	/*! Push a Real constant onto the stack */
	


	/*! Push the value 0.0 onto the stack */
	


	/*! Push the value 1.0 onto the stack */
	


	/*! Push the value 2.0 onto the stack */
	



	/*! Push the Int constant onto the stack */
	


	




	/*! Pull two vectors off the top of the stack, an ideal distance and a force constant
	  calculate the distance between the vectors, subtract the ideal distance and
	  multiply by the force constant and put the result back onto the stack
	*/
	


	/*! Pull three vectors off the top of the stack, an ideal distance and a force constant
	  calculate the angle between the vectors, subtract the ideal angle and
	  multiply by the force constant and put the result back onto the stack
	*/
	


	/*! Pull four vectors off the top of the stack, an absolute dihedral angle and a force constant
	  calculate the dihedral between the vectors, subtract the ideal dihedral and
	  multiply by the force constant and put the result back onto the stack
	*/
	


	/*! Pull four vectors off of the top of the stack, a MULTiplicity, a PHASE and a SCALE.
	  Calculate the dihedral angle and SCALE*(1+cos(DIHEDRAL*MULT+PHASE))
	  and put it back onto the stack.
	*/
	



	/*! Score intramolecularVdw */
	




	/* To calculate intermolecular vdw collisions, two
	providers are required, the first one is pushed onto
	the stack with this command and the second
	is provided by the intermolecularVdw command
	*/
	


	/* Calculate intermolecularVdwCollisions by taking
	one provider off of the alias stack and check for
	collisions with the atoms of the provider given as an argument
	here
	*/
	



	/*! Clear the MoveablePoints from the superposer in the Superposer register */
	


	/*! Clear the FixedPoints from the superposer in the Superposer register */
	



	/*! Store a fixed superposition point into the superposer in the Superposer register */
	


	/*! Store a moveable superposition point into the superposer in the Superposer register */
	



	/*! Carry out a superposition and write the transform and the RMS deviation into the AliasReferencerHolder indicated by _ArrayId */
	



	/*!  Push the superposer score onto the stack
	  Command: sop_pushSuperposeScore-superposerIdx-0000
	*/
	


	


	



	/*! Update the points for all static providers */
	



	/*! Update all the points for the dynamic providers */
	


	


	




#ifdef	SVMInstruction_Enum
/* Include this in the scorerVirtualMachine.h header file */

	sop_nop,

	sop_random01,

	sop_printString,

	sop_popPrintReal,

	sop_popPrintInt,

	sop_printCr,

	sop_goto,

	sop_popIntIfZeroGoto,

	sop_popIntNotZeroGoto,

	sop_popIntIfGtZeroGoto,

	sop_popIntIfLtZeroGoto,

	sop_call,

	sop_return,

	sop_realLessThan,

	sop_realEqual,

	sop_realGreaterThan,

	sop_intLessThan,

	sop_intEqual,

	sop_intGreaterThan,

	sop_pushPointUntransformed,

	sop_pushPointTransformed,

	sop_addReal,

	sop_mulReal,

	sop_subReal,

	sop_divReal,

	sop_dupReal,

	sop_pushRealMem,

	sop_pushReal0,

	sop_pushReal1,

	sop_pushReal2,

	sop_pushIntMem,

	sop_pushInt1,

	sop_distance,

	sop_angle,

	sop_absoluteDihedral,

	sop_periodicDihedral,

	sop_intramolecularVdw,

	sop_pushIntermolecularVdwProvider,

	sop_intermolecularVdw,

	sop_clearSuperposerMoveable,

	sop_clearSuperposerFixed,

	sop_appendAliasToSuperposerFixed,

	sop_appendAliasToSuperposerMoveable,

	sop_superpose,

	sop_pushSuperposeScore,

	sop_recordScorerState,

	sop_recordScoreIfBest,

	sop_synchronizeWithStaticProviders,

	sop_synchronizeWithDynamicProviders,

	sop_debug,

	sop_end,

#endif
#ifdef	SVMInstruction_Code
/* Include this in the initializeGlobals function of Scorer_OVirtualMachine */

	ADDOP(sop_nop,"nop",&eval_nop,NO_ARGS)

	ADDOP(sop_random01,"random01",&eval_random01,NO_ARGS)

	ADDOP(sop_printString,"printString",&eval_printString,ARG2string)

	ADDOP(sop_popPrintReal,"popPrintReal",&eval_popPrintReal,NO_ARGS)

	ADDOP(sop_popPrintInt,"popPrintInt",&eval_popPrintInt,NO_ARGS)

	ADDOP(sop_printCr,"printCr",&eval_printCr,NO_ARGS)

	ADDOP(sop_goto,"goto",&eval_goto,ARG2lineNumber)

	ADDOP(sop_popIntIfZeroGoto,"popIntIfZeroGoto",&eval_popIntIfZeroGoto,ARG2lineNumber)

	ADDOP(sop_popIntNotZeroGoto,"popIntNotZeroGoto",&eval_popIntNotZeroGoto,ARG2lineNumber)

	ADDOP(sop_popIntIfGtZeroGoto,"popIntIfGtZeroGoto",&eval_popIntIfGtZeroGoto,ARG2lineNumber)

	ADDOP(sop_popIntIfLtZeroGoto,"popIntIfLtZeroGoto",&eval_popIntIfLtZeroGoto,ARG2lineNumber)

	ADDOP(sop_call,"call",&eval_call,ARG2lineNumber)

	ADDOP(sop_return,"return",&eval_return,NO_ARGS)

	ADDOP(sop_realLessThan,"realLessThan",&eval_realLessThan,NO_ARGS)

	ADDOP(sop_realEqual,"realEqual",&eval_realEqual,NO_ARGS)

	ADDOP(sop_realGreaterThan,"realGreaterThan",&eval_realGreaterThan,NO_ARGS)

	ADDOP(sop_intLessThan,"intLessThan",&eval_intLessThan,NO_ARGS)

	ADDOP(sop_intEqual,"intEqual",&eval_intEqual,NO_ARGS)

	ADDOP(sop_intGreaterThan,"intGreaterThan",&eval_intGreaterThan,NO_ARGS)

	ADDOP(sop_pushPointUntransformed,"pushPointUntransformed",&eval_pushPointUntransformed,ARG1provider_ARG2alias)

	ADDOP(sop_pushPointTransformed,"pushPointTransformed",&eval_pushPointTransformed,ARG1provider_ARG2alias)

	ADDOP(sop_addReal,"addReal",&eval_addReal,NO_ARGS)

	ADDOP(sop_mulReal,"mulReal",&eval_mulReal,NO_ARGS)

	ADDOP(sop_subReal,"subReal",&eval_subReal,NO_ARGS)

	ADDOP(sop_divReal,"divReal",&eval_divReal,NO_ARGS)

	ADDOP(sop_dupReal,"dupReal",&eval_dupReal,NO_ARGS)

	ADDOP(sop_pushRealMem,"pushRealMem",&eval_pushRealMem,ARG2real)

	ADDOP(sop_pushReal0,"pushReal0",&eval_pushReal0,NO_ARGS)

	ADDOP(sop_pushReal1,"pushReal1",&eval_pushReal1,NO_ARGS)

	ADDOP(sop_pushReal2,"pushReal2",&eval_pushReal2,NO_ARGS)

	ADDOP(sop_pushIntMem,"pushIntMem",&eval_pushIntMem,ARG2int)

	ADDOP(sop_pushInt1,"pushInt1",&eval_pushInt1,NO_ARGS)

	ADDOP(sop_distance,"distance",&eval_distance,NO_ARGS)

	ADDOP(sop_angle,"angle",&eval_angle,NO_ARGS)

	ADDOP(sop_absoluteDihedral,"absoluteDihedral",&eval_absoluteDihedral,NO_ARGS)

	ADDOP(sop_periodicDihedral,"periodicDihedral",&eval_periodicDihedral,NO_ARGS)

	ADDOP(sop_intramolecularVdw,"intramolecularVdw",&eval_intramolecularVdw,ARG1provider)

	ADDOP(sop_pushIntermolecularVdwProvider,"pushIntermolecularVdwProvider",&eval_pushIntermolecularVdwProvider,ARG1provider)

	ADDOP(sop_intermolecularVdw,"intermolecularVdw",&eval_intermolecularVdw,ARG1provider)

	ADDOP(sop_clearSuperposerMoveable,"clearSuperposerMoveable",&eval_clearSuperposerMoveable,ARG1provider)

	ADDOP(sop_clearSuperposerFixed,"clearSuperposerFixed",&eval_clearSuperposerFixed,ARG1provider)

	ADDOP(sop_appendAliasToSuperposerFixed,"appendAliasToSuperposerFixed",&eval_appendAliasToSuperposerFixed,ARG1provider)

	ADDOP(sop_appendAliasToSuperposerMoveable,"appendAliasToSuperposerMoveable",&eval_appendAliasToSuperposerMoveable,ARG1provider)

	ADDOP(sop_superpose,"superpose",&eval_superpose,ARG1provider)

	ADDOP(sop_pushSuperposeScore,"pushSuperposeScore",&eval_pushSuperposeScore,ARG1provider)

	ADDOP(sop_recordScorerState,"recordScorerState",&eval_recordScorerState,NO_ARGS)

	ADDOP(sop_recordScoreIfBest,"recordScoreIfBest",&eval_recordScoreIfBest,NO_ARGS)

	ADDOP(sop_synchronizeWithStaticProviders,"synchronizeWithStaticProviders",&eval_synchronizeWithStaticProviders,NO_ARGS)

	ADDOP(sop_synchronizeWithDynamicProviders,"synchronizeWithDynamicProviders",&eval_synchronizeWithDynamicProviders,NO_ARGS)

	ADDOP(sop_debug,"debug",&eval_debug,NO_ARGS)

	ADDOP(sop_end,"end",&eval_end,NO_ARGS)

#endif
#ifdef	SVMInstruction_Friends

	friend void eval_nop(const ScoreCommand& cmd, Scorer_OVirtualMachine* machine);

	friend void eval_random01(const ScoreCommand& cmd, Scorer_OVirtualMachine* machine);

	friend void eval_printString(const ScoreCommand& cmd, Scorer_OVirtualMachine* machine);

	friend void eval_popPrintReal(const ScoreCommand& cmd, Scorer_OVirtualMachine* machine);

	friend void eval_popPrintInt(const ScoreCommand& cmd, Scorer_OVirtualMachine* machine);

	friend void eval_printCr(const ScoreCommand& cmd, Scorer_OVirtualMachine* machine);

	friend void eval_goto(const ScoreCommand& cmd, Scorer_OVirtualMachine* machine);

	friend void eval_popIntIfZeroGoto(const ScoreCommand& cmd, Scorer_OVirtualMachine* machine);

	friend void eval_popIntNotZeroGoto(const ScoreCommand& cmd, Scorer_OVirtualMachine* machine);

	friend void eval_popIntIfGtZeroGoto(const ScoreCommand& cmd, Scorer_OVirtualMachine* machine);

	friend void eval_popIntIfLtZeroGoto(const ScoreCommand& cmd, Scorer_OVirtualMachine* machine);

	friend void eval_call(const ScoreCommand& cmd, Scorer_OVirtualMachine* machine);

	friend void eval_return(const ScoreCommand& cmd, Scorer_OVirtualMachine* machine);

	friend void eval_realLessThan(const ScoreCommand& cmd, Scorer_OVirtualMachine* machine);

	friend void eval_realEqual(const ScoreCommand& cmd, Scorer_OVirtualMachine* machine);

	friend void eval_realGreaterThan(const ScoreCommand& cmd, Scorer_OVirtualMachine* machine);

	friend void eval_intLessThan(const ScoreCommand& cmd, Scorer_OVirtualMachine* machine);

	friend void eval_intEqual(const ScoreCommand& cmd, Scorer_OVirtualMachine* machine);

	friend void eval_intGreaterThan(const ScoreCommand& cmd, Scorer_OVirtualMachine* machine);

	friend void eval_pushPointUntransformed(const ScoreCommand& cmd, Scorer_OVirtualMachine* machine);

	friend void eval_pushPointTransformed(const ScoreCommand& cmd, Scorer_OVirtualMachine* machine);

	friend void eval_addReal(const ScoreCommand& cmd, Scorer_OVirtualMachine* machine);

	friend void eval_mulReal(const ScoreCommand& cmd, Scorer_OVirtualMachine* machine);

	friend void eval_subReal(const ScoreCommand& cmd, Scorer_OVirtualMachine* machine);

	friend void eval_divReal(const ScoreCommand& cmd, Scorer_OVirtualMachine* machine);

	friend void eval_dupReal(const ScoreCommand& cmd, Scorer_OVirtualMachine* machine);

	friend void eval_pushRealMem(const ScoreCommand& cmd, Scorer_OVirtualMachine* machine);

	friend void eval_pushReal0(const ScoreCommand& cmd, Scorer_OVirtualMachine* machine);

	friend void eval_pushReal1(const ScoreCommand& cmd, Scorer_OVirtualMachine* machine);

	friend void eval_pushReal2(const ScoreCommand& cmd, Scorer_OVirtualMachine* machine);

	friend void eval_pushIntMem(const ScoreCommand& cmd, Scorer_OVirtualMachine* machine);

	friend void eval_pushInt1(const ScoreCommand& cmd, Scorer_OVirtualMachine* machine);

	friend void eval_distance(const ScoreCommand& cmd, Scorer_OVirtualMachine* machine);

	friend void eval_angle(const ScoreCommand& cmd, Scorer_OVirtualMachine* machine);

	friend void eval_absoluteDihedral(const ScoreCommand& cmd, Scorer_OVirtualMachine* machine);

	friend void eval_periodicDihedral(const ScoreCommand& cmd, Scorer_OVirtualMachine* machine);

	friend void eval_intramolecularVdw(const ScoreCommand& cmd, Scorer_OVirtualMachine* machine);

	friend void eval_pushIntermolecularVdwProvider(const ScoreCommand& cmd, Scorer_OVirtualMachine* machine);

	friend void eval_intermolecularVdw(const ScoreCommand& cmd, Scorer_OVirtualMachine* machine);

	friend void eval_clearSuperposerMoveable(const ScoreCommand& cmd, Scorer_OVirtualMachine* machine);

	friend void eval_clearSuperposerFixed(const ScoreCommand& cmd, Scorer_OVirtualMachine* machine);

	friend void eval_appendAliasToSuperposerFixed(const ScoreCommand& cmd, Scorer_OVirtualMachine* machine);

	friend void eval_appendAliasToSuperposerMoveable(const ScoreCommand& cmd, Scorer_OVirtualMachine* machine);

	friend void eval_superpose(const ScoreCommand& cmd, Scorer_OVirtualMachine* machine);

	friend void eval_pushSuperposeScore(const ScoreCommand& cmd, Scorer_OVirtualMachine* machine);

	friend void eval_recordScorerState(const ScoreCommand& cmd, Scorer_OVirtualMachine* machine);

	friend void eval_recordScoreIfBest(const ScoreCommand& cmd, Scorer_OVirtualMachine* machine);

	friend void eval_synchronizeWithStaticProviders(const ScoreCommand& cmd, Scorer_OVirtualMachine* machine);

	friend void eval_synchronizeWithDynamicProviders(const ScoreCommand& cmd, Scorer_OVirtualMachine* machine);

	friend void eval_debug(const ScoreCommand& cmd, Scorer_OVirtualMachine* machine);

	friend void eval_end(const ScoreCommand& cmd, Scorer_OVirtualMachine* machine);

#endif
