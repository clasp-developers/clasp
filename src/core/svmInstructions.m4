changecom('/*','*/')

define(`fEnum',1)
define(`fCode',2)
define(`fFriends',3)
define(`instruction',`
divert(fEnum)
	sop_$1,
divert(fCode)
	ADDOP(sop_$1,"$1",&eval_$1,$2)
divert(fFriends)
	friend void eval_$1(const ScoreCommand& cmd, Scorer_OVirtualMachine* machine);
divert
')

	/* Each `instruction' can have the form (Operation, Id, Index)
	The `instruction' macro defines the operation as the first argument and whether the
	operation uses either or both of the Id and Index arguments.
	The Operation is stored in a single byte.
	The Id is stored in a single unsigned byte.
	The Index is stored in an unsigned word. 
	Each `instruction' fits into a four byte word.
	The Id argument is used to select between different arrays.
	The Index argument is most commonly used to index into an array but sometimes is used
	to select between arrays.
	*/

	instruction(nop,NO_ARGS)

	/*! Generate a random number between 0 and 1 and push onto real stack */
	instruction(random01,NO_ARGS)

	instruction(printString,ARG2string)
	instruction(popPrintReal,NO_ARGS)
	instruction(popPrintInt,NO_ARGS)
	instruction(printCr,NO_ARGS)

	instruction(goto,ARG2lineNumber)
	instruction(popIntIfZeroGoto,ARG2lineNumber)
	instruction(popIntNotZeroGoto,ARG2lineNumber)
	instruction(popIntIfGtZeroGoto,ARG2lineNumber)
	instruction(popIntIfLtZeroGoto,ARG2lineNumber)
	instruction(call,ARG2lineNumber)
	instruction(return,NO_ARGS)

	instruction(realLessThan,NO_ARGS)
	instruction(realEqual,NO_ARGS)
	instruction(realGreaterThan,NO_ARGS)

	instruction(intLessThan,NO_ARGS)
	instruction(intEqual,NO_ARGS)
	instruction(intGreaterThan,NO_ARGS)

        /*! Push an untransformed vector referenced by an alias onto the stack */
	instruction(`pushPointUntransformed',ARG1provider_ARG2alias)

        /*! Push a transformed vector referenced by an alias onto the stack */
	instruction(pushPointTransformed,ARG1provider_ARG2alias)

	/*! 1)popY 2)popX 3)push(X+Y) */
	instruction(addReal,NO_ARGS)

	/*! 1)popY 2)popX 3)push(X*Y) */
	instruction(mulReal,NO_ARGS)

	/*! 1)popY 2)popX 3)push(X-Y) */
	instruction(subReal,NO_ARGS)

	/*! 1)popY 2)popX 3)push(X/Y) */
	instruction(divReal,NO_ARGS)

	/*! Duplicate the top real on the stack */
	instruction(dupReal,NO_ARGS)

	/*! Push a Real constant onto the stack */
	instruction(pushRealMem,ARG2real)
	/*! Push the value 0.0 onto the stack */
	instruction(pushReal0,NO_ARGS)
	/*! Push the value 1.0 onto the stack */
	instruction(pushReal1,NO_ARGS)
	/*! Push the value 2.0 onto the stack */
	instruction(pushReal2,NO_ARGS)

	/*! Push the Int constant onto the stack */
	instruction(pushIntMem,ARG2int)
	instruction(pushInt1,NO_ARGS)


	/*! Pull two vectors off the top of the stack, an ideal distance and a force constant
	  calculate the distance between the vectors, subtract the ideal distance and
	  multiply by the force constant and put the result back onto the stack
	*/
	instruction(distance,NO_ARGS)
	/*! Pull three vectors off the top of the stack, an ideal distance and a force constant
	  calculate the angle between the vectors, subtract the ideal angle and
	  multiply by the force constant and put the result back onto the stack
	*/
	instruction(angle,NO_ARGS)
	/*! Pull four vectors off the top of the stack, an absolute dihedral angle and a force constant
	  calculate the dihedral between the vectors, subtract the ideal dihedral and
	  multiply by the force constant and put the result back onto the stack
	*/
	instruction(absoluteDihedral,NO_ARGS)
	/*! Pull four vectors off of the top of the stack, a MULTiplicity, a PHASE and a SCALE.
	  Calculate the dihedral angle and SCALE*(1+cos(DIHEDRAL*MULT+PHASE))
	  and put it back onto the stack.
	*/
	instruction(periodicDihedral,NO_ARGS)

	/*! Score intramolecularVdw */
	instruction(intramolecularVdw,ARG1provider)


	/* To calculate intermolecular vdw collisions, two
	providers are required, the first one is pushed onto
	the stack with this command and the second
	is provided by the intermolecularVdw command
	*/
	instruction(pushIntermolecularVdwProvider,ARG1provider)
	/* Calculate intermolecularVdwCollisions by taking
	one provider off of the alias stack and check for
	collisions with the atoms of the provider given as an argument
	here
	*/
	instruction(intermolecularVdw,ARG1provider)

	/*! Clear the MoveablePoints from the superposer in the Superposer register */
	instruction(clearSuperposerMoveable,ARG1provider)
	/*! Clear the FixedPoints from the superposer in the Superposer register */
	instruction(clearSuperposerFixed,ARG1provider)

	/*! Store a fixed superposition point into the superposer in the Superposer register */
	instruction(appendAliasToSuperposerFixed,ARG1provider)
	/*! Store a moveable superposition point into the superposer in the Superposer register */
	instruction(appendAliasToSuperposerMoveable,ARG1provider)

	/*! Carry out a superposition and write the transform and the RMS deviation into the AliasReferencerHolder indicated by _ArrayId */
	instruction(superpose,ARG1provider)

	/*!  Push the superposer score onto the stack
	  Command: sop_pushSuperposeScore-superposerIdx-0000
	*/
	instruction(pushSuperposeScore,ARG1provider)
	instruction(recordScorerState,NO_ARGS)
	instruction(recordScoreIfBest,NO_ARGS)

	/*! Update the points for all static providers */
	instruction(synchronizeWithStaticProviders,NO_ARGS)

	/*! Update all the points for the dynamic providers */
	instruction(synchronizeWithDynamicProviders,NO_ARGS)
	instruction(debug,NO_ARGS)
	instruction(end,NO_ARGS)


#ifdef	SVMInstruction_Enum
/* Include this in the scorerVirtualMachine.h header file */
undivert(fEnum)
#endif
#ifdef	SVMInstruction_Code
/* Include this in the initializeGlobals function of Scorer_OVirtualMachine */
undivert(fCode)
#endif
#ifdef	SVMInstruction_Friends
undivert(fFriends)
#endif
