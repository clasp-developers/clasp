/*
    File: scorerBase.h
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See file 'clasp/Copyright' for full details.
 
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/
/* -^- */
#ifndef	ScorerBase_H //[
#define ScorerBase_H


#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include "foundation.h"
#include "object.h"
#include "matrix.h"
#include "holder.h"
#include "rejector.h"

namespace mbb 
{

SMART(Builder);
SMART(PointProvider);
SMART(BuilderState);
SMART(RenderDisplayList);



#if DEBUG_SCORE_EVALUATION
#define LOG_SCORE(scorerState,str) 		\
	{									\
	    scorerState->scoreLog() << __FILE__<<"@"<<__LINE__ <<" "<< str << endl;\
	    LOG(str);				\
	}
#else
#define LOG_SCORE(scorerState,str) 	LOG(str);
#endif




SMART(ScorerBase);
__BEGIN_CLASS_DEFINITION(MbbPackage,O_ScorerBase,ScorerBase,O_Object) // {
public:
	void initialize();
	void oldLispInitialize(RPKeyedArguments args, RPLisp);
	void archiveBase(RPNode node);
private:
    string	_ScorerName;
    RPObjectDictionary	_Data;
    	/*! The Render holds the graphics that are rendered for every
	 * structure that will be scored.  For instance, if you are scoring how
	 * well a structure matches a p53 helix. You can put the p53 helix into
	 * the BackgroundRender and it will be rendered once for every hit.
	 */
    RPRender	_BackgroundRender;
private:
		// Do not archive
	uint	_OligomerChangeCounter;
	uint	_SequenceChangeCounter;
public:

    string getName() { return this->_ScorerName; };

		/*! Virtual functions to handle superposing scorers
		 */
    virtual bool hasSuperposer() { return false;};
    virtual double calculateSuperposerSumOfSquaresOfDifferences(RPScorerState) {_OF(); SUBCLASS_MUST_IMPLEMENT();};
    virtual RPRenderDisplayList renderSuperposeDeviations(RPPointProvider builder, RPScorerState scorerState) {_OF();SUBCLASS_MUST_IMPLEMENT();};

		/*! Return true if this scorer needs all of the
		 * atoms of the model built and false if it
		 * works with just the "interesting" atoms
		 */
    virtual bool needsAllAtomsBuilt();

    virtual RPScorerState createState();

    RPObjectDictionary getData();

    bool canRender() { return true;};
    RPRender rendered(RPKeyedArguments options);

	void setBackgroundRender(RPRender graphics) { this->_BackgroundRender = graphics; };
	RPRender getBackgroundRender() { return this->_BackgroundRender;};

	/*! Called whenever the oligomer of the builder changes the oligomer
	 */
    virtual void oligomerChanged(RPPointProvider builder);

	/*! Called whenever the sequence of the builder changes
	 */
    virtual void sequenceChanged(RPPointProvider builder);

	/*! Called whenever the builder changes
	 * This just calls oligomerChanged and sequenceChanged
	 */
    virtual void builderChanged(RPPointProvider builder);


	/* Score the structure built by the builder.
	 * The lower the number, the better the score.
	 */
    virtual void evaluate(RPPointProvider builder, RPScorerState state);

	/* Score the structure built by the builder.
	 * The lower the number, the better the score.
	 * If the scorer is a superposing scorer then don't superpose
	 */
    virtual void evaluateWithoutSuperposing(RPPointProvider builder, RPScorerState state) {_OF();SUBCLASS_MUST_IMPLEMENT();};

    	/*! Evaluate the subscorer already specified in the builder
	 * including the builders ScorerIdentifier
	 * Don't change the builder ScoreTransform
	 */
    virtual void evaluateUsingGeneratorIdentifier(RPPointProvider builder, RPScorerState state) { this->evaluate(builder,state);};

//    void addRejector(RPRejector rejector);

	/*! This function gets the graphics that are rendered for
	 * each structure scored.  Every hit will have different graphics
	 * generated for it.  For instance, if you are superposing atoms
	 * on a bis-peptide to the p53 helix then you could draw yellow lines between
	 * the atoms on the bis-peptide structure that correspond to atoms on the p53 helix
	 * and return them here.
	 * NOTE: THIS WILL ONLY RETURN CORRECT RESULTS AFTER YOU CALL "score()"
	 */
    virtual RPRenderDisplayList getRenderForScore(RPPointProvider builder, RPScorerState scorerState);


	/*! Insert restraints that emulate the scorer into
	 * the restraint list
	 */
    virtual void insertScoreOperationsIntoRestraintList(RPPointProvider builder, RPCons& restraints ) {_OF();SUBCLASS_MUST_IMPLEMENT();};

	/*! Create a list of restraints that emulate the scorer
	 */
    virtual RPCons createRestraints(RPPointProvider builder) {_OF();SUBCLASS_MUST_IMPLEMENT();};


	/*! Store the OligomerChangeCounter of the builder
	 */
    void storeOligomerChangeCounter(RPPointProvider builder);
	/*! Store the SequenceChangeCounter of the builder
	 */
    void storeSequenceChangeCounter(RPPointProvider builder);
    	/*! If our stored OligomerChangeCounter or
	 * SequenceChangeCounter doesn't match that of the
	 * builder then throw an error.  This is used to trap instances
	 * where the Scorer gets out of sync with the Builder.
	 */
    void throwIfBuilderDoesntMatch(RPPointProvider builder);
    void throwIfOligomerChangeCounterDoesntMatch(RPPointProvider builder);
    void throwIfSequenceChangeCounterDoesntMatch(RPPointProvider builder);

       O_ScorerBase( const O_ScorerBase& ss ); //!< Copy constructor


__END_CLASS_DEFINITION(O_ScorerBase) //}




SMART(BuildAllSelectAll);
__BEGIN_CLASS_DEFINITION(MbbPackage,O_BuildAllSelectAll,BuildAllSelectAll,O_ScorerBase) // {

public:
	void initialize();
	void oldLispInitialize(RPKeyedArguments args, RPLisp);
public:
    virtual bool needsAllAtomsBuilt() { return true;};

	/* Every conformation gets a score of 0.0
	 * and reject is set to false.
	 */
    virtual void evaluate(RPPointProvider builder, RPScorerState state);

       O_BuildAllSelectAll( const O_BuildAllSelectAll& ss ); //!< Copy constructor

__END_CLASS_DEFINITION(O_BuildAllSelectAll) //}



};


#endif //]
