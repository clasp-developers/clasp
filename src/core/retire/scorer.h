/*
    File: scorer.h
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See directory 'clasp/licenses' for full details.
 
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
       
       
#ifndef	Scorer_H //[
#define Scorer_H



#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include "foundation.h"
#include "object.h"
#include "holder.h"
#include "scorerBase.h"
#include "vector3.h"


namespace mbb {

SMART(Alias);
SMART(SuperposeEngine);
SMART(ScoreOperation);
SMART(RenderDisplayList);

SMART(SuperposeAlias);
__BEGIN_CLASS_DEFINITION(MbbPackage,O_SuperposeAlias,SuperposeAlias,O_Object) // {
public:
	void initialize();
	void oldLispInitialize(RPKeyedArguments args, RPLisp);
public:
	void	archiveBase(RPNode node);
private:
	RPAlias	_Alias;
	Vector3	_FixedPosition;
public:
private:	// Do not archive these temporary variables
	int	_MonomerId;
	int	_AtomId;


public:
	void	set(RPAlias alias, const Vector3& position);
	void	setAlias(RPAlias nm) {this->_Alias = nm;};
	RPAlias getAlias() { return this->_Alias;};
	void	setPosition(const Vector3& val) { this->_FixedPosition = val; };
	Vector3	getPosition() { return this->_FixedPosition; };

	void	setMonomerId(int i) { this->_MonomerId = i; };
	int getMonomerId() { return this->_MonomerId; };
	void	setAtomId(int i) { this->_AtomId = i; };
	int getAtomId() { return this->_AtomId; };



__END_CLASS_DEFINITION(O_SuperposeAlias) //}




SMART(ScoreSum);
SMART(Scorer);
__BEGIN_CLASS_DEFINITION(MbbPackage,O_Scorer,Scorer,O_ScorerBase) // {
public:
	void initialize();
	void oldLispInitialize(RPKeyedArguments args, RPLisp);
public:
	void	archiveBase(RPNode node);
private:
	uint			_ScorerIdentifier;
	List<O_SuperposeAlias>	_Superposes;
	RPScoreSum		_ScoreCalculator;
private:	// do not archive
	RPSuperposeEngine		_SuperposeEngine;
private:
	void	_initializeSuperposeEngine();
	void _doSuperpose(RPPointProvider builder, Matrix& transformToFixed);
    	void _evaluate(RPPointProvider builder, RPScorerState scorerState, bool superpose);
public:

	bool hasSuperposer();
	double calculateSuperposerSumOfSquaresOfDifferences(RPScorerState scorerState);

	void addToCalculator(RPScoreOperation);

	uint getScorerIdentifier();
	void setScorerIdentifier(uint id);
    virtual bool needsAllAtomsBuilt();


    virtual void oligomerChanged(RPPointProvider builder);
    virtual void sequenceChanged(RPPointProvider builder);

    virtual void evaluate(RPPointProvider builder, RPScorerState scorerState);
    virtual void evaluateWithoutSuperposing(RPPointProvider builder, RPScorerState scorerState);
    virtual RPRenderDisplayList getRenderForScore(RPPointProvider builder, RPScorerState scorerState);

#if 0
	/* Return a Matrix that transforms the scored structure
	 * into an appropriate frame of reference
	 */
    virtual Matrix getTransformForScore();
    virtual Matrix& transformForScore();
#endif 

    RPRenderDisplayList renderSuperposeDeviations(RPPointProvider builder, RPScorerState scorerState);

    void addSuperposeAlias(RPSuperposeAlias entry) { this->_Superposes.append(entry);};

		/*! Generate a list of restraints
		 * that anchor the atoms to the positions
		 * used in the Scorer
		 */
    void insertSuperpositionsIntoRestraintList(double anchorWeight, RPPointProvider builder, RPCons& restraints );
    		
		/*! Generate a list of restraints
		 * that emulate the score operations
		 * used in the Scorer
		 */
    void insertScoreOperationsIntoRestraintList(RPPointProvider builder, RPCons& restraints );

		/*!Return a list of restraints that emulate this scorer
		 */
    RPCons createRestraints(RPPointProvider builder);



__END_CLASS_DEFINITION(O_Scorer) //}




RPObject prim_superpose(RPExecutable e, RPCons exp, RPLisp lisp );
RPObject prim_setScorer(RPExecutable e, RPCons exp, RPLisp lisp );



};
#endif //]
