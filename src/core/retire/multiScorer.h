/*
    File: multiScorer.h
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
#ifndef	MultiScorer_H //[
#define MultiScorer_H



#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include "foundation.h"
#include "object.h"
#include "holder.h"
#include "scorerBase.h"
#include "alias.h"


namespace mbb {




SMART(Scorer);
SMART(ScoreOperation);
SMART(ScoreSum);






SMART(MultiScorer);
__BEGIN_CLASS_DEFINITION(MbbPackage,O_MultiScorer,MultiScorer,O_ScorerBase) // {
public:
	void	initialize();
	void	archiveBase(RPNode node);
	void	oldLispInitialize(RPKeyedArguments args, RPLisp);

private:
	List<O_Scorer>	_SubScorers;
	RPScoreSum	_MultiScoreCalculator;
public:
    static RPObject prim_setMultiScorer(RPExecutable e, RPCons exp, RPLisp);
private:
	double evaluateGeometryScore(RPScorer scorer);
    void _evaluate(RPPointProvider builder, RPScorerState state,bool superpose);
public:

    void setBuilder(RPPointProvider builder);
    void addScorer(RPScorer scorer);

    RPScorerBase getSubScorer(uint idx);

    void addToCalculatorsOfAllScorers(RPScoreOperation op);
    void addToMultiScorerCalculator(RPScoreOperation op);

    virtual void oligomerChanged(RPPointProvider builder);
    virtual void sequenceChanged(RPPointProvider builder);

	/*! Try each subscorer on the builder and pick the best one
	 * This will change the builders ScoreTransform
	 */
    virtual void evaluate(RPPointProvider builder, RPScorerState state);
    virtual void evaluateWithoutSuperposing(RPPointProvider builder, RPScorerState scorerState);


    	/*! Evaluate the subscorer already specified in the scorerState
	 */
    virtual void evaluateUsingGeneratorIdentifier(RPPointProvider builder, RPScorerState state);

    virtual bool needsAllAtomsBuilt();
    virtual RPScorerState createState();
    virtual RPRenderDisplayList getRenderForScore(RPPointProvider builder, RPScorerState scorerState);

		/*! Generate a list of restraints
		 * that emulate the score operations
		 * used in the Scorer
		 */
    void insertScoreOperationsIntoRestraintList(RPPointProvider builder, RPCons& restraints );

		/*!Return a list of restraints that emulate this scorer
		 */
    RPCons createRestraints(RPPointProvider builder);



__END_CLASS_DEFINITION(O_MultiScorer) //}


};
#endif //]
