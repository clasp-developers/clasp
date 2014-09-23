/*
    File: scoreOperations.cc
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
#define	DEBUG_LEVEL_FULL

#include "scoreOperations.h"
#include "archiveNode.h"
#include "archive.h"
#include "builder.h"
#include "alias.h"
#include "keyedArguments.h"
#include "ovector3.h"
#include "omatrix.h"
#include "scorer.h"
#include "render.h"
#include "rejector.h"
#include "pointProvider.h"
#include "vdwCollisionRejector.h"
#include "scorerState.h"
#include "wrappers.h"

namespace mbb {

void	O_ScoreOperation::initialize()
{
    this->_Scale = 1.0;
}


void	O_ScoreOperation::archiveBase(RPNode node)
{_G();
    node->attributeIfNotDefault("overallScale",this->_Scale,1.0);
}



void	O_ScoreOperation::oldLispInitialize(RPKeyedArguments kargs, RPLisp env)
{
    this->Base::oldLispInitialize(kargs,env);
    this->_Scale = kargs->getDoubleAndRemoveOrDefault("scale",1.0);
}


void	O_ScoreList::oldLispInitialize(RPKeyedArguments arg, RPLisp env)
{
    this->Base::oldLispInitialize(arg,env);
    for ( uint i = 0; i<arg->numberOfPositionalArguments(); i++ )
    {
	RPScoreOperation s = arg->getPositionalArgument(i)->as<O_ScoreOperation>();
	this->addOperation(s);
    }
}


void	O_ScoreList::archiveBase(RPNode node)
{
    this->Base::archiveBase(node);
    node->archiveVector0("operations",this->_Operations);
}




void	O_ScoreList::oligomerChanged(RPPointProvider builder)
{_G();
    ASSERT(builder->notNil());
    List<O_ScoreOperation>::iterator	it;
    for ( it=this->_Operations.begin(); it!=this->_Operations.end(); it++ )
    {
	(*it)->oligomerChanged(builder);
    }
}

void	O_ScoreList::sequenceChanged(RPPointProvider builder)
{_G();
    ASSERT(builder->notNil());
    List<O_ScoreOperation>::iterator	it;
    for ( it=this->_Operations.begin(); it!=this->_Operations.end(); it++ )
    {
	(*it)->sequenceChanged(builder);
    }
}



bool O_ScoreList::needsAllAtomsBuilt()
{_G();
    bool req = false;
    List<O_ScoreOperation>::iterator	it;
    for ( it=this->_Operations.begin(); it!=this->_Operations.end(); it++ )
    {
	req |= (*it)->needsAllAtomsBuilt();
    }
    return req;
}





void	O_ScoreList::renderIntoDisplayList(RPRenderDisplayList dl, RPScorerState scorerState, RPScorerBase scorer, RPPointProvider builder )
{_G();
    ASSERT(builder->notNil());
    List<O_ScoreOperation>::iterator	it;
    for ( it=this->_Operations.begin(); it!=this->_Operations.end(); it++ )
    {
	(*it)->renderIntoDisplayList(dl,scorerState,scorer,builder);
    }
}

void	O_ScoreList::addOperation(RPScoreOperation b)
{_OF();
    ASSERT(b->notNil());
    this->_Operations.push_back(b);
}


void	O_ScoreSum::oldLispInitialize(RPKeyedArguments arg, RPLisp env)
{
    this->Base::oldLispInitialize(arg,env);
}

void	O_ScoreSum::archiveBase(RPNode node)
{
    this->Base::archiveBase(node);
}


double	O_ScoreSum::evaluate(RPScorerState scorerState, RPScorerBase scorer, RPPointProvider builder)
{_G();
/*    ASSERT_NOT_NULL(scorerState); */
    ASSERT_NOT_NULL(scorer);
    ASSERT_NOT_NULL(builder);
    ASSERT(scorer->notNil());
    ASSERT(builder->notNil());
    double sum = 0.0;
    LOG_SCORE(scorerState, BF("O_ScoreSum {"));
    List<O_ScoreOperation>::iterator	it;
    for ( it=this->_Operations.begin(); it!=this->_Operations.end(); it++ )
    {_BLOCK_TRACE("Getting one score for sum");
	double oneSum = (*it)->evaluate(scorerState,scorer,builder);
	sum += oneSum;
	LOG_SCORE( scorerState,BF( "O_ScoreSum: summing current oneScore=%lf  sum= %lf")% oneSum% sum );
    }
    double result = sum*this->getScale();
    LOG_SCORE( scorerState,BF( "O_ScoreSum: Returning summed score %lf")% result );
    LOG_SCORE(scorerState, BF("O_ScoreSum }"));
    return result;
}



void O_ScoreSum::insertRestraints(RPPointProvider builder, RPScorerBase scorer, RPCons& restraints)
{_G();
    List<O_ScoreOperation>::iterator	it;
    for ( it=this->_Operations.begin(); it!=this->_Operations.end(); it++ )
    {_BLOCK_TRACE("Generating restraint for one entry of ScoreSum");
        (*it)->insertRestraints(builder,scorer,restraints);
    }
}



void	O_ScoreMin::oldLispInitialize(RPKeyedArguments arg, RPLisp env)
{
    this->Base::oldLispInitialize(arg,env);
}



void	O_ScoreMin::archiveBase(RPNode node)
{
    this->Base::archiveBase(node);
}


double	O_ScoreMin::evaluate(RPScorerState scorerState, RPScorerBase scorer, RPPointProvider builder)
{_G();
//    ASSERT_NOT_NULL(scorerState);
    ASSERT_NOT_NULL(scorer);
    ASSERT_NOT_NULL(builder);
    ASSERT(scorer->notNil());
    ASSERT(builder->notNil());
    bool gotMin = false;
    double min = 0.0;
    List<O_ScoreOperation>::iterator	it;
    for ( it=this->_Operations.begin(); it!=this->_Operations.end(); it++ )
    {
	double val = (*it)->evaluate(scorerState,scorer,builder);
	if ( !gotMin )
	{
	    gotMin = true;
	    min = val;
	} else
	{
	    if ( val < min ) min = val;
	}
    }
    return min*this->getScale();
}


void O_ScoreMin::insertRestraints(RPPointProvider builder, RPScorerBase scorer, RPCons& restraints)
{_G();
    List<O_ScoreOperation>::iterator	it, minIt;
    double minScore = 9.9e99;
    RPScorerState scorerState = O_ScorerState::create(this->lisp());
    minIt = this->_Operations.end();
    for ( it=this->_Operations.begin(); it!=this->_Operations.end(); it++ )
    {_BLOCK_TRACE("Calculating score for one entry of ScoreMin");
	double score = this->evaluate(scorerState, scorer, builder);
	if ( score < minScore )
	{
	    minScore = score;
	    minIt = it;
	}
        (*it)->insertRestraints(builder,scorer,restraints);
    }
    if ( minIt != this->_Operations.end() )
    {
	(*minIt)->insertRestraints(builder,scorer,restraints);
    }
}




void O_ScoreSuperposition::initialize()
{
    this->Base::initialize();
    this->_AnchorWeight = 1.0;
}


void O_ScoreSuperposition::archiveBase(RPNode node)
{
    this->Base::archiveBase(node);
    node->attributeIfNotDefault("anchorWeight",this->_AnchorWeight,1.0);
}


void	O_ScoreSuperposition::oldLispInitialize(RPKeyedArguments arg, RPLisp env)
{
    this->Base::oldLispInitialize(arg,env);
    this->_AnchorWeight = arg->getDoubleAndRemoveOrDefault("anchorWeight",1.0);
}


double	O_ScoreSuperposition::evaluate(RPScorerState scorerState, RPScorerBase scorer, RPPointProvider builder)
{_OF();
    LOG_SCORE(scorerState,BF("O_ScoreSuperposition {"))
    if ( !scorer->hasSuperposer() )
    {
	THROW(_lisp->create<O_LispError>("You are asking to score the superposition but you haven't defined a superposer"));
    }
    double result = scorer->calculateSuperposerSumOfSquaresOfDifferences(scorerState)*this->getScale();
    LOG_SCORE(scorerState,BF("O_ScoreSuperposition score after applying scale(%lf) = %lf")% this->getScale() % result);
    LOG_SCORE(scorerState,BF("O_ScoreSuperposition }"))
    return result;
}


void	O_ScoreSuperposition::oligomerChanged(RPPointProvider builder)
{_G();
    ASSERT(builder->notNil());
    // Don't have to do anything
}

void	O_ScoreSuperposition::sequenceChanged(RPPointProvider builder)
{_G();
    ASSERT(builder->notNil());
    // Don't have to do anything
}


void	O_ScoreSuperposition::renderIntoDisplayList( RPRenderDisplayList dl, RPScorerState scorerState, 
							RPScorerBase scorer, RPPointProvider builder )
{
    RPRender deviations = scorer->renderSuperposeDeviations(builder,scorerState);
    dl->add(deviations);
}

void O_ScoreSuperposition::insertRestraints(RPPointProvider builder, RPScorerBase scorerBase, RPCons& restraints)
{_G();
    RPScorer scorer = scorerBase->as<O_Scorer>();
    scorer->insertSuperpositionsIntoRestraintList(this->_AnchorWeight,builder,restraints);
}









void	O_ScoreDistance::oldLispInitialize(RPKeyedArguments args, RPLisp env)
{_OF();
    ASSERT_eq(args->numberOfPositionalArguments(),2);
    this->_Alias1 = args->getPositionalArgument(0)->as<O_Alias>();
    this->_Alias2 = args->getPositionalArgument(1)->as<O_Alias>();
    this->_Distance = args->getDoubleAndRemove("distance");
    this->_Force = args->getDoubleAndRemoveOrDefault("force",1.0);
}


void	O_ScoreDistance::archiveBase(RPNode node)
{
    this->Base::archiveBase(node);
    node->archiveObject("alias1",this->_Alias1);
    node->archiveObject("alias2",this->_Alias2);
    node->attribute("distance",this->_Distance);
    node->attribute("force",this->_Force);
    node->attribute("comment",this->_Comment);
}


void	O_ScoreDistance::oligomerChanged(RPPointProvider builder)
{_G();
    ASSERT(builder->notNil());
    this->_Alias1->oligomerChanged(builder);
    this->_Alias2->oligomerChanged(builder);
}

void	O_ScoreDistance::sequenceChanged(RPPointProvider builder)
{_G();
    ASSERT(builder->notNil());
    this->_Alias1->sequenceChanged(builder);
    this->_Alias2->sequenceChanged(builder);
}


double O_ScoreDistance::evaluate(RPScorerState scorerState, RPScorerBase scorer, RPPointProvider builder)
{_G();
    ASSERT(scorer->notNil());
    ASSERT(builder->notNil());
    LOG_SCORE(scorerState,BF("O_ScoreDistance {"))
    Vector3 pos1 = this->_Alias1->getUntransformedAtomPosition(builder,scorerState);
    Vector3 pos2 = this->_Alias2->getUntransformedAtomPosition(builder,scorerState);
    LOG_SCORE(scorerState,BF("ScoreDistance %s pos1 = %s")% this->_Alias1->description()% pos1.asString() );
    LOG_SCORE(scorerState,BF("ScoreDistance %s pos2 = %s")% this->_Alias2->description()% pos2.asString() );
    Vector3 diff = pos1.sub(pos2);
    double dist = diff.length();
    LOG_SCORE(scorerState,BF(" dist = %lf")% dist );
    double d = dist-this->getDistance();
    double res = d*d*this->getForce();
    LOG_SCORE(scorerState,BF(" res = %lf")% res );
    double retres = res*this->getScale();
    LOG_SCORE(scorerState,BF(" res*scale = %lf")% retres );
    LOG_SCORE(scorerState,BF("O_ScoreDistance }"))
    return retres;
}

void	O_ScoreDistance::renderIntoDisplayList( RPRenderDisplayList dl, RPScorerState scorerState, RPScorerBase scorer, RPPointProvider builder )
{_G();
    ASSERT(builder->notNil());
    Vector3 pos1 = this->_Alias1->getUntransformedAtomPosition(builder,scorerState);
    Vector3 pos2 = this->_Alias2->getUntransformedAtomPosition(builder,scorerState);
    LOG(BF(" pos1 = %s") % pos1.asString().c_str()  ); // vp0(( " pos1 = %s", pos1.asString().c_str() ));
    LOG(BF(" pos2 = %s") % pos2.asString().c_str()  ); // vp0(( " pos2 = %s", pos2.asString().c_str() ));
    		// Given pos1, pos2, distance, render a deviation
    renderDistanceDeviation(dl,pos1,pos2,this->getDistance(),O_GrColor::systemColor(this->lisp()->symbol(_sym_kw_yellow)));
}















void	O_ScoreAngle::oldLispInitialize(RPKeyedArguments args, RPLisp env)
{_OF();
    ASSERT_eq(args->numberOfPositionalArguments(),3);
    this->_Alias1 = args->getPositionalArgument(0)->as<O_Alias>();
    this->_Alias2 = args->getPositionalArgument(1)->as<O_Alias>();
    this->_Alias3 = args->getPositionalArgument(2)->as<O_Alias>();
    this->_Radians = args->getDoubleAndRemove("degrees")*0.0174533;
    this->_Force = args->getDoubleAndRemoveOrDefault("force",0.1);
}


void	O_ScoreAngle::archiveBase(RPNode node)
{
    this->Base::archiveBase(node);
    node->archiveObject("alias1",this->_Alias1);
    node->archiveObject("alias2",this->_Alias2);
    node->archiveObject("alias3",this->_Alias3);
    node->attribute("radians",this->_Radians);
    node->attribute("force",this->_Force);
    node->attribute("comment",this->_Comment);
}


void	O_ScoreAngle::oligomerChanged(RPPointProvider builder)
{_G();
    ASSERT(builder->notNil());
    this->_Alias1->oligomerChanged(builder);
    this->_Alias2->oligomerChanged(builder);
    this->_Alias3->oligomerChanged(builder);
}

void	O_ScoreAngle::sequenceChanged(RPPointProvider builder)
{_G();
    ASSERT(builder->notNil());
    this->_Alias1->sequenceChanged(builder);
    this->_Alias2->sequenceChanged(builder);
    this->_Alias3->sequenceChanged(builder);
}


double O_ScoreAngle::evaluate(RPScorerState scorerState, RPScorerBase scorer, RPPointProvider builder)
{_G();
    ASSERT(scorer->notNil());
    ASSERT(builder->notNil());
    Vector3 pos1 = this->_Alias1->getUntransformedAtomPosition(builder,scorerState);
    Vector3 pos2 = this->_Alias2->getUntransformedAtomPosition(builder,scorerState);
    Vector3 pos3 = this->_Alias3->getUntransformedAtomPosition(builder,scorerState);
    LOG(BF(" pos1 = %s") % pos1.asString().c_str()  ); // vp0(( " pos1 = %s", pos1.asString().c_str() ));
    LOG(BF(" pos2 = %s") % pos2.asString().c_str()  ); // vp0(( " pos2 = %s", pos2.asString().c_str() ));
    LOG(BF(" pos3 = %s") % pos3.asString().c_str()  ); // vp0(( " pos3 = %s", pos3.asString().c_str() ));
    double angle = calculateAngle(pos1,pos2,pos3,_lisp);
    LOG(BF(" angle = %lf") % angle ); // vp0(( " angle = %lf", angle));
    double d = angle-this->getRadians();
    double res = d*d*this->getForce();
    LOG(BF(" res = %lf") % res  ); // vp0(( " res = %lf", res ));
    return res*this->getScale();
}

void	O_ScoreAngle::renderIntoDisplayList( RPRenderDisplayList dl, RPScorerState scorerState, RPScorerBase scorer, RPPointProvider builder )
{_G();
    ASSERT(builder->notNil());
//    LOG(BF("scorerState->scoreTransform() = %s") % scorerState->scoreTransform().asString().c_str()  ); // vp0(( "scorerState->scoreTransform() = %s", scorerState->scoreTransform().asString().c_str() ));
    Vector3 pos1 = this->_Alias1->getUntransformedAtomPosition(builder,scorerState);
    Vector3 pos2 = this->_Alias2->getUntransformedAtomPosition(builder,scorerState);
    Vector3 pos3 = this->_Alias3->getUntransformedAtomPosition(builder,scorerState);
    LOG(BF(" pos1 = %s") % pos1.asString().c_str()  ); // vp0(( " pos1 = %s", pos1.asString().c_str() ));
    LOG(BF(" pos2 = %s") % pos2.asString().c_str()  ); // vp0(( " pos2 = %s", pos2.asString().c_str() ));
    LOG(BF(" pos3 = %s") % pos3.asString().c_str()  ); // vp0(( " pos3 = %s", pos3.asString().c_str() ));
    renderAngleDeviation(dl,pos1,pos2,pos3,this->getRadians());
}


















void	O_ScoreDihedral::oldLispInitialize(RPKeyedArguments args, RPLisp env)
{_OF();
    ASSERT_eq(args->numberOfPositionalArguments(),4);
    this->_Alias1 = args->getPositionalArgument(0)->as<O_Alias>();
    this->_Alias2 = args->getPositionalArgument(1)->as<O_Alias>();
    this->_Alias3 = args->getPositionalArgument(2)->as<O_Alias>();
    this->_Alias4 = args->getPositionalArgument(3)->as<O_Alias>();
    this->_Radians = args->getDoubleAndRemove("degrees")*0.0174533;
    this->_Force = args->getDoubleAndRemoveOrDefault("force",0.1);
}


void	O_ScoreDihedral::archiveBase(RPNode node)
{
    this->Base::archiveBase(node);
    node->archiveObject("alias1",this->_Alias1);
    node->archiveObject("alias2",this->_Alias2);
    node->archiveObject("alias3",this->_Alias3);
    node->archiveObject("alias4",this->_Alias4);
    node->attribute("radians",this->_Radians);
    node->attribute("force",this->_Force);
    node->attribute("comment",this->_Comment);
}


void	O_ScoreDihedral::oligomerChanged(RPPointProvider builder)
{_G();
    ASSERT(builder->notNil());
    this->_Alias1->oligomerChanged(builder);
    this->_Alias2->oligomerChanged(builder);
    this->_Alias3->oligomerChanged(builder);
    this->_Alias4->oligomerChanged(builder);
}

void	O_ScoreDihedral::sequenceChanged(RPPointProvider builder)
{_G();
    ASSERT(builder->notNil());
    this->_Alias1->sequenceChanged(builder);
    this->_Alias2->sequenceChanged(builder);
    this->_Alias3->sequenceChanged(builder);
    this->_Alias4->sequenceChanged(builder);
}


double O_ScoreDihedral::evaluate(RPScorerState scorerState, RPScorerBase scorer, RPPointProvider builder)
{_G();
    ASSERT(scorer->notNil());
    ASSERT(builder->notNil());
    Vector3 pos1 = this->_Alias1->getUntransformedAtomPosition(builder,scorerState);
    Vector3 pos2 = this->_Alias2->getUntransformedAtomPosition(builder,scorerState);
    Vector3 pos3 = this->_Alias3->getUntransformedAtomPosition(builder,scorerState);
    Vector3 pos4 = this->_Alias4->getUntransformedAtomPosition(builder,scorerState);
    LOG(BF(" pos1 = %s") % pos1.asString().c_str()  ); // vp0(( " pos1 = %s", pos1.asString().c_str() ));
    LOG(BF(" pos2 = %s") % pos2.asString().c_str()  ); // vp0(( " pos2 = %s", pos2.asString().c_str() ));
    LOG(BF(" pos3 = %s") % pos3.asString().c_str()  ); // vp0(( " pos3 = %s", pos3.asString().c_str() ));
    LOG(BF(" pos4 = %s") % pos4.asString().c_str()  ); // vp0(( " pos4 = %s", pos4.asString().c_str() ));
    double angle = calculateDihedral(pos1,pos2,pos3,pos4,_lisp);
    LOG(BF(" angle = %lf") % angle  ); // vp0(( " angle = %lf", angle ));
    double d = angle-this->getRadians();
    double res = d*d*this->getForce();
    LOG(BF(" res = %lf") % res  ); // vp0(( " res = %lf", res ));
    return res*this->getScale();
}

void	O_ScoreDihedral::renderIntoDisplayList( RPRenderDisplayList dl, RPScorerState scorerState, RPScorerBase scorer, RPPointProvider builder )
{_G();
    ASSERT(builder->notNil());
//    LOG(BF("scorerState->scoreTransform() = %s") % scorerState->scoreTransform().asString().c_str()  ); // vp0(( "scorerState->scoreTransform() = %s", scorerState->scoreTransform().asString().c_str() ));
    Vector3 pos1 = this->_Alias1->getUntransformedAtomPosition(builder,scorerState);
    Vector3 pos2 = this->_Alias2->getUntransformedAtomPosition(builder,scorerState);
    Vector3 pos3 = this->_Alias3->getUntransformedAtomPosition(builder,scorerState);
    Vector3 pos4 = this->_Alias4->getUntransformedAtomPosition(builder,scorerState);
    LOG(BF(" pos1 = %s") % pos1.asString().c_str()  ); // vp0(( " pos1 = %s", pos1.asString().c_str() ));
    LOG(BF(" pos2 = %s") % pos2.asString().c_str()  ); // vp0(( " pos2 = %s", pos2.asString().c_str() ));
    LOG(BF(" pos3 = %s") % pos3.asString().c_str()  ); // vp0(( " pos3 = %s", pos3.asString().c_str() ));
    LOG(BF(" pos4 = %s") % pos4.asString().c_str()  ); // vp0(( " pos4 = %s", pos4.asString().c_str() ));
    		// Given pos1, pos2, distance, render a deviation
    renderDihedralDeviation(dl,pos1,pos2,pos3,pos4,this->getRadians());
}













void	O_ScoreNormalizedDotProduct::oldLispInitialize(RPKeyedArguments args, RPLisp env)
{_OF();
    ASSERT_eq(args->numberOfPositionalArguments(),4);
    this->_Alias1 = args->getPositionalArgument(0)->as<O_Alias>();
    this->_Alias2 = args->getPositionalArgument(1)->as<O_Alias>();
    this->_Alias3 = args->getPositionalArgument(2)->as<O_Alias>();
    this->_Alias4 = args->getPositionalArgument(3)->as<O_Alias>();
    this->_NormalizedDotProduct = args->getDoubleAndRemove("dotProduct");
    this->_Force = args->getDoubleAndRemoveOrDefault("force",0.1);
}


void	O_ScoreNormalizedDotProduct::archiveBase(RPNode node)
{
    this->Base::archiveBase(node);
    node->archiveObject("alias1",this->_Alias1);
    node->archiveObject("alias2",this->_Alias2);
    node->archiveObject("alias3",this->_Alias3);
    node->archiveObject("alias4",this->_Alias4);
    node->attribute("dotProduct",this->_NormalizedDotProduct);
    node->attribute("force",this->_Force);
    node->attribute("comment",this->_Comment);
}


void	O_ScoreNormalizedDotProduct::oligomerChanged(RPPointProvider builder)
{_G();
    ASSERT(builder->notNil());
    this->_Alias1->oligomerChanged(builder);
    this->_Alias2->oligomerChanged(builder);
    this->_Alias3->oligomerChanged(builder);
    this->_Alias4->oligomerChanged(builder);
}

void	O_ScoreNormalizedDotProduct::sequenceChanged(RPPointProvider builder)
{_G();
    ASSERT(builder->notNil());
    this->_Alias1->sequenceChanged(builder);
    this->_Alias2->sequenceChanged(builder);
    this->_Alias3->sequenceChanged(builder);
    this->_Alias4->sequenceChanged(builder);
}


double O_ScoreNormalizedDotProduct::evaluate(RPScorerState scorerState, RPScorerBase scorer, RPPointProvider builder)
{_G();
    ASSERT(scorer->notNil());
    ASSERT(builder->notNil());
    Vector3 pos1 = this->_Alias1->getUntransformedAtomPosition(builder,scorerState);
    Vector3 pos2 = this->_Alias2->getUntransformedAtomPosition(builder,scorerState);
    Vector3 pos3 = this->_Alias3->getUntransformedAtomPosition(builder,scorerState);
    Vector3 pos4 = this->_Alias4->getUntransformedAtomPosition(builder,scorerState);
    LOG(BF(" pos1 = %s") % pos1.asString().c_str()  ); // vp0(( " pos1 = %s", pos1.asString().c_str() ));
    LOG(BF(" pos2 = %s") % pos2.asString().c_str()  ); // vp0(( " pos2 = %s", pos2.asString().c_str() ));
    LOG(BF(" pos3 = %s") % pos3.asString().c_str()  ); // vp0(( " pos3 = %s", pos3.asString().c_str() ));
    LOG(BF(" pos4 = %s") % pos4.asString().c_str()  ); // vp0(( " pos4 = %s", pos4.asString().c_str() ));
    Vector3 diff1 = (pos1.sub(pos2)).normalized(this->lisp());
    Vector3 diff2 = (pos3.sub(pos4)).normalized(this->lisp());
    double dot = diff1.dotProduct(diff2);
    double d = dot-this->getNormalizedDotProduct();
    double res = d*d*this->getForce();
    LOG(BF(" res = %lf") % res  ); // vp0(( " res = %lf", res ));
    return res*this->getScale();
}

void	O_ScoreNormalizedDotProduct::renderIntoDisplayList( RPRenderDisplayList dl, RPScorerState scorerState, RPScorerBase scorer, RPPointProvider builder )
{_OF();
    ASSERT(builder->notNil());
//    LOG(BF("scorerState->scoreTransform() = %s") % scorerState->scoreTransform().asString().c_str()  ); // vp0(( "scorerState->scoreTransform() = %s", scorerState->scoreTransform().asString().c_str() ));
    Vector3 pos1 = this->_Alias1->getUntransformedAtomPosition(builder,scorerState);
    Vector3 pos2 = this->_Alias2->getUntransformedAtomPosition(builder,scorerState);
    Vector3 pos3 = this->_Alias3->getUntransformedAtomPosition(builder,scorerState);
    Vector3 pos4 = this->_Alias4->getUntransformedAtomPosition(builder,scorerState);
    LOG(BF(" pos1 = %s") % pos1.asString().c_str()  ); // vp0(( " pos1 = %s", pos1.asString().c_str() ));
    LOG(BF(" pos2 = %s") % pos2.asString().c_str()  ); // vp0(( " pos2 = %s", pos2.asString().c_str() ));
    LOG(BF(" pos3 = %s") % pos3.asString().c_str()  ); // vp0(( " pos3 = %s", pos3.asString().c_str() ));
    LOG(BF(" pos4 = %s") % pos4.asString().c_str()  ); // vp0(( " pos4 = %s", pos4.asString().c_str() ));
    		// Given pos1, pos2, distance, render a deviation
    renderNormalizedDotProductDeviation(dl,pos1,pos2,pos3,pos4,this->getNormalizedDotProduct());
}























void	O_ScoreDistanceToPoint::oldLispInitialize(RPKeyedArguments args, RPLisp env)
{_OF();
    ASSERT_eq(args->numberOfPositionalArguments(),2);
    this->_Alias = args->getPositionalArgument(0)->as<O_Alias>();
    this->_Point = args->getPositionalArgument(1)->as<O_OVector3>()->get();
    this->_Distance = args->getDoubleAndRemoveOrDefault("distance",0.0);
    this->_Force = args->getDoubleAndRemoveOrDefault("force",1.0);
}


void	O_ScoreDistanceToPoint::archiveBase(RPNode node)
{
    this->Base::archiveBase(node);
    node->archiveObject("alias",this->_Alias);
    node->archivePlainObjectIfDefined<Vector3>( "pos","Vector3",
    					this->_Point.isDefined(), this->_Point);
    node->attribute("distance",this->_Distance);
    node->attribute("force",this->_Force);
    node->attribute("comment",this->_Comment);
}


void	O_ScoreDistanceToPoint::oligomerChanged(RPPointProvider builder)
{_G();
    ASSERT(builder->notNil());
    this->_Alias->oligomerChanged(builder);
}

void	O_ScoreDistanceToPoint::sequenceChanged(RPPointProvider builder)
{_G();
    ASSERT(builder->notNil());
    this->_Alias->sequenceChanged(builder);
}


double O_ScoreDistanceToPoint::evaluate(RPScorerState scorerState, RPScorerBase scorer, RPPointProvider builder)
{_G();
    ASSERT(scorer->notNil());
    ASSERT(builder->notNil());
    Vector3 pos = this->_Alias->getUntransformedAtomPosition(builder,scorerState);
    LOG(BF(" pos = %s") % pos.asString().c_str()  ); // vp0(( " pos = %s", pos.asString().c_str() ));
    Vector3 diff = pos.sub(this->getPoint());
    double dist = diff.length();
    LOG(BF(" dist = %lf") % dist  ); // vp0(( " dist = %lf", dist ));
    double d = dist-this->getDistance();
    double res = d*d*this->getForce();
    LOG(BF(" res = %lf") % res  ); // vp0(( " res = %lf", res ));
    return res*this->getScale();
}


void	O_ScoreDistanceToPoint::renderIntoDisplayList( RPRenderDisplayList dl, RPScorerState scorerState, RPScorerBase scorer, RPPointProvider builder )
{_OF();
    ASSERT(builder->notNil());
    Vector3 pos = this->_Alias->getUntransformedAtomPosition(builder,scorerState);
    LOG(BF(" pos = %s") % pos.asString().c_str()  ); // vp0(( " pos = %s", pos.asString().c_str() ));
    renderDistanceDeviation(dl,pos,this->getPoint(),this->getDistance(),O_GrColor::systemColor(this->lisp()->symbol(_sym_kw_yellow)));
}
















void	O_ScoreBestGeometry::oldLispInitialize(RPKeyedArguments args, RPLisp env)
{
    RPCons targets = args->getAndRemove("targetAtomAliases")->as<O_Cons>();
    this->_TargetAtomAliases.clear();
    this->_TargetAtomAliases.fillFromCons(targets);
    this->_Distance = args->getDoubleAndRemove("distance");
    this->_DistanceAlias = args->getAndRemove("distanceAlias")->as<O_Alias>();
    this->_AngleRadians = args->getDoubleAndRemoveOrDefault("angleDegrees",0.0)*0.0174533;
    this->_AngleAlias = args->getAndRemoveOrNil("angleAlias")->as<O_Alias>();
    this->_DihedralRadians = args->getDoubleAndRemoveOrDefault("dihedralDegrees",0.0)*0.0174533;
    this->_DihedralAlias = args->getAndRemoveOrNil("dihedralAlias")->as<O_Alias>();
    this->_DistanceScale = args->getDoubleAndRemoveOrDefault("distanceScale",1.0)*0.0174533;
    this->_AngleScale = args->getDoubleAndRemoveOrDefault("angleScale",1.0)*0.0174533;
    this->_DihedralScale = args->getDoubleAndRemoveOrDefault("dihedralScale",1.0)*0.0174533;
    this->_Comment = args->getStringAndRemoveOrDefault("comment","");
}

void	O_ScoreBestGeometry::initialize()
{
    this->Base::initialize();
    this->_Distance = 0.0;
    this->_AngleRadians = 0.0;
    this->_DihedralRadians= 0.0;
    this->_DistanceScale = 1.0;
    this->_AngleScale = 1.0;
    this->_DihedralScale = 1.0;
}

void	O_ScoreBestGeometry::archiveBase(RPNode node)
{
    this->Base::archiveBase(node);
    node->archiveVector0("targets",this->_TargetAtomAliases);
    node->archiveObject("DistanceAlias",this->_DistanceAlias);
    node->attribute("Distance",this->_Distance);
    node->archiveObject("AngleAlias",this->_AngleAlias);
    node->attributeIfNotDefault("Angle",this->_AngleRadians,0.0);
    node->archiveObject("DihedralAlias",this->_DihedralAlias);
    node->attributeIfNotDefault("Dihedral",this->_DihedralRadians,0.0);
    node->attributeIfNotDefault("DistanceScale",this->_DistanceScale,1.0);
    node->attributeIfNotDefault("AngleScale",this->_AngleScale,1.0);
    node->attributeIfNotDefault("DihedralScale",this->_DihedralScale,1.0);
    node->attributeIfNotDefault<string>("Comment",this->_Comment,"");
}


void	O_ScoreBestGeometry::oligomerChanged(RPPointProvider builder)
{_G();
    ASSERT(builder->notNil());
    List<O_Alias>::iterator it;
    for ( it=this->_TargetAtomAliases.begin(); it!=this->_TargetAtomAliases.end(); it++ )
    {
	(*it)->oligomerChanged(builder);
    }
    ASSERT(this->_DistanceAlias->notNil());
    this->_DistanceAlias->oligomerChanged(builder);
    if ( this->_AngleAlias->notNil() ) this->_AngleAlias->oligomerChanged(builder);
    if ( this->_DihedralAlias->notNil() ) this->_DihedralAlias->oligomerChanged(builder);
}

void	O_ScoreBestGeometry::sequenceChanged(RPPointProvider builder)
{_G();
    ASSERT(builder->notNil());
    List<O_Alias>::iterator it;
    for ( it=this->_TargetAtomAliases.begin(); it!=this->_TargetAtomAliases.end(); it++ )
    {
	(*it)->sequenceChanged(builder);
    }
    ASSERT(this->_DistanceAlias->notNil());
    this->_DistanceAlias->sequenceChanged(builder);
    if ( this->_AngleAlias->notNil() ) this->_AngleAlias->sequenceChanged(builder);
    if ( this->_DihedralAlias->notNil() ) this->_DihedralAlias->sequenceChanged(builder);
}




void O_ScoreBestGeometry::findBest(RPScorerState scorerState,
				RPScorerBase scorer, RPPointProvider builder,
				double& bestScore, RPAlias& bestAlias)
{_G();
    ASSERT(scorer->notNil());
    ASSERT(builder->notNil());
    bool gotAngle = false;
    bool gotDihedral = false;
    ASSERT(this->_DistanceAlias->notNil());
    Vector3 posDistance = this->_DistanceAlias->getUntransformedAtomPosition(builder,scorerState);
    LOG(BF("posDistance = %s") % posDistance.asString().c_str()  ); // vp0(( "posDistance = %s", posDistance.asString().c_str() ));
    Vector3 posAngle, posDihedral;
    if ( this->_AngleAlias->notNil() )
    {
	gotAngle = true;
	posAngle = this->_AngleAlias->getUntransformedAtomPosition(builder,scorerState);
        LOG(BF("posAngle = %s") % posAngle.asString().c_str()  ); // vp0(( "posAngle = %s", posAngle.asString().c_str() ));
	if ( this->_DihedralAlias->notNil() )
	{
	    gotDihedral = true;
	    posDihedral = this->_DihedralAlias->getUntransformedAtomPosition(builder,scorerState);
    	    LOG(BF("posDihedral = %s") % posDihedral.asString().c_str()  ); // vp0(( "posDihedral = %s", posDihedral.asString().c_str() ));
	}
    }
    bestScore = 987654321.0;
    RPAlias alias;
    List<O_Alias>::iterator it;
    double score;
    for ( it=this->_TargetAtomAliases.begin();
    		 it!=this->_TargetAtomAliases.end(); it++ )
    {
	alias = (*it);
	Vector3 targetPos = alias->getUntransformedAtomPosition(builder,scorerState);
	LOG(BF("targetPos = %s") % targetPos.asString().c_str()  ); // vp0(( "targetPos = %s", targetPos.asString().c_str() ));
	if ( gotDihedral )
	{
	    LOG(BF("Calculating best geometry based on Dihedral/Angle/Distance") ); // vp0(( "Calculating best geometry based on Dihedral/Angle/Distance"));
	    if ( alias->equal(this->_DistanceAlias) )
	    {
		THROW(_lisp->create<O_LispError>("The target alias("+alias->__repr__()+") is the same as Distance alias("+this->_DistanceAlias->__repr__()+")"));
	    }
	    if ( alias->equal(this->_AngleAlias) )
	    {
		THROW(_lisp->create<O_LispError>("The target alias("+alias->__repr__()+") is the same as Angle alias("+this->_AngleAlias->__repr__()+")"));
	    }
	    if ( alias->equal(this->_DihedralAlias) )
	    {
		THROW(_lisp->create<O_LispError>("The target alias("+alias->__repr__()+") is the same as Dihedral alias("+this->_DihedralAlias->__repr__()+")"));
	    }
	    double deltaDist = calculateDistance(targetPos,posDistance,_lisp) - this->_Distance;
	    LOG(BF("deltaDist = %lf") % deltaDist  ); // vp0(( "deltaDist = %lf", deltaDist ));
	    double deltaAngle = calculateAngle(targetPos,posDistance,posAngle,_lisp) - this->_AngleRadians;
	    LOG(BF("deltaAngle = %lf") % deltaAngle  ); // vp0(( "deltaAngle = %lf", deltaAngle ));
	    double deltaDihedral = calculateDihedral(targetPos,posDistance,posAngle,posDihedral,_lisp) - this->_DihedralRadians;
	    LOG(BF("deltaDihedral = %lf") % deltaDihedral  ); // vp0(( "deltaDihedral = %lf", deltaDihedral ));
	    score = deltaDist*deltaDist*this->_DistanceScale
	    		+deltaAngle*deltaAngle*this->_AngleScale
			+deltaDihedral*deltaDihedral*this->_DihedralScale;
	} else if ( gotAngle )
	{
	    LOG(BF("Calculating best geometry based on Angle/Distance") ); // vp0(( "Calculating best geometry based on Angle/Distance"));
	    if ( alias->equal(this->_DistanceAlias) )
	    {
		THROW(_lisp->create<O_LispError>("The target alias("+alias->__repr__()+") is the same as Distance alias("+this->_DistanceAlias->__repr__()+")"));
	    }
	    if ( alias->equal(this->_AngleAlias) )
	    {
		THROW(_lisp->create<O_LispError>("The target alias("+alias->__repr__()+") is the same as Angle alias("+this->_AngleAlias->__repr__()+")"));
	    }
	    double deltaDist = calculateDistance(targetPos,posDistance,_lisp) - this->_Distance;
	    LOG(BF("deltaDist = %lf") % deltaDist  ); // vp0(( "deltaDist = %lf", deltaDist ));
	    double deltaAngle = calculateAngle(targetPos,posDistance,posAngle,_lisp) - this->_AngleRadians;
	    LOG(BF("deltaAngle = %lf") % deltaAngle  ); // vp0(( "deltaAngle = %lf", deltaAngle ));
	    score = deltaDist*deltaDist*this->_DistanceScale
	    		+deltaAngle*deltaAngle*this->_AngleScale;
	} else
	{
	    LOG(BF("Calculating best geometry based on Distance") ); // vp0(( "Calculating best geometry based on Distance"));
	    if ( alias->equal(this->_DistanceAlias) )
	    {
		THROW(_lisp->create<O_LispError>("The target alias("+alias->__repr__()+") is the same as Distance alias("+this->_DistanceAlias->__repr__()+")"));
	    }
	    double deltaDist = calculateDistance(targetPos,posDistance,_lisp) - this->_Distance;
	    LOG(BF("deltaDist = %lf") % deltaDist  ); // vp0(( "deltaDist = %lf", deltaDist ));
	    score = deltaDist*deltaDist*this->_DistanceScale;
	}
	LOG(BF("bestScore = %lf   score = %lf") % bestScore % score  ); // vp0(("bestScore = %lf   score = %lf", bestScore, score ));
	if ( bestScore > score )
	{
	    LOG(BF("This is the best score sofar") ); // vp0(( "This is the best score sofar"));
	    bestScore = score;
	    bestAlias = alias;
	}
    }
}


double	O_ScoreBestGeometry::evaluate(RPScorerState scorerState, RPScorerBase scorer, RPPointProvider builder)
{_OF();
    RPAlias bestAlias;
    double bestScore;
    this->findBest(scorerState,scorer,builder,bestScore,bestAlias);
    double score = bestScore*this->getScale();
    LOG(BF("O_ScoreBestGeometry::evaluate returning score=%lf") % score  ); // vp0(("O_ScoreBestGeometry::evaluate returning score=%lf", score ));
    return score;
}


void	O_ScoreBestGeometry::renderIntoDisplayList( RPRenderDisplayList dl, RPScorerState scorerState, RPScorerBase scorer, RPPointProvider builder )
{_OF();
    ASSERT(builder->notNil());
    RPAlias bestAlias;
    double bestScore;
    this->findBest(scorerState,scorer,builder,bestScore,bestAlias);
    Vector3 bestPos = bestAlias->getUntransformedAtomPosition(builder,scorerState);
    Vector3 distancePos = this->_DistanceAlias->getUntransformedAtomPosition(builder,scorerState);
    LOG(BF(" distancePos = %s") % distancePos.asString().c_str()  ); // vp0(( " distancePos = %s", distancePos.asString().c_str() ));
    LOG(BF(" bestPos = %s") % bestPos.asString().c_str()  ); // vp0(( " bestPos = %s", bestPos.asString().c_str() ));
    renderDistanceDeviation(dl,distancePos,bestPos,this->_Distance,O_GrColor::systemColor(this->lisp()->symbol(_sym_kw_yellow)));
}















void	O_ScoreRejects::oldLispInitialize(RPKeyedArguments args, RPLisp env)
{_OF();
    this->_Rejector = args->getAndRemove("rejector")->as<O_Rejector>();
    ASSERT(this->_Rejector->notNil());
    this->_Scale = args->getDoubleAndRemoveOrDefault("scale",1.0);
}


void	O_ScoreRejects::initialize()
{
    this->Base::initialize();
    this->_Rejector = O_Rejector::nil(this->lisp());
    this->_Scale = 1.0;
}

void	O_ScoreRejects::archiveBase(RPNode node)
{
    this->Base::archiveBase(node);
    node->archiveObject("rejector",this->_Rejector);
    node->attribute("Scale",this->_Scale);
}


void	O_ScoreRejects::oligomerChanged(RPPointProvider builder)
{_G();
    ASSERT(builder->notNil());
    if ( this->_Rejector->notNil() )
    {
	this->_Rejector->oligomerChanged(builder);
    }
}

void	O_ScoreRejects::sequenceChanged(RPPointProvider builder)
{_G();
    ASSERT(builder->notNil());
    if ( this->_Rejector->notNil() )
    {
	this->_Rejector->sequenceChanged(builder);
    }
}


void	O_ScoreRejects::setRejector(RPRejector rejector)
{_G();
    this->_Rejector = rejector;
}

void	O_ScoreRejects::setScale(double scale)
{_G();
    this->_Scale = scale;
}


double	O_ScoreRejects::evaluate(RPScorerState scorerState, RPScorerBase scorer, RPPointProvider builder)
{_OF();
    ASSERT(this->_Rejector->notNil());
    LOG_SCORE(scorerState,BF("O_ScoreRejects{"))
    double score = this->_Rejector->countRejects(scorerState,scorer,builder);
    double scaledScore = score*this->_Scale;
    LOG_SCORE(scorerState,BF("scaled score = %lf")% scaledScore );
    LOG_SCORE(scorerState,BF("O_ScoreRejects}"))
    return scaledScore;
}


void	O_ScoreRejects::renderIntoDisplayList( RPRenderDisplayList dl, RPScorerState scorerState, RPScorerBase scorer, RPPointProvider builder )
{_OF();
    ASSERT(builder->notNil());
    RPRenderDisplayList mine = this->_Rejector->getRenderForScore(builder,scorerState);
    dl->add(mine);
}

bool O_ScoreRejects::needsAllAtomsBuilt()
{
    return this->_Rejector->needsAllAtomsBuilt();
}






void O_ScoreIntraMolecularVdwCollisions::exposeCando(RPLisp e)
{
    class_<O_ScoreIntraMolecularVdwCollisions>(e->lisp())
    ;
}

void O_ScoreIntraMolecularVdwCollisions::exposePython()
{
    IMPLEMENT_ME();
}








void	O_ScoreIntraMolecularVdwCollisions::oldLispInitialize(RPKeyedArguments args, RPLisp env)
{
    this->Base::oldLispInitialize(args,env);
}


void	O_ScoreIntraMolecularVdwCollisions::initialize()
{
    this->Base::initialize();
}

void	O_ScoreIntraMolecularVdwCollisions::archiveBase(RPNode node)
{
    this->Base::archiveBase(node);
}


void	O_ScoreIntraMolecularVdwCollisions::oligomerChanged(RPPointProvider builder)
{_G();
    this->Base::oligomerChanged(builder);
}

void	O_ScoreIntraMolecularVdwCollisions::sequenceChanged(RPPointProvider builder)
{_G();
    this->Base::sequenceChanged(builder);
}


double	O_ScoreIntraMolecularVdwCollisions::evaluate(RPScorerState scorerState, RPScorerBase scorer, RPPointProvider builder)
{_G();
    ASSERT(this->_Rejector->notNil());
    LOG_SCORE(scorerState,BF("O_ScoreIntraMolecularVdwCollisions{"))
    RPVdwCollisionRejector vdwRejector = this->_Rejector->as<O_VdwCollisionRejector>();
    double score = vdwRejector->countIntraMolecularVdwCollisions(builder->as<O_Builder>(),scorerState);
    double scaledScore = score*this->_Scale;
    LOG_SCORE(scorerState,BF("scaled score = %lf")% scaledScore );
    LOG_SCORE(scorerState,BF("O_ScoreIntraMolecularVdwCollisions}"))
    return scaledScore;
}


void	O_ScoreIntraMolecularVdwCollisions::renderIntoDisplayList( RPRenderDisplayList dl, RPScorerState scorerState, RPScorerBase scorer, RPPointProvider builder )
{_G();
    ASSERT(this->_Rejector->notNil());
    ASSERT(builder->notNil());
    RPVdwCollisionRejector vdwRejector = this->_Rejector->as<O_VdwCollisionRejector>();
    RPRenderDisplayList mine = vdwRejector->getRenderForIntraMolecularVdwCollisions(builder->as<O_Builder>(),scorerState);
    dl->add(mine);
}

void O_ScoreIntraMolecularVdwCollisions::insertRestraints(RPPointProvider builder, RPScorerBase scorer, RPCons& restraints )
{_G();
	// Do nothing, the normal non-bond interactions will handle IntraMolecular Nonbond terms
}




void O_ScoreInterMolecularVdwCollisions::exposeCando(RPLisp e)
{
    class_<O_ScoreInterMolecularVdwCollisions>(e->lisp())
    ;
}

void O_ScoreInterMolecularVdwCollisions::exposePython()
{
    IMPLEMENT_ME();
}







void	O_ScoreInterMolecularVdwCollisions::oldLispInitialize(RPKeyedArguments args, RPLisp env)
{
    this->Base::oldLispInitialize(args,env);
}


void	O_ScoreInterMolecularVdwCollisions::initialize()
{
    this->Base::initialize();
}

void	O_ScoreInterMolecularVdwCollisions::archiveBase(RPNode node)
{
    this->Base::archiveBase(node);
}


void	O_ScoreInterMolecularVdwCollisions::oligomerChanged(RPPointProvider builder)
{_G();
    this->Base::oligomerChanged(builder);
}

void	O_ScoreInterMolecularVdwCollisions::sequenceChanged(RPPointProvider builder)
{_G();
    this->Base::sequenceChanged(builder);
}


double	O_ScoreInterMolecularVdwCollisions::evaluate(RPScorerState scorerState, RPScorerBase scorer, RPPointProvider builder)
{_G();
    ASSERT(this->_Rejector->notNil());
    LOG_SCORE(scorerState,BF("O_ScoreInterMolecularVdwCollisions{"));
    RPVdwCollisionRejector vdwRejector = this->_Rejector->as<O_VdwCollisionRejector>();
    double score = vdwRejector->countInterMolecularVdwCollisions(builder->as<O_Builder>(),scorerState);
    double scaledScore = score*this->_Scale;
    LOG_SCORE(scorerState,BF("scaled score = %lf")% scaledScore );
    LOG_SCORE(scorerState,BF("O_ScoreInterMolecularVdwCollisions}"));
    return scaledScore;
}


void	O_ScoreInterMolecularVdwCollisions::renderIntoDisplayList( RPRenderDisplayList dl, RPScorerState scorerState, RPScorerBase scorer, RPPointProvider builder )
{_G();
    ASSERT(builder->notNil());
    ASSERT(this->_Rejector->notNil());
    RPVdwCollisionRejector vdwRejector = this->_Rejector->as<O_VdwCollisionRejector>();
    RPRenderDisplayList mine = vdwRejector->getRenderForInterMolecularVdwCollisions(builder,scorerState);
    dl->add(mine);
}


void O_ScoreInterMolecularVdwCollisions::insertRestraints(RPPointProvider builder, RPScorerBase scorer, RPCons& restraints )
{_G();
    RPVdwCollisionRejector vdwRejector = this->_Rejector->as<O_VdwCollisionRejector>();
    RPMatter fixedMatter = vdwRejector->getFixedMatter();
    RPRestraintFixedNonbond restraint = O_RestraintFixedNonbond::create(this->lisp(),fixedMatter);
    		//
		// Insert into the restraint list
		//
    restraints = O_Cons::create(restraint,restraints,this->lisp());
}















class	ScoreOperation_Exposer : public Exposer
{
    void exposeCando()
    {
	class_<O_ScoreOperation>(this->lisp())
	;
    }

    void exposePython()
    {
#ifdef	USEBOOSTPYTHON
	boost::python::class_<O_ScoreOperation,
	    boost::shared_ptr<O_ScoreOperation>,
	    boost::python::bases <mbb::O_Object>,
	    boost::noncopyable> ("O_ScoreOperation", boost::python::no_init )
	;
#endif
    }
};



class	ScoreSuperposition_Exposer : public Exposer
{
    void exposeCando()
    {
	class_<O_ScoreSuperposition>(this->lisp())
	;
    }

    void exposePython()
    {
#ifdef	USEBOOSTPYTHON
	boost::python::class_<O_ScoreSuperposition,
	    boost::shared_ptr<O_ScoreSuperposition>,
	    boost::python::bases <O_ScoreOperation>,
	    boost::noncopyable> ("O_ScoreSuperposition", boost::python::no_init )
	;
#endif
    }
};



class ScoreList_Exposer : public Exposer
{
void exposeCando()
{
    class_<O_ScoreList>(this->lisp())
    .def("addOperation",&O_ScoreList::addOperation)
    ;
}
void exposePython()
{
#ifdef	USEBOOSTPYTHON
    boost::python::class_<O_ScoreList,
	boost::shared_ptr<O_ScoreList>,
	boost::python::bases <O_ScoreOperation>,
	boost::noncopyable> ("O_ScoreList", boost::python::no_init )
    ;
#endif
}
};



class ScoreMin_Exposer : public Exposer
{
void exposeCando()
{
    class_<O_ScoreMin>(this->lisp())
    ;
}
void exposePython()
{
#ifdef	USEBOOSTPYTHON
    boost::python::class_<O_ScoreMin,
	boost::shared_ptr<O_ScoreMin>,
	boost::python::bases <O_ScoreMin::Base>,
	boost::noncopyable> ("O_ScoreMin", boost::python::no_init )
    ;
#endif
}
};


class ScoreSum_Exposer : public Exposer
{
void exposeCando()
{
    class_<O_ScoreSum>(this->lisp())
    ;
}
void exposePython()
{
#ifdef	USEBOOSTPYTHON
    boost::python::class_<O_ScoreSum,
	boost::shared_ptr<O_ScoreSum>,
	boost::python::bases <O_ScoreList>,
	boost::noncopyable> ("O_ScoreSum", boost::python::no_init )
    ;
#endif
}
};



class ScoreDistance_Exposer : public Exposer
{
void exposeCando()
{
    class_<O_ScoreDistance>(this->lisp())
	.def("setAlias1",&O_ScoreDistance::setAlias1)
	.def("getAlias1",&O_ScoreDistance::getAlias1)
	.def("setAlias2",&O_ScoreDistance::setAlias2)
	.def("getAlias2",&O_ScoreDistance::getAlias2)
	.def("getDistance",&O_ScoreDistance::getDistance)
	.def("setDistance",&O_ScoreDistance::setDistance)
	.def("getForce",&O_ScoreDistance::getForce)
	.def("setForce",&O_ScoreDistance::setForce)
    ;
//    defNoWrapPackage(MbbPackage,"distance",&O_ScoreDistance::prim_distance);
}
void exposePython()
{
#ifdef	USEBOOSTPYTHON
    boost::python::class_<O_ScoreDistance,
	boost::shared_ptr<O_ScoreDistance>,
	boost::python::bases <O_ScoreOperation>,
	boost::noncopyable> ("O_ScoreDistance", boost::python::no_init )
	.def("setAlias1",&O_ScoreDistance::setAlias1)
	.def("getAlias1",&O_ScoreDistance::getAlias1)
	.def("setAlias2",&O_ScoreDistance::setAlias2)
	.def("getAlias2",&O_ScoreDistance::getAlias2)
	.def("getDistance",&O_ScoreDistance::getDistance)
	.def("setDistance",&O_ScoreDistance::setDistance)
	.def("getForce",&O_ScoreDistance::getForce)
	.def("setForce",&O_ScoreDistance::setForce)
    ;
#endif
}
};


class ScoreAngle_Exposer : public Exposer
{
void exposeCando()
{
    class_<O_ScoreAngle>(this->lisp())
	.def("setAlias1",&O_ScoreAngle::setAlias1)
	.def("getAlias1",&O_ScoreAngle::getAlias1)
	.def("setAlias2",&O_ScoreAngle::setAlias2)
	.def("getAlias2",&O_ScoreAngle::getAlias2)
	.def("setAlias3",&O_ScoreAngle::setAlias3)
	.def("getAlias3",&O_ScoreAngle::getAlias3)
	.def("getRadians",&O_ScoreAngle::getRadians)
	.def("setRadians",&O_ScoreAngle::setRadians)
	.def("getForce",&O_ScoreAngle::getForce)
	.def("setForce",&O_ScoreAngle::setForce)
    ;
//    defNoWrapPackage(MbbPackage,"distance",&O_ScoreAngle::prim_distance);
}
void exposePython()
{
#ifdef	USEBOOSTPYTHON
    boost::python::class_<O_ScoreAngle,
	boost::shared_ptr<O_ScoreAngle>,
	boost::python::bases <O_ScoreOperation>,
	boost::noncopyable> ("O_ScoreAngle", boost::python::no_init )
	.def("setAlias1",&O_ScoreAngle::setAlias1)
	.def("getAlias1",&O_ScoreAngle::getAlias1)
	.def("setAlias2",&O_ScoreAngle::setAlias2)
	.def("getAlias2",&O_ScoreAngle::getAlias2)
	.def("setAlias3",&O_ScoreAngle::setAlias3)
	.def("getAlias3",&O_ScoreAngle::getAlias3)
	.def("getRadians",&O_ScoreAngle::getRadians)
	.def("setRadians",&O_ScoreAngle::setRadians)
	.def("getForce",&O_ScoreAngle::getForce)
	.def("setForce",&O_ScoreAngle::setForce)
    ;
#endif
}
};

class ScoreDihedral_Exposer : public Exposer
{
void exposeCando()
{
    class_<O_ScoreDihedral>(this->lisp())
	.def("setAlias1",&O_ScoreDihedral::setAlias1)
	.def("getAlias1",&O_ScoreDihedral::getAlias1)
	.def("setAlias2",&O_ScoreDihedral::setAlias2)
	.def("getAlias2",&O_ScoreDihedral::getAlias2)
	.def("setAlias3",&O_ScoreDihedral::setAlias3)
	.def("getAlias3",&O_ScoreDihedral::getAlias3)
	.def("setAlias4",&O_ScoreDihedral::setAlias4)
	.def("getAlias4",&O_ScoreDihedral::getAlias4)
	.def("getRadians",&O_ScoreDihedral::getRadians)
	.def("setRadians",&O_ScoreDihedral::setRadians)
	.def("getForce",&O_ScoreDihedral::getForce)
	.def("setForce",&O_ScoreDihedral::setForce)
    ;
//    defNoWrapPackage(MbbPackage,"distance",&O_ScoreDihedral::prim_distance);
}
void exposePython()
{
#ifdef	USEBOOSTPYTHON
    boost::python::class_<O_ScoreDihedral,
	boost::shared_ptr<O_ScoreDihedral>,
	boost::python::bases <O_ScoreOperation>,
	boost::noncopyable> ("O_ScoreDihedral", boost::python::no_init )
	.def("setAlias1",&O_ScoreDihedral::setAlias1)
	.def("getAlias1",&O_ScoreDihedral::getAlias1)
	.def("setAlias2",&O_ScoreDihedral::setAlias2)
	.def("getAlias2",&O_ScoreDihedral::getAlias2)
	.def("setAlias3",&O_ScoreDihedral::setAlias3)
	.def("getAlias3",&O_ScoreDihedral::getAlias3)
	.def("setAlias4",&O_ScoreDihedral::setAlias4)
	.def("getAlias4",&O_ScoreDihedral::getAlias4)
	.def("getRadians",&O_ScoreDihedral::getRadians)
	.def("setRadians",&O_ScoreDihedral::setRadians)
	.def("getForce",&O_ScoreDihedral::getForce)
	.def("setForce",&O_ScoreDihedral::setForce)
    ;
#endif
}
};





class ScoreNormalizedDotProduct_Exposer : public Exposer
{
void exposeCando()
{
    class_<O_ScoreNormalizedDotProduct>(this->lisp())
	.def("setAlias1",&O_ScoreNormalizedDotProduct::setAlias1)
	.def("getAlias1",&O_ScoreNormalizedDotProduct::getAlias1)
	.def("setAlias2",&O_ScoreNormalizedDotProduct::setAlias2)
	.def("getAlias2",&O_ScoreNormalizedDotProduct::getAlias2)
	.def("setAlias3",&O_ScoreNormalizedDotProduct::setAlias3)
	.def("getAlias3",&O_ScoreNormalizedDotProduct::getAlias3)
	.def("setAlias4",&O_ScoreNormalizedDotProduct::setAlias4)
	.def("getAlias4",&O_ScoreNormalizedDotProduct::getAlias4)
	.def("getNormalizedDotProduct",&O_ScoreNormalizedDotProduct::getNormalizedDotProduct)
	.def("setNormalizedDotProduct",&O_ScoreNormalizedDotProduct::setNormalizedDotProduct)
	.def("getForce",&O_ScoreNormalizedDotProduct::getForce)
	.def("setForce",&O_ScoreNormalizedDotProduct::setForce)
    ;
//    defNoWrapPackage(MbbPackage,"distance",&O_ScoreNormalizedDotProduct::prim_distance);
}
void exposePython()
{
#ifdef	USEBOOSTPYTHON
    boost::python::class_<O_ScoreNormalizedDotProduct,
	boost::shared_ptr<O_ScoreNormalizedDotProduct>,
	boost::python::bases <O_ScoreOperation>,
	boost::noncopyable> ("O_ScoreNormalizedDotProduct", boost::python::no_init )
	.def("setAlias1",&O_ScoreNormalizedDotProduct::setAlias1)
	.def("getAlias1",&O_ScoreNormalizedDotProduct::getAlias1)
	.def("setAlias2",&O_ScoreNormalizedDotProduct::setAlias2)
	.def("getAlias2",&O_ScoreNormalizedDotProduct::getAlias2)
	.def("setAlias3",&O_ScoreNormalizedDotProduct::setAlias3)
	.def("getAlias3",&O_ScoreNormalizedDotProduct::getAlias3)
	.def("setAlias4",&O_ScoreNormalizedDotProduct::setAlias4)
	.def("getAlias4",&O_ScoreNormalizedDotProduct::getAlias4)
	.def("getNormalizedDotProduct",&O_ScoreNormalizedDotProduct::getNormalizedDotProduct)
	.def("setNormalizedDotProduct",&O_ScoreNormalizedDotProduct::setNormalizedDotProduct)
	.def("getForce",&O_ScoreNormalizedDotProduct::getForce)
	.def("setForce",&O_ScoreNormalizedDotProduct::setForce)
    ;
#endif
}
};






class ScoreDistanceToPoint_Exposer : public Exposer
{
void exposeCando()
{
    class_<O_ScoreDistanceToPoint>(this->lisp())
	.def("setAlias",&O_ScoreDistanceToPoint::setAlias)
	.def("getAlias",&O_ScoreDistanceToPoint::getAlias)
	.def("getPoint",&O_ScoreDistanceToPoint::getPoint)
	.def("setPoint",&O_ScoreDistanceToPoint::setPoint)
	.def("getDistance",&O_ScoreDistanceToPoint::getDistance)
	.def("setDistance",&O_ScoreDistanceToPoint::setDistance)
	.def("getForce",&O_ScoreDistanceToPoint::getForce)
	.def("setForce",&O_ScoreDistanceToPoint::setForce)
    ;
//    defNoWrapPackage(MbbPackage,"distance",&O_ScoreDistanceToPoint::prim_distance);
}
void exposePython()
{
#ifdef	USEBOOSTPYTHON
    boost::python::class_<O_ScoreDistanceToPoint,
	boost::shared_ptr<O_ScoreDistanceToPoint>,
	boost::python::bases <O_ScoreOperation>,
	boost::noncopyable> ("O_ScoreDistanceToPoint", boost::python::no_init )
	.def("setAlias",&O_ScoreDistanceToPoint::setAlias)
	.def("getAlias",&O_ScoreDistanceToPoint::getAlias)
	.def("getPoint",&O_ScoreDistanceToPoint::getPoint)
	.def("setPoint",&O_ScoreDistanceToPoint::setPoint)
	.def("getDistance",&O_ScoreDistanceToPoint::getDistance)
	.def("setDistance",&O_ScoreDistanceToPoint::setDistance)
	.def("getForce",&O_ScoreDistanceToPoint::getForce)
	.def("setForce",&O_ScoreDistanceToPoint::setForce)
    ;
#endif
}
};

class ScoreBestGeometry_Exposer : public Exposer
{
void exposeCando()
{
    class_<O_ScoreBestGeometry>(this->lisp())
    ;
//    defNoWrapPackage(MbbPackage,"distance",&O_ScoreBestGeometry::prim_distance);
}
void exposePython()
{
#ifdef	USEBOOSTPYTHON
    boost::python::class_<O_ScoreBestGeometry,
	boost::shared_ptr<O_ScoreBestGeometry>,
	boost::python::bases <O_ScoreOperation>,
	boost::noncopyable> ("O_ScoreBestGeometry", boost::python::no_init )
    ;
#endif
}
};


class ScoreRejects_Exposer : public Exposer
{
void exposeCando()
{
    class_<O_ScoreRejects>(this->lisp())
    ;
}
void exposePython()
{
#ifdef	USEBOOSTPYTHON
    boost::python::class_<O_ScoreRejects,
	boost::shared_ptr<O_ScoreRejects>,
	boost::python::bases <O_ScoreOperation>,
	boost::noncopyable> ("O_ScoreRejects", boost::python::no_init )
    ;
#endif
}
};

OLD_EXPOSE_CLASS(O_ScoreOperation,ScoreOperation_Exposer);
OLD_EXPOSE_CLASS(O_ScoreSuperposition,ScoreSuperposition_Exposer);
OLD_EXPOSE_CLASS(O_ScoreList,ScoreList_Exposer);
OLD_EXPOSE_CLASS(O_ScoreSum,ScoreSum_Exposer);
OLD_EXPOSE_CLASS(O_ScoreMin,ScoreMin_Exposer);
OLD_EXPOSE_CLASS(O_ScoreDistance,ScoreDistance_Exposer);
OLD_EXPOSE_CLASS(O_ScoreAngle,ScoreAngle_Exposer);
OLD_EXPOSE_CLASS(O_ScoreDihedral,ScoreDihedral_Exposer);
OLD_EXPOSE_CLASS(O_ScoreNormalizedDotProduct,ScoreNormalizedDotProduct_Exposer);
OLD_EXPOSE_CLASS(O_ScoreDistanceToPoint,ScoreDistanceToPoint_Exposer);
OLD_EXPOSE_CLASS(O_ScoreBestGeometry,ScoreBestGeometry_Exposer);
OLD_EXPOSE_CLASS(O_ScoreRejects,ScoreRejects_Exposer);

EXPOSE_CLASS(O_ScoreInterMolecularVdwCollisions);
EXPOSE_CLASS(O_ScoreIntraMolecularVdwCollisions);
};
