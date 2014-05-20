#define	DEBUG_LEVEL_FULL

#include "multiScorer.h"
#include "archiveNode.h"
#include "archive.h"
#include "builder.h"
#include "alias.h"
#include "keyedArguments.h"
#include "scorer.h"
#include "scoreOperations.h"
#include "scorerState.h"
#include "builderState.h"
#include "wrappers.h"


namespace mbb {

/*
__BEGIN_DOC(classes.MultiScorer.!class.MultiScorer)
\requiredKeyed{scorers}{Cons::scorers}

Create an MultiScorer object that maintains a list of Scorers.
__END_DOC
*/
void	O_MultiScorer::oldLispInitialize(RPKeyedArguments args, RPLisp env)
{_G();
    this->Base::oldLispInitialize(args,env);
    RPCons scorers = args->getAndRemoveOrDefault("scorers",O_Cons::nil(this->lisp()))->as<O_Cons>();
    this->_MultiScoreCalculator = args->getAndRemoveOrDefault("calculator",
    				O_ScoreSum::create(this->lisp()))->as<O_ScoreSum>();
    RPCons cur;
    for ( cur=scorers; cur->notNil(); cur = cur->cdr() )
    {
	RPScorer scorer = cur->ocar()->as<O_Scorer>();
	this->addScorer(scorer);
    }
}



//
// Constructor
//

//
// Destructor
//

void	O_MultiScorer::initialize()
{
    this->Base::initialize();
    this->_MultiScoreCalculator = O_ScoreSum::nil(this->lisp());
}


RPScorerBase O_MultiScorer::getSubScorer(uint i)
{_G();
    ASSERT_lt(i,this->_SubScorers.size());
    return this->_SubScorers[i];
}


void	O_MultiScorer::archiveBase(RPNode node)
{_G();
    this->Base::archiveBase(node);
    node->archiveVector0OfObjectsSubClassOf(this->_SubScorers);
    node->archiveObject("calculator",this->_MultiScoreCalculator);
}


void	O_MultiScorer::oligomerChanged(RPPointProvider builder)
{_G();
    this->Base::oligomerChanged(builder);
    ASSERT(builder->notNil());
    		//
		// Tell every Scorer that the oligomer changed
		//
    List<O_Scorer>::iterator	superposerIterator;
    for ( superposerIterator = this->_SubScorers.begin();
    		 superposerIterator != this->_SubScorers.end();
		superposerIterator++ )
    {
	(*superposerIterator)->oligomerChanged(builder);
    }
    if ( this->_MultiScoreCalculator->notNil() )
    {
	if ( builder->notNil() )
	{
            this->_MultiScoreCalculator->oligomerChanged(builder);
	}
    }
}


bool	O_MultiScorer::needsAllAtomsBuilt()
{_G();
    bool needs = this->Base::needsAllAtomsBuilt();
    if ( needs ) return true;
    List<O_Scorer>::iterator	superposerIterator;
    for ( superposerIterator = this->_SubScorers.begin();
    		 superposerIterator != this->_SubScorers.end();
		superposerIterator++ )
    {
	LOG(BF("superposer@%p") % (*superposerIterator).get()  ); // vp0(("superposer@%p", (*superposerIterator).get() ));
	if ( (*superposerIterator)->needsAllAtomsBuilt() ) return true;
    }
    return this->_MultiScoreCalculator->needsAllAtomsBuilt();
}


void	O_MultiScorer::sequenceChanged(RPPointProvider builder)
{_G();
    this->Base::sequenceChanged(builder);
    ASSERT(builder->notNil());
    		//
		// Tell every Scorer that the oligomer changed
		//
    List<O_Scorer>::iterator	superposerIterator;
    for ( superposerIterator = this->_SubScorers.begin();
    		 superposerIterator != this->_SubScorers.end();
		superposerIterator++ )
    {
	(*superposerIterator)->sequenceChanged(builder);
    }
    if ( this->_MultiScoreCalculator->notNil() )
    {
	if ( builder->notNil() )
	{
            this->_MultiScoreCalculator->sequenceChanged(builder);
	}
    }
}



void	O_MultiScorer::addScorer(RPScorer scorer)
{_G();
		// 
		// Give the scorer a unique Identifier
		//
    uint scorerIdentifier = this->_SubScorers.size();
    LOG(BF("Set subscorer ScorerIdentifier to %d")% scorerIdentifier );
    scorer->setScorerIdentifier(scorerIdentifier);
    this->_SubScorers.push_back(scorer);
}



void	O_MultiScorer::addToMultiScorerCalculator(RPScoreOperation op)
{_G();
    List<O_Scorer>::iterator	superposerIterator;
    if ( this->_MultiScoreCalculator->isNil() )
    {
	this->_MultiScoreCalculator = O_ScoreSum::create(this->lisp());
    }
    this->_MultiScoreCalculator->addOperation(op);
}


void	O_MultiScorer::addToCalculatorsOfAllScorers(RPScoreOperation op)
{_G();
    List<O_Scorer>::iterator	superposerIterator;
    for ( superposerIterator = this->_SubScorers.begin();
    		 superposerIterator != this->_SubScorers.end();
		superposerIterator++ )
    {
	(*superposerIterator)->addToCalculator(op);
    }
}


void O_MultiScorer::_evaluate(RPPointProvider builder, RPScorerState scorerState, bool superpose)
{_G();
Vector3				v1, v2, vDiff;
double				delta, sc, part;
RPScorerState			bestScorerState;
RPScorerState			oneScorerState;
DEPRECIATED();
#if 0
    this->throwIfBuilderDoesntMatch(builder);
    ASSERT_NOT_NULL(scorerState);
    bestScorerState = scorerState;
    bestScorerState->clear();
    oneScorerState = O_ScorerState::create(this->lisp());
    ASSERT(builder->notNil());
    double bestScore = 9e99;
    double oneScore;
    RPScorer bestScorer = O_Scorer::nil(this->lisp());
    ASSERT_greaterThan(this->_SubScorers.size(),0);
	    //
	    // Try every Scorer and then calculate the score
	    // for the geometry
	    //
    bool sawBest = false;
    List<O_Scorer>::iterator	subScorerIterator;
    uint ssidx;
    for ( subScorerIterator = this->_SubScorers.begin(), ssidx=0;
    		 subScorerIterator != this->_SubScorers.end();
		subScorerIterator++,ssidx++ )
    { _BLOCK_TRACEF(BF("About to evaluate subscorer %d/%d for builder state %s")
    		% (*subScorerIterator)->getScorerIdentifier() 
		% this->_SubScorers.size() 
		% builder->stateIdentifier() );
        if ( superpose )
	{
	    (*subScorerIterator)->evaluate(builder,oneScorerState);
	} else
	{
	    (*subScorerIterator)->evaluateWithoutSuperposing(builder,oneScorerState);
	}
#ifdef	DEBUG_ON
	LOG(BF( "current evaluated score for scorerI(%u) oneScore = %s")% 
		(*subScorerIterator)->getScorerIdentifier() 
		% oneScorerState->summary().c_str());
	stringstream sdebug;
	builder->streamDump(sdebug);
	LOG(BF( "The builder state(%s) that generated that score(%lf)\n%s")%
		builder->stateIdentifier()
		% oneScorerState->getScore()
		% sdebug.str() );
#endif
	if ( oneScorerState->isBetterThan(bestScorerState) )
	{ _BLOCK_TRACEF(BF("Current score(%lf) is better than the previous best(%lf) so this becomes bestScorerState")
				% oneScorerState->getScore() 
				% bestScorerState->getScore() );
	    LOG(BF("Setting bestScorerState from oneScorerState") ); // vp0(( "Setting bestScorerState from oneScorerState"));
	    bestScorerState->copyFromOtherState(oneScorerState);
    	    ASSERT(bestScorerState->isScoreSet());
	    LOG(BF("Current best scorer: %s") % bestScorerState->summary().c_str() ); // vp0(( "Current best scorer: %s", bestScorerState->summary().c_str()));
	} else
	{
	    LOG(BF("Ignoring oneScore state") ); // vp0(("Ignoring oneScore state"));
	}
    }
    	// If we have rejectors then check them with the best state
    LOG(BF("Telling builder bestScore: %s") % bestScorerState->summary().c_str() ); // vp0(("Telling builder bestScore: %s", bestScorerState->summary().c_str()));
    if ( this->_MultiScoreCalculator->notNil() )
    {
	LOG_SCORE(bestScorerState,BF("O_MultiScorer::evaluate{"));
	double score = this->_MultiScoreCalculator->evaluate(bestScorerState,this->sharedThis<O_MultiScorer>(),builder);
	bestScorerState->addToScore(score);
	LOG_SCORE(bestScorerState,BF("adding multiscorer score = %lf")% score );
	LOG_SCORE(bestScorerState,BF("final score = %lf")% bestScorerState->getScore() );
	LOG_SCORE(bestScorerState,BF("O_MultiScorer::evaluate}"));
    }
#endif
}


void O_MultiScorer::evaluate(RPPointProvider builder, RPScorerState scorerState)
{_G();
    this->_evaluate(builder,scorerState,true);
}

void O_MultiScorer::evaluateWithoutSuperposing(RPPointProvider builder, RPScorerState scorerState)
{_G();
    this->_evaluate(builder,scorerState,false);
}


void O_MultiScorer::evaluateUsingGeneratorIdentifier(RPPointProvider builder, RPScorerState scorerState)
{_G();
    DEPRECIATED();
#if 0
Vector3				v1, v2, vDiff;
double				delta, sc, part;
    this->throwIfBuilderDoesntMatch(builder);
    ASSERT_NOT_NULL(scorerState);
    ASSERT(builder->notNil());
    ASSERT_greaterThan(this->_SubScorers.size(),0);
    ASSERT_lt(scorerState->getScorerIdentifier(),this->_SubScorers.size());
	    //
	    // Try every Scorer and then calculate the score
	    // for the geometry
	    //
    bool sawBest = false;
    uint ssidx = scorerState->getScorerIdentifier();
    List<O_Scorer>::iterator	subScorerIterator;
    subScorerIterator = this->_SubScorers.begin()+ssidx;
    { _BLOCK_TRACEF(BF("About to evaluate subscorer %d/%d for builder state %s")
    		% (*subScorerIterator)->getScorerIdentifier() 
		% this->_SubScorers.size() 
		% builder->stateIdentifier() );
	(*subScorerIterator)->evaluate(builder,scorerState);
#ifdef	DEBUG_ON
	LOG(BF( "current evaluated score for scorerI(%u) oneScore = %s")% 
		(*subScorerIterator)->getScorerIdentifier() 
		% scorerState->summary().c_str());
	stringstream sdebug;
	builder->streamDump(sdebug);
	LOG(BF( "The builder state(%s) that generated that score(%lf)\n%s")%
		builder->stateIdentifier()
		% scorerState->getScore()
		% sdebug.str() );
#endif
    }
    LOG(BF("Telling builder bestScore: %s") % scorerState->summary().c_str() ); // vp0(("Telling builder bestScore: %s", scorerState->summary().c_str()));
    if ( this->_MultiScoreCalculator->notNil() )
    {
	LOG_SCORE(scorerState,BF("O_MultiScorer::evaluate{"));
	double score = this->_MultiScoreCalculator->evaluate(scorerState,this->sharedThis<O_MultiScorer>(),builder);
	scorerState->addToScore(score);
	LOG_SCORE(scorerState,BF("adding multiscorer score = %lf")% score );
	LOG_SCORE(scorerState,BF("final score = %lf")% scorerState->getScore() );
	LOG_SCORE(scorerState,BF("O_MultiScorer::evaluate}"));
    }
#endif
}

RPRenderDisplayList	O_MultiScorer::getRenderForScore(RPPointProvider builder, RPScorerState scorerState)
{_G();
    this->throwIfBuilderDoesntMatch(builder);
    RPScorer scorer;
    ASSERT_NOT_NULL(scorerState);
    ASSERT_lessThan(scorerState->getScorerIdentifier(),this->_SubScorers.size());
    LOG(BF("scorerState->getScorerIdentifier() = %d") % scorerState->getScorerIdentifier()  ); // vp0(("scorerState->getScorerIdentifier() = %d", scorerState->getScorerIdentifier() ));
    scorer = this->_SubScorers[scorerState->getScorerIdentifier()];
    return scorer->getRenderForScore(builder,scorerState);
}

RPScorerState	O_MultiScorer::createState()
{
    return O_ScorerState::create(this->lisp());
}



void O_MultiScorer::insertScoreOperationsIntoRestraintList(RPPointProvider builder, RPCons& restraints)
{_G();
    ASSERT_NOT_NULL(this->_MultiScoreCalculator);
    	
	//
	// Evaluate the sub-scorers first to figure out which one is the best
	//
    RPScorerState scorerState = this->createState();
    this->evaluate(builder,scorerState);
    uint bestScorerIdentifier = scorerState->getScorerIdentifier();
    RPScorerBase bestScorer = this->_SubScorers[bestScorerIdentifier];
    bestScorer->insertScoreOperationsIntoRestraintList(builder,restraints);
    this->_MultiScoreCalculator->insertRestraints(builder,this->sharedThis<O_MultiScorer>(),restraints);
}


RPCons O_MultiScorer::createRestraints(RPPointProvider builder)
{_G();
    RPCons restraints = O_Cons::nil(this->lisp());
    this->insertScoreOperationsIntoRestraintList(builder,restraints);
    return restraints;
}





class	MultiScorer_Exposer : public Exposer
{
void exposeCando()
{
    class_<O_MultiScorer>(this->lisp())
	.def("addScorer",&O_MultiScorer::addScorer)
	.def("getSubScorer",&O_MultiScorer::getSubScorer)
	.def("addToCalculatorsOfAllScorers",&O_MultiScorer::addToCalculatorsOfAllScorers)
	.def("addToMultiScorerCalculator",&O_MultiScorer::addToMultiScorerCalculator)
    ;
}
void exposePython()
{
#ifdef	USEBOOSTPYTHON
    boost::python::class_<O_MultiScorer,
	boost::shared_ptr<O_MultiScorer>,
	boost::python::bases <mbb::O_ScorerBase>,
	boost::noncopyable> ("O_MultiScorer", boost::python::no_init )
	.def("addScorer",&O_MultiScorer::addScorer)
    ;
#endif
}
};


OLD_EXPOSE_CLASS(O_MultiScorer,MultiScorer_Exposer);
};





