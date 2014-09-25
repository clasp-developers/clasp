/*
    File: exhaustiveSearch.cc
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
#define	DEBUG_LEVEL_FULL

#include "exhaustiveSearch.h"
#include "archiveNode.h"
#include "archive.h"
#include "lisp.h"
#include "builder.h"
#include "scorer.h"
#include "generator.h"
#include "builderState.h"
#include "hits.h"
#include "scorerBase.h"
#include "render.h"
#include "keyedObject.h"
#include "keyedArguments.h"
#include "scorerStatistics.h"
#include "scorerState.h"
#include "wrappers.h"





namespace mbb {



//
// Constructor
//

//
// Destructor
//

void	O_ExhaustiveSearch::archive(::mbb::RPNode node)
{
    this->O_Search::archive(node);
}

void	O_ExhaustiveSearch::initialize()
{
    this->Base::initialize();
    this->_NumberOfProcesses = 1;
    this->_ProcessNumber = 0;
    this->_NumberOfConformationsPerSequence = 0;
    this->_UseRandomConformations = false;
}



void	O_ExhaustiveSearch::setDefaultOptions()
{
    this->Base::setDefaultOptions();
    this->_NumberOfProcesses = 1;
    this->_ProcessNumber = 0;
    this->_NumberOfConformationsPerSequence = 0;
    this->_UseRandomConformations = false;
}

void	O_ExhaustiveSearch::setOptions(RPCons opts)
{
    this->setKeyedOptions(opts->asKeyedArguments());
}

void	O_ExhaustiveSearch::setKeyedOptions(RPKeyedArguments kargs)
{_G();
    this->setDefaultOptions();
    this->Base::setKeyedOptions(kargs);
    this->_NumberOfConformationsPerSequence = kargs->getIntAndRemoveOrDefault("NumberOfConformationsPerSequence",0);
    this->_NumberOfProcesses = kargs->getIntAndRemoveOrDefault("NumberOfProcesses",1);
    this->_ProcessNumber = kargs->getIntAndRemoveOrDefault("ProcessNumber",0);
    this->_UseRandomConformations = kargs->getBoolAndRemoveOrDefault("UseRandomConformations",false);
    kargs->throwOnUnusedArguments(0);
}


RPObject O_ExhaustiveSearch::prim_exhaustiveSearch(RPExecutable e, RPCons args, RPEnvironment environ, RPLisp lisp )
{_G();
    RPExhaustiveSearch search = O_ExhaustiveSearch::create(lisp);
    RPKeyedArguments kargs = O_KeyedArguments::createFromKeyedObjectCons(args,lisp);
    search->setGenerator(kargs->getPositionalArgument(0)->as<O_Generator>());
//    search->setScorer(kargs->getPositionalArgument(1)->as<O_ScorerBase>());
    search->setHitList(kargs->getPositionalArgument(1)->as<O_HitList>());
    RPCons options = kargs->getAndRemoveOrDefault("options",O_Cons::nil(lisp))->as<O_Cons>();
    kargs->throwOnUnusedArguments(3);
    search->setOptions(options);
    search->run();
    return search;
}

RPObject O_ExhaustiveSearch::prim_exhaustiveSearchOneSequence(RPExecutable e, RPCons args, RPEnvironment environ, RPLisp lisp )
{_G();
    RPKeyedArguments kargs = O_KeyedArguments::createFromKeyedObjectCons(args,lisp);
    RPExhaustiveSearch search = O_ExhaustiveSearch::create(lisp);
    uint sequenceIndex = kargs->getPositionalArgument(0)->as<O_Int>()->get();
    search->setGenerator(kargs->getPositionalArgument(1)->as<O_Generator>());
//    search->setScorer(kargs->getPositionalArgument(2)->as<O_ScorerBase>());
    search->setHitList(kargs->getPositionalArgument(2)->as<O_HitList>());
    RPCons options = kargs->getAndRemoveOrDefault("options",O_Cons::nil(lisp))->as<O_Cons>();
    kargs->throwOnUnusedArguments(4);
    search->setOptions(options);
    	// Set up the builder
    RPGenerator generator = search->getGenerator();
//    RPScorerBase scorer = search->getScorer();
    RPHitList hitList = search->getHitList();
    generator->firstOligomer();
    generator->firstSequence();
    generator->firstConformation();
    hitList->getData()->set("generator",generator);
    LongLongInt totalNumberOfBuilds = 0;
    generator->gotoSequence(sequenceIndex);
    if ( search->getVerbose() ) _lisp->print(BF("Searching sequence#%6lld") % sequenceIndex);
    RPScorerStatistics statistics = O_ScorerStatistics::create(lisp);
    totalNumberOfBuilds += search->searchCurrentSequence(statistics);
    return search;
}

RPObject O_ExhaustiveSearch::prim_exhaustiveSearchConformationsOfCurrentSequence(RPExecutable e, RPCons args, RPEnvironment environ, RPLisp lisp )
{_G();
    RPExhaustiveSearch search = O_ExhaustiveSearch::create(lisp);
    RPKeyedArguments kargs = O_KeyedArguments::createFromKeyedObjectCons(args,lisp);
    search->setGenerator(kargs->getPositionalArgument(0)->as<O_Generator>());
//    search->setScorer(kargs->getPositionalArgument(1)->as<O_ScorerBase>());
    search->setHitList(kargs->getPositionalArgument(1)->as<O_HitList>());
    RPCons options = kargs->getAndRemoveOrDefault("options",O_Cons::nil(lisp))->as<O_Cons>();
    kargs->throwOnUnusedArguments(2);
    search->setOptions(options);
    RPHitList hitList = search->getHitList();
    hitList->getData()->set("generator",search->getGenerator());
    LongLongInt totalNumberOfBuilds = 0;
    RPScorerStatistics statistics = O_ScorerStatistics::create(lisp);
    totalNumberOfBuilds += search->conformationalSearchWithMultipleHits( statistics );
    return search;
}

#if 0
/*!
 * This is the old way to run the exhaustive search
 * It would only search the first Oligomer and break it up into 
 * (NumberOfProcesses) jobs
 *
 * This won't work if we have multiple oligomers so I have to fix it at some
 * point.
 */
void	O_ExhaustiveSearch::oldrun()
{_G();
    ASSERT_NOT_NULL(this->_Builder);
    ASSERT_NOT_NULL(this->_Scorer);
    ASSERT_NOT_NULL(this->_HitList);
    RPBuilder builder = this->_Builder;
    RPScorerBase scorer = this->_Scorer;
    RPHitList hitList = this->_HitList;
    builder->firstOligomer();
    builder->firstSequence();
    builder->firstConformation();
    hitList->getData()->set("builder",builder);
    LongLongInt numberOfProcesses;
    numberOfProcesses = this->_NumberOfProcesses;
    LongLongInt processNumber;
    processNumber = this->_ProcessNumber;
    if ( numberOfProcesses == 0 ) numberOfProcesses = 1;
    processNumber = MAX(processNumber,0);
    processNumber = MIN(processNumber,numberOfProcesses-1);
    this->lisp()->print(BF("numberOfProcesses = %6lld") % numberOfProcesses );
    this->lisp()->print(BF("processNumber     = %6lld") % processNumber);
    LongLongInt totalNumberOfBuilds = 0;
    LongLongInt	totalNumberOfSequences = builder->numberOfSequences();
    this->lisp()->print(BF("There are %lld TOTAL sequences to search exhaustively") %totalNumberOfSequences);
    LongLongInt sequenceStep = totalNumberOfSequences/numberOfProcesses;
    if ( sequenceStep == 0 )
    {
        THROW(_lisp->create<O_LispError>("There is fewer than one sequence per process, reduce the number of processes"));
    }
    this->lisp()->print(BF("This process will carry out %2.0lf%% of the searcher") % (sequenceStep/((double)(totalNumberOfSequences))*100.0));
    LongLongInt sequenceIndexBegin = sequenceStep*processNumber;
    LongLongInt sequenceIndexEnd = sequenceStep*(processNumber+1);
    this->lisp()->print(BF("Searching sequence#%6lld to %6lld") % sequenceIndexBegin% sequenceIndexEnd );
    LongLongInt sequenceIndex;
    RPScorerStatistics statistics = O_ScorerStatistics::create(this->lisp());
    for ( sequenceIndex = sequenceIndexBegin; sequenceIndex != sequenceIndexEnd;
    		sequenceIndex++ )
    {_BLOCK_TRACEF(BF("Loop for conformations of sequence %lld") % sequenceIndex );
        builder->gotoSequence(sequenceIndex);
	scorer->oligomerChanged(builder);
	scorer->sequenceChanged(builder);
        this->lisp()->print(BF("Searching sequence#%6lld") % sequenceIndex);
	totalNumberOfBuilds += this->searchCurrentSequence(statistics);
    }
    this->lisp()->print(BF("\nBuild %lld TOTAL structures (oligomers*sequences*conformations)\n\n") % totalNumberOfBuilds );
}
#endif


void	O_ExhaustiveSearch::run()
{_G();
    ASSERT_NOT_NULL(this->_Generator);
    ASSERT_NOT_NULL(this->_HitList);
    RPGenerator generator = this->getGenerator();
    RPHitList hitList = this->_HitList;
    generator->firstOligomer();
    generator->firstSequence();
    generator->firstConformation();
    hitList->getData()->set("generator",generator);
    this->lisp()->print(BF("There are %lld oligomers to search exhaustively") % generator->numberOfOligomers() );
    LongLongInt totalNumberOfBuilds = 0;
    for ( uint oligomerIndex = 0; oligomerIndex<generator->numberOfOligomers(); oligomerIndex++ )
    {
	generator->gotoOligomerIndexDontBuild(oligomerIndex);
	LongLongInt	totalNumberOfSequences = generator->numberOfSequencesInCurrentOligomer();
	this->lisp()->print(BF("Oligomer #%u has %lld sequences to search exhaustively") % oligomerIndex% totalNumberOfSequences);
	LongLongInt sequenceIndexBegin = 0;
	LongLongInt sequenceIndexEnd = totalNumberOfSequences;
	this->lisp()->print(BF("Searching sequence#%6lld to %6lld") % sequenceIndexBegin % (sequenceIndexEnd-1) );
	LongLongInt sequenceIndex;
	RPScorerStatistics statistics = O_ScorerStatistics::create(this->lisp());
	for ( sequenceIndex = sequenceIndexBegin; sequenceIndex != sequenceIndexEnd;
		    sequenceIndex++ )
	{_BLOCK_TRACEF(BF("Loop for conformations of sequence %lld") % sequenceIndex );
	    generator->gotoSequence(sequenceIndex);
	    this->lisp()->print(BF("Searching sequence#%6lld") % sequenceIndex);
	    totalNumberOfBuilds += this->searchCurrentSequence(statistics);
	}
    }
    this->lisp()->print(BF("\nBuild %lld TOTAL structures (oligomers*sequences*conformations)\n\n") % totalNumberOfBuilds );
}



LongLongInt O_ExhaustiveSearch::conformationalSearchWithMultipleHits( RPScorerStatistics statistics)
{_G();
    LongLongInt numberOfConformations;
    RPGenerator generator = this->getGenerator();
    RPHitList hitList = this->getHitList();
    RPBuilderState bestBuilderState;
    RPScorerState bestScorerState;
    RPScorerState tempScorerState;
    bool conformationsRemain;
    LongLongInt confCount;
    { _BLOCK_TRACEF(BF("Set up"));
	bestBuilderState = O_BuilderState::create(this->lisp());
	bestScorerState = generator->createScorerState();
	tempScorerState = generator->createScorerState();
	generator->firstConformation();
	if ( this->_UseRandomConformations )
	{
	    if ( this->getVerbose() ) 
	    {
		this->lisp()->print(BF("Building %d RANDOM conformations for each sequence") %this->_NumberOfConformationsPerSequence );
	    }
	    numberOfConformations = this->_NumberOfConformationsPerSequence;
	} else
	{
	    numberOfConformations = generator->numberOfNecessaryConformationsInCurrentSequence();
	    if ( this->getVerbose() ) 
	    {
		this->lisp()->print(BF("Building every conformation for this sequence, thats %lld conformations") % numberOfConformations );
	    }
	}
	conformationsRemain = true;
	confCount = 0;
    }
    statistics->reset();
    { _BLOCK_TRACEF(BF("Block while conformationsRemain"));
	while ( conformationsRemain )
	{ _BLOCK_TRACEF(BF("conformations still remain" ));
	    tempScorerState->clear();
	    LOG(BF("About to evaluate scorer") ); // vp0(("About to evaluate scorer"));
	    generator->buildNecessaryUntransformedAtomPositionsAndEvaluateScorer(tempScorerState);
	    if ( this->getVerbosity()>1 )
	    {
		this->lisp()->print(BF("conf#%04u tempScorerState = %s") % confCount% tempScorerState->summary().c_str() );
	    }
	    statistics->update(tempScorerState);
    	    if ( hitList->isAHit(tempScorerState) )
	    {
		LOG(BF("We found a better score") ); // vp0(("We found a better score"));
		statistics->registerHit(tempScorerState);
		bestScorerState->copyFromOtherState(tempScorerState);
		generator->saveBuilderState(bestBuilderState);
		LOG(BF("Added hit with bestScore=%lf") % bestScorerState->getScore() ); // vp0(("Added hit with bestScore=%lf",bestScorerState->getScore()));
		RPScorerState hitScorerState = bestScorerState->copy();
			    // The builder is already in the bestBuilderState
		RPHit hit = hitList->createHitWithStates(hitScorerState);
		if ( this->getVerbose() ) 
		{
		    this->lisp()->print(BF("Added a hit with score: %lf") % bestScorerState->getScore() );
//		    this->lisp()->print(BF("%s") % hitList->__repr__().c_str() );
		}
	    } else
	    {
		LOG(BF("Not better than bestScorerState") ); // vp0(("Not better than bestScorerState"));
	    }
	    if ( !this->_UseRandomConformations )
	    {
		LOG(BF("Moving to the next conformation") ); // vp0(("Moving to the next conformation"));
		conformationsRemain = generator->incrementConformation();
	    } else
	    {
		LOG(BF("Moving to a random conformation") ); // vp0(("Moving to a random conformation"));
		generator->randomizeConformation();
		conformationsRemain = true;
	    }
	    confCount++;
	    if ( confCount % 10000 == 0 )
	    {
		if ( this->getVerbose() ) this->lisp()->print(BF("  Done %8lld/%8lld conformations, bestScore = %s") %				confCount% numberOfConformations% bestScorerState->summary().c_str());
	    }
	    if ( confCount >= numberOfConformations ) break;
	}
    }
    if ( this->getVerbose() ) 
    {
	this->lisp()->print(BF("  Done %8lld/%8lld conformations, bestScore = %s") % confCount % numberOfConformations % bestScorerState->summary().c_str());
        this->lisp()->print(BF("%s") % statistics->summary().c_str() );
    }
    return confCount;
}



LongLongInt O_ExhaustiveSearch::searchCurrentSequence(RPScorerStatistics statistics)
{_G();
    LongLongInt numberOfConformations;
    RPGenerator generator = this->getGenerator();
    RPHitList hitList = this->getHitList();
    RPBuilderState bestBuilderState;
    RPScorerState bestScorerState;
    RPScorerState tempScorerState;
    bool conformationsRemain;
    LongLongInt confCount;
    { _BLOCK_TRACEF(BF("Set up"));
	bestBuilderState = O_BuilderState::create(this->lisp());
	bestScorerState = generator->createScorerState();
	tempScorerState = generator->createScorerState();
	generator->firstConformation();
	if ( this->_UseRandomConformations )
	{
	    if ( this->getVerbose() ) 
	    {
		this->lisp()->print(BF("Building %d RANDOM conformations for each sequence") % this->_NumberOfConformationsPerSequence );
	    }
	    numberOfConformations = this->_NumberOfConformationsPerSequence;
	} else
	{
	    numberOfConformations = generator->numberOfNecessaryConformationsInCurrentSequence();
	    if ( this->getVerbose() ) 
	    {
		this->lisp()->print(BF("Building every conformation for this sequence, thats %lld conformations" ) %			numberOfConformations );
	    }
	}
	conformationsRemain = true;
	confCount = 0;
    }
    statistics->reset();
    { _BLOCK_TRACEF(BF("Block while conformationsRemain"));
	while ( conformationsRemain )
	{ _BLOCK_TRACEF(BF("conformations still remain" ));
	    tempScorerState->clear();
	    LOG(BF("About to evaluate scorer") ); // vp0(("About to evaluate scorer"));
	    generator->buildNecessaryUntransformedAtomPositionsAndEvaluateScorer(tempScorerState);
	    if ( this->getVerbosity()>1 )
	    {
		this->lisp()->print(BF("conf#%04u tempScorerState = %s") % confCount% tempScorerState->summary().c_str() );
	    }
	    statistics->update(tempScorerState);
	    if ( tempScorerState->isBetterThan(bestScorerState) )
	    {
		LOG(BF("We found a better score") ); // vp0(("We found a better score"));
		statistics->registerHit(tempScorerState);
		bestScorerState->copyFromOtherState(tempScorerState);
		generator->saveBuilderState(bestBuilderState);
		LOG(BF("Found potential hit") ); // vp0(("Found potential hit"));
	    } else
	    {
		LOG(BF("Not better than bestScorerState") ); // vp0(("Not better than bestScorerState"));
	    }
	    generator->advanceConformation(this->_UseRandomConformations);
	    confCount++;
	    if ( confCount % 10000 == 0 )
	    {
		if ( this->getVerbose() )
		{
		    this->lisp()->print(BF("  Done %8lld/%8lld conformations, bestScore = %s") % 
				confCount % numberOfConformations % bestScorerState->summary().c_str());
		}
	    }
	    if ( confCount >= numberOfConformations ) break;
	}
    }
    if ( this->getVerbose() ) 
    {
	this->lisp()->print(BF("  Done %8lld/%8lld conformations, bestScore = %s")
		    % confCount % numberOfConformations % bestScorerState->summary());
    }
    if ( hitList->isAHit(bestScorerState ) )
    {
	LOG(BF("Added hit with bestScore=%s") % bestScorerState->summary().c_str() ); // vp0(("Added hit with bestScore=%s",bestScorerState->summary().c_str()));
	RPScorerState hitScorerState = bestScorerState->copy();
		    // Restore the builder to the bestBuilderState
	generator->restoreState(bestBuilderState);
//        bestBuilderState->getBuilder()->buildInterestingUntransformedAtomPositions(); // TEST TEST TEST
	// The scorer doesn't need to be informed if only 
	// the conformation of the builder changes
	RPHit hit = hitList->createHitWithStates(generator, bestBuilderState, hitScorerState );
	if ( this->getVerbose() ) 
	{
	    this->lisp()->print(BF("Added hit with score: %lf") % bestScorerState->getScore() );
	    this->lisp()->print(BF("%s") % hitList->__repr__().c_str() );
	    this->lisp()->print(BF("%s") % statistics->summary().c_str() );
	}
    }
    return confCount;
}



class	ExhaustiveSearch_Exposer : public Exposer
{
    void exposeCando()
    {
	class_<O_ExhaustiveSearch>(this->lisp())
	;
	defNoWrapPackage(MbbPackage,"exhaustiveSearch",&O_ExhaustiveSearch::prim_exhaustiveSearch,this->lisp()); // ctor
	defNoWrapPackage(MbbPackage,"exhaustiveSearchOneSequence",&O_ExhaustiveSearch::prim_exhaustiveSearchOneSequence,this->lisp()); // ctor
	defNoWrapPackage(MbbPackage,"exhaustiveSearchConformationsOfCurrentSequence",&O_ExhaustiveSearch::prim_exhaustiveSearchConformationsOfCurrentSequence,this->lisp()); // ctor
    }
    void exposePython()
    {
#ifdef	USEBOOSTPYTHON
	boost::python::class_<O_ExhaustiveSearch,
	    boost::shared_ptr<O_ExhaustiveSearch>,
	    boost::python::bases <mbb::O_Object>,
	    boost::noncopyable> ("O_ExhaustiveSearch", boost::python::no_init )
	;
#endif
    }
};



OLD_EXPOSE_CLASS(O_ExhaustiveSearch,ExhaustiveSearch_Exposer);

};
