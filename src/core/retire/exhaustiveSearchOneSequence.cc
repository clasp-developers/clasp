/*
    File: exhaustiveSearchOneSequence.cc
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

#include "exhaustiveSearchOneSequence.h"
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

void	O_ExhaustiveSearchOneSequence::archive(::mbb::RPNode node)
{
    this->O_Search::archive(node);
}

void	O_ExhaustiveSearchOneSequence::initialize()
{
    this->Base::initialize();
    this->_NumberOfPartitions = 1;
    this->_PartitionNumber = 0;
    this->_NumberOfConformationsPerSequence = 0;
    this->_UseRandomConformations = false;
}



void	O_ExhaustiveSearchOneSequence::setDefaultOptions()
{
    this->Base::setDefaultOptions();
    this->_NumberOfPartitions = 1;
    this->_PartitionNumber = 0;
    this->_NumberOfConformationsPerSequence = 0;
    this->_UseRandomConformations = false;
}

void	O_ExhaustiveSearchOneSequence::setOptions(RPCons args)
{
    this->setKeyedOptions(args->asKeyedArguments());
}

void	O_ExhaustiveSearchOneSequence::setKeyedOptions(RPKeyedArguments kargs)
{_G();
    this->setDefaultOptions();
    this->Base::setKeyedOptions(kargs);
    this->_NumberOfPartitions = kargs->getIntAndRemoveOrDefault("NumberOfPartitions",1);
    this->_PartitionNumber = kargs->getIntAndRemoveOrDefault("PartitionNumber",0);
    this->_UseRandomConformations = kargs->getBoolAndRemoveOrDefault("UseRandomConformations",false);
    this->_NumberOfConformationsPerSequence = kargs->getIntAndRemoveOrDefault("NumberOfConformationsPerSequence",0);
    kargs->throwOnUnusedArguments(0);
}


    RPObject O_ExhaustiveSearchOneSequence::__init__(RPExecutable exec, RPCons args, RPEnvironment environ, RPLisp lisp)
    {
	RPEnvironment bargs = this->parse__init__arguments<O_ExhaustiveSearchOneSequence>(args,environ);
	RPScorerVirtualMachine scorer = _ARG(RPScorerVirtualMachine,"scorer");
	RPHitList hitList = _ARG(RPHitList,"hitList");
	RPCons options = _ARG(RPCons,"options");
	this->setScorer(scorer);
	this->setHitList(hitList);
	this->setOptions(options);
	hitList->setScorer(scorer);
	return lisp->onil();
    }



void O_ExhaustiveSearchOneSequence::run()
{_G();
    RPScorerStatistics statistics = O_ScorerStatistics::create(this->lisp());
    this->_Statistics = statistics;
    LongLongInt numberOfConformations;
    RPScorerVirtualMachine scorer = this->getScorer();
    RPHitList hitList = this->getHitList();
    RPScorerState bestScorerState;
    RPScorerState tempScorerState;
    bool conformationsRemain;
    LongLongInt confCount = 0;
    { _BLOCK_TRACEF(BF("Set up"));
	bestBuilderState = O_BuilderState::create(this->lisp());
	bestScorerState = generator->createScorerState();
	tempScorerState = generator->createScorerState();
	generator->firstConformation();
	if ( this->_UseRandomConformations )
	{
	    if ( this->getVerbose() ) this->lisp()->print(BF("Building %d RANDOM conformations for each sequence") % this->_NumberOfConformationsPerSequence );
	    numberOfConformations = this->_NumberOfConformationsPerSequence;
	} else
	{
	    numberOfConformations = generator->numberOfNecessaryConformationsInCurrentSequence();
	    if ( this->getVerbose() ) this->lisp()->print(BF("Building every conformation for this "
							     "sequence, thats %lld conformations") % numberOfConformations );
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
		RPHit hit = hitList->createHitWithStates(generator, bestBuilderState,hitScorerState);
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
		if ( this->getVerbose() ) this->lisp()->print(BF("  Done %8lld/%8lld conformations, bestScore = %s") % 	confCount % numberOfConformations % bestScorerState->summary().c_str());
	    }
	    if ( confCount >= numberOfConformations ) break;
	}
    }
    if ( this->getVerbose() ) 
    {
	this->lisp()->print(BF("  Done %8lld/%8lld conformations, bestScore = %s") % confCount % numberOfConformations % bestScorerState->summary().c_str());
        this->lisp()->print(BF("%s") % statistics->summary().c_str() );
    }
    this->_ConformationsBuilt = confCount;
}







class	ExhaustiveSearchOneSequence_Exposer : public Exposer
{
    void exposeCando()
    {
	class_<O_ExhaustiveSearchOneSequence>(this->lisp())
	.def("run",&O_ExhaustiveSearchOneSequence::run)
	;
    }
    void exposePython()
    {
#ifdef	USEBOOSTPYTHON
	boost::python::class_<O_ExhaustiveSearchOneSequence,
	    boost::shared_ptr<O_ExhaustiveSearchOneSequence>,
	    boost::python::bases <O_ExhaustiveSearchOneSequence::Base>,
	    boost::noncopyable> ("O_ExhaustiveSearchOneSequence", boost::python::no_init )
	;
#endif
    }
};




OLD_EXPOSE_CLASS(O_ExhaustiveSearchOneSequence,ExhaustiveSearchOneSequence_Exposer);
};
