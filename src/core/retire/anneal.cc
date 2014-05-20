#define	DEBUG_LEVEL_FULL

#include "simulatedAnnealing.h"
#include "archiveNode.h"
#include "archive.h"
#include "lisp.h"
#include "builder.h"
#include "generator.h"
#include "builderState.h"
#include "hits.h"
#include "scorerBase.h"
#include "scorerState.h"
#include "render.h"
#include "numerics.h"
#include "keyedObject.h"
#include "posixTime.h"
#include "searchStatistics.h"
#include "keyedArguments.h"
#include "wrappers.h"



namespace mbb
{


    STORE_PREDEFINED_SYMBOL(_sym_MbbPackage_simulatedAnnealingSearchTypeConverter);


    void ScorePoint::archive(RPNode node)
    {
	node->attribute("s",this->_Score);
	node->attribute("a",this->_Accepted);
    }

    RPCons ScorePoint::asCons(RPLisp env)
    {
	return O_Cons::createList(env->create<O_Real>(this->_Score),env->create<O_Bool>(this->_Accepted),env);
    }


    void OneTemperatureScoreSeries::archive(RPNode node)
    {
	node->attribute("temperature",this->_Temperature);
	node->archiveVectorPlainObjects("scores",this->_Scores);
    }

    RPCons OneTemperatureScoreSeries::asCons(RPLisp env)
    {
	RPCons dummy = O_Cons::create(O_Object::nil(env),O_Cons::nil(env),env);
	RPCons cur = dummy;
	for ( vector<ScorePoint>::iterator it=this->_Scores.begin(); it!=this->_Scores.end(); it++ )
	{
	    RPCons one = O_Cons::create(it->asCons(env),O_Cons::nil(env),env);
	    cur->setCdr(one);
	    cur = one;
	}
	RPCons result = O_Cons::createList(env->create<O_Real>(this->_Temperature),dummy->cdr(),env);
	return result;
    }



    void O_ScoreLogger::initialize()
    {
	this->Base::initialize();
	this->_CreationTime = O_PosixTime::nil(this->lisp());
	this->_LastUpdateTime = O_PosixTime::nil(this->lisp());
    }

    void O_ScoreLogger::archiveBase(RPNode node)
    {
	this->Base::archiveBase(node);
	node->archiveVectorPlainObjects("temperatureScoreSeries",this->_TemperatureScoreSeries);
	node->archiveObject("creationTime",this->_CreationTime);
	node->archiveObject("lastUpdateTime",this->_LastUpdateTime);
    }


    void O_ScoreLogger::newTemperature(double temperature)
    {_OF();
	if ( this->_CreationTime->isNil() )
	{
	    this->_CreationTime->setToLocalTime();
	    this->_LastUpdateTime->setToLocalTime();
	}
	OneTemperatureScoreSeries one(_lisp);
	one._Temperature = temperature;
	this->_TemperatureScoreSeries.push_back(one);
    }

    void O_ScoreLogger::appendScore(double score, bool accepted)
    {_G();
	ASSERT(this->_TemperatureScoreSeries.size()!=0);
	OneTemperatureScoreSeries& one = this->_TemperatureScoreSeries.back();
	ScorePoint scorePoint(_lisp);
	scorePoint._Score = score;
	scorePoint._Accepted = accepted;
	one._Scores.push_back(scorePoint);
	this->_LastUpdateTime->setToLocalTime();
    }





    RPCons O_ScoreLogger::asCons()
    {_G();
	RPCons dummy = O_Cons::create(O_Object::nil(this->lisp()),O_Cons::nil(this->lisp()),this->lisp());
	RPCons cur = dummy;
	for (vector<OneTemperatureScoreSeries>::iterator it=this->_TemperatureScoreSeries.begin();
	     it!=this->_TemperatureScoreSeries.end(); it++ )
	{
	    RPCons one = O_Cons::create(it->asCons(this->lisp()),O_Cons::nil(this->lisp()),this->lisp());
	    cur->setCdr(one);
	    cur = one;
	}
	return dummy->cdr();
    }

    RPPosixTimeDuration O_ScoreLogger::runDuration()
    {_G();
	RPPosixTimeDuration dur = this->_LastUpdateTime->sub(this->_CreationTime);
	return dur;
    }




    void O_ScoreLoggerList::lisp_initGlobals(RPLisp e)
    {
    }

    void O_ScoreLoggerList::exposeCando(RPLisp lisp)
    {
	class_<O_ScoreLoggerList>()
	    .def("asCons",&O_ScoreLoggerList::asCons)
	    .def("append",&O_ScoreLoggerList::append)
	    ;
    }


    void O_ScoreLoggerList::exposePython()
    {
#ifdef	USEBOOSTPYTHON //[
	boost::python::class_<O_ScoreLoggerList,
	    boost::shared_ptr<O_ScoreLoggerList>,
	    boost::python::bases <O_ScoreLoggerList::Base>,
	    boost::noncopyable> ("O_ScoreLoggerList", boost::python::no_init )
	    ;
#endif //]
    }


    void O_ScoreLoggerList::oldLispInitialize(RPKeyedArguments kargs, RPLisp env)
    {
	this->Base::oldLispInitialize(kargs,env);
    }

    void O_ScoreLoggerList::initialize()
    {
	this->_ScoreLoggers.clear();
    }

    void O_ScoreLoggerList::archiveBase(RPNode node)
    {
	this->Base::archiveBase(node);
	node->archiveVector0("scoreLoggers",this->_ScoreLoggers);
    }



    void O_ScoreLoggerList::append(RPScoreLogger logger)
    {_G();
	this->_ScoreLoggers.append(logger);
    }





    RPCons O_ScoreLoggerList::asCons()
    {_G();
	RPCons first = O_Cons::create(this->lisp());
	RPCons cur = first;
	for (List<O_ScoreLogger>::iterator si=this->_ScoreLoggers.begin(); si!=this->_ScoreLoggers.end(); si++ )
	{
	    RPCons one = O_Cons::create((*si)->asCons(),this->lisp());
	    cur->setCdr(one);
	    cur = one;
	}
	return first->cdr();
    }






//
// Constructor
//

//
// Destructor
//
    void O_Anneal::exposeCando(RPLisp lisp)
    {
	class_<O_Anneal>(lisp->lisp())
	    .def("estimateInitialTemperature",&O_Anneal::estimateInitialTemperature)
	    .def("perturbStateAndScore",&O_Anneal::perturbStateAndScore)
	    .def("checkAccept",&O_Anneal::checkAccept)
	    .def("run",&O_Anneal::run)
	    .def("getScoreLogger",&O_Anneal::getScoreLogger)
	    .def("setupScoreLogger",&O_Anneal::setupScoreLogger)
	    ;
	defNoWrapPackage(MbbPackage,"simulatedAnnealingSearchForSequenceWithBestConformation",&O_Anneal::prim_simulatedAnnealingSearchForSequenceWithBestConformation,lisp->lisp()); // ctor
	defNoWrapPackage(MbbPackage,"simulatedAnnealingSearchForBestConformationOfOneSequence",&O_Anneal::prim_simulatedAnnealingSearchForBestConformationOfOneSequence,lisp->lisp()); // ctor
    }

    void O_Anneal::lisp_initGlobals(RPLisp lisp)
    {_G();
	CREATE_PREDEFINED_SYMBOL(_sym_MbbPackage_simulatedAnnealingSearchTypeConverter,
				 MbbPackage,"simulatedAnnealingSearchTypeConverter");
	enum_<AnnealType>(MbbPackage,"AnnealType",_sym_MbbPackage_simulatedAnnealingSearchTypeConverter,lisp->lisp())
	    .value("sequenceWithBestConformation", sequenceWithBestConformation)
	    .value("bestConformationOfOneSequence", bestConformationOfOneSequence)
	    ;
    }

    void	O_Anneal::oldLispInitialize(RPKeyedArguments kargs, RPLisp env)
    {_G();
	this->Base::oldLispInitialize(kargs,env);
	RPGenerator generator = kargs->getAndRemove("generator")->as<O_Generator>();
	RPHitList hitList = kargs->getAndRemove("hitList")->as<O_HitList>();
	int searchType = kargs->getIntAndRemove("searchType");
	RPCons options = kargs->getAndRemoveOrDefault("options",O_Cons::nil(this->lisp()))->as<O_Cons>();
	this->setGenerator(generator);
	this->setHitList(hitList);
	this->setOptions(options);
	this->setSearchType((AnnealType)(searchType));
	this->takeInitialStep();
    }

    void	O_Anneal::initialize()
    {
	this->Base::initialize();
	this->_TookInitialStep = false;
	this->_ScoreLogger = O_ScoreLogger::nil(this->lisp());
	this->setDefaultOptions();
    }




    void	O_Anneal::setDefaultOptions()
    { // Do not use _OF()
	this->Base::setDefaultOptions();
	this->_UseRandomConformations = true;
	this->_MaxPerturbationsToFindStartingPoint = 10000;
	this->_NumberOfTemperatureEstimationSteps = 100;
	this->_NumberOfConformationsPerSequence= 100;
	this->_NumberOfTemperatureSteps= 100;
	this->_NumberOfStepsPerTemperature = 100;
	this->_EstimateInitialTemperature = true;
	this->_InitialTemperature = 1.0;
	this->_PerturbOligomerProbability = 0.01;
	this->_ShowProgress = true;
	this->_TemperatureStepScale = 0.9;
	this->_ScaleInitialTemperatureEstimate = 10.0;
	this->_MaxRejectsPerTemperatureEstimationStep = 1000;
    }

    void	O_Anneal::setOptions(RPCons options)
    {
	this->setKeyedOptions(options->asKeyedArguments());
    }


    void	O_Anneal::setKeyedOptions(RPKeyedArguments koptions)
    {_G();
	RPCons cur;
	this->Base::setKeyedOptions(koptions);
	this->_MaxPerturbationsToFindStartingPoint = koptions->getIntAndRemoveOrDefault("MaxPerturbationsToFindStartingPoint",this->_MaxPerturbationsToFindStartingPoint);
	this->_NumberOfConformationsPerSequence = koptions->getIntAndRemoveOrDefault("NumberOfConformationsPerSequence",this->_NumberOfConformationsPerSequence);
	this->_NumberOfTemperatureSteps = koptions->getIntAndRemoveOrDefault("NumberOfTemperatureSteps",this->_NumberOfTemperatureSteps);
	this->_NumberOfTemperatureEstimationSteps = koptions->getIntAndRemoveOrDefault("NumberOfTemperatureEstimationSteps",this->_NumberOfTemperatureEstimationSteps);
	this->_ScaleInitialTemperatureEstimate = koptions->getDoubleAndRemoveOrDefault("ScaleInitialTemperatureEstimate",this->_ScaleInitialTemperatureEstimate);
	this->_TemperatureStepScale = koptions->getDoubleAndRemoveOrDefault("TemperatureStepScale",this->_TemperatureStepScale);
	this->_NumberOfStepsPerTemperature = koptions->getIntAndRemoveOrDefault("NumberOfStepsPerTemperature",this->_NumberOfStepsPerTemperature);
	this->_UseRandomConformations = koptions->getBoolAndRemoveOrDefault("UseRandomConformations",this->_UseRandomConformations);
	if ( koptions->recognizesKey("InitialTemperature") )
	{
	    this->_EstimateInitialTemperature = false;
	}
	this->_InitialTemperature = koptions->getDoubleAndRemoveOrDefault("InitialTemperature",this->_InitialTemperature);
	this->_PerturbOligomerProbability = koptions->getDoubleAndRemoveOrDefault("PerturbOligomerProbability",this->_PerturbOligomerProbability);
	koptions->throwOnUnusedArguments(0);
	if ( this->_UseRandomConformations && this->_NumberOfConformationsPerSequence <= 0 )
	{
	    TOSS(_lisp->create<O_LispError>("If you set UseRandomConformations to true then you must specify numberOfConformationsPerSequence>0"));
	}
    }



    string O_Anneal::description() const
    {
	stringstream ss;
	ss << this->className();
#if 0
	ss << "options: (::" << endl;
#define	VAR(x) boost::format("%1%:%2%") % #x % this->x
	ss << VAR(_NumberOfTemperatureSteps) << endl;
	ss << VAR(_NumberOfTemperatureEstimationSteps) << endl;
	ss << VAR(_NumberOfStepsPerTemperature) << endl;
#endif
	return ss.str();
    }






    RPObject O_Anneal::prim_simulatedAnnealingSearchForSequenceWithBestConformation(RPExecutable e, RPCons args, RPEnvironment environ, RPLisp lisp )
    {_G();
	if ( args->length()<2 ) TOSS(_lisp->create<O_LispError>("You must provide a Generator and a hitList and optional:options"));
	RPAnneal search = lisp->create<O_Anneal>();
	search->setGenerator(args->listref<O_Generator>(0));
//    search->setScorer(args->listref<O_ScorerBase>(1));
	search->setHitList(args->listref<O_HitList>(1));
	search->setSearchType(sequenceWithBestConformation);
	if ( args->length() == 3 )
	{
	    RPCons options = args->listref<O_Cons>(2);
	    search->setOptions(options);
	}
	search->takeInitialStep();
	search->run();
	return search;
    }


    RPObject O_Anneal::prim_simulatedAnnealingSearchForBestConformationOfOneSequence(RPExecutable e, RPCons args, RPEnvironment environ, RPLisp lisp )
    {_G();
	if ( args->length()<2 ) TOSS(_lisp->create<O_LispError>("You must provide a Generator and a hitList and optional:options"));
	RPAnneal search = lisp->lisp()->create<O_Anneal>();
	search->setGenerator(args->listref<O_Generator>(0));
//    search->setScorer(args->listref<O_ScorerBase>(1));
	search->setHitList(args->listref<O_HitList>(1));
	search->setSearchType(bestConformationOfOneSequence);
	if ( args->length() == 3 )
	{
	    RPCons options = args->listref<O_Cons>(2);
	    search->setOptions(options);
	}
	search->takeInitialStep();
	search->run();
	return search;
    }


    void	O_Anneal::perturbSequence(RPGenerator generator)
    {_G();
	//
	// Generate a trial state
	//
#if 0
	if ( randomNumber01(_lisp)<this->_PerturbOligomerProbability )
	{
//	if (this->_ShowProgress) _lisp->print(BF( "      Perturbing oligomer/sequence" ));
	    builder->perturbOligomer();
	} else
	{
//	if ( this->_ShowProgress ) _lisp->print(BF( "      Perturbing sequence" ));
	    builder->perturbSequence();
	}
#endif

	//
	// For now just perturb the sequence
	//
	generator->perturbSequence();
	if ( this->_ShowProgress )
	{
	    string sequence = generator->getSequenceAsString();
	    _lisp->print(BF("#  sequence = %s") % sequence.c_str() );
	}
    }

    void O_Anneal::searchForBestConformation(RPGenerator generator,
							       RPBuilderState bestBuilderState, 
							       RPScorerState bestScorerState )
    {_G();
	ASSERT_NOT_NULL(bestBuilderState);
	ASSERT_NOT_NULL(bestScorerState);
	bestScorerState->clear();
	RPScorerState tempScorerState = generator->createScorerState();
	LongLongInt numberOfConformations = this->_NumberOfConformationsPerSequence;
	if ( numberOfConformations == 0 )
	{
	    numberOfConformations = generator->numberOfNecessaryConformationsInCurrentSequence();
	}
	if ( !this->_UseRandomConformations )
	{
	    generator->firstConformation();
	}
	for ( LongLongInt conf=0; conf<numberOfConformations; conf++ )
	{_BLOCK_TRACEF(BF("loop %lu of number of conformations(%lu)") % conf % numberOfConformations);
	    if ( this->_UseRandomConformations )
	    {
		generator->randomizeConformation();
	    } else
	    {
		generator->incrementConformation();
	    }
	    this->_NumberOfConformationsBuilt++;
	    tempScorerState->clear();
	    LOG(BF("The builder is now ready to score in state: %s")% generator->stateIdentifier());
	    generator->buildNecessaryUntransformedAtomPositionsAndEvaluateScorer(tempScorerState);
	    if ( tempScorerState->isBetterThan(bestScorerState) )
	    {_BLOCK_TRACEF(BF("Found the best score so far = %lf")% tempScorerState->getScore());
		generator->saveBuilderState(bestBuilderState);
		bestScorerState->copyFromOtherState(tempScorerState);

#if 0 // def	DEBUG_ON
		generator->getBuilder()->logFirstInterestingAtom("simulatedAnnealing(repeat: just found better scoring state and just saved it)");
		LOG(BF("Dumping the saved BuilderState"));
		LOG(bestBuilderState->asXmlString().c_str());
#endif
	    }
	}
	generator->restoreConformationOfBuilderState(bestBuilderState);
#if 0 // def	DEBUG_ON
	generator->getBuilder()->logFirstInterestingAtom("simulatedAnnealing(just restored best scoring state)");
	stringstream builderStateStream;
	generator->streamDump(builderStateStream);
	LOG(builderStateStream.str().c_str());
	LOG(BF("Dumping the saved BuilderState"));
	LOG(bestBuilderState->asXmlString().c_str());
#endif
    }


    void O_Anneal::perturbStateAndScore( RPGenerator generator, RPBuilderState bestBuilderState, RPScorerState bestScorerState)
    {_G();
	ASSERT(this->_TookInitialStep);
	if ( this->_SearchType == sequenceWithBestConformation )
	{
	    this->perturbSequence(generator);
	    this->searchForBestConformation(generator,bestBuilderState,bestScorerState);
	    return;
	} else if ( this->_SearchType == bestConformationOfOneSequence )
	{
	    LOG(BF("Perturbing conformation") ); // vp0(("Perturbing conformation"));
	    // perturb conformation
	    //
	    generator->perturbConformation();
	    this->_NumberOfConformationsBuilt++;
	    //
	    // Build and score the conformation
	    //
	    LOG(BF("Evaluating scorer") ); // vp0(( "Evaluating scorer"));
	    bestScorerState->clear();
	    generator->buildNecessaryUntransformedAtomPositionsAndEvaluateScorer(bestScorerState);

	    if ( this->_ShowProgress )
	    {
		_lisp->print(BF("#  conformation score = %lf") % bestScorerState->getScore() );
	    }
	    generator->saveBuilderState(bestBuilderState);
	    return;
	}
	TOSS(_lisp->create<O_LispError>("Unknown _SearchType"));
    }



    double	O_Anneal::_estimateInitialTemperature(RPSearchStatistics stats )
    {_G();
	if (!this->_TookInitialStep)
	{
	    this->takeInitialStep();
	}
	RPGenerator generator = this->getGenerator();

	if ( this->_ShowProgress )
	{
	    _lisp->print(BF("Estimating initial temperature"));
	}
	RPBuilderState builderState = O_BuilderState::create(this->lisp());
	RPScorerState bestScorerState = generator->createScorerState();
	this->perturbStateAndScore(generator,builderState,bestScorerState);
	double previousScore = bestScorerState->getScore();
	double largestDelta = 0.0;
	double absDelta, delta;
	this->_InitialTemperatureBuilds = 0;
	this->_InitialTemperatureRejects = 0;
	for ( int i=0; i<this->_NumberOfTemperatureEstimationSteps; i++ )
	{
	    //
	    // Perturb the current state
	    //
	    for ( uint ai=0; ai<this->_MaxRejectsPerTemperatureEstimationStep; ai++ )
	    {
		this->_InitialTemperatureBuilds++;
		this->perturbStateAndScore(generator,builderState,bestScorerState);
		stats->incrementBuilds();
		if (!bestScorerState->getReject()) 
		{
		    break;
		} else
		{
		    stats->incrementRejects();
		    this->_InitialTemperatureRejects++;
		}
	    }
	    // If we get here and there was a reject then 
	    // the scoring function is generating way too many rejects
	    // and we won't be able to estimate the initial temperature
	    if ( bestScorerState->getReject() )
	    {
		stringstream ss;
		ss << "This scoring function generated too many rejects" << endl;
		ss << "Attempted " << this->_MaxRejectsPerTemperatureEstimationStep;
		ss << " and all of them were rejected.";
		TOSS(_lisp->create<O_LispError>(ss.str()));
	    }
	    stats->incrementHits();
	    double curScore = bestScorerState->getScore();
	    delta = bestScorerState->getScore() - previousScore;
	    previousScore = bestScorerState->getScore();
	    absDelta = fabs(delta);
	    largestDelta = MAX(absDelta,largestDelta);
	    if ( this->_ShowProgress )
	    {
		if ( i % 10 == 0 )
		{
		    _lisp->print(BF("%5s %8s %8s %8s") % "Step"%"Score"% "Delta"% "MaxDelta" );
		}
		_lisp->print(BF("%5d %8.3lf %8.3lf %8.3lf") % i% curScore% absDelta% largestDelta );
	    }
	}
	double initTemp = largestDelta*this->_ScaleInitialTemperatureEstimate;
	if ( this->_ShowProgress )
	{
	    _lisp->print(BF( "Initial temperature estimation report"));
	    _lisp->print(BF( "Initial temperature = %lf") % initTemp );
	    _lisp->print(BF( "There were %d builds and %d rejects")
			 % this->_InitialTemperatureBuilds
			 % this->_InitialTemperatureRejects );
	}
    	// Estimate the temperature by scaling up the largest delta.
	return largestDelta*this->_ScaleInitialTemperatureEstimate;
    }


/*! Setup to run the simulated annealing simulation.
 */
    void O_Anneal::takeInitialStep()
    {_G();
	RPGenerator generator = this->getGenerator();
	if ( this->_SearchType == sequenceWithBestConformation )
	{
	    generator->perturbOligomer();
	    generator->firstSequence();
	} else
	{
	    generator->setupCurrentOligomerAndSequence();
	}
	generator->firstConformation();
	this->_TookInitialStep = true;
    }


    double	O_Anneal::estimateInitialTemperature(RPSearchStatistics stats )
    {_G();
	RPGenerator generator = this->getGenerator();
	if ( this->_SearchType == sequenceWithBestConformation )
	{
	    generator->perturbOligomer();
	    generator->firstSequence();
	}
	generator->firstConformation();
	double temperature = this->_estimateInitialTemperature(stats);
	return temperature;
    }

    void	O_Anneal::setupScoreLogger()
    {
	this->_ScoreLogger = O_ScoreLogger::create(this->lisp());
    }

    RPScoreLogger O_Anneal::getScoreLogger()
    {
	return this->_ScoreLogger;
    }


    bool O_Anneal::checkAccept(RPScorerState scorerState, double temperature, double currentScore )
    {_OF();
	bool acceptStep = false;
	double delta = scorerState->getScore() - currentScore;
	if ( this->_ShowProgress )
	{
	    _lisp->print(BF("delta = %lf") % delta );
	}
	if ( !scorerState->getReject() )
	{
	    if ( delta < 0 ) acceptStep = true;
	    else
	    {
		double exponent = - delta / temperature;
		double a = exp(exponent);
		double rand = randomNumber01(_lisp);
		if ( this->_ShowProgress )
		{
		    _lisp->print(BF("rand(%lf) < a(%lf) = %d") % rand% a% (rand<a) );
		}
		if ( rand < a )
		{
		    acceptStep = true;
		}
	    }
	}
	_lisp->print(BF( "acceptStep = %d") % acceptStep );
	return acceptStep;
    }


    void	O_Anneal::run()
    {_G();
	double a, rand, exponent;
	int stepCount = 0;
	this->_NumberOfConformationsBuilt = 0;
	LOG(BF("Running Anneal"));
	ASSERT_NOT_NULL(this->_Generator);
	ASSERT_NOT_NULL(this->_HitList);
	RPGenerator generator = this->getGenerator();
	RPHitList hitList = this->_HitList;
	ASSERT_NOT_NULL(hitList);
	ASSERT(hitList->notNil());
	RPRender bestScoreGraphics = this->lisp()->create<O_RenderDisplayList>();
	bestScoreGraphics->setName("bestScore");
	ASSERT(this->_TookInitialStep);
	RPScorerState scorerState = generator->createScorerState();
	RPScorerState currentScorerState = generator->createScorerState();
	RPBuilderState currentState = O_BuilderState::create(this->lisp());
	RPBuilderState builderState = O_BuilderState::create(this->lisp());
	if ( this->_SearchType == sequenceWithBestConformation )
	{
	    this->searchForBestConformation(generator,currentState,scorerState);
	}
	double temperature = this->_InitialTemperature;
	if ( this->_EstimateInitialTemperature )
	{
	    if ( this->_ShowProgress )
	    {
		_lisp->print(BF("Estimating initial temperature"));
	    }
	    temperature = this->estimateInitialTemperature(hitList->getSearchStatistics());
	    if ( this->_ShowProgress )
	    {
		_lisp->print(BF("Estimated initial temperature = %lf") % temperature);
	    }
	    ASSERT(temperature!=0.0);
	}
	if ( temperature == 0.0 )
	{
	    TOSS(_lisp->create<O_LispError>("Temperature cannot be zero!!!!!"));
	}
	//
	// Evaluate the score at the current position
	//
	generator->evaluateScorer(scorerState);
	double currentScore = scorerState->getScore();
	this->_Accepts = 0;
	int maxInTempAccepts = MIN(10,((int)(0.1*this->_NumberOfStepsPerTemperature)));
	for ( int tempStep = 0; tempStep < this->_NumberOfTemperatureSteps; tempStep++ )
	{   _BLOCK_TRACEF(BF("Temperature step loop of sequence %d") % tempStep );
	    if ( this->_ScoreLogger->notNil() )
	    {
		this->_ScoreLogger->newTemperature(temperature);
	    }
	    int inTempAccepts = 0;
	    for ( int inTempStep = 0; inTempStep < this->_NumberOfStepsPerTemperature; inTempStep++ )
	    { _BLOCK_TRACEF(BF("Toptemperature(%lf) step(%d)")% temperature % inTempStep);
		if ( this->_ShowProgress )
		{
		    _lisp->print(BF("------------------ step start -----------------"));
		    _lisp->print(BF("Current state = %s   currentScore = %lf") % generator->changeCountersAsString().c_str()% currentScore );
		}
		//
		// Save the current builder state
		//
		generator->saveBuilderState(currentState);

		//
		// Perturb the current state
		//
		{ _BLOCK_TRACE("In this block I should return the best score that I find");
		    this->perturbStateAndScore(generator,builderState,scorerState);
		    LOG(BF("The best score I found was %lf")% scorerState->getScore());
		}
		if ( this->_ShowProgress )
		{
		    _lisp->print(BF("    Perturbed state %s    score = %lf") % builderState->changeCountersAsString().c_str()% scorerState->getScore() );
		    LOG(BF("    Perturbed state %s    score = %lf\n")% 
			builderState->changeCountersAsString().c_str()
			% scorerState->getScore() );
		}
#if DEBUG_ON
		generator->getBuilder()->logFirstInterestingAtom("simulatedAnnealing(just left perturbStateAndScore)");
		stringstream log;
		generator->streamDump(log);
		LOG(BF("Just evaluated score(%lf) Logging state for OligomerBuilder")% scorerState->getScore());
		LOG(BF("%s")%log.str());
#endif
		//
		// Check if the score is a hit
		if ( hitList->isAHit(scorerState) )
		{
		    LOG(BF("Adding hit with scorerState = %s")% scorerState->summary());
		    // The scorer doesn't need to be informed of conformation changes
#if 0 // def	DEBUG_ON
		    generator->getBuilder()->logFirstInterestingAtom("simulatedAnnealing(about to call createHitWithStates)");
#endif
		    RPHit hit = hitList->createHitWithStates(generator,builderState,scorerState);
		}

		// If this score is better than the best then
		// keep the state
		bool acceptStep = false;
		acceptStep = this->checkAccept(scorerState,temperature,currentScore);
		double prevScore = currentScore;
		if ( this->_ScoreLogger->notNil() )
		{
		    this->_ScoreLogger->appendScore(scorerState->getScore(),acceptStep);
		}
		if ( acceptStep )
		{
		    this->_Accepts++;
		    inTempAccepts++;
		    currentScore = scorerState->getScore();
		} else
		{
		    generator->restoreState(currentState);
		}
	    	// If there are maxInTempAccepts 
		if ( inTempAccepts > maxInTempAccepts ) 
		{
		    LOG(BF("Encountered too many in temperature accepts, advancing temperature") ); // vp0(("Encountered too many in temperature accepts, advancing temperature"));
		    break;
		}
		if ( this->_ShowProgress )
		{
		    const char * acceptStr;
		    if ( acceptStep ) acceptStr = "Yes";
		    else acceptStr = "No";
		    int stepsLeft = this->_NumberOfStepsPerTemperature-inTempStep;
		    _lisp->print(BF("  Step[%5d] Temp[%5.3lf] Prev.Score[%10.3lf] Score[%10.3lf] Accept[%3s] StepsLeft[%3d]") %  stepCount % temperature% prevScore% scorerState->getScore()% acceptStr% stepsLeft  );
		}
		stepCount++;
	    }
	    if ( inTempAccepts == 0 ) break;
	    temperature *= this->_TemperatureStepScale;
	}
	if ( this->_ShowProgress ) 
	{
	    _lisp->print(BF("\nCarried out %d simulated annealing steps\n") % stepCount );
	    _lisp->print(BF("\nBuild %d TOTAL structures\n") % this->_NumberOfConformationsBuilt );
	}
    }


    class	ScoreLogger_Exposer : public Exposer
    {
	void exposeCando()
	{
	    class_<O_ScoreLogger>(this->lisp())
		.def("asCons",&O_ScoreLogger::asCons)
		.def("runDuration",&O_ScoreLogger::runDuration)
		;
	}
	void exposePython()
	{
#if 0 // def	USEBOOSTPYTHON
	    boost::python::class_<O_ScoreLogger,
		boost::shared_ptr<O_ScoreLogger>,
		boost::python::bases <mbb::O_Object>,
		boost::noncopyable> ("O_ScoreLogger", boost::python::no_init )
		;
#endif
	}
    };




    OLD_EXPOSE_CLASS(O_ScoreLogger,ScoreLogger_Exposer);
    EXPOSE_CLASS_AND_GLOBALS(O_ScoreLoggerList);
    EXPOSE_CLASS_AND_GLOBALS(O_Anneal);



};


