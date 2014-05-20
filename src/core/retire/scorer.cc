#define	DEBUG_LEVEL_FULL

#include "scorer.h"
#include "archiveNode.h"
#include "archive.h"
#include "builder.h"
#include "alias.h"
#include "render.h"
#include "restraint.h"
#include "ovector3.h"
#include "omatrix.h"
#include "keyedArguments.h"
#include "scoreOperations.h"
#include "scorerState.h"
#include "builderState.h"
#include "superposeEngine.h"
#include "hits.h"
#include "wrappers.h"


namespace mbb {


/*
__BEGIN_DOC(classes.Superpose.!class.Superpose)
\positional{Alias::monomerAtomAlias OVector3::position}

Create an Superpose object that maintains a \sa{monomerAtomAlias} name and the point that it is supposed to superimpose on top of.
__END_DOC
*/
void	O_SuperposeAlias::oldLispInitialize(RPKeyedArguments args, RPLisp env)
{_G();
    LOG(BF("Arguments before Base::oldLispInitialize =%s") % args->__repr__() );
    this->Base::oldLispInitialize(args,env);
    LOG(BF("Arguments after Base::oldLispInitialize =%s") % args->__repr__() );
    if ( args->numberOfPositionalArguments()<2 ) 
	THROW(_lisp->create<O_LispError>("You must provide 2 args: monomerAtomAlias position"));
    this->_Alias = args->getPositionalArgument(0)->as<O_Alias>();
    this->_FixedPosition = args->getPositionalArgument(1)->as<O_OVector3>()->get();
}








void	O_SuperposeAlias::initialize()
{
    this->Base::initialize();
    this->_Alias = O_Alias::nil(this->lisp());
    this->_MonomerId = -1;
    this->_AtomId = -1;
}









void	O_SuperposeAlias::archiveBase(RPNode node)
{
    this->Base::archiveBase(node);
    node->archiveObject("alias",this->_Alias);
    node->archivePlainObjectIfDefined<Vector3>( "pos","Vector3",
    					this->_FixedPosition.isDefined(),
					this->_FixedPosition );
}



void	O_SuperposeAlias::set( RPAlias alias, const Vector3& pos )
{_G();
    this->setAlias(alias);
    this->setPosition(pos);
}






/*
__BEGIN_DOC(classes.Scorer.!class.Scorer)
\requiredKeyed{superposeAtoms}{Cons::superposeAtoms}
\optionalKeyed{calculator}{ScoreSum::calculator}

Create an Scorer object.
__END_DOC
*/
void	O_Scorer::oldLispInitialize(RPKeyedArguments args, RPLisp env)
{_G();
    this->Base::oldLispInitialize(args,env);
    RPCons items = args->getAndRemoveOrDefault("superposeAtoms",O_Cons::nil(this->lisp()))->as<O_Cons>();
    this->_ScoreCalculator = args->getAndRemove("calculator")->as<O_ScoreSum>();
    for ( RPCons p=items; p->notNil(); p=p->cdr() )
    {
	RPSuperposeAlias item = p->car<O_SuperposeAlias>();
	this->_Superposes.push_back(item);
    }
}


//
// Constructor
//

void	O_Scorer::initialize()
{_G();
    LOG(BF("Initializing scorer"));
    this->Base::initialize();
    this->_SuperposeEngine = O_SuperposeEngine::nil(this->lisp());
    this->_ScoreCalculator = O_ScoreSum::create(this->lisp());
    this->_ScorerIdentifier = UndefinedUnsignedInt;
}

//
// Destructor
//

void	O_Scorer::archiveBase(RPNode node)
{_G();
    this->Base::archiveBase(node);
    node->archiveVector0OfObjectsSubClassOf(this->_Superposes);
    node->archiveObject("calculator",this->_ScoreCalculator);
    node->attribute("identifier",this->_ScorerIdentifier);
}


void	O_Scorer::oligomerChanged(RPPointProvider builder)
{_G();
List<O_SuperposeAlias>::iterator	it;
RPAlias					alias;
int					monomerId;
    this->Base::oligomerChanged(builder);
    for ( it=this->_Superposes.begin(); it!=this->_Superposes.end(); it++ )
    {
	alias = (*it)->getAlias();
	monomerId = builder->getMonomerIdForUniqueAlias(alias);
	(*it)->setMonomerId(monomerId);
    }
    if ( this->_ScoreCalculator->notNil() )
    {
	if ( builder->notNil() )
	{
            this->_ScoreCalculator->oligomerChanged(builder);
	}
    }
}

void	O_Scorer::sequenceChanged(RPPointProvider builder)
{_G();
List<O_SuperposeAlias>::iterator	it;
int				atomId;
    this->Base::sequenceChanged(builder);
    for ( it=this->_Superposes.begin(); it!=this->_Superposes.end(); it++ )
    {
        LOG(BF("MonomerId = %d") % (*it)->getMonomerId()  ); // vp0(("MonomerId = %d",(*it)->getMonomerId() ));
	atomId = builder->indexOfInterestingAtomWithAlias((*it)->getMonomerId(),(*it)->getAlias());
	LOG(BF("    Got atomId=%d") % atomId ); // vp0(("    Got atomId=%d",atomId));
	(*it)->setAtomId(atomId);
    }
    if ( this->_ScoreCalculator->notNil() )
    {
	if ( builder->notNil() )
	{
            this->_ScoreCalculator->sequenceChanged(builder);
	}
    }
}




bool	O_Scorer::needsAllAtomsBuilt()
{_G();
    bool needsAll = this->Base::needsAllAtomsBuilt();
    if ( needsAll ) return true;
    return this->_ScoreCalculator->needsAllAtomsBuilt();
}





void	O_Scorer::_initializeSuperposeEngine()
{_G();
RPCoordinateArray	fixed, moveable;
List<O_SuperposeAlias>::iterator	it;
    this->_SuperposeEngine = this->lisp()->create<O_SuperposeEngine>();
    fixed = this->lisp()->create<O_CoordinateArray>();
    moveable = this->lisp()->create<O_CoordinateArray>();
    for ( it=this->_Superposes.begin(); it!=this->_Superposes.end(); it++ )
    {
        fixed->appendElement((*it)->getPosition());
	  // initially fill the moveable points with
	  // the same data as the fixed points just to
	  // fill it in with something
        moveable->appendElement((*it)->getPosition());
    }
    this->_SuperposeEngine->setFixedAllPoints(fixed);
    this->_SuperposeEngine->setMoveableAllPoints(moveable);
}



void O_Scorer::_doSuperpose(RPPointProvider builder, Matrix& transformToFixed)
{_G();
List<O_SuperposeAlias>::iterator	sit;
RPCoordinateArray		moveable;
O_CoordinateArray::iterator	cit;
Vector3				pos;
    ASSERT_NOT_NULL(this->_SuperposeEngine);
    if ( this->_SuperposeEngine->isNil() )
    {
        this->_initializeSuperposeEngine();
    }
    moveable = this->_SuperposeEngine->getMoveableCoordinates();
    ASSERT(moveable->size()==this->_Superposes.size());

    		//
		// Copy the superpose coordinates into moveable
		//
    {_BLOCK_TRACE("Copying superpose coordinates into moveable array");
	for ( sit=this->_Superposes.begin(), cit=moveable->begin();
		    sit!=this->_Superposes.end(); sit++, cit++ )
	{
	    pos = builder->getUntransformedAtomPosition((*sit)->getAtomId());
	    LOG(BF("Position for atomId(%d) = %s") % (*sit)->getAtomId() % pos.asString().c_str()  ); // vp0(("Position for atomId(%d) = %s",(*sit)->getAtomId(),pos.asString().c_str() ));
	    *cit = pos;
	}
    }
    transformToFixed = this->_SuperposeEngine->superpose();
}


bool O_Scorer::hasSuperposer()
{
    return this->_Superposes.size()!=0;
}


double O_Scorer::calculateSuperposerSumOfSquaresOfDifferences(RPScorerState scorerState)
{_OF();
    ASSERTP(this->_SuperposeEngine->notNil(),"You asked for the sum of squares of differences but you have no superpose engine");
    double score = this->_SuperposeEngine->sumOfSquaresOfDifferences(scorerState);
    return score;
}

void O_Scorer::_evaluate(RPPointProvider builder, RPScorerState scorerState, bool superpose)
{_G();
    IMPLEMENT_ME();
#if 0
    this->throwIfBuilderDoesntMatch(builder);
    LOG(BF("Setting scorerState ScorerIdentifier to %d")% this->getScorerIdentifier() );
    scorerState->clear();
    LOG_SCORE(scorerState,BF("O_Scorer::evaluate {"));
    scorerState->setScorerIdentifier(this->getScorerIdentifier());
    ASSERT_NOT_NULL(builder);
    ASSERT(builder->notNil());
    double score = 0.0;
    if ( this->_Superposes.size() > 0 )
    {_BLOCK_TRACE("This is a superposing scorer");
        {_BLOCK_TRACE("Carrying out superposition");
	    Matrix transform;
	    if ( superpose )
	    {
		LOG_SCORE(scorerState,BF("Carrying out superposition"));
		this->_doSuperpose(builder,transform);
	    }
#if DEBUG_SCORE_EVALUATION
	    scorerState->scoreLog() << "I don't log the superpose operation but here is a summary of the superposition and results" << endl;
	    this->_SuperposeEngine->sumOfSquaresOfDifferences(scorerState );
#endif
	    LOG_SCORE( scorerState,BF( "Did superpose for %s got transform: \n%s")% builder->stateIdentifier() % transform.asString() );
//	    LOG_SCORE(BF( scorerState, "Superposition report:\n%s")% this->_SuperposeEngine->report() );
//	    scorerState->setScoreTransform(transform);
    LOG_SCORE(scorerState,BF("Builder state after superposition:{\n%s\n}")% builder->stateAsString() );
	}
	if ( this->_ScoreCalculator->isNil() )
	{_BLOCK_TRACE("Calculating score: there is no ScoreCalculator so use superposeEngine");
	    LOG_SCORE(scorerState,BF("There is no ScoreCalculator - calculating score from superposeEngine"));
	    LOG(BF("Evaluating sumOfSquaresOfDifferences") ); // vp0(("Evaluating sumOfSquaresOfDifferences"));
	    ASSERT_NOT_NULL(this->_SuperposeEngine);
	    ASSERT(this->_SuperposeEngine->notNil());
	    score = this->calculateSuperposerSumOfSquaresOfDifferences(scorerState);
	    scorerState->setScore(score);
	} else
	{_BLOCK_TRACE("Calculating score: using score calculator");
	    score = this->_ScoreCalculator->evaluate(scorerState, this->sharedThis<O_Scorer>(),builder);
	    LOG(BF("Setting scorer state score=%lf") % score  ); // vp0(("Setting scorer state score=%lf", score ));
	    scorerState->setScore(score);
	}
    } else
    {_BLOCK_TRACE("This is not a superposing scorer, no superposition happening");
	if ( this->_ScoreCalculator->isNil() )
	{
	    THROW(_lisp->create<O_LispError>("There must be either a Superposer or a score calculator"));
	}
	score = this->_ScoreCalculator->evaluate(scorerState, this->sharedThis<O_Scorer>(),builder);
	LOG(BF("Setting scorer state score=%lf") % score  ); // vp0(("Setting scorer state score=%lf", score ));
	scorerState->setScore(score);
    }
    LOG_SCORE(scorerState,BF("Returning from O_Scorer::evaluate with score=%lf")% score );
    LOG_SCORE(scorerState,BF("O_Scorer::evaluate }"));
#endif	
}

void O_Scorer::evaluate(RPPointProvider builder, RPScorerState scorerState)
{
    this->_evaluate(builder,scorerState,true);
}

void O_Scorer::evaluateWithoutSuperposing(RPPointProvider builder, RPScorerState scorerState)
{
    this->_evaluate(builder,scorerState,false);
}
void O_Scorer::setScorerIdentifier(uint id)
{_G();
    this->_ScorerIdentifier = id;
}

uint O_Scorer::getScorerIdentifier()
{_G();
    return this->_ScorerIdentifier;
}


RPRenderDisplayList O_Scorer::getRenderForScore(RPPointProvider builder, RPScorerState scorerState)
{_G();
    this->throwIfBuilderDoesntMatch(builder);
    RPRenderDisplayList dlAll = O_RenderDisplayList::create(this->lisp());
    dlAll->setName("Scorer");
    RPRenderDisplayList dlOps;
    if ( this->_ScoreCalculator->notNil() )
    {
        dlOps = O_RenderDisplayList::create(this->lisp());
	this->_ScoreCalculator->renderIntoDisplayList(dlOps,scorerState,
					this->sharedThis<O_Scorer>(),builder);
    } else
    {
	dlOps = this->renderSuperposeDeviations(builder,scorerState );
    }
    dlAll->add(dlOps);
    dlAll->add(this->Base::getRenderForScore(builder,scorerState));
    if ( this->getBackgroundRender()->notNil() )
    {
	dlAll->add(this->getBackgroundRender());
    }
    return dlAll;
}

RPRenderDisplayList O_Scorer::renderSuperposeDeviations(RPPointProvider builder, RPScorerState scorerState)
{_G();
#ifdef	DEBUG_ON
    stringstream sdebug;
#endif
    this->throwIfBuilderDoesntMatch(builder);
    RPRenderDisplayList dl = O_RenderDisplayList::create(this->lisp());
    LOG(BF("About to render superpose deviations"));
    dl->setName("superposeDeviations");
    if ( this->_Superposes.size() != 0 )
    {
//	this->_doSuperpose(this->_LatestTransform);
	RPGrColor color = O_GrColor::systemColor(_lisp->symbol(_sym_kw_yellow));
	dl->add(color);
	List<O_SuperposeAlias>::iterator	sit;
	for ( sit=this->_Superposes.begin();
		    sit!=this->_Superposes.end(); sit++ )
	{
	    Vector3 moveablePos = builder->getScoreTransformedAtomPosition((*sit)->getAtomId(),scorerState);
	    LOG(BF("Position for atomId(%d) = %s") % (*sit)->getAtomId() % moveablePos.asString().c_str()  ); // vp0(("Position for atomId(%d) = %s",(*sit)->getAtomId(),moveablePos.asString().c_str() ));
	    LOG(BF("Line from moveablePos(%s) to (*sit)->getPosition(%s)") % moveablePos.asString().c_str() % (*sit)->getPosition().asString().c_str()  ); // vp0(("Line from moveablePos(%s) to (*sit)->getPosition(%s)", moveablePos.asString().c_str(), (*sit)->getPosition().asString().c_str() ));
	    RPGrLine line = O_GrLine::create(moveablePos,(*sit)->getPosition(),this->lisp());
	    dl->add(line);
	    LOG(BF("moveablePos = %s") % moveablePos.asString().c_str()  ); // vp0(("moveablePos = %s", moveablePos.asString().c_str() ));
#ifdef	DEBUG_ON
	    sdebug << "moveablePos atom(" << (*sit)->getAtomId() << ") = " << moveablePos.asString() << endl;
#endif
	    RPGrSphere sphere = O_GrSphere::create(moveablePos,0.1,this->lisp());
	    dl->add(sphere);
	}
    }
#ifdef	DEBUG_ON
    RPGrInformation info = O_GrInformation::create(sdebug.str(),this->lisp());
    dl->add(info);
#endif
    return dl;
}


void	O_Scorer::addToCalculator(RPScoreOperation op)
{_G();
    ASSERT_NOT_NULL(this->_ScoreCalculator);
    ASSERT(this->_ScoreCalculator->notNil());
    this->_ScoreCalculator->addOperation(op);
}









#if 0
Matrix	O_Scorer::getTransformForScore()
{_G();
    
    if ( this->_Superposes.size()==0 )
    {
	this->_LatestTransform.identity();
//    } else
//    {
//        this->_doSuperpose(this);
    }
//    return moveableTransform;
    return this->_LatestTransform;
}

Matrix& O_Scorer::transformForScore()
{_G();
    return this->_LatestTransform;
}
#endif




void O_Scorer::insertSuperpositionsIntoRestraintList(double anchorWeight, RPPointProvider builder, RPCons& restraints )
{_G();
    this->throwIfBuilderDoesntMatch(builder);
    List<O_SuperposeAlias>::iterator it;
    for ( it=this->_Superposes.begin();
    		 it!=this->_Superposes.end(); it++ )
    {
	RPRestraintAnchor anchor = O_RestraintAnchor::create(this->lisp());
	RPAtom atom = builder->getInterestingAtom((*it)->getAtomId());
	anchor->setAtom(atom);
	anchor->setAnchorPos((*it)->getPosition());
	anchor->setWeight(anchorWeight);
	restraints = O_Cons::create(anchor,restraints,this->lisp());
    }
}


void O_Scorer::insertScoreOperationsIntoRestraintList(RPPointProvider builder, RPCons& restraints)
{_G();
    ASSERT_NOT_NULL(this->_ScoreCalculator);
    this->_ScoreCalculator->insertRestraints(builder,this->sharedThis<O_Scorer>(),restraints);
}


RPCons O_Scorer::createRestraints(RPPointProvider builder)
{_G();
    RPCons restraints = O_Cons::nil(this->lisp());
    this->insertScoreOperationsIntoRestraintList(builder,restraints);
    return restraints;
}



class	Scorer_Exposer : public Exposer
{
    void exposeCando()
    {
	class_<O_Scorer>(this->lisp())
	    .def("addSuperposeAlias",&O_Scorer::addSuperposeAlias)
	    .def("addToCalculator",&O_Scorer::addToCalculator)
	    ;
//	def("create_Scorer",&create_Scorer);
//	defNoWrapPackage(MbbPackage,"setScorer",&prim_setScorer);
    }
    void exposePython()
    {
#ifdef	USEBOOSTPYTHON
    boost::python::class_<O_Scorer,
	boost::shared_ptr<O_Scorer>,
	boost::python::bases <O_Scorer::Base>,
	boost::noncopyable> ("O_Scorer", boost::python::no_init )
	.def("add",&O_Scorer::add)
    ;
//    boost::python::def("create_Scorer",&create_Scorer);
#endif
    }
};



class	SuperposeAlias_Exposer : public Exposer
{
    void exposeCando()
    {
	class_<O_SuperposeAlias>(this->lisp())
	    .def("setAlias",&O_SuperposeAlias::setAlias)
	    .def("getAlias",&O_SuperposeAlias::getAlias)
	    .def("getPosition",&O_SuperposeAlias::getPosition)
	    .def("setPosition",&O_SuperposeAlias::setPosition)
	    .def("set",&O_SuperposeAlias::set)
	;
//	defNoWrapPackage(MbbPackage,"superpose",&prim_superpose);
    }
    void exposePython()
    {
#ifdef	USEBOOSTPYTHON
	boost::python::class_<O_SuperposeAlias,
	    boost::shared_ptr<O_SuperposeAlias>,
	    boost::python::bases <mbb::O_Object>,
	    boost::noncopyable> ("O_SuperposeAlias", boost::python::no_init )
	    .def("setAlias",&O_SuperposeAlias::setAlias)
	    .def("getAlias",&O_SuperposeAlias::getAlias)
	    .def("getPosition",&O_SuperposeAlias::getPosition)
	    .def("setPosition",&O_SuperposeAlias::setPosition)
	    .def("set",&O_SuperposeAlias::set)
	;
//	boost::python::def("create_SuperposeAlias",&O_SuperposeAlias::create);
#endif
    }

};

OLD_EXPOSE_CLASS(O_SuperposeAlias,SuperposeAlias_Exposer);
OLD_EXPOSE_CLASS(O_Scorer,Scorer_Exposer);

}; // namespace mbb

