/*
    File: scorerBase.cc
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

#include "lisp.h"
#include "scorerBase.h"
#include "archiveNode.h"
#include "archive.h"
#include "builder.h"
#include "render.h"
#include "omatrix.h"
#include "ovector3.h"
#include "oligomer.h"
#include "builderState.h"
#include "keyedArguments.h"
#include "scorerState.h"
#include "vdwCollisionRejector.h"
#include "wrappers.h"



namespace mbb 
{



    void O_ScorerBase::oldLispInitialize(RPKeyedArguments args, RPLisp env)
    {_G();
	this->Base::oldLispInitialize(args,env);
	this->_ScorerName = args->getStringAndRemove("name");
	this->_BackgroundRender = args->getAndRemoveOrDefault("render",O_Render::nil(this->lisp()))->as<O_Render>();
    }



//
// Constructor
//

    void	O_ScorerBase::initialize()
    {
	this->Base::initialize();
	this->_BackgroundRender = O_Render::nil(this->lisp());
	this->_Data = O_ObjectDictionary::create(this->lisp());
    }


    RPObjectDictionary O_ScorerBase::getData()
    {_OF();
	ASSERT_NOT_NULL(this->_Data);
	ASSERT(this->_Data->notNil());
	return this->_Data;
    }

//
// Destructor
//

    void	O_ScorerBase::archiveBase(RPNode node)
    {
	node->attributeIfNotDefault<string>("scorerName",this->_ScorerName,"undefScorer");
	node->archiveObject("BackgroundRender",this->_BackgroundRender);
	node->archiveObjectIfDefined("data",this->_Data);
    }


/*! If we have a VdwCollisionChecker then return true because
 * we need all atoms built
 */
    bool	O_ScorerBase::needsAllAtomsBuilt()
    {_G();
	bool needs = false;
	LOG(BF("Does the scorer need all atoms built? -> %d") % needs  ); // vp0(("Does the scorer need all atoms built? -> %d", needs ));
	return needs;
    }


    void	O_ScorerBase::oligomerChanged(RPPointProvider builder)
    {
	this->storeOligomerChangeCounter(builder);
    }



    void	O_ScorerBase::sequenceChanged(RPPointProvider builder)
    {_G();
//	this->throwIfOligomerChangeCounterDoesntMatch(builder);
	this->storeSequenceChangeCounter(builder);
    }

    void	O_ScorerBase::builderChanged(RPPointProvider builder)
    {
	this->oligomerChanged(builder);
	this->sequenceChanged(builder);
    }


    RPRender O_ScorerBase::rendered(RPKeyedArguments options)
    {
	RPRenderDisplayList	render = O_RenderDisplayList::create(this->lisp());
	render->setName("Background");
	render->add(this->_BackgroundRender);
	return render;
    }


    void O_ScorerBase::storeOligomerChangeCounter(RPPointProvider builder)
    {_G();
	this->_OligomerChangeCounter = builder->getOligomerChangeCounter();
    }


    void O_ScorerBase::storeSequenceChangeCounter(RPPointProvider builder)
    {_G();
	this->_SequenceChangeCounter = builder->getSequenceChangeCounter();
    }



    void O_ScorerBase::throwIfBuilderDoesntMatch(RPPointProvider builder)
    {
	IMPLEMENT_ME();
//	this->throwIfOligomerChangeCounterDoesntMatch(builder);
//	this->throwIfSequenceChangeCounterDoesntMatch(builder);
    }

//     void O_ScorerBase::throwIfOligomerChangeCounterDoesntMatch(RPPointProvider builder)
//     {_G();
// 	if ( builder->getOligomerChangeCounter() != this->_OligomerChangeCounter )
// 	{
// 	    stringstream ss;
// 	    ss << "The builder Oligomer was changed(";
// 	    ss << builder->getOligomerChangeCounter();
// 	    ss << ") without informing the scorer(";
// 	    ss << this->_OligomerChangeCounter << ")";
// 	    THROW(_lisp->create<O_LispError>(ss.str()));
// 	}
//     }
//     void O_ScorerBase::throwIfSequenceChangeCounterDoesntMatch(RPPointProvider builder)
//     {_OF();
// 	if ( builder->getSequenceChangeCounter() != this->_SequenceChangeCounter )
// 	{
// 	    stringstream ss;
// 	    ss << "The builder Sequence was changed(";
// 	    ss << builder->getSequenceChangeCounter();
// 	    ss << ") without informing the scorer(";
// 	    ss << this->_SequenceChangeCounter << ")";
// 	    THROW(_lisp->create<O_LispError>(ss.str()));
// 	}
//     }





    RPRenderDisplayList	O_ScorerBase::getRenderForScore(RPPointProvider builder, RPScorerState scorerState)
    {_G();
	RPRenderDisplayList rend;
	this->throwIfBuilderDoesntMatch(builder);
	rend = this->lisp()->create<O_RenderDisplayList>();
	rend->setName("ScorerBase");
	string sequence = builder->getCurrentSequence();
	stringstream ss;
	ss << "Score: " << scorerState->getScore() << endl;
	ss << endl;
	ss << sequence << endl;

	RPGrInformation info = O_GrInformation::create(ss.str(),this->lisp());
	rend->add(info);
	return rend;
    }



    RPScorerState O_ScorerBase::createState()
    {
	return O_ScorerState::create(this->lisp());
    }

    void O_ScorerBase::evaluate(RPPointProvider builder, RPScorerState state)
    {_G();
	SUBCLASS_MUST_IMPLEMENT();
    }












    void	O_BuildAllSelectAll::oldLispInitialize(RPKeyedArguments args, RPLisp env)
    {_G();
	this->Base::oldLispInitialize(args,env);
    }



//
// Constructor
//

    void	O_BuildAllSelectAll::initialize()
    {
	this->Base::initialize();
    }

//
// Destructor
//



    void	O_BuildAllSelectAll::evaluate(RPPointProvider builder, RPScorerState state)
    {
	state->setScore(0.0);
	state->setReject(false);
    }






    class	ScorerBase_Exposer : public Exposer
    {
	void exposeCando()
	{
	    class_<O_ScorerBase>(this->lisp())
		.def("getName",&O_ScorerBase::getName)
		.def("getData",&O_ScorerBase::getData)
		.def("evaluate",&O_ScorerBase::evaluate)
		.def("evaluateWithoutSuperposing",&O_ScorerBase::evaluateWithoutSuperposing)
		.def("createState",&O_ScorerBase::createState)
//	    .def("addRejector",&O_ScorerBase::addRejector)
		.def("builderChanged",&O_ScorerBase::builderChanged)
		.def("getBackgroundRender",&O_ScorerBase::getBackgroundRender)
		.def("setBackgroundRender",&O_ScorerBase::setBackgroundRender)
		;
	}

	void exposePython()
	{
#ifdef	USEBOOSTPYTHON
	    boost::python::class_<O_ScorerBase,
		boost::shared_ptr<O_ScorerBase>,
		boost::python::bases <mbb::O_Object>,
		boost::noncopyable> ("O_ScorerBase", boost::python::no_init )
		.def("getName",&O_ScorerBase::getName)
		.def("evaluate",&O_ScorerBase::evaluate)
		.def("addRejector",&O_ScorerBase::addRejector)
		.def("createState",&O_ScorerBase::createState)
		.def("builderChanged",&O_ScorerBase::builderChanged)
		.def("getBackgroundRender",&O_ScorerBase::getBackgroundRender)
		.def("setBackgroundRender",&O_ScorerBase::setBackgroundRender)
		;
#endif
	}
    };

    class	BuildAllSelectAll_Exposer : public Exposer
    {
	void exposeCando()
	{
	    class_<O_BuildAllSelectAll>(this->lisp())
		;
	}

	void exposePython()
	{
#ifdef	USEBOOSTPYTHON
	    boost::python::class_<O_BuildAllSelectAll,
		boost::shared_ptr<O_BuildAllSelectAll>,
		boost::python::bases <O_ScorerBase>,
		boost::noncopyable> ("O_BuildAllSelectAll", boost::python::no_init )
		;
#endif
	}
    };

    OLD_EXPOSE_CLASS(O_ScorerBase,ScorerBase_Exposer);
    OLD_EXPOSE_CLASS(O_BuildAllSelectAll,BuildAllSelectAll_Exposer);


};
