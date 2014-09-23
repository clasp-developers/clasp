/*
    File: scorerStageList.cc
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

#include "scorerStageList.h"
#include "archiveNode.h"
#include "archive.h"
#include "lisp.h"
#include "keyedArguments.h"
#include "keyedObject.h"
#include "cons.h"
#include "scorerStage.h"
#include "wrappers.h"

namespace mbb {


void O_ScorerStageList::oldLispInitialize(RPKeyedArguments kargs, RPLisp env)
{
    this->Base::oldLispInitialize(kargs,env);
}



void	O_ScorerStageList::initialize()
{
    this->Base::initialize();
}

void	O_ScorerStageList::archive(RPNode node)
{
    node->archiveVector0("stages",this->_Stages);
}



void	O_ScorerStageList::addStage(RPScorerStage stage)
{_G();
    this->_Stages.push_back(stage);
}

RPScorerStage O_ScorerStageList::getScorerStageWithName(const string& name)
{_G();
    List<O_ScorerStage>::iterator it;
    for ( it=this->_Stages.end()-1; it>=this->_Stages.begin(); it-- )
    {
	if ( (*it)->getName() == name ) return *it;
    }
    return O_ScorerStage::nil(this->lisp());
}

RPCons	O_ScorerStageList::entries()
{_G();
    List<O_ScorerStage>::iterator it;
    RPCons entries = O_Cons::nil(this->lisp());
    for ( it=this->_Stages.end()-1; it>=this->_Stages.begin(); it-- )
    {
	RPScorerStage stage = *it;
	entries = O_Cons::create(stage,entries,this->lisp());
    }
    return entries;
}

class	ScorerStageList_Exposer : public Exposer
{
void exposeCando()
{
	class_<O_ScorerStageList>(this->lisp())
	.def("addStage",&O_ScorerStageList::addStage)
	;
}

void exposePython()
{
#ifdef	USEBOOSTPYTHON //[
    boost::python::class_<O_ScorerStageList,
	boost::shared_ptr<O_ScorerStageList>,
	boost::python::bases <mbb::O_Object>,
	boost::noncopyable> ("O_ScorerStageList", boost::python::no_init )
	.def("addStage",&O_ScorerStageList::addStage)
    ;
#endif //]
}

};

OLD_EXPOSE_CLASS(O_ScorerStageList,ScorerStageList_Exposer);
};
