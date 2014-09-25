/*
    File: scorerStage.cc
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

#include "scorerStage.h"
#include "archiveNode.h"
#include "archive.h"
#include "lisp.h"
#include "keyedArguments.h"
#include "keyedObject.h"
#include "scorerBase.h"
#include "hits.h"
#include "wrappers.h"

namespace mbb {



void	O_ScorerStage::oldLispInitialize(RPKeyedArguments kargs, RPLisp env)
{
    this->Base::oldLispInitialize(kargs,env);
    	// your stuff here
    this->_Name = kargs->getStringAndRemoveOrDefault("name","-noStageName-");
    this->_Comment = kargs->getStringAndRemoveOrDefault("comment","-noStageComment-");
}

void	O_ScorerStage::initialize()
{
    this->Base::initialize();
    this->_Scorer = O_ScorerBase::nil(this->lisp());
    this->_HitList = O_HitList::nil(this->lisp());
    this->_Score = 1234.56789;
}

void	O_ScorerStage::archive(RPNode node)
{
    node->attribute("name",this->_Name);
    node->attribute("comment",this->_Comment);
    node->archiveObject("hitList",this->_HitList);
    node->archiveObject("scorer",this->_Scorer);
    node->attribute("score",this->_Score);
}


void	O_ScorerStage::setScorer(RPScorerBase sc)
{_G();
    this->_Scorer = sc;
}


RPScorerBase O_ScorerStage::getScorer()
{_G();
    return this->_Scorer;
}


void	O_ScorerStage::setHitList(RPHitList sc)
{_G();
    this->_HitList = sc;
}


RPHitList O_ScorerStage::getHitList()
{_G();
    return this->_HitList;
}



class	ScorerStage_Exposer : public Exposer
{
void exposeCando()
{
	class_<O_ScorerStage>(this->lisp())
	.def("getName",&O_ScorerStage::getName)
	.def("setName",&O_ScorerStage::setName)
	.def("getScore",&O_ScorerStage::getScore)
	.def("setScore",&O_ScorerStage::setScore)
	.def("getComment",&O_ScorerStage::getComment)
	.def("setComment",&O_ScorerStage::setComment)
	.def("getHitList",&O_ScorerStage::getHitList)
	.def("setHitList",&O_ScorerStage::setHitList)
	.def("getScorer",&O_ScorerStage::getScorer)
	.def("setScorer",&O_ScorerStage::setScorer)
	;
}

void exposePython()
{
#ifdef	USEBOOSTPYTHON //[
    boost::python::class_<O_ScorerStage,
	boost::shared_ptr<O_ScorerStage>,
	boost::python::bases <mbb::O_Object>,
	boost::noncopyable> ("O_ScorerStage", boost::python::no_init )
	.def("getName",&O_ScorerStage::getName)
	.def("setName",&O_ScorerStage::setName)
	.def("getComment",&O_ScorerStage::getComment)
	.def("setComment",&O_ScorerStage::setComment)
	.def("getHitList",&O_ScorerStage::getHitList)
	.def("setHitList",&O_ScorerStage::setHitList)
	.def("getScorer",&O_ScorerStage::getScorer)
	.def("setScorer",&O_ScorerStage::setScorer)
    ;
#endif //]
}

};

OLD_EXPOSE_CLASS(O_ScorerStage,ScorerStage_Exposer);
};
