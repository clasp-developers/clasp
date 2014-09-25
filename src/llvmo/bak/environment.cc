/*
    File: environment.cc
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

#include <string.h>
#include "core/common.h"
#include "stringSet.h"
#include "symbolTable.h"
#include "environment.h"
#include "archiveNode.h"
#include "serialize.h"
#include "archive.h"
#include "lambdaListHandler.h"
#include "standardObject.h"
#include "activationFrame.h"
#include "evaluator.h"
#include "render.h"
#include "wrappers.h"


#define	MAX_CONS_CHARS	1024

namespace llvmo
{


    REGISTER_CLASS(llvmo,FunctionEnvironment_O);
    REGISTER_CLASS(llvmo,BlockEnvironment_O);
    REGISTER_CLASS(llvmo,TagbodyEnvironment_O);

    EXPOSE_CLASS(llvmo,Environment_O);

    void Environment_O::exposeCando(core::Lisp_sp lisp)
    {
	core::class_<Environment_O>(,core::no_init)
	    .def("classifyValue",&Environment_O::classifyValue)
	    .def("classifyFunctionLookup",&Environment_O::classifyFunctionLookup)
	    .def("getParentEnvironment",&Environment_O::getParentEnvironment)
	    .def("metaData",&Environment_O::metaData)
	    .def("setf_metaData",&Environment_O::setf_metaData)
	    .def("push_metaData",&Environment_O::push_metaData)
	    .def("hasMetaData",&Environment_O::hasMetaData)
	    ;
    }

    void Environment_O::exposePython(core::Lisp_sp lisp)
    {_G();
	PYTHON_CLASS(LlvmoPkg,Environment,"","",_lisp)
	    ;
    }



//
// Constructor
//


    void Environment_O::initialize()
    {
	this->Base::initialize();
	// The top of the local variable stack is the global Binder
	// so any local variables declared on the top level are automatically global variables
	//
	this->_ParentEnvironment = Environment_O::nil(_lisp);
	this->_Id = _lisp->nextEnvironmentId();
	this->_Depth = 0; 
    }


    core::T_sp Environment_O::metaData(core::Symbol_sp key)
    {_G();
	core::SymbolMap<core::T_O>::iterator it = this->_MetaData.find(key);
	if ( it==this->_MetaData.end() ) return core::T_O::_nil;
	return it->second;
    }

       

    void Environment_O::setParent(Environment_sp environ)
    {_G();
	this->_ParentEnvironment = environ;
	if ( this->_ParentEnvironment->isNil() )
	{
	    this->_Depth = 0;
	} else
	{
	    this->_Depth = this->_ParentEnvironment->getDepth()+1;
	}
    }

    Environment_sp Environment_O::getParentEnvironment() const
    {_OF();
	ASSERTNOTNULL(this->_ParentEnvironment);
	return this->_ParentEnvironment;
    }


    void Environment_O::_environmentStackFill(int level, stringstream& sout)
    {_OF();
	sout << (BF("%s[%03d]---------\n%s") % this->className() % this->_Depth % this->summaryOfContents()).str();
	if ( this->_ParentEnvironment->notNil() )
	{
	    this->_ParentEnvironment->_environmentStackFill(level+1,sout);
	}
    }

    string Environment_O::environmentStackAsString()
    {_OF();
	stringstream sout;
	this->_environmentStackFill(1,sout);
	return sout.str();
    }



    void Environment_O::dump()
    {_OF();
	string ss = this->environmentStackAsString();
	_lisp->print(BF("%s") % ss);
    }


    string Environment_O::__repr__() const
    {_G();
	stringstream ss;
	ss << "#<" << this->className() << " ";
	ss <<this->summaryOfContents();
	if ( this->_ParentEnvironment->notNil() )
	{
	    ss << this->_ParentEnvironment->__repr__();
	}
	ss << " >";
	return ss.str();
    }







    bool Environment_O::_findValue(core::Symbol_sp sym, int& depth, int& index ) const
    {_G();
	if ( this->_ParentEnvironment->isNil() ) return false;
	++depth;
	return this->_ParentEnvironment->_findValue(sym,depth,index);
    }




    bool Environment_O::findValue(core::Symbol_sp sym, int& depth, int& index) const
    {_G();
	depth = 0;
	index =-1;
	return this->_findValue(sym,depth,index);
    }




    bool Environment_O::_findFunction(core::Symbol_sp sym, int& depth, int& index ) const
    {_G();
	if ( this->_ParentEnvironment->isNil() ) return false;
	++depth;
	return this->_ParentEnvironment->_findFunction(sym,depth,index);
    }


    bool Environment_O::findFunction(core::Symbol_sp sym, int& depth, int& index ) const
    {_G();
	depth = 0;
	index =-1;
	return this->_findFunction(sym,depth,index);
    }





    core::Cons_sp Environment_O::classifyValue(core::Symbol_sp sym) const
    {_G();
	if ( sym->specialP() )
	{
	    return core::Cons_O::create(core::_sym_specialVar,sym,_lisp);
	}
	int depth;
	int index;
	if ( this->findValue(sym,depth,index) )
	{
	    return core::Cons_O::createList(core::_sym_lexicalVar,sym,core::Cons_O::create(core::Fixnum_O::create(depth),core::Fixnum_O::create(index),_lisp),_lisp);
	}
	THROW(_lisp->error(BF("Could not find %s in lexica/dynamic environment") % sym->__repr__() ));
    }




    core::Cons_sp Environment_O::classifyFunctionLookup(core::Symbol_sp sym) const
    {_G();
	int depth;
	int index;
	if ( this->findValue(sym,depth,index) )
	{
	    return core::Cons_O::createList(core::_sym_lexicalFunction,sym,core::Cons_O::create(core::Fixnum_O::create(depth),core::Fixnum_O::create(index),_lisp),_lisp);
	}
	if ( sym->fboundp() )
	{
	    return core::Cons_O::create(core::_sym_specialFunction,sym,_lisp);
	}
	THROW(_lisp->error(BF("Could not find %s in lexica/dynamic environment") % sym->__repr__() ));
    }


    Environment_sp Environment_O::find_block_named_environment(core::Symbol_sp blockName) const
    {_OF();
	if ( this->_ParentEnvironment->isNil() )
	{
	    THROW(_lisp->error(BF("Could not find block with name[%s]") % blockName->__repr__() ));
	}
	return this->_ParentEnvironment->find_block_named_environment(blockName);
    }


    Environment_sp Environment_O::find_tagbody_tag_environment(core::Symbol_sp tag) const
    {_OF();
	IMPLEMENT_MEF(BF("Handle new environments"));
	if ( this->_ParentEnvironment->isNil() )
	{
	    THROW(_lisp->error(BF("Could not find tagbody tag with name[%s]") % tag->__repr__() ));
	}
	return this->_ParentEnvironment->find_tagbody_tag_environment(tag);
    }


    string Environment_O::summaryOfContents() const
    {_G();
	if ( this->isNil() ) return "#<Environment nil>";
	stringstream ss;
	for ( core::SymbolMap<core::T_O>::const_iterator it=this->_MetaData.begin();
	      it!=this->_MetaData.end(); it++ )
	{
	    ss << it->first->__repr__() << " --> " << it->second->__repr__() << endl;
	}
	return ss.str();
    }










#if 0

    Function_sp Environment_O::lookupSymbolMacro(core::Symbol_sp sym, bool& foundIt) const
    {_G();
	LOG(BF("Looking to see if there is a symbol-macro with name(%s)") % sym->__repr__() );
	ASSERTNOTNULL(this->_ParentEnvironment);
	if ( this->_ParentEnvironment->isNil() )
	{
	    // There is no symbol-macro with this name, return nil/false
	    foundIt = false;
	    return lisp()->nil<Function_O>();
	}
	return this->_ParentEnvironment->lookupSymbolMacro(sym,foundIt);
    }






#endif








    ValueEnvironment_O::ValueEnvironment_O(const core::MetaClass_sp& mc) : Base(mc) {};

    ValueEnvironment_O::~ValueEnvironment_O()
    {_G();
    }




    void ValueEnvironment_O::dump()
    {_G();
	printf("%s\n", this->summaryOfContents().c_str() );
    }



    bool ValueEnvironment_O::_findValue(core::Symbol_sp sym, int& depth, int& index) const
    {_G();
	LOG(BF("Looking for binding for symbol(%s)") % sym->__repr__() );
//    LOG(BF("The frame stack is %d deep") % this->depth() );
	ASSERTNOTNULL(this->_ParentEnvironment);
	const_symbol_iterator fi;
	fi = this->_SymbolIndex.find(sym);
	if ( fi==this->_SymbolIndex.end() )
	{
	    if ( this->_ParentEnvironment->isNil() )
	    {
		LOG(BF("Could not find symbol"));
		return false;
	    }
	    LOG(BF("Moving up a level"));
	    ++depth;
	    return this->_ParentEnvironment->_findValue(sym,depth,index);
	}
	index = fi->second;
	LOG(BF(" Found binding %s")% fi->second );
	return true;
    }



    ValueEnvironment_sp ValueEnvironment_O::createSingleTopLevelEnvironment()
    {_G();
	GC_RESERVE_TRY(ValueEnvironment_O,env ){
	    GC_RESERVE_GET(ValueEnvironment_O,env );
	    env->_ParentEnvironment = Environment_O::_nil;
	}
	return env;
    }


    ValueEnvironment_sp ValueEnvironment_O::createForLambdaListHandler(core::LambdaListHandler_sp llh, Environment_sp parent)
    {_G();
	ValueEnvironment_sp env(ValueEnvironment_O::create(_lisp));
	env->setupForLambdaListHandler(llh,parent);
	return env;
    }

    ValueEnvironment_sp ValueEnvironment_O::createForArgumentPassing(int numberOfArguments, Environment_sp parent)
    {_G();
	ValueEnvironment_sp env(ValueEnvironment_O::create(_lisp));
	env->_ParentEnvironment = parent;
	return env;
    }



    void ValueEnvironment_O::setupForLambdaListHandler(core::LambdaListHandler_sp llh, Environment_sp parent)
    {_G();
	core::Cons_sp classifiedSymbols = llh->classifiedSymbols();
	this->_ParentEnvironment = parent;
	int numberOfLexicals = 0;
	int highestLexicalIndex = -1;
	for ( core::Cons_sp cur = classifiedSymbols; cur->notNil(); cur=cur->cdr() )
	{
	    core::Cons_sp classifiedSymbol = cur->ocar()->as<core::Cons_O>();
	    core::Symbol_sp classification = classifiedSymbol->ocar()->as<core::Symbol_O>();
	    if ( classification == core::_sym_lexicalVar )
	    {
		core::Symbol_sp target = classifiedSymbol->ocadr()->as<core::Symbol_O>();
		int targetFrameIndex = classifiedSymbol->ocddr()->as<core::Fixnum_O>()->get();
		this->_SymbolIndex[target] = targetFrameIndex;
		++numberOfLexicals;	
		if ( targetFrameIndex > highestLexicalIndex ) highestLexicalIndex = targetFrameIndex;
	    } else if ( classification == core::_sym_specialVar )
	    {
		this->_SymbolIndex[classifiedSymbol->ocdr()->as<core::Symbol_O>()] = SPECIAL_TARGET;
	    } else
	    {
		THROW(_lisp->error(BF("Illegal symbol classification: %s") % classifiedSymbol->__repr__() ));
	    }
	}
	if (numberOfLexicals != (highestLexicalIndex+1) )
	{
	    THROW(_lisp->error(BF("Mismatch between number of lexicals[%d] and the highest lexical index+1[%d]") % numberOfLexicals % (highestLexicalIndex+1) ));
	}
    }


    string	ValueEnvironment_O::summaryOfContents() const
    {_G();
	stringstream ss;
	if ( this->isNil() )
	{
	    ss << ":top-level-environment" << endl;
	} else
	{
	    ss << (BF(":id %d :depth %d ") % this->_Id % this->_Depth ).str()  << endl;
	    for ( const_symbol_iterator it=this->_SymbolIndex.begin(); it!=this->_SymbolIndex.end();it++)
	    {
		ss << it->first->__repr__() << "#"<<it->second<<" -> ";
	    }
	}
	ss << this->Base::summaryOfContents();
	return ss.str();
    }









    EXPOSE_CLASS(llvmo,ValueEnvironment_O);

    void ValueEnvironment_O::exposeCando(core::Lisp_sp lisp)
    {
	core::class_<ValueEnvironment_O>()
	    ;
	af_def(LlvmoPkg,"makeValueEnvironment",&ValueEnvironment_O::createForLambdaListHandler);
	af_def(LlvmoPkg,"makeValueEnvironmentForArgumentPassing",&ValueEnvironment_O::createForArgumentPassing);
    }

    void ValueEnvironment_O::exposePython(core::Lisp_sp lisp)
    {_G();
	PYTHON_CLASS(LlvmoPkg,ValueEnvironment,"","",_lisp)
	    ;
    }





















    bool FunctionEnvironment_O::_findFunction(core::Symbol_sp sym, int& depth, int& index ) const
    {_G();
	LOG(BF("Looking for binding for symbol(%s)") % sym->__repr__() );
//    LOG(BF("The frame stack is %d deep") % this->depth() );
	ASSERTNOTNULL(this->_ParentEnvironment);
	map<core::Symbol_sp,int>::const_iterator fi;
	fi = this->_FunctionIndices.find(sym);
	if ( fi==this->_FunctionIndices.end() )
	{
	    if ( this->_ParentEnvironment->isNil() )
	    {
		LOG(BF("Could not find symbol"));
		return false;
	    }
	    LOG(BF("Moving up a level"));
	    ++depth;
	    return this->_ParentEnvironment->_findFunction(sym,depth,index);
	}
	index = fi->second;
	LOG(BF(" Found binding %s")% fi->second );
	return true;
    }



//
// Constructor
//

    FunctionEnvironment_sp FunctionEnvironment_O::create( Environment_sp parent )
    {_G();
	FunctionEnvironment_sp environ(RP_Create<FunctionEnvironment_O>(_lisp));
	environ->_ParentEnvironment = parent;
	return environ;
    }

    FunctionEnvironment_sp FunctionEnvironment_O::create( int numEntries, Environment_sp parent )
    {_G();
	FunctionEnvironment_sp environ(FunctionEnvironment_O::create(parent));
	return environ;
    }





    BlockEnvironment_sp BlockEnvironment_O::create(Environment_sp parent)
    {_G();
	BlockEnvironment_sp environ = BlockEnvironment_O::create(_lisp);
	environ->setParent(parent);
	return environ;
    }





    BlockEnvironment_sp BlockEnvironment_O::create(core::Symbol_sp blockSymbol, Environment_sp parent)
    {_G();
	BlockEnvironment_sp environ = BlockEnvironment_O::create(parent);
	environ->setBlockSymbol(blockSymbol);
	return environ;
    }


    string BlockEnvironment_O::summaryOfContents() const
    {_G();
	if ( this->isNil() ) return "#<BlockEnvironment nil>";
	return (BF("    :block-name %s\n") % this->getBlockSymbol()->__repr__() ).str();
    }



    void BlockEnvironment_O::initialize()
    {
	this->Base::initialize();
    }


    void BlockEnvironment_O::archiveBase(core::ArchiveP node)
    {
	IMPLEMENT_ME();
    }


    Environment_sp BlockEnvironment_O::find_block_named_environment(core::Symbol_sp blockName) const
    {_OF();
	if ( this->getBlockSymbol() == blockName ) return this->sharedThis<BlockEnvironment_O>();
	return this->getParentEnvironment()->find_block_named_environment(blockName);
    }



    void FunctionEnvironment_O::initialize()
    {
	this->Base::initialize();
    }


    void FunctionEnvironment_O::archiveBase(core::ArchiveP node)
    {
	IMPLEMENT_ME();
    }



    string FunctionEnvironment_O::summaryOfContents() const
    {_G();
	stringstream ss;
	ss << "#<" << this->className() << " put stuff here >";
	return ss.str();
    }


    void FunctionEnvironment_O::bind_function(core::Symbol_sp sym, core::Function_sp form)
    {_G();
	int nextIdx = this->_FunctionIndices.size();
	this->_FunctionIndices[sym] = nextIdx;
    }




















//
// Constructor
//

//
// Destructor
//

    TagbodyEnvironment_sp TagbodyEnvironment_O::create(Environment_sp parent)
    {_G();
	TagbodyEnvironment_sp environ = TagbodyEnvironment_O::create(_lisp);
	environ->setParent(parent);
	return environ;
    }






    void TagbodyEnvironment_O::initialize()
    {
	this->Base::initialize();
    }

    void TagbodyEnvironment_O::addTag(core::Symbol_sp tag, core::Cons_sp ip)
    {_OF();
	ASSERTF(this->_Tags.count(tag)==0,BF("The tag[%s] has already been defined in this tagbody"));
	this->_Tags[tag] = ip;
    };

    TagbodyEnvironment_O::const_iterator TagbodyEnvironment_O::find(core::Symbol_sp tag) const
    {_OF();
	return this->_Tags.find(tag);
    }

    string TagbodyEnvironment_O::tagsAsString() const
    {_OF();
	stringstream ss;
	for ( const_iterator it=this->_Tags.begin(); it!=this->_Tags.end(); it++ )
	{
	    ss << it->first->__repr__() << " ";
	}
	return ss.str();
    }


    Environment_sp TagbodyEnvironment_O::find_tagbody_tag_environment(core::Symbol_sp tag) const
    {_OF();
	const_iterator it = this->find(tag);
	if ( it != this->end() )
	{
	    return this->sharedThis<TagbodyEnvironment_O>();
	}
	return this->getParentEnvironment()->find_tagbody_tag_environment(tag);
    }





}; // namespace llvmo
