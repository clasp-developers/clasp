/*
    File: funcallableStandardClass.cc
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
//#define DEBUG_LEVEL_FULL

#include <clasp/core/foundation.h>
#include <clasp/core/lisp.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/builtInClass.h>
#include <clasp/core/standardObject.h>
#include <clasp/core/package.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/wrappers.h>

namespace core {
FuncallableStandardClass_sp FuncallableStandardClass_O::create(Class_sp mc) {
  GC_ALLOCATE(FuncallableStandardClass_O, bic);
  //	FuncallableStandardClass_sp bic = FuncallableStandardClass_sp(new FuncallableStandardClass_O());
  return bic;
}

void FuncallableStandardClass_O::initialize() {
  _OF();
  this->Base::initialize();
//  this->_InstanceCoreClass = _Nil<Class_O>();
}

#if defined(XML_ARCHIVE)
void FuncallableStandardClass_O::archiveBase(ArchiveP node) {
  IMPLEMENT_ME();
}
#endif // defined(XML_ARCHIVE)

#if 0 // All functions
    void	FuncallableStandardClass_O::defineYourSlotsFromBinderArchiveNode(ArchiveP node)
    {
	IMPLEMENT_ME(); // Create a forward-referenced-class
	if ( node==NULL ) return;
	this->_SlotSpecifiers.clear();
	vector<ArchiveP>::iterator	ci;
	for ( ci=node->begin_Children(); ci!=node->end_Children(); ci++ )
	{
	    Symbol_sp sym = _lisp->intern((*ci)->getUniqueIdCharacters());
	    LOG(BF("Adding slot(%s)") % sym->fullName() );
	    this->_SlotSpecifiers.push_back(SlotSpecifier_O::create(sym,_lisp));
	}
    }


    void	FuncallableStandardClass_O::initialize()
    {
	this->Base::initialize();
//    this->_InstanceVariableNames = _Nil<T_O>();
//	this->_SlotSpecifiers.clear();
//	this->_InstanceCoreClass = _Nil<FuncallableStandardClass_O>();
    }

    FuncallableStandardClass_sp FuncallableStandardClass_O::create(Lisp_sp lisp,Symbol_sp name /*, uint instanceClassSymbol */)
    {
        GC_ALLOCATE(FuncallableStandardClass_O,oclass );
	    oclass->_Name = name;
	    oclass->_InstanceClassSymbol = UNDEFINED_SYMBOL; // Not used anymore instanceClassSymbol;
	    oclass->_InstanceCoreClass = _Nil<BuiltInClass_O>();
//    oclass->_InstanceVariableNames = _Nil<T_O>();
	return oclass;
    }

    /*! Return a list of classes from a classListDesignator which can be...
      nil - return a Cons containing StandardObject.
      [a metaclass] - return a Cons containing that metaclass.
      [Cons of metaclasses] - return the cons of metaclasses.
    */
    Cons_sp FuncallableStandardClass_O::classListDesignator(T_sp baseClassesDesignator, Lisp_sp lisp)
    {
	Cons_sp baseClasses;
	if ( baseClassesDesignator.nilp() )
	{
	    baseClasses = Cons_O::create(lisp->classFromClassSymbol(StandardObject_O::static_classSymbol()),lisp);
	} else
	{_BLOCK_TRACE("About to assign base class");
	    if ( baseClassesDesignator->consP() )
	    {
		baseClasses = baseClassesDesignator;
	    } else
	    {
		LOG(BF("baseClassesDesignator class(%s) value(%s)")
		    % baseClassesDesignator->className() % baseClassesDesignator->__repr__() );
		LOG(BF("The base class(%s) was supplied as a bare symbol")
		    % baseClassesDesignator->__repr__() );
		baseClasses = Cons_O::create(baseClassesDesignator.as<Class_O>(),_lisp);
	    }
	}
	return baseClasses;
    }



#if 0
BuiltInClass_sp FuncallableStandardClass_O::getInstanceCoreClass() const
{_OF();
  return this->_InstanceCoreClass;
}

void FuncallableStandardClass_O::setInstanceCoreClass(BuiltInClass_sp mc)
    {_OF();
	this->_InstanceCoreClass = mc;
    }
#endif
    void	FuncallableStandardClass_O::describe()
    {
	IMPLEMENT_ME();
#if 0
	_lisp->print(BF("------------  FuncallableStandardClass name: %s    instanceClassSymbol: %d") % this->_Name->__repr__() % this->_InstanceClassSymbol );
//    _lisp->print(BF("Instance variables: %s") % this->_InstanceVariableNames->__repr__().c_str() );
	_lisp->print(BF("%s") % this->dumpInfo() );
	bool sawBaseClasses = false;
	for ( baseClassIterator it=this->baseClasses_begin(); it!=this->baseClasses_end(); it++ )
	{
	    _lisp->print(BF("Baseclass--->"));
	    (*it)->describe();
	    sawBaseClasses = true;
	}
	if ( !sawBaseClasses)
	{
	    _lisp->print(BF("Did not see any base classes"));
	}
#endif
    }

    string FuncallableStandardClass_O::dumpInfo()
    {
	IMPLEMENT_ME();
#if 0
	stringstream ss;
	ss << this->Base::dumpInfo();
	ss << "CoreBuiltInClass: " << this->_InstanceCoreClass->getPackagedName() << std::endl;
	return ss.str();
#endif
    }



    string FuncallableStandardClass_O::dumpMethods()
    {
	IMPLEMENT_ME();
#if 0
	return this->Base::dumpMethods();
#endif
    }


    T_sp FuncallableStandardClass_O::instanceSig() const
    {
	return FuncallableStandardClass_O::___staticClass->slots();
    }


    uint FuncallableStandardClass_O::numberOfSlots()
    {
	IMPLEMENT_ME();
#if 0
	return this->_SlotSpecifiers.size();
#endif
    }

#if 0
    FuncallableStandardClass_O::slotIterator FuncallableStandardClass_O::find(Symbol_sp sym)
    {
	IMPLEMENT_ME();
#if 0
	ASSERTNOTNULL(sym);
	LOG(BF("Looking in FuncallableStandardClass for slot for symbol: %s") % sym->fullName() );
	slotIterator it;
	for ( it=this->_SlotSpecifiers.begin(); it!=this->_SlotSpecifiers.end(); it++ )
	{
	    if ( (*it)->_SlotName == sym ) break;
	}
#ifdef DEBUG_ON
	if ( it==this->_SlotSpecifiers.end() )
	{
	    LOG(BF("Could not find slot"));
	} else
	{
	    LOG(BF("Found slot"));
	}
#endif
	return it;
#endif
    }
#endif

#if 0
    void FuncallableStandardClass_O::appendInstanceVariablesFromListOfSymbols(Cons_sp instanceVariableNames)
    {
	FuncallableStandardClass_O::slotIterator si;
	for (Cons_sp ci = instanceVariableNames; ci.notnilp(); ci=ci->cdr() )
	{
	    Symbol_sp sym = ci->ocar().as<Symbol_O>();
	    if ( this->find(sym) != this->_SlotSpecifiers.end() )
	    {
		SIMPLE_ERROR(BF((_lisp->creat"There is already a slot with name(%s)")%sym->currentName()));
	    }
	    LOG(BF("Appending symbol(%s) as a slot")% sym->currentName() );
	    this->_SlotNames.insert(sym);
	}
    }
    void FuncallableStandardClass_O::appendInstanceVariablesFromFuncallableStandardClass(FuncallableStandardClass_sp cc)
    {
	FuncallableStandardClass_O::slotIterator si;
	for (si = cc->begin(); si!=cc->end(); si++ )
	{
	    if ( this->_SlotNames.count(*si)>0 )
	    {
		SIMPLE_ERROR(BF(boost::format("There is already a slot with name(%s)")%(*si)->currentName()));
	    }
	    this->_SlotNames.insert(*si);
	}
    }
#endif


    void FuncallableStandardClass_O::resetSlots()
    {
	IMPLEMENT_ME();
#if 0
	this->_SlotSpecifiers.clear();
#endif
    }



    void FuncallableStandardClass_O::setupAccessors(Cons_sp slotNames)
    {
	IMPLEMENT_ME(); // Dont pass the slot names, use the slots already defined
#if 0
	this->_InstanceVariableNames = slotNames;
	while ( slotNames.notnilp() )
	{
	    Symbol_sp slotName = slotNames->ocar().as<Symbol_O>();
	    string setterName = "set_"+slotName->symbolNameAsString();
	    Symbol_sp setterSymbol = _lisp->internKeyword(setterName);
	    SlotSetter_sp setterForm = SlotSetter_O::create(setterSymbol,_lisp);
	    this->addMethod(setterSymbol,setterForm);
	    string getterName = "get_"+slotName->symbolNameAsString();
	    Symbol_sp getterSymbol = _lisp->internKeyword(getterName);
	    SlotGetter_sp getterForm = SlotGetter_O::create(getterSymbol,_lisp);
	    this->addMethod(getterSymbol,getterForm);
	    slotNames = slotNames->cdr();
	}
#endif
    }

#endif




};
