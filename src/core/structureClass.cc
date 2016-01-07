/*
    File: structureClass.cc
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
#define DEBUG_LEVEL_FULL

#include <clasp/core/foundation.h>
#include <clasp/core/structureClass.h>
#include <clasp/core/lisp.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/builtInClass.h>
#include <clasp/core/standardObject.h>
#include <clasp/core/package.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/wrappers.h>

namespace core {
StructureClass_sp StructureClass_O::createUncollectable() {
  GC_ALLOCATE_UNCOLLECTABLE(StructureClass_O, bic);
  return bic;
}

#if 0
    StructureClass_sp StructureClass_O::create(Class_sp mc)
    {
        GC_ALLOCATE(StructureClass_O,bic);
//	StructureClass_sp bic = StructureClass_sp(new StructureClass_O());
	return bic;
    }
#endif

StructureClass_O::StructureClass_O() {
}

void StructureClass_O::initialize() {
  _OF();
  this->Base::initialize();
  this->initializeSlots(REF_NUMBER_OF_SLOTS_IN_CLASSES);
  this->_InstanceCoreClass = _Nil<Class_O>();
}

#if defined(XML_ARCHIVE)
void StructureClass_O::archiveBase(ArchiveP node) {
  IMPLEMENT_ME();
}
#endif // defined(XML_ARCHIVE)

#if 0 // All functions
    void	StructureClass_O::defineYourSlotsFromBinderArchiveNode(ArchiveP node)
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


    void	StructureClass_O::initialize()
    {
	this->Base::initialize();
//    this->_InstanceVariableNames = _Nil<T_O>();
//	this->_SlotSpecifiers.clear();
	this->_InstanceCoreClass = _Nil<BuiltInClass_O>();
    }

    StructureClass_sp StructureClass_O::create(Lisp_sp lisp,Symbol_sp name /*, uint instanceClassSymbol */)
    {
        GC_ALLOCATE(StructureClass_O,oclass );
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
    Cons_sp StructureClass_O::classListDesignator(T_sp baseClassesDesignator, Lisp_sp lisp)
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






    BuiltInClass_sp StructureClass_O::getInstanceCoreClass() const
    {_OF();
	return this->_InstanceCoreClass;
    }

    void StructureClass_O::setInstanceCoreClass(BuiltInClass_sp mc)
    {_OF();
	this->_InstanceCoreClass = mc;
    }

    void	StructureClass_O::describe()
    {
	IMPLEMENT_ME();
#if 0
	_lisp->print(BF("------------  StructureClass name: %s    instanceClassSymbol: %d") % this->_Name->__repr__() % this->_InstanceClassSymbol );
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

    string StructureClass_O::dumpInfo()
    {
	IMPLEMENT_ME();
#if 0
	stringstream ss;
	ss << this->Base::dumpInfo();
	ss << "CoreBuiltInClass: " << this->_InstanceCoreClass->getPackagedName() << std::endl;
	return ss.str();
#endif
    }

    string StructureClass_O::dumpMethods()
    {
	IMPLEMENT_ME();
#if 0
	return this->Base::dumpMethods();
#endif
    }


    uint StructureClass_O::numberOfSlots()
    {
	IMPLEMENT_ME();
#if 0
	return this->_SlotSpecifiers.size();
#endif
    }

#if 0
    StructureClass_O::slotIterator StructureClass_O::find(Symbol_sp sym)
    {
	IMPLEMENT_ME();
#if 0
	ASSERTNOTNULL(sym);
	LOG(BF("Looking in StructureClass for slot for symbol: %s") % sym->fullName() );
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
    T_sp StructureClass_O::allocate_newNil()
    {
	IMPLEMENT_ME();
#if 0
	T_sp obj = this->_InstanceCoreClass->new_instance(_Nil<Function_O>(), 
							  _Nil<T_O>(),
							  _Nil<Environment_O>(), _lisp );
	StructureClass_sp thisClass = this->sharedThis<StructureClass_O>();
	obj->__setClass(thisClass);
	return obj;
#endif
    }
#endif

#if 0
    void StructureClass_O::appendInstanceVariablesFromListOfSymbols(Cons_sp instanceVariableNames)
    {
	StructureClass_O::slotIterator si;
	for (Cons_sp ci = instanceVariableNames; ci.notnilp(); ci=ci->cdr() )
	{
	    Symbol_sp sym = ci->ocar().as<Symbol_O>();
	    if ( this->find(sym) != this->_SlotSpecifiers.end() )
	    {
		SIMPLE_ERROR(BF(boost::format("There is already a slot with name(%s)")%sym->currentName()));
	    }
	    LOG(BF("Appending symbol(%s) as a slot")% sym->currentName() );
	    this->_SlotNames.insert(sym);
	}
    }
    void StructureClass_O::appendInstanceVariablesFromStructureClass(StructureClass_sp cc)
    {
	StructureClass_O::slotIterator si;
	for (si = cc->begin(); si!=cc->end(); si++ )
	{
	    if ( this->_SlotNames.count(*si)>0 )
	    {
		SIMPLE_ERROR(BF((_lisp->creat"There is already a slot with name(%s)")%(*si)->currentName()));
	    }
	    this->_SlotNames.insert(*si);
	}
    }
#endif


    void StructureClass_O::resetSlots()
    {
	IMPLEMENT_ME();
#if 0
	this->_SlotSpecifiers.clear();
#endif
    }



    void StructureClass_O::setupAccessors(List_sp slotNames)
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

void StructureClass_O::exposeCando(Lisp_sp lisp) {
  class_<StructureClass_O>();
}
void StructureClass_O::exposePython(Lisp_sp lisp) {
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(CorePkg, StructureClass, "", "", _lisp);
#endif
}

EXPOSE_CLASS(core, StructureClass_O);
};
