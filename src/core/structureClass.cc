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
//  printf("%s:%d In StructureClass_O ctor\n", __FILE__, __LINE__ );
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


CL_LISPIFY_NAME("core:ensure-structure-class");
CL_DEFUN StructureClass_sp StructureClass_O::ensure_structure_class(Symbol_sp name, T_sp included_class, List_sp mixins)
{
  GC_ALLOCATE(StructureClass_O, sc);
  sc->setName(name);
  List_sp direct_superclasses = _Nil<T_O>();
  if ( included_class.notnilp() ) {
    direct_superclasses = Cons_O::create(included_class,_Nil<T_O>());
  }
  for ( auto cur : mixins ) {
    T_sp mix = oCar(cur);
    direct_superclasses = Cons_O::create(mix,direct_superclasses);
  }
  sc->setInstanceBaseClasses(direct_superclasses);
  eval::funcall(core::_sym_setf_findClass,sc,name);
  return sc;
}


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

    void StructureClass_O::resetSlots()
    {
	IMPLEMENT_ME();
    }



    void StructureClass_O::setupAccessors(List_sp slotNames)
    {
	IMPLEMENT_ME(); // Dont pass the slot names, use the slots already defined
    }

#endif




};
