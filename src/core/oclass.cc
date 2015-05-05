/*
    File: oclass.cc
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

#include <oclass.h>
#include <clasp/core/lisp.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/standardObject.h>
#include <clasp/core/package.h>
#include <clasp/core/lambdaListHandler.h>
#include <keyedArguments.h>
#include <clasp/core/wrappers.h>

namespace core
{

/*
  __BEGIN_DOC( classes.classMethods, Class methods)
  In Cando-Script class names like ``Hit'' or ``Real'' return objects that are of the class ``Class''.  These objects respond to the following methods.
  __END_DOC
*/
    BuiltInClass_sp BuiltInClass_O::create(Lisp_sp lisp,const string name, int instanceClassId;
    {_G();
	LOG(BF("Creating BuiltInClass_O name(%s) instanceClassId=%d")
	    % name.c_str() % instanceClassId  );
        GC_ALLOCATE(BuiltInClass_O,oclass );
	oclass->_Name = lisp->intern(name);
	oclass->_InstanceClassId = instanceClassId;
	return oclass;
    }



    BuiltInClass_O::~BuiltInClass_O()
    {

    }


    void	BuiltInClass_O::archive(ArchiveP node)
    {
	IMPLEMENT_ME();
    }


#if 0
    T_sp BuiltInClass_O::allocateAndInitialize()
    {
	T_sp o = this->allocate_newNil();
	o->initialize();
	return o;
    }
#endif

    void	BuiltInClass_O::initialize()
    {
	this->Base::initialize();
//    this->_InitializationArguments = _Nil<LambdaListHandler_O>();
//    LOG(BF("For class(%s)@%p handler@%p") % this->static_className() % ((void*)(this)) % this->_InitializationArguments.get() );
    }



    BuiltInClass_sp BuiltInClass_O::getInstanceCoreClass() const
    {_OF();
	return this->sharedThis<BuiltInClass_O>();
    }




/* See the description in object.cc Class_O::describe
 */
    void	BuiltInClass_O::describe()
    {_G();
	_lisp->print(BF("-------------  Class name: %s    instanceClassId: %s") % this->_Name->__repr__() % this->_InstanceClassId->__repr__() );
	for ( Class_O::baseClassIterator it = this->_DirectSuperClasses.begin();
	      it!=this->_DirectSuperClasses.end(); it++ )
	{
	    _lisp->print(BF("Base class: %s") % (*it)->className() );
	}
	_lisp->print(BF("%s") % this->dumpInfo() );
    }


    void	StandardClass_O::archive(ArchiveP node)
    {
	IMPLEMENT_ME();
    }

    void	StandardClass_O::defineYourSlotsFromBinderArchiveNode(ArchiveP node)
    {_G();
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



    void	StandardClass_O::initialize()
    {
	this->Base::initialize();
//    this->_InstanceVariableNames = _Nil<T_O>();
	this->_SlotSpecifiers.clear();
	this->_InstanceCoreClass = _Nil<BuiltInClass_O>();
    }

    StandardClass_sp StandardClass_O::create(Lisp_sp lisp,Symbol_sp name, uint instanceClassId)
    {
        GC_ALLOCATE(StandardClass_O,oclass );
	oclass->_Name = name;
	oclass->_InstanceClassId = instanceClassId;
	oclass->_InstanceCoreClass = _Nil<BuiltInClass_O>();
	return oclass;
    }

    /*! Return a list of classes from a classListDesignator which can be...
      nil - return a Cons containing StandardObject.
      [a metaclass] - return a Cons containing that metaclass.
      [Cons of metaclasses] - return the cons of metaclasses.
    */
    Cons_sp StandardClass_O::classListDesignator(T_sp baseClassesDesignator, Lisp_sp lisp)
    {_G();
	Cons_sp baseClasses;
	if ( baseClassesDesignator.nilp() )
	{
	    baseClasses = Cons_O::create(lisp->classFromClassId(StandardObject_O::static_classId()),lisp);
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








    BuiltInClass_sp StandardClass_O::getInstanceCoreClass() const
    {_OF();
	return this->_InstanceCoreClass;
    }

    void StandardClass_O::setInstanceCoreClass(BuiltInClass_sp mc)
    {_OF();
	this->_InstanceCoreClass = mc;
    }

    void	StandardClass_O::describe()
    {_G();
	_lisp->print(BF("------------  StandardClass name: %s    instanceClassId: %d") % this->_Name->__repr__() % this->_InstanceClassId );
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
    }

    string StandardClass_O::dumpInfo()
    {_G();
	stringstream ss;
	ss << this->Base::dumpInfo();
	ss << "CoreBuiltInClass: " << this->_InstanceCoreClass->getPackagedName() << std::endl;
	return ss.str();
    }

    string StandardClass_O::dumpMethods()
    {
	return this->Base::dumpMethods();
    }


    uint StandardClass_O::numberOfSlots()
    {
	return this->_SlotSpecifiers.size();
    }


    StandardClass_O::slotIterator StandardClass_O::find(Symbol_sp sym)
    {_G();
	ASSERTNOTNULL(sym);
	LOG(BF("Looking in StandardClass for slot for symbol: %s") % sym->fullName() );
	slotIterator it;
	for ( it=this->_SlotSpecifiers.begin(); it!=this->_SlotSpecifiers.end(); it++ )
	{
	    if ( (*it)->_SlotName == sym ) break;
	}
#ifdef	DEBUG_ON
	if ( it==this->_SlotSpecifiers.end() )
	{
	    LOG(BF("Could not find slot"));
	} else
	{
	    LOG(BF("Found slot"));
	}
#endif
	return it;
    }







#if 0
    T_sp StandardClass_O::allocate_newNil()
    {_G();
	DEPRECIATED(); // Is this really?
	T_sp obj = this->_InstanceCoreClass->new_instance(_Nil<Function_O>(), 
							  _Nil<T_O>(),
							  _Nil<T_O>(), _lisp );
	StandardClass_sp thisClass = this->sharedThis<StandardClass_O>();
	obj->__setClass(thisClass);
	return obj;
    }
#endif


    void StandardClass_O::resetSlots()
    {_G();
	this->_SlotSpecifiers.clear();
    }



    void StandardClass_O::setupAccessors(List_sp slotNames)
    {_G();
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


    void BuiltInClass_O::exposeCando(Lisp_sp lisp)
    {
	class_<BuiltInClass_O>()
	    ;
    }
    void BuiltInClass_O::exposePython(Lisp_sp lisp)
    {_G();
	PYTHON_CLASS(CorePkg,BuiltInClass,"","",_lisp)
	    ;
    }




    void StandardClass_O::exposeCando(Lisp_sp lisp)
    {
	class_<StandardClass_O>()
	    ;
    }
    void StandardClass_O::exposePython(Lisp_sp lisp)
    {_G();
	PYTHON_CLASS(CorePkg,StandardClass,"","",_lisp)
	    ;
    }


    EXPOSE_CLASS(core,BuiltInClass_O);
    EXPOSE_CLASS(core,StandardClass_O);

};
