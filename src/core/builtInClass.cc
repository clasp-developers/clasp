#define	DEBUG_LEVEL_FULL

#include "foundation.h"
#include "builtInClass.h"
#include "lisp.h"
#include "evaluator.h"
#include "standardObject.h"
#include "package.h"
#include "lambdaListHandler.h"
#include "wrappers.h"

namespace core
{

    BuiltInClass_sp BuiltInClass_O::create(Symbol_sp instanceClassSymbol)
    {_G();
	LOG(BF("Creating BuiltInClass_O instanceClassSymbol=%d") % instanceClassSymbol  );
        GC_ALLOCATE(BuiltInClass_O,oclass );
	oclass->setName(instanceClassSymbol);
	return((oclass));
    }

    BuiltInClass_sp BuiltInClass_O::createUncollectable()
    {_G();
        GC_ALLOCATE_UNCOLLECTABLE(BuiltInClass_O,oclass );
	return((oclass));
    }


    BuiltInClass_O::BuiltInClass_O()
    {
    }

    BuiltInClass_O::~BuiltInClass_O()
    {

    }


#if defined(XML_ARCHIVE)
    void	BuiltInClass_O::archive(ArchiveP node)
    {
	IMPLEMENT_ME();
    }
#endif // defined(XML_ARCHIVE)

    void	BuiltInClass_O::initialize()
    {
	this->Base::initialize();
	this->initializeSlots(REF_NUMBER_OF_SLOTS_IN_CLASSES);
//    LOG(BF("For class(%s)@%p handler@%p") % this->static_className() % ((void*)(this)) % this->_InitializationArguments.get() );
    }




/* See the description in object.cc Class_O::describe
 */
    void	BuiltInClass_O::describe()
    {_G();
	_lisp->print(BF("-------------  Class name: %s")
		     % _rep_(this->name()) ); //InstanceClassSymbol );
	for ( Cons_sp cur = this->directSuperclasses(); cur.notnilp(); cur=cCdr(cur) )
	{
	    _lisp->print(BF("Base class: %s") % (oCar(cur).as<Class_O>())->className() );
	}
	_lisp->print(BF("%s") % this->dumpInfo() );
        if ( this->_creator == NULL ) {
            printf("this->_allocator -> NULL\n");
        } else {
            this->_creator->describe();
        }
        _lisp->print(BF("cxxDerivableClassP() -> %d") % this->cxxDerivableClassP());
        _lisp->print(BF("primaryCxxDerivableClassP() -> %d") % this->primaryCxxDerivableClassP());
    }





    void BuiltInClass_O::exposeCando(Lisp_sp lisp)
    {
	class_<BuiltInClass_O>()
	    ;
    }
    void BuiltInClass_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,BuiltInClass,"","",_lisp)
	    ;
#endif
    }



    EXPOSE_CLASS(core,BuiltInClass_O);

};



