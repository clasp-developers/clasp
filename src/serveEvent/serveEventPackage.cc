#include <boost/mpl/list.hpp>


#include "core/foundation.h"
#include "core/object.h"
#include "core/lisp.h"
#include "core/builtInClass.h"
#include "serveEvent/serveEventPackage.h"
#include "serveEvent/serveEvent.h"
#include "core/str.h"
#include "core/wrappers.h"


namespace serveEvent
{

#define EXPOSE_TO_CANDO
#define Use_ServeEventPkg
#define EXTERN_REGISTER
#include "serveEvent_initClasses_inc.h"
#undef EXTERN_REGISTER
#undef Use_ServeEventPkg
#undef EXPOSE_TO_CANDO

};



using namespace core;




namespace serveEvent
{

#pragma GCC visibility push(default)
#define ServeEventPkg_SYMBOLS
#define DO_SYMBOL(cname,idx,pkgName,lispName,export) core::Symbol_sp cname = UNDEFINED_SYMBOL;
#include "symbols_scraped_inc.h"
#undef DO_SYMBOL
#undef ServeEventPkg_SYMBOLS
#pragma GCC visibility pop


    void ServeEventExposer::expose(core::Lisp_sp lisp,core::Exposer::WhatToExpose what) const
    {_G();
	switch (what)
	{
	case candoClasses:
	{
#define ServeEventPkg_SYMBOLS
#define DO_SYMBOL(cname,idx,pkg,lispname,exportp) {cname = _lisp->internUniqueWithPackageName(pkg,lispname); cname->exportYourself(exportp);}
#include "serveEvent/symbols_scraped_inc.h"
#undef DO_SYMBOL
#undef ServeEventPkg_SYMBOLS


#define ALL_STAGES
#define Use_ServeEventPkg
#define INVOKE_REGISTER
#include "serveEvent_initClasses_inc.h"
#undef INVOKE_REGISTER
#undef Use_ServeEventPkg
#undef ALL_STAGES

	}	
	break;
	case candoFunctions:
	{
	    //nothing
	    initialize_serveEvent_functions();
	};
	break;
	case candoGlobals:
	{
	    initialize_serveEvent_globals();
	};
	break;
	case pythonClasses:
	case pythonFunctions:
	case pythonGlobals:
	{
	    IMPLEMENT_ME();
	}
	break;
	}
    }
	
};


#if USE_INTRUSIVE_SMART_PTR==1
#define EXPAND_CLASS_MACROS

#if defined(USE_MPS) // MPS doesn't require INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS
#define _CLASS_MACRO(_T_) \
    STATIC_CLASS_INFO(_T_); 
#else
#define _CLASS_MACRO(_T_) \
    STATIC_CLASS_INFO(_T_); \
    INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(_T_);
#endif

#include "serveEvent_initClasses_inc.h"
#undef _CLASS_MACRO
#undef EXPAND_CLASS_MACROS
#endif




