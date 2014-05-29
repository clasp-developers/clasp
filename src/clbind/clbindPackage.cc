#include <boost/mpl/list.hpp>


#include "core/foundation.h"
#include "core/object.h"
#include "core/lisp.h"
#include <core/standardClass.h>
#include "core/builtInClass.h"
#include "core/str.h"
#include "clbind/clbindPackage.h"
#include "clbind/clbind.h"
#include <clbind/adapter.h>
#include <clbind/class_registry.h>
#include <clbind/class_rep.h>
#include "core/wrappers.h"


namespace clbind
{

#define EXPOSE_TO_CANDO
#define Use_ClbindPkg
#define EXTERN_REGISTER
#include "clbind_initClasses_inc.h"
#undef EXTERN_REGISTER
#undef Use_ClbindPkg
#undef EXPOSE_TO_CANDO

};



//
// Load the gctools::GcInfo<core-classes>::Kind specializers
//
#define NAMESPACE_clbind
#include "main/gc_interface.h"
#undef NAMESPACE_clbind


using namespace core;




namespace clbind
{

#pragma GCC visibility push(default)
#define ClbindPkg_SYMBOLS
#define DO_SYMBOL(cname,idx,pkgName,lispName,export) core::Symbol_sp cname = UNDEFINED_SYMBOL;
#include "symbols_scraped_inc.h"
#undef DO_SYMBOL
#undef ClbindPkg_SYMBOLS
#pragma GCC visibility pop


    void ClbindExposer::expose(core::Lisp_sp lisp,core::Exposer::WhatToExpose what) const
    {_G();
	switch (what)
	{
	case candoClasses:
	{
#define ClbindPkg_SYMBOLS
#define DO_SYMBOL(cname,idx,pkg,lispname,exportp) {cname = _lisp->internWithPackageName(pkg,lispname); cname->exportYourself(exportp);}
#include "clbind/symbols_scraped_inc.h"
#undef DO_SYMBOL
#undef ClbindPkg_SYMBOLS


#define ALL_STAGES
#define Use_ClbindPkg
#define INVOKE_REGISTER
#define LOOKUP_SYMBOL(s,p) DEFAULT_LOOKUP_SYMBOL(s,p)
#include "clbind_initClasses_inc.h"
#undef LOOKUP_SYMBOL
#undef INVOKE_REGISTER
#undef Use_ClbindPkg
#undef ALL_STAGES

	}	
	break;
	case candoFunctions:
	{
	    //nothing
//	    initialize_clbind();
	};
	break;
	case candoGlobals:
	{
	    list<string> nicknames;
	    list<string> usePackages = { "COMMON-LISP", "CLOS", ClbindPkg };
	    _lisp->makePackage("SB-BSD-CLBIND",nicknames,usePackages);
            initialize_clbind();
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

#include "clbind_initClasses_inc.h"
#undef _CLASS_MACRO
#undef EXPAND_CLASS_MACROS
#endif




