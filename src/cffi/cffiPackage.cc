#include <boost/mpl/list.hpp>


#include "core/foundation.h"
#include "core/object.h"
#include "core/lisp.h"
#include "core/builtInClass.h"
#include "cffi/cffiPackage.h"
#include "cffi.h"
#include "core/str.h"
#include "core/wrappers.h"


namespace cffi
{

#define EXPOSE_TO_CANDO
#define Use_CffiPkg
#define EXTERN_REGISTER
#include "cffi_initClasses_inc.h"
#undef EXTERN_REGISTER
#undef Use_CffiPkg
#undef EXPOSE_TO_CANDO

};



using namespace core;




namespace cffi
{

#pragma GCC visibility push(default)
#define CffiPkg_SYMBOLS
#define DO_SYMBOL(cname,idx,pkgName,lispName,export) core::Symbol_sp cname = UNDEFINED_SYMBOL;
#include "symbols_scraped_inc.h"
#undef DO_SYMBOL
#undef CffiPkg_SYMBOLS
#pragma GCC visibility pop


    void CffiExposer::expose(core::Lisp_sp lisp,core::Exposer::WhatToExpose what) const
    {_G();
	switch (what)
	{
	case candoClasses:
	{
#define CffiPkg_SYMBOLS
#define DO_SYMBOL(cname,idx,pkg,lispname,exportp) {cname = _lisp->internWithPackageName(pkg,lispname); cname->exportYourself(exportp);}
#include "cffi/symbols_scraped_inc.h"
#undef DO_SYMBOL
#undef CffiPkg_SYMBOLS


#define ALL_STAGES
#define Use_CffiPkg
#define INVOKE_REGISTER
#define LOOKUP_SYMBOL(s,p) DEFAULT_LOOKUP_SYMBOL(s,p)
#include "cffi_initClasses_inc.h"
#undef LOOKUP_SYMBOL
#undef INVOKE_REGISTER
#undef Use_CffiPkg
#undef ALL_STAGES

	}	
	break;
	case candoFunctions:
	{
	    //nothing
	    initialize_cffi();
	};
	break;
	case candoGlobals:
	{
	    
//	initializeLlvmConstants(_lisp);
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



#ifdef USE_MPS
//
// Include the Kinds
//
#define NAMESPACE_cffi
#include GARBAGE_COLLECTION_INCLUDE
#undef NAMESPACE_cffi
#endif


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

#include "cffi_initClasses_inc.h"
#undef _CLASS_MACRO
#undef EXPAND_CLASS_MACROS
#endif




