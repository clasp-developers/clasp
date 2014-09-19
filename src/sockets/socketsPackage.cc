#include <boost/mpl/list.hpp>


#include "core/foundation.h"
#include "core/object.h"
#include "core/lisp.h"
#include "core/builtInClass.h"
#include "sockets/socketsPackage.h"
#include "sockets/sockets.h"
#include "core/str.h"
#include "core/wrappers.h"


namespace sockets
{

#define EXPOSE_TO_CANDO
#define Use_SocketsPkg
#define EXTERN_REGISTER
#include "sockets_initClasses_inc.h"
#undef EXTERN_REGISTER
#undef Use_SocketsPkg
#undef EXPOSE_TO_CANDO

};



using namespace core;




namespace sockets
{

#pragma GCC visibility push(default)
#define SocketsPkg_SYMBOLS
#define DO_SYMBOL(cname,idx,pkgName,lispName,export) core::Symbol_sp cname = UNDEFINED_SYMBOL;
#include "symbols_scraped_inc.h"
#undef DO_SYMBOL
#undef SocketsPkg_SYMBOLS
#pragma GCC visibility pop


    void SocketsExposer::expose(core::Lisp_sp lisp,core::Exposer::WhatToExpose what) const
    {_G();
	switch (what)
	{
	case candoClasses:
	{
#define SocketsPkg_SYMBOLS
#define DO_SYMBOL(cname,idx,pkg,lispname,exportp) {cname = _lisp->internUniqueWithPackageName(pkg,lispname); cname->exportYourself(exportp);}
#include "sockets/symbols_scraped_inc.h"
#undef DO_SYMBOL
#undef SocketsPkg_SYMBOLS


#define ALL_STAGES
#define Use_SocketsPkg
#define INVOKE_REGISTER
#include "sockets_initClasses_inc.h"
#undef INVOKE_REGISTER
#undef Use_SocketsPkg
#undef ALL_STAGES

	}	
	break;
	case candoFunctions:
	{
	    //nothing
	    initialize_sockets();
	};
	break;
	case candoGlobals:
	{
	    list<string> nicknames;
	    list<string> usePackages = { "COMMON-LISP", "CLOS", SocketsPkg };
	    _lisp->makePackage("SB-BSD-SOCKETS",nicknames,usePackages);
	    initialize_sockets_globals();
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

#include "sockets_initClasses_inc.h"
#undef _CLASS_MACRO
#undef EXPAND_CLASS_MACROS
#endif




