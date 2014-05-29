#ifndef	corePackage_H
#define corePackage_H

#include "foundation.h"
#include "lisp.h"
#include "corePackage.fwd.h"

namespace core
{

//#define SYMBOLS_EXTERN
//#i n c l u d e "symbols_scraped_inc.h"

    extern const char* CorePkg_nicknames[];


    class CoreExposer : public core::Exposer
    {
    public:
	CoreExposer(Lisp_sp lisp);
        DISABLE_NEW();
	virtual void expose(core::Lisp_sp lisp, WhatToExpose what) const;

    public:
	/*! Lisp_O::startupLispEnvironment calls this to create the core classes */
	static CoreExposer* create_core_classes(Lisp_sp lisp);
    public:
	void define_essential_globals(Lisp_sp lisp);

#ifdef USE_MPS
        GC_RESULT onHeapScanGCRoots(GC_SCAN_ARGS_PROTOTYPE);
#endif
        
    };

    void add_defsetf_access_update(Symbol_cp access_fn, Symbol_cp update_fn);


};


#endif
