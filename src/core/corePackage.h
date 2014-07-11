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
	static CoreExposer* create_core_packages_and_classes();
    public:
	void define_essential_globals(Lisp_sp lisp);

    };

    void add_defsetf_access_update(Symbol_sp access_fn, Symbol_sp update_fn);


};


#endif
