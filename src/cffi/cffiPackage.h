
#ifndef	cffiCando_H
#define cffiCando_H

#include "core/common.h"




NAMESPACE_PACKAGE_ASSOCIATION(cffi,CffiPkg,"CFFI-SYS");


namespace cffi
{


#define	CffiPkg_SYMBOLS
#define DO_SYMBOL(cname,idx,pkg,lispname,export) extern core::Symbol_sp cname;
#include "symbols_scraped_inc.h"
#undef DO_SYMBOL
#undef CffiPkg_SYMBOLS



    class CffiExposer : public core::Exposer
    {
    private:
    public:
        DISABLE_NEW();
    CffiExposer(core::Lisp_sp lisp) : Exposer(lisp,CffiPkg) {};
        virtual void expose(core::Lisp_sp lisp,WhatToExpose what) const;
    };



};
#endif
