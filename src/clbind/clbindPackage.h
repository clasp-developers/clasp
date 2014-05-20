


#ifndef	clbind_clbindPackage_H
#define clbind_clbindPackage_H

#include "core/common.h"


NAMESPACE_PACKAGE_ASSOCIATION(clbind,ClbindPkg,"CLBIND");


namespace clbind
{




    class ClbindExposer : public core::PackageExposer
    {
    private:
    public:
    ClbindExposer(core::Lisp_sp lisp) : PackageExposer(lisp,ClbindPkg) {};
        virtual void expose(core::Lisp_sp lisp,WhatToExpose what) const;
    };





};
#endif

