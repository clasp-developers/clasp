#ifndef	asttooling_asttoolingPackage_H
#define asttooling_asttoolingPackage_H

#include "core/common.h"


NAMESPACE_PACKAGE_ASSOCIATION(asttooling,AstToolingPkg,"AST-TOOLING");


namespace asttooling
{




    class AsttoolingExposer : public core::PackageExposer
    {
    private:
    public:
    AsttoolingExposer(core::Lisp_sp lisp) : PackageExposer(lisp,AstToolingPkg) {
        
    };
        virtual void expose(core::Lisp_sp lisp,WhatToExpose what) const;
    };





};
#endif

