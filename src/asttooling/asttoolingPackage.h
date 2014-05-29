#ifndef	asttooling_asttoolingPackage_H
#define asttooling_asttoolingPackage_H

#include "core/common.h"


NAMESPACE_PACKAGE_ASSOCIATION(asttooling,AstToolingPkg,"AST-TOOLING");


namespace asttooling
{




    class AsttoolingExposer : public core::Exposer
    {
    private:
    public:
        DISABLE_NEW();
        AsttoolingExposer(core::Lisp_sp lisp) : Exposer(lisp,AstToolingPkg) {
        
    };
        virtual void expose(core::Lisp_sp lisp,WhatToExpose what) const;
    };





};
#endif

