#ifndef	mpiPackage_H
#define mpiPackage_H

#include "core/foundation.h"
#include "core/lisp.h"



NAMESPACE_PACKAGE_ASSOCIATION(mpip,MpiPkg,"MPI");



namespace mpip
{



    class MpiExposer : public core::Exposer
    {
    private:
    public:
        DISABLE_NEW();
	MpiExposer(core::Lisp_sp lisp) : Exposer(lisp,MpiPkg) {};
        virtual void expose(core::Lisp_sp lisp,WhatToExpose what) const;
    };




};


#endif
