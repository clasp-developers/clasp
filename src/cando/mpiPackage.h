#ifndef	mpiPackage_H
#define mpiPackage_H

#include "core/foundation.h"
#include "core/lisp.h"



NAMESPACE_PACKAGE_ASSOCIATION(mpi,MpiPkg,"MPI");



namespace mpi
{



    class MpiExposer : public core::Exposer
    {
    private:
	int _stuff;
    public:
        DISABLE_NEW();
        // Why is there no constructor like other exposers have?
	virtual string packageName() const { return MpiPkg;};
        virtual void expose(core::Lisp_sp lisp,WhatToExpose what) const;
    };




};


#endif
