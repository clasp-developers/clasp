#ifndef	mpiPackage_H
#define mpiPackage_H

#include "core/foundation.h"
#include "core/lisp.h"



NAMESPACE_PACKAGE_ASSOCIATION(mpi,MpiPkg,"MPI");



namespace mpi
{



    class MpiExposer : public core::PackageExposer
    {
    private:
	int _stuff;
    public:
	virtual string packageName() const { return MpiPkg;};
        virtual void expose(core::Lisp_sp lisp,WhatToExpose what) const;
    };




};


#endif
