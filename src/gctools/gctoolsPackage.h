
#ifndef	gctoolsCando_H
#define gctoolsCando_H

#include "core/common.h"
#include "gctoolsPackage.fwd.h"

NAMESPACE_PACKAGE_ASSOCIATION(gctools,GcToolsPkg,"GCTOOLS")



namespace gctools
{

    extern bool _GlobalDebugAllocations;

    class GcToolsExposer : public core::PackageExposer
    {
    private:
    public:
    GcToolsExposer(core::Lisp_sp lisp) : PackageExposer(lisp,GcToolsPkg) {};
        virtual void expose(core::Lisp_sp lisp,WhatToExpose what) const;
    };



struct TestingClass
{
    int x;
    TestingClass() : x(0) {};
    TestingClass(int i) :x(i) {};
    void dump() const { printf("%d ",x);};
    GC_RESULT onHeapScanGCRoots(GC_SCAN_ARGS_PROTOTYPE)
    {
        return GC_RES_OK;
    };
};
    



    void af_cleanup();

    void initialize_bootstrap_kinds();


};
#endif
