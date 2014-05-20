


#ifndef	serveEvent_serveEventPackage_H
#define serveEvent_serveEventPackage_H

#include "core/common.h"


NAMESPACE_PACKAGE_ASSOCIATION(serveEvent,ServeEventPkg,"SERVE-EVENT-INTERNAL");


namespace serveEvent
{




    class ServeEventExposer : public core::PackageExposer
    {
    private:
    public:
    ServeEventExposer(core::Lisp_sp lisp) : PackageExposer(lisp,ServeEventPkg) {};
        virtual void expose(core::Lisp_sp lisp,WhatToExpose what) const;
    };





};
#endif

