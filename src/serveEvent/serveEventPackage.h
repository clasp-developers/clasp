


#ifndef	serveEvent_serveEventPackage_H
#define serveEvent_serveEventPackage_H

#include "core/common.h"


NAMESPACE_PACKAGE_ASSOCIATION(serveEvent,ServeEventPkg,"SERVE-EVENT-INTERNAL");


namespace serveEvent
{




    class ServeEventExposer : public core::Exposer
    {
    private:
    public:
        DISABLE_NEW();
    ServeEventExposer(core::Lisp_sp lisp) : Exposer(lisp,ServeEventPkg) {};
        virtual void expose(core::Lisp_sp lisp,WhatToExpose what) const;
    };





};
#endif

