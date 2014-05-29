


#ifndef	sockets_socketsPackage_H
#define sockets_socketsPackage_H

#include "core/common.h"


NAMESPACE_PACKAGE_ASSOCIATION(sockets,SocketsPkg,"SOCKETS-INTERNAL");


namespace sockets
{




    class SocketsExposer : public core::Exposer
    {
    private:
    public:
        DISABLE_NEW();
    SocketsExposer(core::Lisp_sp lisp) : Exposer(lisp,SocketsPkg) {};
        virtual void expose(core::Lisp_sp lisp,WhatToExpose what) const;
    };





};
#endif

