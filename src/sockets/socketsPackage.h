


#ifndef	sockets_socketsPackage_H
#define sockets_socketsPackage_H

#include "core/common.h"


NAMESPACE_PACKAGE_ASSOCIATION(sockets,SocketsPkg,"SOCKETS-INTERNAL");


namespace sockets
{




    class SocketsExposer : public core::PackageExposer
    {
    private:
    public:
    SocketsExposer(core::Lisp_sp lisp) : PackageExposer(lisp,SocketsPkg) {};
        virtual void expose(core::Lisp_sp lisp,WhatToExpose what) const;
    };





};
#endif

