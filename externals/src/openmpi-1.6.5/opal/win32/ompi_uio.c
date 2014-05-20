/*
 Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
                         University Research and Technology
                         Corporation.  All rights reserved.
 Copyright (c) 2004-2005 The University of Tennessee and The University
                         of Tennessee Research Foundation.  All rights
                         reserved.
 Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
                         University of Stuttgart.  All rights reserved.
 Copyright (c) 2004-2005 The Regents of the University of California.
                         All rights reserved.
 $COPYRIGHT$
 
 Additional copyrights may follow
 
 $HEADER$
 */

#include "opal_config.h"
#include "opal/win32/ompi_uio.h"
#include <errno.h>

/*
 Highly doubt if the windows sockets ever set errno to EAGAIN. There might
 be some weird conversion to map this or I might have to rewrite this piece
 of code to handle the windows error flags 
 */

int writev( int fd, struct iovec * iov, int cnt )
{
   int err;
   DWORD sendlen;

   err = WSASend((SOCKET) fd, &(iov->data), cnt, &sendlen, 0, NULL, NULL);

   if (err < 0) {
      return err;
   }
   return (int) sendlen;
} 


int readv( int fd, struct iovec * iov, int cnt )
{
   int err;
   DWORD recvlen = 0;
   DWORD flags = 0;

   err = WSARecv((SOCKET) fd, &(iov->data), cnt, &recvlen, &flags, NULL, NULL);

   if( err < 0 ) {
	   return err;
   }
   return (int) recvlen;
} 

