/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#ifndef OPAL_GET_SOCKET_ERROR_H
#define OPAL_GET_SOCKET_ERROR_H

/* In windows, sockets return differnt error codes than the linux counter parts. Although,
   one can find there are some similarities in the naming, there are definite differences.
   opal_socket_errno is defined to be errno under linux and opal_get_socket_errno under
   windows to ensure that the code which uses errno does not have to be changed. In windows,
   the mapping is taken care of by opal_get_socket_errno().
   
   ANYONE USING SOCKET FUNCTIONS' RETURN VALUE PLEASE USE opal_socket_errno INSTEAD
   OF errno FOR COMPATIBILITY  */

#include <errno.h>
#include "opal/constants.h"
#ifdef __WINDOWS__
#define opal_socket_errno opal_get_socket_errno()

/* some of these have been defined in newer version of errno.h*/
#if (defined(__WINDOWS__) && !defined(_MSC_VER)) || _MSC_VER < 1600

#define EWOULDBLOCK       WSAEWOULDBLOCK
#define EINPROGRESS       WSAEINPROGRESS
#define EALREADY          WSAEALREADY
#define ENOTSOCK          WSAENOTSOCK
#define EDESTADDRREQ      WSAEDESTADDRREQ
#define EMSGSIZE          WSAEMSGSIZE
#define EPROTOTYPE        WSAEPROTOTYPE
#define ENOPROTOOPT       WSAENOPROTOOPT
#define EPROTONOSUPPORT   WSAEPROTONOSUPPORT
#define EOPNOTSUPP        WSAEOPNOTSUPP
#define EAFNOSUPPORT      WSAEAFNOSUPPORT
#define EADDRINUSE        WSAEADDRINUSE
#define EADDRNOTAVAIL     WSAEADDRNOTAVAIL
#define ENETDOWN          WSAENETDOWN
#define ENETUNREACH       WSAENETUNREACH
#define ENETRESET         WSAENETRESET
#define ECONNABORTED      WSAECONNABORTED
#define ECONNRESET        WSAECONNRESET
#define ENOBUFS           WSAENOBUFS
#define EISCONN           WSAEISCONN
#define ENOTCONN          WSAENOTCONN
#define ETIMEDOUT         WSAETIMEDOUT
#define ECONNREFUSED      WSAECONNREFUSED
#define ELOOP             WSAELOOP
#define EHOSTUNREACH      WSAEHOSTUNREACH

#endif /*defined(_MSC_VER) && _MSC_VER < 1600*/

#define ESOCKTNOSUPPORT   WSAESOCKTNOSUPPORT
#define EPFNOSUPPORT      WSAEPFNOSUPPORT
#define ESHUTDOWN         WSAESHUTDOWN
#define ETOOMANYREFS      WSAETOOMANYREFS
#define EHOSTDOWN         WSAEHOSTDOWN
#define EPROCLIM          WSAEPROCLIM
#define EUSERS            WSAEUSERS
#define EDQUOT            WSAEDQUOT
#define ESTALE            WSAESTALE
#define EREMOTE           WSAEREMOTE

/*
 * pound define opal_get_error() to be opal_errno. so, in windows land
 * this simply defaults to being errno
 */

/* return directly from the case statments */

static __inline int opal_get_socket_errno(void) {
    int ret = WSAGetLastError();
    switch (ret) {
      case WSAEINTR: return EINTR; 
      case WSAEBADF: return EBADF; 
      case WSAEACCES: return EACCES;           
      case WSAEFAULT: return EFAULT;          
      case WSAEINVAL: return EINVAL;          
      case WSAEMFILE: return EMFILE;          
      case WSAEWOULDBLOCK: return EWOULDBLOCK;      
      case WSAEINPROGRESS: return EINPROGRESS;     
      case WSAEALREADY: return EALREADY;         
      case WSAENOTSOCK: return ENOTSOCK;        
      case WSAEDESTADDRREQ: return EDESTADDRREQ;     
      case WSAEMSGSIZE: return EMSGSIZE;       
      case WSAEPROTOTYPE: return EPROTOTYPE;       
      case WSAENOPROTOOPT: return ENOPROTOOPT;     
      case WSAEPROTONOSUPPORT: return EPROTONOSUPPORT;  
      case WSAESOCKTNOSUPPORT: return ESOCKTNOSUPPORT; 
      case WSAEOPNOTSUPP: return EOPNOTSUPP;      
      case WSAEPFNOSUPPORT: return EPFNOSUPPORT;    
      case WSAEAFNOSUPPORT: return EAFNOSUPPORT;    
      case WSAEADDRINUSE: return EADDRINUSE;      
      case WSAEADDRNOTAVAIL: return EADDRNOTAVAIL;    
      case WSAENETDOWN: return ENETDOWN;       
      case WSAENETUNREACH: return ENETUNREACH;      
      case WSAENETRESET: return ENETRESET;      
      case WSAECONNABORTED: return ECONNABORTED;     
      case WSAECONNRESET: return ECONNRESET;      
      case WSAENOBUFS: return ENOBUFS;         
      case WSAEISCONN: return EISCONN;         
      case WSAENOTCONN: return ENOTCONN;        
      case WSAESHUTDOWN: return ESHUTDOWN;        
      case WSAETOOMANYREFS: return ETOOMANYREFS;    
      case WSAETIMEDOUT: return ETIMEDOUT;       
      case WSAECONNREFUSED: return ECONNREFUSED;    
      case WSAELOOP: return ELOOP;           
      case WSAENAMETOOLONG: return ENAMETOOLONG;    
      case WSAEHOSTDOWN: return EHOSTDOWN;       
      case WSAEHOSTUNREACH: return EHOSTUNREACH;    
      case WSAENOTEMPTY: return ENOTEMPTY;       
      case WSAEPROCLIM: return EPROCLIM;        
      case WSAEUSERS: return EUSERS;          
      case WSAEDQUOT: return EDQUOT;          
      case WSAESTALE: return ESTALE;          
      case WSAEREMOTE: return EREMOTE;         
      default: printf("Feature not implemented: %d %s\n", __LINE__, __FILE__); return OPAL_ERROR;
    };                                
}

#else 
#define opal_socket_errno errno
#endif

#endif /* OPAL_GET_ERROR_H */
