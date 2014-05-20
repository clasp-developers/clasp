/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2009 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "opal_config.h"

#include "opal/win32/ompi_inet.h"
#include "opal/util/output.h"

/* 
 * convert from presentation format (which usually means ASCII printable)
 *    to network format (which is usually some kind of binary format).
 *
 * return:
 *    1 if the address was valid for the specified address family
 *    0 if the address wasn't valid (`dst' is untouched in this case)
 *    -1 if some other error occurred (`dst' is untouched in this case, too)
 */
int ompi_inet_pton(int af, const char *src, void *dst)
{
    int addr_len;
    struct sockaddr sa;
    struct sockaddr_in *sin = (struct sockaddr_in *)&sa;
    struct sockaddr_in6 *sin6 = (struct sockaddr_in6 *)&sa;

    memset (&sa, 0, sizeof(struct sockaddr));

    switch (af) {
    case AF_INET:
        addr_len = sizeof(struct sockaddr_in);
        break;

    case AF_INET6:
        addr_len = sizeof(struct sockaddr_in6);
        break;

    default:
        return -1;
    }

    if ( 0 == WSAStringToAddress ((LPTSTR) src, af, NULL, (LPSOCKADDR) &sa, &addr_len )) {
        switch (af) {
        case AF_INET:
            memcpy (dst, &sin->sin_addr, sizeof(struct in_addr));
            break;

        case AF_INET6:
            memcpy (dst, &sin6->sin6_addr, sizeof(struct in6_addr));
            break;
        }
        return 1;
    } else {
        opal_output(0, "WSAStringToAddress failed %s:%d. Error code: %d", __FILE__, __LINE__, GetLastError());
        return 0;
    }
}


/* 
 * convert a network format address to presentation format.
 *
 * return:
 *    pointer to presentation format address (`dst'), or NULL.
 */
const char *ompi_inet_ntop(int af, const void *src, char *dst, size_t size)
{
    int addr_len;
    struct sockaddr sa;
    DWORD str_len = size;
	struct sockaddr_in *sin = (struct sockaddr_in *)&sa;
	struct sockaddr_in6 *sin6 = (struct sockaddr_in6 *)&sa;

    memset (&sa, 0, sizeof (struct sockaddr));

    switch (af) {
    case AF_INET:
        addr_len = sizeof(struct sockaddr_in);
		sin->sin_family = af;
		memcpy (&sin->sin_addr, src, sizeof (struct in_addr));
        break;

    case AF_INET6:
        addr_len = sizeof(struct sockaddr_in6);
		sin6->sin6_family = af;
		memcpy (&sin6->sin6_addr, src, sizeof (struct in6_addr));
        break;

    default:
        return NULL;
    }

    if ( 0 == WSAAddressToString ((LPSOCKADDR) &sa, addr_len, NULL, dst, &str_len )) {
        return dst;
    } else {
        opal_output(0, "WSAAddressToString failed %s:%d. Error code: %d", __FILE__, __LINE__, GetLastError());
        return NULL;
    }
}
