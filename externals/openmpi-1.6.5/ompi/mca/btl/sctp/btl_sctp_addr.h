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
/**
 * @file
 */
#ifndef MCA_BTL_SCTP_ADDR_H
#define MCA_BTL_SCTP_ADDR_H

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif


/**
 * Structure used to publish SCTP connection information to peers.
 */
struct mca_btl_sctp_addr_t {
    struct in_addr addr_inet;     /**< IPv4 address in network byte order */
    in_port_t      addr_port;     /**< listen port */
    unsigned short addr_inuse;    /**< local meaning only */
};
typedef struct mca_btl_sctp_addr_t mca_btl_sctp_addr_t;

#endif

