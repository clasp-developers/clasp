/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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

#ifndef MCA_BTL_SCTP_UTILS_H
#define MCA_BTL_SCTP_UTILS_H

#include "btl_sctp.h"
#include "btl_sctp_frag.h"
#include "btl_sctp_endpoint.h"
#include "btl_sctp_addr.h"
#include <string.h>
#include <sys/socket.h>
#include <netinet/sctp.h>

struct sockaddr_in mca_btl_sctp_utils_sockaddr_from_frag(struct mca_btl_sctp_frag_t *frag);
struct sockaddr_in mca_btl_sctp_utils_sockaddr_from_endpoint(struct mca_btl_base_endpoint_t *ep);
int mca_btl_sctp_utils_writev(int sd, struct iovec *vec, size_t len, 
        struct sockaddr *to_addr, socklen_t to_len, uint16_t stream_no);

#endif
