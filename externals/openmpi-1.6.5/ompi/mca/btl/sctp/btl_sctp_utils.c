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
 * Copyright (c) 2008-2009 Sun Microsystems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/* *****************************************************************************
 *  General purpose routines.
 * ***************************************************************************
 */


#include "btl_sctp_utils.h"
#include "ompi/mca/btl/base/btl_base_error.h"

/**
 * struct sockaddr_in mca_btl_sctp_utils_sockaddr_from_frag
 * --------------------------------------------------------
 *  Returns a sockaddr_in struct associated with the mca_btl_sctp_frag_t *frag.
 */
struct sockaddr_in mca_btl_sctp_utils_sockaddr_from_frag(struct mca_btl_sctp_frag_t *frag) {
    struct sockaddr_in btl_sockaddr;
    memset(&btl_sockaddr, 0, sizeof(struct sockaddr_in));
    btl_sockaddr.sin_family = AF_INET;
    btl_sockaddr.sin_port = frag->endpoint->endpoint_addr->addr_port;
    btl_sockaddr.sin_addr.s_addr = frag->endpoint->endpoint_addr->addr_inet.s_addr;

    return btl_sockaddr;
}

/**
 * struct sockaddr_in mca_btl_sctp_utils_sockaddr_from_endpoint
 * ------------------------------------------------------------
 *  Returns a sockaddr_in struct associated with the mca_btl_base_endpoint_t *ep.
 */
struct sockaddr_in mca_btl_sctp_utils_sockaddr_from_endpoint(struct mca_btl_base_endpoint_t *ep) {
    struct sockaddr_in btl_sockaddr;
    memset(&btl_sockaddr, 0, sizeof(struct sockaddr_in));
    btl_sockaddr.sin_family = AF_INET;
    btl_sockaddr.sin_port = ep->endpoint_addr->addr_port;
    btl_sockaddr.sin_addr.s_addr = ep->endpoint_addr->addr_inet.s_addr;

    return btl_sockaddr;
}

/**
 * int mca_btl_sctp_utils_writev(int sd, struct iovec *vec, size_t len, 
 *         struct sockaddr *to_addr, socklen_t to_len, uint16_t stream_no)
 * -----------------------------------------------------------------------
 *  Vector write.
 */
int mca_btl_sctp_utils_writev(int sd, struct iovec *vec, size_t len, 
        struct sockaddr *to_addr, socklen_t to_len, uint16_t stream_no) {

#if OMPI_MCA_BTL_SCTP_CONCATENATES_IOVS
    /* required for Solaris since struct msghdr has no msg_control field */

    int current_cnt=0, total_offset=0, total=0, byte_sent;
    char *send_buf;

    /* count the total and allocate space to copy to */
    while(current_cnt < len) {
        total += vec[current_cnt].iov_len;
        current_cnt++;
    }
    if(NULL == (send_buf = (char *) malloc(total))) {
        BTL_ERROR(("Ran out of memory."));
        return 0;
    }

    /* combine all iovcnt's into one message.  write all or nothing */
    current_cnt=0;
    while(current_cnt < len) {
        memcpy(send_buf+total_offset, vec[current_cnt].iov_base, vec[current_cnt].iov_len);
        total_offset += vec[current_cnt].iov_len;
        current_cnt++;
    }

    byte_sent = sctp_sendmsg(sd, send_buf, total, (struct sockaddr *) to_addr, 
                    sizeof(*to_addr), 0, 0, stream_no, 0, 0);
    free(send_buf);

    return byte_sent;

#else
    /* uses iovec directly */

    char outcmesg[CMSG_SPACE(sizeof(struct sctp_sndrcvinfo))];
    struct cmsghdr *cmesg;
    struct msghdr outmesg;
    struct sctp_sndrcvinfo *srinfo;

    /* Default these to 0. */
    uint32_t ppid = 0;
    uint32_t flags = 0;
    uint32_t ttl = 0;
    uint32_t cxt = 0;

    outmesg.msg_name = to_addr;
    outmesg.msg_namelen = to_len;
    outmesg.msg_iov = vec;
    outmesg.msg_iovlen = 1;
    outmesg.msg_flags = 0;
    outmesg.msg_control = outcmesg;
    outmesg.msg_controllen = sizeof(outcmesg);

    cmesg = CMSG_FIRSTHDR(&outmesg);
    cmesg->cmsg_len = CMSG_LEN(sizeof(struct sctp_sndrcvinfo));
    outmesg.msg_controllen = cmesg->cmsg_len;
    cmesg->cmsg_level = IPPROTO_SCTP;
    cmesg->cmsg_type = SCTP_SNDRCV;

    srinfo = (struct sctp_sndrcvinfo *)CMSG_DATA(cmesg);
    memset(srinfo, 0, sizeof(struct sctp_sndrcvinfo));
    srinfo->sinfo_ppid = ppid;
    srinfo->sinfo_flags = flags;
    srinfo->sinfo_stream = stream_no;
    srinfo->sinfo_timetolive = ttl;
    srinfo->sinfo_context = cxt;

    return sendmsg(sd, &outmesg, 0);
#endif
}
