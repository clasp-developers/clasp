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
 * Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 * In windows, many of the socket functions return an EWOULDBLOCK
 * instead of \ things like EAGAIN, EINPROGRESS, etc. It has been
 * verified that this will \ not conflict with other error codes that
 * are returned by these functions \ under UNIX/Linux environments 
 */

#include "ompi_config.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_UIO_H
#include <sys/uio.h>
#endif
#ifdef HAVE_NET_UIO_H
#include <net/uio.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */

#include "opal/opal_socket_errno.h"
#include "ompi/mca/btl/base/btl_base_error.h"
#include "btl_tcp_frag.h" 
#include "btl_tcp_endpoint.h"

static void mca_btl_tcp_frag_common_constructor(mca_btl_tcp_frag_t* frag) 
{ 
    frag->base.des_src = NULL;
    frag->base.des_src_cnt = 0;
    frag->base.des_dst = NULL;
    frag->base.des_dst_cnt = 0;
}

static void mca_btl_tcp_frag_eager_constructor(mca_btl_tcp_frag_t* frag) 
{ 
    frag->size = mca_btl_tcp_module.super.btl_eager_limit;   
    frag->my_list = &mca_btl_tcp_component.tcp_frag_eager;
    mca_btl_tcp_frag_common_constructor(frag); 
}

static void mca_btl_tcp_frag_max_constructor(mca_btl_tcp_frag_t* frag) 
{ 
    frag->size = mca_btl_tcp_module.super.btl_max_send_size; 
    frag->my_list = &mca_btl_tcp_component.tcp_frag_max;
    mca_btl_tcp_frag_common_constructor(frag); 
}

static void mca_btl_tcp_frag_user_constructor(mca_btl_tcp_frag_t* frag) 
{ 
    frag->size = 0; 
    frag->my_list = &mca_btl_tcp_component.tcp_frag_user;
    mca_btl_tcp_frag_common_constructor(frag); 
}


OBJ_CLASS_INSTANCE(
    mca_btl_tcp_frag_t, 
    mca_btl_base_descriptor_t, 
    NULL, 
    NULL); 

OBJ_CLASS_INSTANCE(
    mca_btl_tcp_frag_eager_t, 
    mca_btl_base_descriptor_t, 
    mca_btl_tcp_frag_eager_constructor, 
    NULL); 

OBJ_CLASS_INSTANCE(
    mca_btl_tcp_frag_max_t, 
    mca_btl_base_descriptor_t, 
    mca_btl_tcp_frag_max_constructor, 
    NULL); 

OBJ_CLASS_INSTANCE(
    mca_btl_tcp_frag_user_t, 
    mca_btl_base_descriptor_t, 
    mca_btl_tcp_frag_user_constructor, 
    NULL); 


bool mca_btl_tcp_frag_send(mca_btl_tcp_frag_t* frag, int sd)
{
    int cnt=-1;
    size_t i, num_vecs;

    /* non-blocking write, but continue if interrupted */
    while(cnt < 0) {
        cnt = writev(sd, frag->iov_ptr, frag->iov_cnt);
        if(cnt < 0) {
            switch(opal_socket_errno) {
            case EINTR:
                continue;
            case EWOULDBLOCK:
                return false;
            case EFAULT:
                BTL_ERROR(("mca_btl_tcp_frag_send: writev error (%p, %lu)\n\t%s(%lu)\n",
                    frag->iov_ptr[0].iov_base, (unsigned long) frag->iov_ptr[0].iov_len,
                    strerror(opal_socket_errno), (unsigned long) frag->iov_cnt));
                mca_btl_tcp_endpoint_close(frag->endpoint);
                return false;
            default:
                BTL_ERROR(("mca_btl_tcp_frag_send: writev failed: %s (%d)", 
                           strerror(opal_socket_errno),
                           opal_socket_errno));
                mca_btl_tcp_endpoint_close(frag->endpoint);
                return false;
            }
        }
    }

    /* if the write didn't complete - update the iovec state */
    num_vecs = frag->iov_cnt;
    for(i=0; i<num_vecs; i++) {
        if(cnt >= (int)frag->iov_ptr->iov_len) {
            cnt -= frag->iov_ptr->iov_len;
            frag->iov_ptr++;
            frag->iov_idx++;
            frag->iov_cnt--;
        } else {
            frag->iov_ptr->iov_base = (ompi_iov_base_ptr_t)
                (((unsigned char*)frag->iov_ptr->iov_base) + cnt);
            frag->iov_ptr->iov_len -= cnt;
            break;
        }
    }
    return (frag->iov_cnt == 0);
}

bool mca_btl_tcp_frag_recv(mca_btl_tcp_frag_t* frag, int sd)
{
    int cnt, dont_copy_data = 0;
    size_t i, num_vecs;
    mca_btl_base_endpoint_t* btl_endpoint = frag->endpoint;

 repeat:
    num_vecs = frag->iov_cnt;
#if MCA_BTL_TCP_ENDPOINT_CACHE
    if( 0 != btl_endpoint->endpoint_cache_length ) {
        size_t length;
        /* It's strange at the first look but cnt have to be set to the full amount of data
         * available. After going to advance_iov_position we will use cnt to detect if there
         * is still some data pending.
         */
        cnt = length = btl_endpoint->endpoint_cache_length;
        for( i = 0; i < frag->iov_cnt; i++ ) {
            if( length > frag->iov_ptr[i].iov_len )
                length = frag->iov_ptr[i].iov_len;
            if( (0 == dont_copy_data) || (length < frag->iov_ptr[i].iov_len) ) {
                memcpy( frag->iov_ptr[i].iov_base, btl_endpoint->endpoint_cache_pos, length );
            } else {
                frag->segments[0].seg_addr.pval = btl_endpoint->endpoint_cache_pos;
                frag->iov_ptr[i].iov_base = btl_endpoint->endpoint_cache_pos;
            }
            btl_endpoint->endpoint_cache_pos += length;
            btl_endpoint->endpoint_cache_length -= length;
            length = btl_endpoint->endpoint_cache_length;
            if( 0 == length ) {
                btl_endpoint->endpoint_cache_pos = btl_endpoint->endpoint_cache;
                break;
            }
        }
        goto advance_iov_position;
    }
    /* What's happens if all iovecs are used by the fragment ? It still work, as we reserve one
     * iovec for the caching in the fragment structure (the +1).
     */
    frag->iov_ptr[num_vecs].iov_base = btl_endpoint->endpoint_cache_pos;
    frag->iov_ptr[num_vecs].iov_len  = 
        mca_btl_tcp_component.tcp_endpoint_cache - btl_endpoint->endpoint_cache_length;
    num_vecs++;
#endif  /* MCA_BTL_TCP_ENDPOINT_CACHE */

    /* non-blocking read, but continue if interrupted */
    cnt = -1;
    while( cnt < 0 ) {
        cnt = readv(sd, frag->iov_ptr, num_vecs);
	if( 0 < cnt ) goto advance_iov_position;
	if( cnt == 0 ) {
	    mca_btl_tcp_endpoint_close(btl_endpoint);
	    return false;
	}
	switch(opal_socket_errno) {
	case EINTR:
	    continue;
	case EWOULDBLOCK:
	    return false;
	case EFAULT:
            BTL_ERROR(("mca_btl_tcp_frag_recv: readv error (%p, %lu)\n\t%s(%lu)\n",
                       frag->iov_ptr[0].iov_base, (unsigned long) frag->iov_ptr[0].iov_len,
                       strerror(opal_socket_errno), (unsigned long) frag->iov_cnt));
	    mca_btl_tcp_endpoint_close(btl_endpoint);
	    return false;
	default:
            BTL_ERROR(("mca_btl_tcp_frag_recv: readv failed: %s (%d)", 
                       strerror(opal_socket_errno),
                       opal_socket_errno));
	    mca_btl_tcp_endpoint_close(btl_endpoint);
	    return false;
	}
    };

 advance_iov_position:
    /* if the read didn't complete - update the iovec state */
    num_vecs = frag->iov_cnt;
    for( i = 0; i < num_vecs; i++ ) {
        if( cnt < (int)frag->iov_ptr->iov_len ) {
            frag->iov_ptr->iov_base = (ompi_iov_base_ptr_t)
                (((unsigned char*)frag->iov_ptr->iov_base) + cnt);
            frag->iov_ptr->iov_len -= cnt;
            cnt = 0;
            break;
	}
	cnt -= frag->iov_ptr->iov_len;
	frag->iov_idx++;
	frag->iov_ptr++;
	frag->iov_cnt--;
    }
#if MCA_BTL_TCP_ENDPOINT_CACHE
    btl_endpoint->endpoint_cache_length = cnt;
#endif  /* MCA_BTL_TCP_ENDPOINT_CACHE */

    /* read header */
    if(frag->iov_cnt == 0) {
        if (btl_endpoint->endpoint_nbo && frag->iov_idx == 1) MCA_BTL_TCP_HDR_NTOH(frag->hdr);
        switch(frag->hdr.type) {
        case MCA_BTL_TCP_HDR_TYPE_SEND:
            if(frag->iov_idx == 1 && frag->hdr.size) {
                frag->segments[0].seg_addr.pval = frag+1;
                frag->segments[0].seg_len = frag->hdr.size;
                frag->iov[1].iov_base = (IOVBASE_TYPE*)(frag->segments[0].seg_addr.pval);
                frag->iov[1].iov_len = frag->hdr.size;
                frag->iov_cnt++;
#ifndef __sparc
                /* The following cannot be done for sparc code 
                 * because it causes alignment errors when accessing
                 * structures later on in the btl and pml code.
                 */
                dont_copy_data = 1;
#endif
                goto repeat;
            }
            break;
        case MCA_BTL_TCP_HDR_TYPE_PUT:
            if(frag->iov_idx == 1) {
                frag->iov[1].iov_base = (IOVBASE_TYPE*)frag->segments;
                frag->iov[1].iov_len = frag->hdr.count * sizeof(mca_btl_base_segment_t);
                frag->iov_cnt++;
                goto repeat;
            } else if (frag->iov_idx == 2) {
                for( i = 0; i < frag->hdr.count; i++ ) {
                    frag->iov[i+2].iov_base = (IOVBASE_TYPE*)ompi_ptr_ltop(frag->segments[i].seg_addr.lval);
                    frag->iov[i+2].iov_len = frag->segments[i].seg_len;
                }
                frag->iov_cnt += frag->hdr.count;
                goto repeat;
            }
            break;
        case MCA_BTL_TCP_HDR_TYPE_GET:
        default:
            break;
        }
        return true;
    }
    return false;
}

