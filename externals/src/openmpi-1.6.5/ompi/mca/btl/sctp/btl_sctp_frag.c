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
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
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
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */

#include "opal/opal_socket_errno.h"
#include "opal/include/opal_stdint.h"
#include "ompi/mca/btl/base/btl_base_error.h"
#include "btl_sctp_frag.h" 
#include "btl_sctp_endpoint.h"

#include "btl_sctp.h"
#include "btl_sctp_addr.h"
#include "btl_sctp_utils.h"
#include <sys/socket.h>
#include <netinet/sctp.h>

static void mca_btl_sctp_frag_common_constructor(mca_btl_sctp_frag_t* frag) 
{ 
    frag->base.des_src = NULL;
    frag->base.des_src_cnt = 0;
    frag->base.des_dst = NULL;
    frag->base.des_dst_cnt = 0;
}

static void mca_btl_sctp_frag_eager_constructor(mca_btl_sctp_frag_t* frag) 
{ 
    frag->size = mca_btl_sctp_module.super.btl_eager_limit;   
    frag->my_list = &mca_btl_sctp_component.sctp_frag_eager;
    mca_btl_sctp_frag_common_constructor(frag); 
}

static void mca_btl_sctp_frag_max_constructor(mca_btl_sctp_frag_t* frag) 
{ 
    frag->size = mca_btl_sctp_module.super.btl_max_send_size; 
    frag->my_list = &mca_btl_sctp_component.sctp_frag_max;
    mca_btl_sctp_frag_common_constructor(frag); 
}

static void mca_btl_sctp_frag_user_constructor(mca_btl_sctp_frag_t* frag) 
{ 
    frag->size = 0; 
    frag->my_list = &mca_btl_sctp_component.sctp_frag_user;
    mca_btl_sctp_frag_common_constructor(frag); 
}


OBJ_CLASS_INSTANCE(
        mca_btl_sctp_frag_t, 
        mca_btl_base_descriptor_t, 
        NULL, 
        NULL); 

OBJ_CLASS_INSTANCE(
        mca_btl_sctp_frag_eager_t, 
        mca_btl_base_descriptor_t, 
        mca_btl_sctp_frag_eager_constructor, 
        NULL); 

OBJ_CLASS_INSTANCE(
        mca_btl_sctp_frag_max_t, 
        mca_btl_base_descriptor_t, 
        mca_btl_sctp_frag_max_constructor, 
        NULL); 

OBJ_CLASS_INSTANCE(
        mca_btl_sctp_frag_user_t, 
        mca_btl_base_descriptor_t, 
        mca_btl_sctp_frag_user_constructor, 
        NULL); 



/**
 * int mca_btl_sctp_frag_get_msg_size(mca_btl_sctp_frag_t *frag)
 * -------------------------------------------
 *  Returns the full size of the message to send (stored in the iov array)
 *  including all header information.
 */

int mca_btl_sctp_frag_get_msg_size(mca_btl_sctp_frag_t *frag) {
    int count;
    int size = 0;

    for(count = 0; count < (int) frag->iov_cnt; count++) {
        size += frag->iov_ptr[count].iov_len;
    }

    return size;
}



/**
 * mca_btl_sctp_frag_large_send(mca_btl_sctp_frag_t* frag, int sd)
 * ---------------------------------------------------------------
 *  Send a frag that is too large to send in one call to a vector write.
 */

bool mca_btl_sctp_frag_large_send(mca_btl_sctp_frag_t* frag, int sd, int iov_fragment, int *amt_sent) {
    int done = 0;
    int count_down = 0;
    int cnt = -1;
    int data_sent = 0;
    int to_send;
    struct sockaddr_in btl_sockaddr;

    *amt_sent = 0;

    /* Determine full size of message that needs to be sent. */
    count_down = mca_btl_sctp_frag_get_msg_size(frag);
        
    /* Setup addressing information. */
    btl_sockaddr = mca_btl_sctp_utils_sockaddr_from_frag(frag);

    while(!done) {
        if(frag->iov_ptr->iov_len == 0) {
            /* Just used to jump over the 3rd empty iovec in the array. Open MPI
             * sets up their message as shown above and so this is added for
             * potential compatibility? 
             */
            frag->iov_ptr++;
        }

        if(frag->iov_ptr->iov_len <= MCA_BTL_SCTP_MAX_FRAG_SIZE) {
            to_send = frag->iov_ptr->iov_len;
        } else { /* iov_ptr->iov_len > MCA_BTL_SCTP_MAX_FRAG_SIZE */
            to_send = MCA_BTL_SCTP_MAX_FRAG_SIZE;
        }
            
        if(mca_btl_sctp_component.sctp_if_11) {
            cnt = sctp_sendmsg(sd, frag->iov_ptr->iov_base, to_send, 0, 0, 0, 0, 0, 0, 0 );
        } else {
            cnt = sctp_sendmsg(sd, frag->iov_ptr->iov_base, to_send, (struct sockaddr *)&btl_sockaddr, 
                    sizeof(btl_sockaddr), 0, 0, 0, 0, 0 );
        }
                                   
                    
        if(cnt >= 0) {
            SCTP_BTL_ERROR(("mca_btl_sctp_frag_large_send() sent %d bytes.\n",cnt));
        } else {
            /* cnt < 0  */
            switch(opal_socket_errno) {
            case EINTR:
            case EWOULDBLOCK:
                if(data_sent) {SCTP_BTL_ERROR(("leaving large_send (data_sent = %d)\n",data_sent));}
                cnt=0;
                break;
            case EFAULT:
                BTL_ERROR(("sctp_sendmsg error (%p, %" PRIsize_t ")\n\t%s(%" PRIsize_t ")\n",
                           frag->iov_ptr[0].iov_base, frag->iov_ptr[0].iov_len,
                           strerror(opal_socket_errno), frag->iov_cnt));
            default:
                {
                    BTL_ERROR(("sctp_sendmsg failed with errno=%d", opal_socket_errno));
                    mca_btl_sctp_endpoint_close(frag->endpoint);
                    return false;
                }
            }
        }

        if(cnt > 0) {
            /* update frag book-keeping with each iteration */

            /* SCTP sends all or nothing */
            assert(to_send == cnt);
                
            data_sent += cnt;
            *amt_sent = data_sent;
            if(frag->iov_ptr->iov_len <= MCA_BTL_SCTP_MAX_FRAG_SIZE)
            {
                /* completed sending this vector element */
                    
                assert(cnt == (int) frag->iov_ptr->iov_len);
                    
                frag->iov_ptr++;
                frag->iov_idx++;
                frag->iov_cnt--;                    
            }
            else /* iov_ptr->iov_len > MCA_BTL_SCTP_MAX_FRAG_SIZE */
            {
                /* sent only a portion of this vector element */
                    
                assert(cnt < (int) frag->iov_ptr->iov_len);
                assert(cnt == MCA_BTL_SCTP_MAX_FRAG_SIZE);
                
                frag->iov_ptr->iov_base = (ompi_iov_base_ptr_t)
                    (((unsigned char*)frag->iov_ptr->iov_base) + cnt);
                frag->iov_ptr->iov_len -= cnt;                    
            }
        }
        if(cnt == 0) { /* just in case nothing can be sent, we'll go back to the
                      *  progress function */
            return false;
        }
        if(data_sent == count_down) {
            /* If data_sent == count_down then we're done. */
            done = 1;
        }
    }

    return (done == 1);
}


/**
 * mca_btl_sctp_frag_send(mca_btl_sctp_frag_t* frag, int sd)
 * ---------------------------------------------------------------
 *  Send a message frag.
 */

bool mca_btl_sctp_frag_send(mca_btl_sctp_frag_t* frag, int sd)
{
    int zero=0,cnt=-1;
    size_t i, num_vecs;
    int large_message_send_return;
    int large_vector;

    /* Check each iov_len field in frag.iov[] and see is any of them are
     * above 64K.
     */

    size_t count;
    int large_msg = 0;

    for(count = 0; count < frag->iov_cnt; count++) {
        zero += frag->iov_ptr[count].iov_len;

        /* True if we have a message that is too long to send in one shot via
         * SCTP.
         */
        if(frag->iov_ptr[count].iov_len > MCA_BTL_SCTP_MAX_FRAG_SIZE) {
            large_msg = 1;  /* Set large message flag to true. */
            large_vector = count;
            break;
        }
    }

    /* if only an empty iov element remains, let it fall
     *  through in order to decrement the count */
    if(0 == zero) {
        cnt = 0; /* don't try to send */
    }
            


    /* non-blocking write, but continue if interrupted */
    if(large_msg) {
        large_message_send_return = mca_btl_sctp_frag_large_send(frag, sd, large_vector, &cnt);
    }
    else if(!large_msg) {
        /* Setup addressing information. */
        socklen_t len;
        struct sockaddr_in btl_sockaddr;
        btl_sockaddr = mca_btl_sctp_utils_sockaddr_from_frag(frag);
        len = sizeof(struct sockaddr_in);

        while(cnt < 0) {
            if(mca_btl_sctp_component.sctp_if_11) {
                cnt = mca_btl_sctp_utils_writev(sd, frag->iov_ptr, frag->iov_cnt, 0, 0, 0);
            } else {
                cnt = mca_btl_sctp_utils_writev(sd, frag->iov_ptr, frag->iov_cnt, (struct sockaddr *)&btl_sockaddr, len, 0);
            }
                    
            if(cnt >= 0) {
                SCTP_BTL_ERROR(("mca_btl_sctp_frag_send() sd=%d, sent %d bytes.\n",sd, cnt));
            } else {
                /* cnt < 0  */
                switch(opal_socket_errno) {
                case EINTR:
                case EWOULDBLOCK:
                    return false;
                case EFAULT:
                    BTL_ERROR(("mca_btl_sctp_utils_writev error (%p, %" PRIsize_t ")\n\t%s(%" PRIsize_t ")\n",
                               frag->iov_ptr[0].iov_base, frag->iov_ptr[0].iov_len,
                               strerror(opal_socket_errno), frag->iov_cnt));
                default:
                    {
                        BTL_ERROR(("mca_btl_sctp_utils_writev failed with errno=%d", opal_socket_errno));
                        mca_btl_sctp_endpoint_close(frag->endpoint);
                        return false;
                    }
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
        
    }
        
    return (frag->iov_cnt == 0);        
}


/**
 * bool mca_btl_sctp_frag_recv(mca_btl_sctp_frag_t *frag, int sd, char *buf, int len)
 * ----------------------------------------------------------------------------------
 *  Recv message frag.
 */

bool mca_btl_sctp_frag_recv(mca_btl_sctp_frag_t* frag, int sd, char *buf, int len)
{
    if(mca_btl_sctp_component.sctp_if_11) {
        /* 1 to 1 */
        int cnt;
        size_t i, num_vecs;
        mca_btl_base_endpoint_t* btl_endpoint = frag->endpoint;

repeat11:
        num_vecs = frag->iov_cnt;
#if MCA_BTL_SCTP_ENDPOINT_CACHE
        if( 0 != btl_endpoint->endpoint_cache_length ) {
            size_t length = btl_endpoint->endpoint_cache_length;
            /* It's strange at the first look but cnt have to be set to the full amount of data available.
             * After going to advance_iov_position11 we will use cnt to detect if there is still some
             * data pending.
             */
            cnt = btl_endpoint->endpoint_cache_length;
            for( i = 0; i < frag->iov_cnt; i++ ) {
                if( length > frag->iov_ptr[i].iov_len )
                    length = frag->iov_ptr[0].iov_len;
                memcpy( frag->iov_ptr[i].iov_base, btl_endpoint->endpoint_cache_pos, length );
                btl_endpoint->endpoint_cache_pos += length;
                btl_endpoint->endpoint_cache_length -= length;
                length = btl_endpoint->endpoint_cache_length;
                if( 0 == length ) {
                    btl_endpoint->endpoint_cache_pos = btl_endpoint->endpoint_cache;
                    break;
                }
            }
            goto advance_iov_position11;
        }
        /* What's happens if all iovecs are used by the fragment ? It still work, as we reserve one
         * iovec for the caching in the fragment structure (the +1).
         */
        frag->iov_ptr[num_vecs].iov_base = btl_endpoint->endpoint_cache;
        frag->iov_ptr[num_vecs].iov_len  = mca_btl_sctp_component.sctp_endpoint_cache;
        num_vecs++;
#endif  /* MCA_BTL_SCTP_ENDPOINT_CACHE */

        /* non-blocking read, but continue if interrupted */
        cnt = -1;
        while( cnt < 0 ) {
            cnt = readv(sd, frag->iov_ptr, num_vecs);
            if(cnt >= 0) {SCTP_BTL_ERROR(("readv (sd=%d) %d bytes\n", sd, cnt));}
            if(cnt < 0) {
                switch(opal_socket_errno) {
                    case EINTR:
                        continue;
                    case ECONNRESET:
                    case EBADF:
                        close(sd);
                    case EWOULDBLOCK:
                        return false;
                    case EFAULT:
                        opal_output( 0, "mca_btl_sctp_frag_recv: readv error (%p, %d)\n\t%s(%d)\n",
                                frag->iov_ptr[0].iov_base, (int) frag->iov_ptr[0].iov_len,
                                strerror(opal_socket_errno), (int) frag->iov_cnt );
                    default:
                        opal_output(0, "mca_btl_sctp_frag_recv: readv failed with errno=%d",
                                opal_socket_errno);
                        mca_btl_sctp_endpoint_close(btl_endpoint);
                        return false;
                }
            }
            if( cnt == 0 ) {
                return false;
            }
            goto advance_iov_position11;
        };

advance_iov_position11:
        /* if the write didn't complete - update the iovec state */
        num_vecs = frag->iov_cnt;
        for( i = 0; i < num_vecs; i++ ) {
            if( cnt >= (int)frag->iov_ptr->iov_len ) {
                cnt -= frag->iov_ptr->iov_len;
                frag->iov_idx++;
                frag->iov_ptr++;
                frag->iov_cnt--;
            } else {
                frag->iov_ptr->iov_base = (ompi_iov_base_ptr_t)
                    (((unsigned char*)frag->iov_ptr->iov_base) + cnt);
                frag->iov_ptr->iov_len -= cnt;
                cnt = 0;
                break;
            }
        }

#if MCA_BTL_SCTP_ENDPOINT_CACHE
        btl_endpoint->endpoint_cache_length = cnt;
#endif  /* MCA_BTL_SCTP_ENDPOINT_CACHE */

        /* read header */
        if(frag->iov_cnt == 0) {
            if (btl_endpoint->endpoint_nbo) {
                MCA_BTL_SCTP_HDR_NTOH(frag->hdr);
            }
            switch(frag->hdr.type) {
                case MCA_BTL_SCTP_HDR_TYPE_SEND:
                    if(frag->iov_idx == 1 && frag->hdr.size) {
                        frag->iov[1].iov_base = (IOVBASE_TYPE*)(frag+1);
                        frag->iov[1].iov_len = frag->hdr.size;
                        frag->segments[0].seg_addr.pval = frag+1;
                        frag->segments[0].seg_len = frag->hdr.size;
                        frag->iov_cnt++;
                        goto repeat11;
                    }
                    break;
                case MCA_BTL_SCTP_HDR_TYPE_PUT:
                    if(frag->iov_idx == 1) {
                        frag->iov[1].iov_base = (IOVBASE_TYPE*)frag->segments;
                        frag->iov[1].iov_len = frag->hdr.count * sizeof(mca_btl_base_segment_t);
                        frag->iov_cnt++;
                        goto repeat11;
                    } else if (frag->iov_idx == 2) {
                        for(i=0; i<frag->hdr.count; i++) {
                            frag->iov[i+2].iov_base = (IOVBASE_TYPE*)ompi_ptr_ltop(frag->segments[i].seg_addr.lval);
                            frag->iov[i+2].iov_len = frag->segments[i].seg_len;
                            frag->iov_cnt++;
                        }
                        goto repeat11;
                    }
                    break;
                case MCA_BTL_SCTP_HDR_TYPE_GET:
                default:
                    break;
            }
            return true;
        }
        return false;
    }

    else {
        int cnt;
        size_t i, num_vecs;
        mca_btl_base_endpoint_t* btl_endpoint = frag->endpoint;

        /* Ugly way of getting my own macro in to jump back into the recv_handler
         * and progress engine so that I can do my next read from the socket. 
         */
        int done = 0;   

repeat:
        /* In other words, we've packed the frag with the data from buf and need
         * to return to the recv_handler and the subsequent progress engine to
         * get another piece of data... hence the notion of 'done.' */
        if(done) {
            goto ret_false;
        }

        num_vecs = frag->iov_cnt;
#if MCA_BTL_SCTP_ENDPOINT_CACHE
        if( 0 != btl_endpoint->endpoint_cache_length ) {
            size_t length = btl_endpoint->endpoint_cache_length;
            /* It's strange at the first look but cnt have to be set to the full amount of data available.
             * After going to advance_iov_position we will use cnt to detect if there is still some
             * data pending.
             */
            cnt = btl_endpoint->endpoint_cache_length;
            for( i = 0; i < frag->iov_cnt; i++ ) {
                if( length > frag->iov_ptr[i].iov_len )
                    length = frag->iov_ptr[0].iov_len;
                memcpy( frag->iov_ptr[i].iov_base, btl_endpoint->endpoint_cache_pos, length );
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

        frag->iov_ptr[num_vecs].iov_base = btl_endpoint->endpoint_cache;
        frag->iov_ptr[num_vecs].iov_len  = mca_btl_sctp_component.sctp_endpoint_cache;
        num_vecs++;
#endif  /* MCA_BTL_SCTP_ENDPOINT_CACHE */

        /* non-blocking read, but continue if interrupted */
        cnt = -1;
        while( cnt < 0 ) {

            /* Replaces the traditional readv() of the endpoint_recv_handler. */
            memcpy(frag->iov_ptr->iov_base, buf, len);
            cnt = len;

            if(cnt < 0) {
                /* TODO move full error handling code to recv_handler */
                /* never happens. len would have to passed in -1... */
                /* ...plus I don't think the errno from the sctp_recvmsg
                 *  will percolate this far (reset at other syscalls)!
                 */
                switch(opal_socket_errno) {
                    case EINTR:
                    case EWOULDBLOCK:
                        return false;
                    case EFAULT:
                        opal_output( 0, "mca_btl_sctp_frag_recv: error (%p, %d)\n\t%s(%d)\n",
                                frag->iov_ptr[0].iov_base, (int) frag->iov_ptr[0].iov_len,
                                strerror(opal_socket_errno), (int) frag->iov_cnt );
                    default:
                        opal_output(0, "mca_btl_sctp_frag_recv: failed with errno=%d",
                                opal_socket_errno);
                        mca_btl_sctp_endpoint_close(btl_endpoint);
                        return false;
                }
            }
            if( cnt == 0 ) {
                return false;
            }
            goto advance_iov_position;
        };

advance_iov_position:
        /* if the write didn't complete - update the iovec state */
        num_vecs = frag->iov_cnt;
        for( i = 0; i < num_vecs; i++ ) {
            if( cnt >= (int)frag->iov_ptr->iov_len ) {
                cnt -= frag->iov_ptr->iov_len;
                frag->iov_idx++;
                frag->iov_ptr++;
                frag->iov_cnt--;
            } else {
                frag->iov_ptr->iov_base = (ompi_iov_base_ptr_t)
                    (((unsigned char*)frag->iov_ptr->iov_base) + cnt);
                frag->iov_ptr->iov_len -= cnt;
                cnt = 0;
                break;
            }
        }

ret_false:
        /* NOT SURE IF I NEED THIS BLOCK... */
        /* Further... the reason I do an 'if(done)' check here is that the code
         * beneath gets executed along with the 'goto advance_iov_position' which
         * is hit elsewhere.
         */
        if(done) {
            frag->iov_ptr->iov_base = (ompi_iov_base_ptr_t)
                (((unsigned char*)frag->iov_ptr->iov_base) + cnt);
            frag->iov_ptr->iov_len -= cnt;
            cnt = 0;
        }
        /* ...UP TO HERE. */
#if MCA_BTL_SCTP_ENDPOINT_CACHE
        btl_endpoint->endpoint_cache_length = cnt;
#endif  /* MCA_BTL_SCTP_ENDPOINT_CACHE */

        /* read header */
        if(frag->iov_cnt == 0) {
            if (btl_endpoint->endpoint_nbo) {
                MCA_BTL_SCTP_HDR_NTOH(frag->hdr);
            }
            switch(frag->hdr.type) {
                case MCA_BTL_SCTP_HDR_TYPE_SEND:
                    if(frag->iov_idx == 1 && frag->hdr.size) {
                        frag->iov[1].iov_base = (IOVBASE_TYPE*)(frag+1);
                        frag->iov[1].iov_len = frag->hdr.size;
                        frag->segments[0].seg_addr.pval = frag+1;
                        frag->segments[0].seg_len = frag->hdr.size;
                        frag->iov_cnt++;
                        done = 1;
                        goto repeat;
                    }
                    break;
                case MCA_BTL_SCTP_HDR_TYPE_PUT:
                    if(frag->iov_idx == 1) {
                        frag->iov[1].iov_base = (IOVBASE_TYPE*)frag->segments;
                        frag->iov[1].iov_len = frag->hdr.count * sizeof(mca_btl_base_segment_t);
                        frag->iov_cnt++;
                        done = 1;
                        goto repeat;
                    } else if (frag->iov_idx == 2) {
                        for(i=0; i<frag->hdr.count; i++) {
                            frag->iov[i+2].iov_base = (IOVBASE_TYPE*)ompi_ptr_ltop(frag->segments[i].seg_addr.lval);
                            frag->iov[i+2].iov_len = frag->segments[i].seg_len;
                            frag->iov_cnt++;
                        }
                        done = 1;
                        goto repeat;
                    }
                    break;
                case MCA_BTL_SCTP_HDR_TYPE_GET:
                default:
                    break;
            }
            return true;
        }
        return false;
    }
}
