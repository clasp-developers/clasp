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

#ifndef BTL_SCTP_RECV_HANDLER_H
#define BTL_SCTP_RECV_HANDLER_H

void mca_btl_sctp_print_sri(struct sctp_sndrcvinfo *sri);
void mca_btl_sctp_recv_handler(int sd, short flags, void *user);
int mca_btl_sctp_recv_handler_initbuf(void);
void mca_btl_sctp_recv_handler_freebuf(void);

#endif
