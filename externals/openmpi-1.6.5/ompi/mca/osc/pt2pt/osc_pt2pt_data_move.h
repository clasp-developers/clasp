/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#ifndef OMPI_MCA_OSC_PT2PT_DATA_MOVE_H
#define OMPI_MCA_OSC_PT2PT_DATA_MOVE_H

#include "osc_pt2pt_sendreq.h"
#include "osc_pt2pt_replyreq.h"

/* send a sendreq (the request from the origin for a Put, Get, or
   Accumulate, including the payload for Put and Accumulate) */
int ompi_osc_pt2pt_sendreq_send(ompi_osc_pt2pt_module_t *module,
                                ompi_osc_pt2pt_sendreq_t *sendreq);

/* send a replyreq (the request from the target of a Get, with the
   payload for the origin */
int ompi_osc_pt2pt_replyreq_send(ompi_osc_pt2pt_module_t *module,
                                 ompi_osc_pt2pt_replyreq_t *replyreq);

/* receive the target side of a sendreq for a put, directly into the user's window */
int ompi_osc_pt2pt_sendreq_recv_put(ompi_osc_pt2pt_module_t *module,
                                     ompi_osc_pt2pt_send_header_t *header,
                                     void *payload);

/* receive the target side of a sendreq for an accumulate, possibly
   using a temproart buffer, then calling the reduction functions */
int ompi_osc_pt2pt_sendreq_recv_accum(ompi_osc_pt2pt_module_t *module,
                                      ompi_osc_pt2pt_send_header_t *header,
                                      void *payload);

/* receive the origin side of a replyreq (the reply part of an
   MPI_Get), directly into the user's window */
int ompi_osc_pt2pt_replyreq_recv(ompi_osc_pt2pt_module_t *module,
                                 ompi_osc_pt2pt_sendreq_t *sendreq,
                                 ompi_osc_pt2pt_reply_header_t *header,
                                 void *payload);

int ompi_osc_pt2pt_control_send(ompi_osc_pt2pt_module_t *module,
                                ompi_proc_t *proc,
                                uint8_t type, int32_t value0, int32_t value1);

#endif
