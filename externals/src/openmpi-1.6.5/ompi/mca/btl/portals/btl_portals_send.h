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

#ifndef OMPI_BTL_PORTALS_SEND_H
#define OMPI_BTL_PORTALS_SEND_H

#include "btl_portals_frag.h"


#define MCA_BTL_PORTALS_PROGRESS_QUEUED_SENDS() \
    if ((0 != opal_list_get_size(&(mca_btl_portals_module.portals_queued_sends))) && \
        (mca_btl_portals_module.portals_outstanding_ops <                            \
         mca_btl_portals_module.portals_max_outstanding_ops)) {                      \
        mca_btl_portals_frag_t *qfrag = (mca_btl_portals_frag_t*)                    \
            opal_list_remove_first(&(mca_btl_portals_module.portals_queued_sends));  \
        OPAL_OUTPUT_VERBOSE((90, mca_btl_portals_component.portals_output,           \
                             "retransmit for frag 0x%lx, 0x%lx",                     \
                             (unsigned long) qfrag,                                  \
                             (unsigned long) qfrag->base.des_cbfunc));               \
        return mca_btl_portals_send(&mca_btl_portals_module.super,                   \
                                    qfrag->endpoint,                                 \
                                    &(qfrag->base),                                  \
                                    qfrag->hdr.tag);                                 \
    }                                                                                \
    return OMPI_SUCCESS;


#endif /* OMPI_BTL_PORTALS_SEND_H */
