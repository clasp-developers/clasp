/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "osc_pt2pt_replyreq.h"

#include "opal/class/opal_list.h"
#include "opal/datatype/opal_convertor.h"

int
ompi_osc_pt2pt_replyreq_alloc_init(ompi_osc_pt2pt_module_t *module,
                                   int origin,
                                   ompi_ptr_t origin_request,
                                   OPAL_PTRDIFF_TYPE target_displacement,
                                   int target_count,
                                   struct ompi_datatype_t *datatype,
                                   ompi_osc_pt2pt_replyreq_t **replyreq)
{
    int ret;
    void *target_addr = (unsigned char*) module->p2p_win->w_baseptr + 
        (target_displacement * module->p2p_win->w_disp_unit);    


    /* allocate a replyreq */
    ret = ompi_osc_pt2pt_replyreq_alloc(module, 
                                     origin,
                                     replyreq);
    if (OMPI_SUCCESS != ret) return ret;

    /* initialize local side of replyreq */
    ret = ompi_osc_pt2pt_replyreq_init_target(*replyreq,
                                           target_addr,
                                           target_count,
                                           datatype);
    if (OMPI_SUCCESS != ret) {
        ompi_osc_pt2pt_replyreq_free(*replyreq);
        return ret;
    }

    /* initialize remote side of replyreq */
    ret = ompi_osc_pt2pt_replyreq_init_origin(*replyreq,
                                           origin_request);
    if (OMPI_SUCCESS != ret) {
        ompi_osc_pt2pt_replyreq_free(*replyreq);
        return ret;
    }

    return OMPI_SUCCESS;
}


static void ompi_osc_pt2pt_replyreq_construct(ompi_osc_pt2pt_replyreq_t *replyreq)
{
    OBJ_CONSTRUCT(&(replyreq->rep_target_convertor), opal_convertor_t);
}

static void ompi_osc_pt2pt_replyreq_destruct(ompi_osc_pt2pt_replyreq_t *replyreq)
{
    OBJ_DESTRUCT(&(replyreq->rep_target_convertor));
}


OBJ_CLASS_INSTANCE(ompi_osc_pt2pt_replyreq_t, opal_list_item_t,
                   ompi_osc_pt2pt_replyreq_construct, 
                   ompi_osc_pt2pt_replyreq_destruct);
