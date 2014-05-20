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

#ifndef OSC_PT2PT_LONGREQ_H
#define OSC_PT2PT_LONGREQ_H

#include "osc_pt2pt.h"
#include "osc_pt2pt_mpireq.h"

struct ompi_osc_pt2pt_longreq_t {
    ompi_osc_pt2pt_mpireq_t mpireq;

    /* warning - this doesn't always have a sane value */
    ompi_osc_pt2pt_module_t *req_module;

    struct ompi_op_t *req_op;
    struct ompi_datatype_t *req_datatype;
};
typedef struct ompi_osc_pt2pt_longreq_t ompi_osc_pt2pt_longreq_t;
OBJ_CLASS_DECLARATION(ompi_osc_pt2pt_longreq_t);

static inline int
ompi_osc_pt2pt_longreq_alloc(ompi_osc_pt2pt_longreq_t **longreq)
{
    opal_free_list_item_t *item;
    int ret;

    OPAL_FREE_LIST_GET(&mca_osc_pt2pt_component.p2p_c_longreqs,
                       item, ret);

    *longreq = (ompi_osc_pt2pt_longreq_t*) item;
    return ret;
}

static inline int
ompi_osc_pt2pt_longreq_free(ompi_osc_pt2pt_longreq_t *longreq)
{
    OPAL_FREE_LIST_RETURN(&mca_osc_pt2pt_component.p2p_c_longreqs,
                          &longreq->mpireq.super.super);
    return OMPI_SUCCESS;
}

#endif
