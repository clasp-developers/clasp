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
 * Copyright (c) 2006-2007 Voltaire All rights reserved.
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "opal/mca/base/mca_base_param.h"
#include "opal/util/output.h"


#include "ompi/mca/btl/btl.h"
#include "ompi/mca/btl/base/base.h"


int mca_btl_base_param_register(mca_base_component_t *version,
        mca_btl_base_module_t *module)
{
    int value, err = 0;
    char *msg;

#define REG_INT(N, H, D, L, T) \
    mca_base_param_reg_int(version, N, H, false, false, D, &value); \
    if(value < (L)) \
        err = -1; \
    else \
        D = (T)value;

    REG_INT("exclusivity", "BTL exclusivity (must be >= 0)",
            module->btl_exclusivity, 0, uint32_t);

    asprintf(&msg, "BTL bit flags (general flags: SEND=%d, PUT=%d, GET=%d, SEND_INPLACE=%d, RDMA_MATCHED=%d, HETEROGENEOUS_RDMA=%d; flags only used by the \"dr\" PML (ignored by others): ACK=%d, CHECKSUM=%d, RDMA_COMPLETION=%d; flags only used by the \"bfo\" PML (ignored by others): FAILOVER_SUPPORT=%d)",
             MCA_BTL_FLAGS_SEND,
             MCA_BTL_FLAGS_PUT,
             MCA_BTL_FLAGS_GET,
             MCA_BTL_FLAGS_SEND_INPLACE,
             MCA_BTL_FLAGS_RDMA_MATCHED,
             MCA_BTL_FLAGS_HETEROGENEOUS_RDMA,
             MCA_BTL_FLAGS_NEED_ACK,
             MCA_BTL_FLAGS_NEED_CSUM,
             MCA_BTL_FLAGS_RDMA_COMPLETION,
             MCA_BTL_FLAGS_FAILOVER_SUPPORT);
    REG_INT("flags", msg,
            module->btl_flags,
            0, uint32_t);
    free(msg);

    REG_INT("rndv_eager_limit", "Size (in bytes) of \"phase 1\" fragment sent for all large messages (must be >= 0 and <= eager_limit)",
            module->btl_rndv_eager_limit, 0, size_t);

    REG_INT("eager_limit", "Maximum size (in bytes) of \"short\" messages (must be >= 1).",
            module->btl_eager_limit, 1, size_t);

    REG_INT("max_send_size", "Maximum size (in bytes) of a single \"phase 2\" fragment of a long message when using the pipeline protocol (must be >= 1)",
             module->btl_max_send_size, 1, size_t);

    if(module->btl_flags & MCA_BTL_FLAGS_PUT) {
        /* Obsolete synonym for rdma_pipeline_send_length -- no help
           message needed because it's a "hidden" parameter. */
        mca_base_param_reg_int(version, "min_rdma_size", "", true, false,
                0, &value);
        if(value != 0) {
            opal_output(0, "min_rdma_size BTL parameter is deprecated. Please "
                   "use the rdma_pipeline_send_length BTL parameter instead\n");
            module->btl_rdma_pipeline_send_length = (size_t)value;
        }

        REG_INT("rdma_pipeline_send_length", "Length of the \"phase 2\" portion of a large message (in bytes) when using the pipeline protocol.  This part of the message will be split into fragments of size max_send_size and sent using send/receive semantics (must be >= 0; only relevant when the PUT flag is set)",
                module->btl_rdma_pipeline_send_length, 0, size_t);

        /* Obsolete synonym for rdma_pipeline_frag_size -- no help
           message needed because it's a "hidden" parameter. */
        mca_base_param_reg_int(version, "max_rdma_size", "", true, false,
                               0, &value);
        if (0 != value) {
            opal_output(0, "The max_rdma_size BTL parameter is deprecated.  Please use the rdma_pipeline_frag_size BTL parameter instead");
            module->btl_rdma_pipeline_frag_size = (size_t)value;
        }

        REG_INT("rdma_pipeline_frag_size", "Maximum size (in bytes) of a single \"phase 3\" fragment from a long message when using the pipeline protocol.  These fragments will be sent using RDMA semantics (must be >= 1; only relevant when the PUT flag is set)",
                module->btl_rdma_pipeline_frag_size, 1, size_t);

        REG_INT("min_rdma_pipeline_size", "Messages smaller than this size (in bytes) will not use the RDMA pipeline protocol.  Instead, they will be split into fragments of max_send_size and sent using send/receive semantics (must be >=0, and is automatically adjusted up to at least (eager_limit+btl_rdma_pipeline_send_length); only relevant when the PUT flag is set)",
                module->btl_min_rdma_pipeline_size, 0, size_t);
        if (module->btl_min_rdma_pipeline_size < 
            (module->btl_eager_limit + module->btl_rdma_pipeline_send_length)) {
            module->btl_min_rdma_pipeline_size = 
                module->btl_eager_limit + module->btl_rdma_pipeline_send_length;
        }
    }

    REG_INT("bandwidth", "Approximate maximum bandwidth of interconnect"
            "(0 = auto-detect value at run-time [not supported in all BTL modules], >= 1 = bandwidth in Mbps)", module->btl_bandwidth, 0, uint32_t);

    REG_INT("latency", "Approximate latency of interconnect (must be >= 0)",
            module->btl_latency, 0, uint32_t);

    return err;
}
