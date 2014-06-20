/*
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * @file
 * Functions called by BTL to handle error events
 */

#ifndef MCA_BTL_IB_FAILOVER_H
#define MCA_BTL_IB_FAILOVER_H

BEGIN_C_DECLS

void mca_btl_openib_handle_endpoint_error(mca_btl_openib_module_t *openib_btl,
					  mca_btl_base_descriptor_t *des,
					  int qp,
					  ompi_proc_t* remote_proc,
					  mca_btl_openib_endpoint_t* endpoint);
void mca_btl_openib_handle_btl_error(mca_btl_openib_module_t* openib_btl);
void btl_openib_handle_failover_control_messages(mca_btl_openib_control_header_t *ctl_hdr,
                                                 mca_btl_openib_endpoint_t* ep);

END_C_DECLS

#endif
