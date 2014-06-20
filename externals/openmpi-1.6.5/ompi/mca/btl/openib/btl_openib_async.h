/*
 * Copyright (c) 2007-2008 Mellanox Technologies. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * @file
 */

#ifndef MCA_BTL_OPENIB_ASYNC_H
#define MCA_BTL_OPENIB_ASYNC_H
#include "btl_openib_endpoint.h"

void*      btl_openib_async_thread(void *one_hca);
void       mca_btl_openib_load_apm(struct ibv_qp *qp, mca_btl_openib_endpoint_t *ep);
int        btl_openib_async_command_done(int exp);
#if HAVE_XRC
void       mca_btl_openib_load_apm_xrc_rcv(uint32_t qp_num, mca_btl_openib_endpoint_t *ep);
#endif

#define APM_ENABLED (0 != mca_btl_openib_component.apm_lmc || 0 != mca_btl_openib_component.apm_ports)

#endif
