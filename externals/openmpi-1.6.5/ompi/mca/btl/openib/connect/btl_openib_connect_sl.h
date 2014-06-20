/*
 * Copyright (c) 2011      Mellanox Technologies.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef BTL_OPENIB_CONNECT_SL_H
#define BTL_OPENIB_CONNECT_SL_H

BEGIN_C_DECLS

int btl_openib_connect_get_pathrecord_sl(
                struct ibv_context *context_arg,
                uint32_t port_num,
                uint16_t lid,
                uint16_t rem_lid);

void btl_openib_connect_sl_finalize(void);

END_C_DECLS

#endif /* BTL_OPENIB_CONNECT_SL_H */
