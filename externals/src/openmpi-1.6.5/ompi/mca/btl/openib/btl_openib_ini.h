/*
 * Copyright (c) 2006-2013 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2008      Mellanox Technologies. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * @file
 */

#ifndef MCA_PTL_IB_PARAMS_H
#define MCA_PTL_IB_PARAMS_H

#include "btl_openib.h"


/*
 * Struct to hold the settable values that may be specified in the INI
 * file
 */
typedef struct ompi_btl_openib_ini_values_t {
    uint32_t mtu;
    bool mtu_set;

    uint32_t use_eager_rdma;
    bool use_eager_rdma_set;

    char *receive_queues;

    int32_t max_inline_data;
    bool max_inline_data_set;

    bool rdmacm_reject_causes_connect_error;
    bool rdmacm_reject_causes_connect_error_set;

    bool ignore_device;
    bool ignore_device_set;
} ompi_btl_openib_ini_values_t;


BEGIN_C_DECLS

    /**
     * Read in the INI files containing device params
     */
    int ompi_btl_openib_ini_init(void);

    /**
     * Query the read-in params for a given device
     */
    int ompi_btl_openib_ini_query(uint32_t vendor_id,
                                  uint32_t vendor_part_id,
                                  ompi_btl_openib_ini_values_t *values);

    /**
     * Shut down / release all internal state
     */
    int ompi_btl_openib_ini_finalize(void);

    /**
     * string to int convertors with dec/hex autodetection
     */
    int ompi_btl_openib_ini_intify(char *string);
    int ompi_btl_openib_ini_intify_list(char *str, uint32_t **values, int *len);

END_C_DECLS

#endif
