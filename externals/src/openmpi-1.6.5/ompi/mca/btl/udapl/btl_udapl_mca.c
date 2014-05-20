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
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2007-2008 Sun Microsystems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "opal/mca/base/mca_base_param.h"
#include "btl_udapl.h"
#include "btl_udapl_mca.h"
#include <string.h>

/*
 * Utility routine for string parameter registration.
 * 
 * @param param_name (IN)        MCA parameter name
 * @param param_desc (IN)        MCA parameter description
 * @param default_value (IN)     MCA parameter default value
 * @param out_value (OUT)        value of MCA parameter; either default,
 *                                  or value as determined from typical
 *                                  MCA parameter setting methods 
 * @param flags (IN)             MCA parameter boundary flag
 * @return                       OMPI_SUCCESS or OMPI_ERR_BAD_PARAM
 */
static inline int mca_btl_udapl_reg_string(const char* param_name,
                                           const char* param_desc,
                                           const char* default_value,
                                           char **out_value, int flags)
{
    char *value;
    
    mca_base_param_reg_string(&mca_btl_udapl_component.super.btl_version, 
        param_name, param_desc, false, false, default_value, &value);

    if (NULL == value && !((flags & REGSTR_EMPTY_OK) == REGSTR_EMPTY_OK)) {
        BTL_ERROR(("ERROR: MCA Parameter %s : Value (NULL) out of range : "
            "Default value (%s)\n \t Parameter Description : %s",
            param_name, default_value, param_desc));
        return OMPI_ERR_BAD_PARAM;
    }

    if ((flags & REGSTR_EMPTY_NOT_OK) && 0 == strlen(value)) {
        BTL_ERROR(("ERROR: MCA Parameter %s : Value (%s) out of range : "
            "Default value (%s)\n \t Parameter Description : %s",
            param_name, value, default_value, param_desc));
        return OMPI_ERR_BAD_PARAM;
    }

    *out_value = value;
    return OMPI_SUCCESS;
}


/*
 * Utility routine for integer parameter registration.
 *
 * @param param_name (IN)        MCA parameter name
 * @param param_desc (IN)        MCA parameter description
 * @param default_value (IN)     MCA parameter default value
 * @param out_value (OUT)        value of MCA parameter; either default,
 *                                  or value as determined from typical
 *                                  MCA parameter setting methods 
 * @param flags (IN)             MCA parameter boundary flag
 * @return                       OMPI_SUCCESS or OMPI_ERR_BAD_PARAM
 */
static inline int mca_btl_udapl_reg_int(const char* param_name,
                                        const char* param_desc,
                                        int default_value, int *out_value,
                                        int flags)
{
    int value;

    mca_base_param_reg_int(&mca_btl_udapl_component.super.btl_version, 
        param_name, param_desc, false, false, default_value, &value);

    if ((flags & REGINT_NEG_ONE_OK) && -1 == value) {
        *out_value = value;
        return OMPI_SUCCESS;
    }
    if (((flags & REGINT_GE_ZERO) && value < 0) ||
        ((flags & REGINT_GE_ONE) && value < 1) ||
        ((flags & REGINT_NONZERO) && 0 == value)) {
        BTL_ERROR(("ERROR: MCA Parameter %s : Value (%d) out of range : "
            "Default value (%d)\n \t Parameter Description : %s\n",
            param_name, value, default_value, param_desc));
        return OMPI_ERR_BAD_PARAM;
    }
    *out_value = value;
    return OMPI_SUCCESS;
}


/*
 * Register and check all MCA parameters
 *
 * @return                 OMPI_SUCCESS or OMPI_ERR_BAD_PARAM
 */
int mca_btl_udapl_register_mca_params(void) 
{
    int ival, rc, tmp_rc;

    rc = OMPI_SUCCESS;

    /* register uDAPL component parameters */
    CHECK_PARAM_REGISTER_RETURN_VALUE(mca_btl_udapl_reg_int("free_list_num", 
        "Initial size of free lists (must be >= 1).", 
        8,
        &mca_btl_udapl_component.udapl_free_list_num,
        REGINT_GE_ONE), tmp_rc, rc);
    
    CHECK_PARAM_REGISTER_RETURN_VALUE(mca_btl_udapl_reg_int("free_list_max",
        "Maximum size of free lists "
        "(-1 = infinite, otherwise must be >= 1).",
        -1,
        &mca_btl_udapl_component.udapl_free_list_max,
        REGINT_NEG_ONE_OK | REGINT_GE_ONE), tmp_rc, rc);

    CHECK_PARAM_REGISTER_RETURN_VALUE(mca_btl_udapl_reg_int("free_list_inc", 
        "Increment size of free lists (must be >= 1).",
        8,
        &mca_btl_udapl_component.udapl_free_list_inc,
        REGINT_GE_ONE), tmp_rc, rc);

    CHECK_PARAM_REGISTER_RETURN_VALUE(mca_btl_udapl_reg_string("mpool",
        "Name of the memory pool to be used.",
        "rdma",
        &mca_btl_udapl_component.udapl_mpool_name,
        REGSTR_EMPTY_NOT_OK), tmp_rc, rc);

    CHECK_PARAM_REGISTER_RETURN_VALUE(mca_btl_udapl_reg_int("max_modules",
        "Maximum number of supported HCAs.",
        8,
        &ival,
        REGINT_GE_ONE), tmp_rc, rc);
    mca_btl_udapl_component.udapl_max_btls = (uint32_t) ival;

    CHECK_PARAM_REGISTER_RETURN_VALUE(mca_btl_udapl_reg_int("num_recvs",
        "Total number of receive buffers to keep posted "
        "per endpoint (must be >= 1).",
        8,
        &mca_btl_udapl_component.udapl_num_recvs,
        REGINT_GE_ONE), tmp_rc, rc);

    CHECK_PARAM_REGISTER_RETURN_VALUE(mca_btl_udapl_reg_int("num_sends",
        "Maximum number of sends to post on an endpoint "
        "(must be >= 1).",
        7,
        &mca_btl_udapl_component.udapl_num_sends,
        REGINT_GE_ONE), tmp_rc, rc);

    CHECK_PARAM_REGISTER_RETURN_VALUE(mca_btl_udapl_reg_int("sr_win",
        "Window size at which point an explicit "
        "credit message will be generated (must be >= 1).",
        4,
        &mca_btl_udapl_component.udapl_sr_win,
        REGINT_GE_ONE), tmp_rc, rc);

    CHECK_PARAM_REGISTER_RETURN_VALUE(mca_btl_udapl_reg_int("use_eager_rdma",
        "Use of RDMA for small messages : "
        "1 = default, use RDMA for small messages; "
        "0 = do not use RDMA for small messages. ",
        1,
        &mca_btl_udapl_component.udapl_use_eager_rdma,
        REGINT_GE_ZERO), tmp_rc, rc); 

    CHECK_PARAM_REGISTER_RETURN_VALUE(mca_btl_udapl_reg_int("eager_rdma_num",
        "Number of RDMA buffers to allocate "
        "for small messages (must be >= 1).",
        32,
        &mca_btl_udapl_component.udapl_eager_rdma_num,
        REGINT_GE_ONE), tmp_rc, rc); 
        
    CHECK_PARAM_REGISTER_RETURN_VALUE(mca_btl_udapl_reg_int("max_eager_rdma_peers",
        "Maximum number of peers allowed to use "
        "RDMA for short messages (independently RDMA will "
        "still be used for large messages, (must be >= 0; "
        "if zero then RDMA will not be used for short messages).",
        16,
        &mca_btl_udapl_component.udapl_max_eager_rdma_peers,
        REGINT_GE_ZERO), tmp_rc, rc);
        
    CHECK_PARAM_REGISTER_RETURN_VALUE(mca_btl_udapl_reg_int("eager_rdma_win",
        "Window size at which point an explicit "
        "credit message will be generated (must be >= 1).",
        28,
        &mca_btl_udapl_component.udapl_eager_rdma_win,
        REGINT_GE_ONE), tmp_rc, rc);

    CHECK_PARAM_REGISTER_RETURN_VALUE(mca_btl_udapl_reg_int("timeout",
        "Connection timeout, in microseconds.",
        MCA_BTL_UDAPL_CONN_TIMEOUT_DEFAULT,
        &ival,
        REGINT_GE_ONE), tmp_rc, rc);
    mca_btl_udapl_component.udapl_timeout = (uint32_t) ival;        

    CHECK_PARAM_REGISTER_RETURN_VALUE(mca_btl_udapl_reg_int("conn_priv_data",
        "Use connect private data to establish connections "
        "(not supported by all uDAPL implementations).",
        0,
        &mca_btl_udapl_component.udapl_conn_priv_data,
        REGINT_GE_ZERO), tmp_rc, rc);

    CHECK_PARAM_REGISTER_RETURN_VALUE(mca_btl_udapl_reg_int("async_events",
        "The asynchronous event queue will only be "
        "checked after entering progress this number of times.",
        100000000,
        &mca_btl_udapl_component.udapl_async_events,
        REGINT_GE_ONE), tmp_rc, rc);

    CHECK_PARAM_REGISTER_RETURN_VALUE(mca_btl_udapl_reg_int("buffer_alignment",
        "Preferred communication buffer alignment, "
        "in bytes (must be >= 1).",
        DAT_OPTIMAL_ALIGNMENT,
        &mca_btl_udapl_component.udapl_buffer_alignment,
        REGINT_GE_ONE), tmp_rc, rc);

    CHECK_PARAM_REGISTER_RETURN_VALUE(mca_btl_udapl_reg_string("if_include",
        "Comma-delimited list of interfaces to be included "
        "(e.g. \"ibd0,ibd1 or OpenIB-cma,OpenIB-cma-1\"; empty value means "
        "to use all interfaces found). Mutually exclusive with "
        "btl_udapl_if_exclude.",
        NULL, &mca_btl_udapl_component.if_include,
        REGSTR_EMPTY_OK), tmp_rc, rc);

    CHECK_PARAM_REGISTER_RETURN_VALUE(mca_btl_udapl_reg_string("if_exclude",
        "Comma-delimited list of interfaces to be excluded from use "
        "(e.g. \"ibd0,ibd1 or OpenIB-cma,OpenIB-cma-1\"; empty value means "
        "not to exclude any). Mutually exclusive with btl_udapl_if_include.",
        NULL, &mca_btl_udapl_component.if_exclude,
        REGSTR_EMPTY_OK), tmp_rc, rc);

    CHECK_PARAM_REGISTER_RETURN_VALUE(mca_btl_udapl_reg_int("verbose",
        "Verbosity level of the uDAPL BTL (-1 thru 100)",
        VERBOSE_SHOW_HELP,
        &(mca_btl_udapl_component.udapl_verbosity),
        REGINT_NEG_ONE_OK), tmp_rc, rc);

    CHECK_PARAM_REGISTER_RETURN_VALUE(mca_btl_udapl_reg_int("compare_subnet",
        "By default uDAPL BTL will compare subnets using netmask to "
        "determine if an interface is reachable. Setting this parameter to "
        "0 will essentially turn this comparison off and the uDAPL BTL will "
        "assume all uDAPL interfaces are reachable (0 or 1, default==1).",
        1,
        &(mca_btl_udapl_component.udapl_compare_subnet),
        REGINT_GE_ZERO), tmp_rc, rc);

    /* register uDAPL module parameters */
    CHECK_PARAM_REGISTER_RETURN_VALUE(mca_btl_udapl_reg_int("async_evd_qlen",
        "The asynchronous event dispatcher queue length.",
        MCA_BTL_UDAPL_ASYNC_EVD_QLEN_DEFAULT,
        (int*)&mca_btl_udapl_module.udapl_async_evd_qlen,
        REGINT_GE_ONE), tmp_rc, rc);

    CHECK_PARAM_REGISTER_RETURN_VALUE(mca_btl_udapl_reg_int("conn_evd_qlen",
        "The connection event dispatcher queue length is "
        "a function of the number of connections expected.",
        MCA_BTL_UDAPL_CONN_EVD_QLEN_DEFAULT,
        (int*)&mca_btl_udapl_module.udapl_conn_evd_qlen,
        REGINT_GE_ONE), tmp_rc, rc);

    CHECK_PARAM_REGISTER_RETURN_VALUE(mca_btl_udapl_reg_int("dto_evd_qlen",
        "The data transfer operation event dispatcher queue length is "
        "a function of the number of connections as well as the "
        "maximum number of outstanding data transfer operations.",
        MCA_BTL_UDAPL_DTO_EVD_QLEN_DEFAULT,
        (int*)&mca_btl_udapl_module.udapl_dto_evd_qlen,
        REGINT_GE_ONE), tmp_rc, rc);

    CHECK_PARAM_REGISTER_RETURN_VALUE(mca_btl_udapl_reg_int("max_request_dtos",
        "Maximum number of outstanding "
        "submitted sends and rdma operations per endpoint, (see Section "
        "6.6.6 of uDAPL Spec.).",
        MCA_BTL_UDAPL_MAX_REQUEST_DTOS_DEFAULT,
        (int*)&mca_btl_udapl_module.udapl_max_request_dtos,
        REGINT_GE_ONE), tmp_rc, rc);

    CHECK_PARAM_REGISTER_RETURN_VALUE(mca_btl_udapl_reg_int("max_recv_dtos",
        "Maximum number of outstanding "
        "submitted receive operations per endpoint, (see Section "
        "6.6.6 of uDAPL Spec.).",
        MCA_BTL_UDAPL_MAX_RECV_DTOS_DEFAULT,
        (int*)&mca_btl_udapl_module.udapl_max_recv_dtos,
        REGINT_GE_ONE), tmp_rc, rc);

    mca_btl_udapl_module.super.btl_exclusivity =
        MCA_BTL_EXCLUSIVITY_DEFAULT - 10;
    mca_btl_udapl_module.super.btl_eager_limit = 8*1024;
    mca_btl_udapl_module.super.btl_rndv_eager_limit = 8*1024;
    mca_btl_udapl_module.super.btl_max_send_size = 64*1024;
    mca_btl_udapl_module.super.btl_rdma_pipeline_send_length = 512*1024;
    mca_btl_udapl_module.super.btl_rdma_pipeline_frag_size = 128 * 1024;
    mca_btl_udapl_module.super.btl_min_rdma_pipeline_size = 0;
    mca_btl_udapl_module.super.btl_flags = MCA_BTL_FLAGS_PUT | MCA_BTL_FLAGS_SEND;
    mca_btl_udapl_module.super.btl_bandwidth = 225;
    mca_btl_udapl_module.super.btl_latency = 0;

    mca_btl_base_param_register(&mca_btl_udapl_component.super.btl_version,
            &mca_btl_udapl_module.super);

    return rc;
}
