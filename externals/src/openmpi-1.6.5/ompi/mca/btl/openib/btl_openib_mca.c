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
 * Copyright (c) 2006-2011 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2006-2009 Mellanox Technologies. All rights reserved.
 * Copyright (c) 2006-2007 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2006-2007 Voltaire All rights reserved.
 * Copyright (c) 2009-2010 Oracle and/or its affiliates.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <string.h>

#include "opal/mca/installdirs/installdirs.h"
#include "orte/util/show_help.h"
#include "opal/util/output.h"
#include "opal/mca/base/mca_base_param.h"
#include "btl_openib.h"
#include "btl_openib_mca.h"
#include "btl_openib_ini.h"
#include "connect/base.h"

#ifdef HAVE_IBV_FORK_INIT
#define OMPI_HAVE_IBV_FORK_INIT 1
#else
#define OMPI_HAVE_IBV_FORK_INIT 0
#endif

/*
 * Local flags
 */
enum {
    REGINT_NEG_ONE_OK = 0x01,
    REGINT_GE_ZERO = 0x02,
    REGINT_GE_ONE = 0x04,
    REGINT_NONZERO = 0x08,

    REGINT_MAX = 0x88
};


enum {
    REGSTR_EMPTY_OK = 0x01,

    REGSTR_MAX = 0x88
};


/*
 * utility routine for string parameter registration
 */
static int reg_string(const char* param_name, 
                      const char* deprecated_param_name,
                      const char* param_desc,
                      const char* default_value, char **out_value,
                      int flags)
{
    int index;
    char *value;
    index = mca_base_param_reg_string(&mca_btl_openib_component.super.btl_version,
                                      param_name, param_desc, false, false,
                                      default_value, &value);
    if (NULL != deprecated_param_name) {
        mca_base_param_reg_syn(index, 
                               &mca_btl_openib_component.super.btl_version, 
                               deprecated_param_name, true);
    }
    mca_base_param_lookup_string(index, &value);

    if (0 != (flags & REGSTR_EMPTY_OK) && 0 == strlen(value)) {
        opal_output(0, "Bad parameter value for parameter \"%s\"",
                param_name);
        return OMPI_ERR_BAD_PARAM;
    }
    *out_value = value;
    return OMPI_SUCCESS;
}


/*
 * utility routine for integer parameter registration
 */
static int reg_int(const char* param_name, 
                   const char* deprecated_param_name,
                   const char* param_desc,
                   int default_value, int *out_value, int flags)
{
    int index, value;
    index = mca_base_param_reg_int(&mca_btl_openib_component.super.btl_version,
                                   param_name, param_desc, false, false,
                                   default_value, NULL);
    if (NULL != deprecated_param_name) {
        mca_base_param_reg_syn(index, 
                               &mca_btl_openib_component.super.btl_version, 
                               deprecated_param_name, true);
    }
    mca_base_param_lookup_int(index, &value);
    
    if (0 != (flags & REGINT_NEG_ONE_OK) && -1 == value) {
        *out_value = value;
        return OMPI_SUCCESS;
    }
    if ((0 != (flags & REGINT_GE_ZERO) && value < 0) ||
        (0 != (flags & REGINT_GE_ONE) && value < 1) ||
        (0 != (flags & REGINT_NONZERO) && 0 == value)) {
        opal_output(0, "Bad parameter value for parameter \"%s\"",
                param_name);
        return OMPI_ERR_BAD_PARAM;
    }
    *out_value = value;
    return OMPI_SUCCESS;
}

/*
 * Register and check all MCA parameters
 */
int btl_openib_register_mca_params(void)
{
    char default_qps[100];
    uint32_t mid_qp_size;
    int i;
    char *msg, *str, *pkey;
    int ival, ival2, ret, tmp;

    ret = OMPI_SUCCESS;
#define CHECK(expr) do {\
        tmp = (expr); \
        if (OMPI_SUCCESS != tmp) ret = tmp; \
     } while (0)

    /* register openib component parameters */
    CHECK(reg_int("verbose", NULL,
                  "Output some verbose OpenIB BTL information "
                  "(0 = no output, nonzero = output)", 0, &ival, 0));
    mca_btl_openib_component.verbose = (0 != ival);

    CHECK(reg_int("warn_no_device_params_found",
                  "warn_no_hca_params_found",
                  "Warn when no device-specific parameters are found in the INI file specified by the btl_openib_device_param_files MCA parameter "
                  "(0 = do not warn; any other value = warn)",
                  1, &ival, 0));
    mca_btl_openib_component.warn_no_device_params_found = (0 != ival);
    CHECK(reg_int("warn_default_gid_prefix", NULL,
                  "Warn when there is more than one active ports and at least one of them connected to the network with only default GID prefix configured "
                  "(0 = do not warn; any other value = warn)",
                  1, &ival, 0));
    mca_btl_openib_component.warn_default_gid_prefix = (0 != ival);
    CHECK(reg_int("warn_nonexistent_if", NULL,
                  "Warn if non-existent devices and/or ports are specified in the btl_openib_if_[in|ex]clude MCA parameters "
                  "(0 = do not warn; any other value = warn)",
                  1, &ival, 0));
    mca_btl_openib_component.warn_nonexistent_if = (0 != ival);

    /* If we print a warning about not having enough registered memory
       available, do we want to abort? */
    CHECK(reg_int("abort_not_enough_reg_mem", NULL,
                  "If there is not enough registered memory available on the system for Open MPI to function properly, Open MPI will issue a warning.  If this MCA parameter is set to true, then Open MPI will also abort all MPI jobs "
                  "(0 = warn, but do not abort; any other value = warn and abort)",
                  0, &ival, 0));
    mca_btl_openib_component.abort_not_enough_reg_mem = (0 != ival);

    CHECK(reg_int("poll_cq_batch", NULL,
                  "Retrieve up to poll_cq_batch completions from CQ",
                  MCA_BTL_OPENIB_CQ_POLL_BATCH_DEFAULT, &ival, REGINT_GE_ONE));

    mca_btl_openib_component.cq_poll_batch = (ival > MCA_BTL_OPENIB_CQ_POLL_BATCH_DEFAULT)? MCA_BTL_OPENIB_CQ_POLL_BATCH_DEFAULT : ival;

    if (OMPI_HAVE_IBV_FORK_INIT) {
        ival2 = -1;
    } else {
        ival2 = 0;
    }
    CHECK(reg_int("want_fork_support", NULL,
                  "Whether fork support is desired or not "
                  "(negative = try to enable fork support, but continue even if it is not available, 0 = do not enable fork support, positive = try to enable fork support and fail if it is not available)",
                  ival2, &ival, 0));
#ifdef HAVE_IBV_FORK_INIT
    mca_btl_openib_component.want_fork_support = ival;
#else
    if (0 != ival) {
        orte_show_help("help-mpi-btl-openib.txt",
                       "ibv_fork requested but not supported", true,
                       orte_process_info.nodename);
        return OMPI_ERROR;
    }
#endif

    asprintf(&str, "%s/mca-btl-openib-device-params.ini",
             opal_install_dirs.pkgdatadir);
    if (NULL == str) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    CHECK(reg_string("device_param_files", "hca_param_files",
                     "Colon-delimited list of INI-style files that contain device vendor/part-specific parameters",
                     str, &mca_btl_openib_component.device_params_file_names, 
                     0));
    free(str);

    CHECK(reg_string("device_type", NULL,
                     "Specify to only use IB or iWARP network adapters "
                     "(infiniband = only use InfiniBand HCAs; iwarp = only use iWARP NICs; all = use any available adapters)",
                     "all", &str, 0));
    if (0 == strcasecmp(str, "ib") ||
        0 == strcasecmp(str, "infiniband")) {
        mca_btl_openib_component.device_type = BTL_OPENIB_DT_IB;
    } else if (0 == strcasecmp(str, "iw") ||
               0 == strcasecmp(str, "iwarp")) {
        mca_btl_openib_component.device_type = BTL_OPENIB_DT_IWARP;
    } else if (0 == strcasecmp(str, "all")) {
        mca_btl_openib_component.device_type = BTL_OPENIB_DT_ALL;
    } else {
        orte_show_help("help-mpi-btl-openib.txt",
                       "ibv_fork requested but not supported", true,
                       orte_process_info.nodename);
        return OMPI_ERROR;
    }
    free(str);

    CHECK(reg_int("max_btls", NULL,
                  "Maximum number of device ports to use "
                  "(-1 = use all available, otherwise must be >= 1)",
                  -1, &mca_btl_openib_component.ib_max_btls,
                  REGINT_NEG_ONE_OK | REGINT_GE_ONE));
    CHECK(reg_int("free_list_num", NULL,
                  "Initial size of free lists "
                  "(must be >= 1)",
                  8, &mca_btl_openib_component.ib_free_list_num,
                  REGINT_GE_ONE));
    CHECK(reg_int("free_list_max", NULL,
                  "Maximum size of free lists "
                  "(-1 = infinite, otherwise must be >= 0)",
                  -1, &mca_btl_openib_component.ib_free_list_max,
                  REGINT_NEG_ONE_OK | REGINT_GE_ONE));
    CHECK(reg_int("free_list_inc", NULL,
                  "Increment size of free lists "
                  "(must be >= 1)",
                  32, &mca_btl_openib_component.ib_free_list_inc,
                  REGINT_GE_ONE));
    CHECK(reg_string("mpool", NULL,
                     "Name of the memory pool to be used (it is unlikely that you will ever want to change this)",
                     "rdma", &mca_btl_openib_component.ib_mpool_name,
                     0));
    CHECK(reg_int("reg_mru_len", NULL,
                  "Length of the registration cache most recently used list "
                  "(must be >= 1)",
                  16, (int*) &mca_btl_openib_component.reg_mru_len,
                  REGINT_GE_ONE));

    CHECK(reg_int("cq_size", "ib_cq_size",
                  "Minimum size of the OpenFabrics completion queue "
                  "(CQs are automatically sized based on the number "
                  "of peer MPI processes; this value determines the "
                  "*minimum* size of all CQs)",
                  8192, &ival, REGINT_GE_ONE));
    mca_btl_openib_component.ib_cq_size[BTL_OPENIB_LP_CQ] =
        mca_btl_openib_component.ib_cq_size[BTL_OPENIB_HP_CQ] = (uint32_t) ival;

    CHECK(reg_int("max_inline_data", "ib_max_inline_data",
                  "Maximum size of inline data segment "
                  "(-1 = run-time probe to discover max value, otherwise must be >= 0). "
                  "If not explicitly set, use max_inline_data from "
                  "the INI file containing device-specific parameters",
                  -1, &ival, REGINT_NEG_ONE_OK | REGINT_GE_ZERO));
    mca_btl_openib_component.ib_max_inline_data = (int32_t) ival;

    CHECK(reg_string("pkey", "ib_pkey_val", 
                     "OpenFabrics partition key (pkey) value. "
                     "Unsigned integer decimal or hex values are allowed (e.g., \"3\" or \"0x3f\") and will be masked against the maximum allowable IB partition key value (0x7fff)",
                     "0", &pkey, 0));
    mca_btl_openib_component.ib_pkey_val = 
        ompi_btl_openib_ini_intify(pkey) & MCA_BTL_IB_PKEY_MASK;
    free(pkey);

    CHECK(reg_int("psn", "ib_psn",
                  "OpenFabrics packet sequence starting number "
                  "(must be >= 0)",
                  0, &ival, REGINT_GE_ZERO));
    mca_btl_openib_component.ib_psn = (uint32_t) ival;

    CHECK(reg_int("ib_qp_ous_rd_atom", NULL, 
                  "InfiniBand outstanding atomic reads "
                  "(must be >= 0)",
                  4, &ival, REGINT_GE_ZERO));
    mca_btl_openib_component.ib_qp_ous_rd_atom = (uint32_t) ival;

    asprintf(&msg, "OpenFabrics MTU, in bytes (if not specified in INI files).  Valid values are: %d=256 bytes, %d=512 bytes, %d=1024 bytes, %d=2048 bytes, %d=4096 bytes",
             IBV_MTU_256,
             IBV_MTU_512,
             IBV_MTU_1024,
             IBV_MTU_2048,
             IBV_MTU_4096);
    if (NULL == msg) {
        /* Don't try to recover from this */
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    CHECK(reg_int("mtu", "ib_mtu", msg, IBV_MTU_1024, &ival, 0));
    free(msg);
    if (ival < IBV_MTU_1024 || ival > IBV_MTU_4096) {
        orte_show_help("help-mpi-btl-openib.txt", "invalid mca param value",
                       true, "invalid value for btl_openib_ib_mtu",
                       "btl_openib_ib_mtu reset to 1024");
        mca_btl_openib_component.ib_mtu = IBV_MTU_1024;
    } else {
        mca_btl_openib_component.ib_mtu = (uint32_t) ival;
    }

    CHECK(reg_int("ib_min_rnr_timer", NULL, "InfiniBand minimum "
                  "\"receiver not ready\" timer, in seconds "
                  "(must be >= 0 and <= 31)",
                  25, &ival, 0));
    if (ival > 31) {
        orte_show_help("help-mpi-btl-openib.txt", "invalid mca param value",
                       true, "btl_openib_ib_min_rnr_timer > 31",
                       "btl_openib_ib_min_rnr_timer reset to 31");
        ival = 31;
    } else if (ival < 0){
        orte_show_help("help-mpi-btl-openib.txt", "invalid mca param value",
                   true, "btl_openib_ib_min_rnr_timer < 0",
                   "btl_openib_ib_min_rnr_timer reset to 0");
        ival = 0;
    }
    mca_btl_openib_component.ib_min_rnr_timer = (uint32_t) ival;

    CHECK(reg_int("ib_timeout", NULL,
                  "InfiniBand transmit timeout, plugged into formula: 4.096 microseconds * (2^btl_openib_ib_timeout) "
                  "(must be >= 0 and <= 31)",
                  20, &ival, 0));
    if (ival > 31) {
        orte_show_help("help-mpi-btl-openib.txt", "invalid mca param value",
                       true, "btl_openib_ib_timeout > 31",
                       "btl_openib_ib_timeout reset to 31");
        ival = 31;
    } else if (ival < 0) {
        orte_show_help("help-mpi-btl-openib.txt", "invalid mca param value",
                   true, "btl_openib_ib_timeout < 0",
                   "btl_openib_ib_timeout reset to 0");
        ival = 0;
    }
    mca_btl_openib_component.ib_timeout = (uint32_t) ival;

    CHECK(reg_int("ib_retry_count", NULL,
                  "InfiniBand transmit retry count "
                  "(must be >= 0 and <= 7)",
                  7, &ival, 0));
    if (ival > 7) {
        orte_show_help("help-mpi-btl-openib.txt", "invalid mca param value",
                       true, "btl_openib_ib_retry_count > 7",
                       "btl_openib_ib_retry_count reset to 7");
        ival = 7;
    } else if (ival < 0) {
        orte_show_help("help-mpi-btl-openib.txt", "invalid mca param value",
                   true, "btl_openib_ib_retry_count < 0",
                   "btl_openib_ib_retry_count reset to 0");
        ival = 0;
    }
    mca_btl_openib_component.ib_retry_count = (uint32_t) ival;

    CHECK(reg_int("ib_rnr_retry", NULL,
                  "InfiniBand \"receiver not ready\" "
                  "retry count; applies *only* to SRQ/XRC queues.  PP queues "
                  "use RNR retry values of 0 because Open MPI performs "
                  "software flow control to guarantee that RNRs never occur "
                  "(must be >= 0 and <= 7; 7 = \"infinite\")",
                  7, &ival, 0));
    if (ival > 7) {
        orte_show_help("help-mpi-btl-openib.txt", "invalid mca param value",
                       true, "btl_openib_ib_rnr_retry > 7",
                       "btl_openib_ib_rnr_retry reset to 7");
        ival = 7;
    } else if (ival < 0) {
        orte_show_help("help-mpi-btl-openib.txt", "invalid mca param value",
                   true, "btl_openib_ib_rnr_retry < 0",
                   "btl_openib_ib_rnr_retry reset to 0");
        ival = 0;
    }
    mca_btl_openib_component.ib_rnr_retry = (uint32_t) ival;

    CHECK(reg_int("ib_max_rdma_dst_ops", NULL, "InfiniBand maximum pending RDMA "
                  "destination operations "
                  "(must be >= 0)",
                  4, &ival, REGINT_GE_ZERO));
    mca_btl_openib_component.ib_max_rdma_dst_ops = (uint32_t) ival;

    CHECK(reg_int("ib_service_level", NULL, "InfiniBand service level "
                  "(must be >= 0 and <= 15)",
                  0, &ival, 0));
    if (ival > 15) {
        orte_show_help("help-mpi-btl-openib.txt", "invalid mca param value",
                       true, "btl_openib_ib_service_level > 15",
                       "btl_openib_ib_service_level reset to 15");
        ival = 15;
    } else if (ival < 0) {
        orte_show_help("help-mpi-btl-openib.txt", "invalid mca param value",
                   true, "btl_openib_ib_service_level < 0",
                   "btl_openib_ib_service_level reset to 0");
        ival = 0;
    }
    mca_btl_openib_component.ib_service_level = (uint32_t) ival;

#if (ENABLE_DYNAMIC_SL)
    CHECK(reg_int("ib_path_record_service_level", NULL,
                  "Enable getting InfiniBand service level from PathRecord "
                  "(must be >= 0, 0 = disabled, positive = try to get the "
                  "service level from PathRecord)",
                  0, &ival, REGINT_GE_ZERO));
    mca_btl_openib_component.ib_path_record_service_level = (uint32_t) ival;
#endif

    CHECK(reg_int("use_eager_rdma", NULL, "Use RDMA for eager messages "
                  "(-1 = use device default, 0 = do not use eager RDMA, "
                  "1 = use eager RDMA)",
                  -1, &ival, 0));
    mca_btl_openib_component.use_eager_rdma = (int32_t) ival;

    CHECK(reg_int("eager_rdma_threshold", NULL,
                  "Use RDMA for short messages after this number of "
                  "messages are received from a given peer "
                  "(must be >= 1)",
                  16, &ival, REGINT_GE_ONE));
    mca_btl_openib_component.eager_rdma_threshold = (int32_t) ival;

    CHECK(reg_int("max_eager_rdma", NULL, "Maximum number of peers allowed to use "
                  "RDMA for short messages (RDMA is used for all long "
                  "messages, except if explicitly disabled, such as "
                  "with the \"dr\" pml) "
                  "(must be >= 0)",
                  16, &ival, REGINT_GE_ZERO));
    mca_btl_openib_component.max_eager_rdma = (int32_t) ival;

    CHECK(reg_int("eager_rdma_num", NULL, "Number of RDMA buffers to allocate "
                  "for small messages "
                  "(must be >= 1)",
                  16, &ival, REGINT_GE_ONE));
    mca_btl_openib_component.eager_rdma_num = (int32_t) (ival + 1);

    CHECK(reg_int("btls_per_lid", NULL, "Number of BTLs to create for each "
                  "InfiniBand LID "
                  "(must be >= 1)",
                  1, &ival, REGINT_GE_ONE));
    mca_btl_openib_component.btls_per_lid = (uint32_t) ival;

    CHECK(reg_int("max_lmc", NULL, "Maximum number of LIDs to use for each device port "
                  "(must be >= 0, where 0 = use all available)",
                  0, &ival, REGINT_GE_ZERO));
    mca_btl_openib_component.max_lmc = (uint32_t) ival;

#if OPAL_HAVE_THREADS
    CHECK(reg_int("enable_apm_over_lmc", NULL, "Maximum number of alternative paths for each device port "
                  "(must be >= -1, where 0 = disable apm, -1 = all available alternative paths )",
                  0, &ival, REGINT_NEG_ONE_OK|REGINT_GE_ZERO));
    mca_btl_openib_component.apm_lmc = (uint32_t) ival;
    CHECK(reg_int("enable_apm_over_ports", NULL, "Enable alternative path migration (APM) over different ports of the same device "
                  "(must be >= 0, where 0 = disable APM over ports, 1 = enable APM over ports of the same device)",
                  0, &ival, REGINT_GE_ZERO));
    mca_btl_openib_component.apm_ports = (uint32_t) ival;

    CHECK(reg_int("use_async_event_thread", NULL,
                "If nonzero, use the thread that will handle InfiniBand asynchronous events",
                1, &ival, 0));
    mca_btl_openib_component.use_async_event_thread = (0 != ival);

#if BTL_OPENIB_FAILOVER_ENABLED
    /* failover specific output */
    CHECK(reg_int("verbose_failover", NULL,
                  "Output some verbose OpenIB BTL failover information "
                  "(0 = no output, nonzero = output)", 0, &ival, 0));
    mca_btl_openib_component.verbose_failover = opal_output_open(NULL);
    opal_output_set_verbosity(mca_btl_openib_component.verbose_failover, ival);

    CHECK(reg_int("port_error_failover", NULL,
                  "If nonzero, asynchronous port errors will trigger failover",
                  0, &ival, 0));
    mca_btl_openib_component.port_error_failover = (0 != ival);

    /* Make non writeable parameter that indicates failover is configured in. */
    tmp = mca_base_param_reg_int(&mca_btl_openib_component.super.btl_version,
                                 "failover_enabled",
                                 "openib failover is configured: run with bfo PML to support failover between openib BTLs",
                                 false, true,
                                 1, NULL);
#endif

    CHECK(reg_int("enable_srq_resize", NULL,
                  "Enable/Disable on demand SRQ resize. "
                  "(0 = without resizing, nonzero = with resizing)", 1, &ival, 0));
    mca_btl_openib_component.enable_srq_resize = (0 != ival);
#else
    mca_btl_openib_component.enable_srq_resize = 0;
#endif

    CHECK(reg_int("buffer_alignment", NULL,
                  "Preferred communication buffer alignment, in bytes "
                  "(must be > 0 and power of two)",
                  64, &ival, REGINT_GE_ZERO));
    if(ival <= 1 || (ival & (ival - 1))) {
        orte_show_help("help-mpi-btl-openib.txt", "wrong buffer alignment",
                true, ival, orte_process_info.nodename, 64);
        mca_btl_openib_component.buffer_alignment = 64;
    } else {
        mca_btl_openib_component.buffer_alignment = (uint32_t) ival;
    }

    CHECK(reg_int("use_message_coalescing", NULL,
                  "If nonzero, use message coalescing", 1, &ival, 0));
    mca_btl_openib_component.use_message_coalescing = (0 != ival);

    CHECK(reg_int("cq_poll_ratio", NULL,
                  "How often to poll high priority CQ versus low priority CQ",
                  100, &ival, REGINT_GE_ONE));
    mca_btl_openib_component.cq_poll_ratio = (uint32_t)ival;
    CHECK(reg_int("eager_rdma_poll_ratio", NULL,
                  "How often to poll eager RDMA channel versus CQ",
                  100, &ival, REGINT_GE_ONE));
    mca_btl_openib_component.eager_rdma_poll_ratio = (uint32_t)ival;
    CHECK(reg_int("hp_cq_poll_per_progress", NULL,
                  "Max number of completion events to process for each call "
                  "of BTL progress engine",
                  10, &ival, REGINT_GE_ONE));
    mca_btl_openib_component.cq_poll_progress = (uint32_t)ival;

    /* Info only */
    mca_base_param_reg_int(&mca_btl_openib_component.super.btl_version,
                           "have_fork_support",
                           "Whether the OpenFabrics stack supports applications that invoke the \"fork()\" system call or not (0 = no, 1 = yes). "
                           "Note that this value does NOT indicate whether the system being run on supports \"fork()\" with OpenFabrics applications or not.",
                           false, true,
                           OMPI_HAVE_IBV_FORK_INIT ? 1 : 0,
                           NULL);

    mca_btl_openib_module.super.btl_exclusivity = MCA_BTL_EXCLUSIVITY_DEFAULT;

    mca_btl_openib_module.super.btl_eager_limit = 12 * 1024;
    mca_btl_openib_module.super.btl_rndv_eager_limit = 12 * 1024;
    mca_btl_openib_module.super.btl_max_send_size = 64 * 1024;
    mca_btl_openib_module.super.btl_rdma_pipeline_send_length = 1024 * 1024;
    mca_btl_openib_module.super.btl_rdma_pipeline_frag_size = 1024 * 1024;
    mca_btl_openib_module.super.btl_min_rdma_pipeline_size = 256 * 1024;
    mca_btl_openib_module.super.btl_flags = MCA_BTL_FLAGS_RDMA |
        MCA_BTL_FLAGS_NEED_ACK | MCA_BTL_FLAGS_NEED_CSUM | MCA_BTL_FLAGS_HETEROGENEOUS_RDMA;
#if BTL_OPENIB_FAILOVER_ENABLED
    mca_btl_openib_module.super.btl_flags |= MCA_BTL_FLAGS_FAILOVER_SUPPORT;
#endif
    /* Default to bandwidth auto-detection */
    mca_btl_openib_module.super.btl_bandwidth = 0;
    mca_btl_openib_module.super.btl_latency = 4;
    CHECK(mca_btl_base_param_register(
            &mca_btl_openib_component.super.btl_version,
            &mca_btl_openib_module.super));

    /* setup all the qp stuff */
    mid_qp_size = mca_btl_openib_module.super.btl_eager_limit / 4;
    /* round mid_qp_size to smallest power of two */
    for(i = 31; i > 0; i--) {
        if(!(mid_qp_size & (1<<i))) {
            continue;
        }
        mid_qp_size = (1<<i);
        break;
    }

    if(mid_qp_size <= 128) {
        mid_qp_size = 1024;
    }

    snprintf(default_qps, 100,
            "P,128,256,192,128:S,%u,1024,1008,64:S,%u,1024,1008,64:S,%u,1024,1008,64",
            mid_qp_size,
            (uint32_t)mca_btl_openib_module.super.btl_eager_limit,
            (uint32_t)mca_btl_openib_module.super.btl_max_send_size);

    mca_btl_openib_component.default_recv_qps = strdup(default_qps);
    if(NULL == mca_btl_openib_component.default_recv_qps) {
        BTL_ERROR(("Unable to allocate memory for default receive queues string.\n"));
        return OMPI_ERROR;
    }

    CHECK(reg_string("receive_queues", NULL,
                     "Colon-delimited, comma-delimited list of receive queues: P,4096,8,6,4:P,32768,8,6,4",
                     default_qps, &mca_btl_openib_component.receive_queues, 
                     0));
    mca_btl_openib_component.receive_queues_source = 
        (0 == strcmp(default_qps, 
                     mca_btl_openib_component.receive_queues)) ? 
        BTL_OPENIB_RQ_SOURCE_DEFAULT : BTL_OPENIB_RQ_SOURCE_MCA;

    CHECK(reg_string("if_include", NULL,
                     "Comma-delimited list of devices/ports to be used (e.g. \"mthca0,mthca1:2\"; empty value means to use all ports found).  Mutually exclusive with btl_openib_if_exclude.",
                     NULL, &mca_btl_openib_component.if_include,
                     0));

    CHECK(reg_string("if_exclude", NULL,
                     "Comma-delimited list of device/ports to be excluded (empty value means to not exclude any ports).  Mutually exclusive with btl_openib_if_include.",
                     NULL, &mca_btl_openib_component.if_exclude,
                     0));

    CHECK(reg_string("ipaddr_include", NULL,
                     "Comma-delimited list of IP Addresses to be used (e.g. \"192.168.1.0/24\").  Mutually exclusive with btl_openib_ipaddr_exclude.",
                     NULL, &mca_btl_openib_component.ipaddr_include,
                     0));

    CHECK(reg_string("ipaddr_exclude", NULL,
                     "Comma-delimited list of IP Addresses to be excluded (e.g. \"192.168.1.0/24\").  Mutually exclusive with btl_openib_ipaddr_include.",
                     NULL, &mca_btl_openib_component.ipaddr_exclude,
                     0));

    CHECK(reg_int("gid_index", NULL,
                  "GID index to use on verbs device ports",
                  0, &mca_btl_openib_component.gid_index,
                  REGINT_GE_ZERO));

#if BTL_OPENIB_MALLOC_HOOKS_ENABLED
    CHECK(reg_int("memalign", NULL,
                  "[64 | 32 | 0] - Enable (64bit or 32bit)/Disable(0) memory"
                  "alignment for all malloc calls if btl openib is used.",
                  32, &mca_btl_openib_component.use_memalign,
                  REGINT_GE_ZERO));
    
    if (mca_btl_openib_component.use_memalign != 32  
        && mca_btl_openib_component.use_memalign != 64
        && mca_btl_openib_component.use_memalign != 0){ 
        orte_show_help("help-mpi-btl-openib.txt", "invalid mca param value",
                       true, "Wrong btl_openib_memalign parameter value. Allowed values: 64, 32, 0.",
                       "btl_openib_memalign is reset to 32");
        mca_btl_openib_component.use_memalign = 32; 
    }
    reg_int("memalign_threshold", NULL,
            "Allocating memory more than btl_openib_memalign_threshhold"
            "bytes will automatically be algined to the value of btl_openib_memalign bytes."
            "memalign_threshhold defaults to the same value as mca_btl_openib_eager_limit.",
            mca_btl_openib_component.eager_limit,
            &ival,
            REGINT_GE_ZERO);
    if (ival < 0){
        orte_show_help("help-mpi-btl-openib.txt", "invalid mca param value",
                       true, "btl_openib_memalign_threshold must be positive",
                       "btl_openib_memalign_threshold is reset to btl_openib_eager_limit");
        ival = mca_btl_openib_component.eager_limit;
    }
    mca_btl_openib_component.memalign_threshold = (size_t)ival;
#endif
    /* Register any MCA params for the connect pseudo-components */
    if (OMPI_SUCCESS == ret) {
        ret = ompi_btl_openib_connect_base_register();
    }

    return ret;
}

