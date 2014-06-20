/*
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "opal_config.h"

#include "opal/constants.h"
#include "opal/dss/dss.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"

#include "opal/mca/hwloc/hwloc.h"
#include "opal/mca/hwloc/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */
#include "opal/mca/hwloc/base/static-components.h"


/*
 * Globals
 */
int opal_hwloc_base_output = -1;
opal_list_t opal_hwloc_base_components;
bool opal_hwloc_base_inited = false;
#if OPAL_HAVE_HWLOC
hwloc_topology_t opal_hwloc_topology=NULL;
#endif
opal_hwloc_base_map_t opal_hwloc_base_map = OPAL_HWLOC_BASE_MAP_NONE;
opal_hwloc_base_mbfa_t opal_hwloc_base_mbfa = OPAL_HWLOC_BASE_MBFA_ERROR;


int opal_hwloc_base_open(void)
{
    if (opal_hwloc_base_inited) {
        return OPAL_SUCCESS;
    }
    opal_hwloc_base_inited = true;

#if OPAL_HAVE_HWLOC
    {
        int value;
        opal_data_type_t tmp;
        char *str_value;
        
        /* Debugging / verbose output */
        mca_base_param_reg_int_name("hwloc", "base_verbose", 
                                    "Verbosity level of the hwloc framework",
                                    false, false,
                                    0, &value);
        if (0 != value) {
            opal_hwloc_base_output = opal_output_open(NULL);
        } else {
            opal_hwloc_base_output = -1;
        }

        /* hwloc_base_mbind_policy */
        switch (opal_hwloc_base_map) {
        case OPAL_HWLOC_BASE_MAP_NONE:
            str_value = "none";
            break;
        case OPAL_HWLOC_BASE_MAP_LOCAL_ONLY:
            str_value = "local_only";
            break;
        }
        mca_base_param_reg_string_name("hwloc", "base_mem_alloc_policy",
                                       "General memory allocations placement policy (this is not memory binding). "
                                       "\"none\" means that no memory policy is applied. \"local_only\" means that a process' memory allocations will be restricted to its local NUMA node. "
                                       "If using direct launch, this policy will not be in effect until after MPI_INIT. "
                                       "Note that operating system paging policies are unaffected by this setting. For example, if \"local_only\" is used and local NUMA node memory is exhausted, a new memory allocation may cause paging.",
                                       false, false, str_value, &str_value);
        if (strcasecmp(str_value, "none") == 0) {
            opal_hwloc_base_map = OPAL_HWLOC_BASE_MAP_NONE;
        } else if (strcasecmp(str_value, "local_only") == 0 ||
                   strcasecmp(str_value, "local-only") == 0) {
            opal_hwloc_base_map = OPAL_HWLOC_BASE_MAP_LOCAL_ONLY;
        } else {
            char hostname[32];
            gethostname(hostname, sizeof(hostname));
            opal_show_help("help-opal-hwloc-base.txt", "invalid mem_alloc_policy",
                           true, hostname, getpid(), str_value);
            return OPAL_ERR_BAD_PARAM;
        }
        
        /* hwloc_base_bind_failure_action */
        switch (opal_hwloc_base_mbfa) {
        case OPAL_HWLOC_BASE_MBFA_WARN:
            str_value = "warn";
            break;
        case OPAL_HWLOC_BASE_MBFA_ERROR:
            str_value = "error";
            break;
        }
        mca_base_param_reg_string_name("hwloc", "base_mem_bind_failure_action",
                                       "What Open MPI will do if it explicitly tries to bind memory to a specific NUMA location, and fails.  Note that this is a different case than the general allocation policy described by hwloc_base_alloc_policy.  A value of \"warn\" means that Open MPI will warn the first time this happens, but allow the job to continue (possibly with degraded performance).  A value of \"error\" means that Open MPI will abort the job if this happens.",
                                       false, false, str_value, &str_value);
        if (strcasecmp(str_value, "warn") == 0) {
            opal_hwloc_base_mbfa = OPAL_HWLOC_BASE_MBFA_WARN;
        } else if (strcasecmp(str_value, "error") == 0) {
            opal_hwloc_base_mbfa = OPAL_HWLOC_BASE_MBFA_ERROR;
        } else {
            char hostname[32];
            gethostname(hostname, sizeof(hostname));
            opal_show_help("help-opal-hwloc-base.txt", 
                           "invalid mem_bind_failure_action",
                           true, hostname, getpid(), str_value);
            return OPAL_ERR_BAD_PARAM;
        }
        
        /* to support tools such as ompi_info, add the components
         * to a list
         */
        OBJ_CONSTRUCT(&opal_hwloc_base_components, opal_list_t);
        if (OPAL_SUCCESS !=
            mca_base_components_open("hwloc", opal_hwloc_base_output,
                                     mca_hwloc_base_static_components,
                                     &opal_hwloc_base_components, true)) {
            return OPAL_ERROR;
        }

        /* declare the hwloc data types */
        tmp = OPAL_HWLOC_TOPO;
        if (OPAL_SUCCESS != (value = opal_dss.register_type(opal_hwloc_pack,
                                                            opal_hwloc_unpack,
                                                            (opal_dss_copy_fn_t)opal_hwloc_copy,
                                                            (opal_dss_compare_fn_t)opal_hwloc_compare,
                                                            (opal_dss_size_fn_t)opal_hwloc_size,
                                                            (opal_dss_print_fn_t)opal_hwloc_print,
                                                            (opal_dss_release_fn_t)opal_hwloc_release,
                                                            OPAL_DSS_STRUCTURED,
                                                            "OPAL_HWLOC_TOPO", &tmp))) {
            return value;
        }
    }
#endif

    return OPAL_SUCCESS;
}
