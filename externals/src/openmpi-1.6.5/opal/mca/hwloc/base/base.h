/*
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OPAL_HWLOC_BASE_H
#define OPAL_HWLOC_BASE_H

#include "opal_config.h"

#include "opal/dss/dss_types.h"

#include "opal/mca/hwloc/hwloc.h"

/*
 * Global functions for MCA overall hwloc open and close
 */

BEGIN_C_DECLS

/**
 * Initialize the hwloc MCA framework
 *
 * @retval OPAL_SUCCESS Upon success
 * @retval OPAL_ERROR Upon failure
 *
 * This must be the first function invoked in the hwloc MCA
 * framework.  It initializes the hwloc MCA framework, finds
 * and opens hwloc components, etc.
 *
 * This function is invoked during opal_init().
 * 
 * This function fills in the internal global variable
 * opal_hwloc_base_components_opened, which is a list of all
 * hwloc components that were successfully opened.  This
 * variable should \em only be used by other hwloc base
 * functions -- it is not considered a public interface member --
 * and is only mentioned here for completeness.
 *
 * Note that this function does NOT fill the global variable
 * opal_hwloc_topology, nor does it set the process-wide memory
 * affinity policy.  Filling opal_hwloc_topology via
 * hwloc_topology_load() can be expensive (and/or serialized by the
 * OS); it may not be desireable to call this function in every MPI
 * process on a machine.  Hence, it is the responsibility for an upper
 * layer to both fill opal_hwloc_topology in some scalable way, as
 * well as to invoke opal_hwloc_base_set_process_membind_policy()
 * (after opal_hwloc_topology has been loaded) to set the process-wide
 * memory affinity policy.
 */
OPAL_DECLSPEC int opal_hwloc_base_open(void);

/**
 * Shut down the hwloc MCA framework.
 *
 * @retval OPAL_SUCCESS Always
 *
 * This function shuts down everything in the hwloc MCA
 * framework, and is called during opal_finalize().
 *
 * It must be the last function invoked on the hwloc MCA
 * framework.
 */
OPAL_DECLSPEC int opal_hwloc_base_close(void);

/**
 * Debugging output stream
 */
OPAL_DECLSPEC extern int opal_hwloc_base_output;
OPAL_DECLSPEC extern opal_list_t opal_hwloc_base_components;
OPAL_DECLSPEC extern bool opal_hwloc_base_inited;
OPAL_DECLSPEC extern bool opal_hwloc_topology_inited;

#if OPAL_HAVE_HWLOC
/* datatype support */
OPAL_DECLSPEC int opal_hwloc_pack(opal_buffer_t *buffer, const void *src,
                                  int32_t num_vals,
                                  opal_data_type_t type);
OPAL_DECLSPEC int opal_hwloc_unpack(opal_buffer_t *buffer, void *dest,
                                    int32_t *num_vals,
                                    opal_data_type_t type);
OPAL_DECLSPEC int opal_hwloc_copy(hwloc_topology_t *dest,
                                  hwloc_topology_t src,
                                  opal_data_type_t type);
OPAL_DECLSPEC int opal_hwloc_compare(const hwloc_topology_t topo1,
                                     const hwloc_topology_t topo2,
                                     opal_data_type_t type);
OPAL_DECLSPEC int opal_hwloc_print(char **output, char *prefix,
                                   hwloc_topology_t src,
                                   opal_data_type_t type);
OPAL_DECLSPEC int opal_hwloc_size(size_t *size,
                                  hwloc_topology_t src,
                                  opal_data_type_t type);
OPAL_DECLSPEC void opal_hwloc_release(opal_dss_value_t *value);

/**
 * Report a bind failure using the normal mechanisms if a component
 * fails to bind memory -- according to the value of the
 * hwloc_base_bind_failure_action MCA parameter.
 */
OPAL_DECLSPEC int opal_hwloc_base_report_bind_failure(const char *file,
                                                      int line,
                                                      const char *msg, 
                                                      int rc);

#endif

/**
 * Enum for what memory allocation policy we want for user allocations.
 * MAP = memory allocation policy.
 */
typedef enum {
    OPAL_HWLOC_BASE_MAP_NONE,
    OPAL_HWLOC_BASE_MAP_LOCAL_ONLY
} opal_hwloc_base_map_t;

/**
 * Global reflecting the MAP (set by MCA param).
 */
OPAL_DECLSPEC extern opal_hwloc_base_map_t opal_hwloc_base_map;

/**
 * Enum for what to do if the hwloc framework tries to bind memory
 * and fails.  MBFA = memory bind failure action.
 */
typedef enum {
    OPAL_HWLOC_BASE_MBFA_WARN,
    OPAL_HWLOC_BASE_MBFA_ERROR
} opal_hwloc_base_mbfa_t;

/**
 * Global reflecting the MBFA (set by MCA param).
 */
OPAL_DECLSPEC extern opal_hwloc_base_mbfa_t opal_hwloc_base_mbfa;

/**
 * This function sets the process-wide memory affinity policy
 * according to opal_hwloc_base_map and opal_hwloc_base_bfa.  It needs
 * to be a separate, standalone function (as opposed to being done
 * during opal_hwloc_base_open()) because opal_hwloc_topology is not
 * loaded by opal_hwloc_base_open().  Hence, an upper layer needs to
 * invoke this function after opal_hwloc_topology has been loaded.
 */
OPAL_DECLSPEC int opal_hwloc_base_set_process_membind_policy(void);

END_C_DECLS

#endif /* OPAL_HWLOC_BASE_H */
