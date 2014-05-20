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
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 */

#ifndef MCA_GRPCOMM_BASE_H
#define MCA_GRPCOMM_BASE_H

/*
 * includes
 */
#include "orte_config.h"

#include "opal/class/opal_list.h"
#include "opal/mca/mca.h"

#include "orte/mca/grpcomm/grpcomm.h"


/*
 * Global functions for MCA overall collective open and close
 */
BEGIN_C_DECLS

/*
 * function definitions
 */
ORTE_DECLSPEC    int orte_grpcomm_base_open(void);
ORTE_DECLSPEC    int orte_grpcomm_base_select(void);
ORTE_DECLSPEC    int orte_grpcomm_base_close(void);

/*
 * globals that might be needed
 */

ORTE_DECLSPEC extern int orte_grpcomm_base_output;
ORTE_DECLSPEC extern bool mca_grpcomm_base_selected;
ORTE_DECLSPEC extern opal_list_t mca_grpcomm_base_components_available;
ORTE_DECLSPEC extern orte_grpcomm_base_component_t mca_grpcomm_base_selected_component;
ORTE_DECLSPEC extern int orte_grpcomm_profile_fd;

#if !ORTE_DISABLE_FULL_SUPPORT

/*
 * Base functions
 */
ORTE_DECLSPEC   int orte_grpcomm_base_allgather_list(opal_list_t *names,
                                                     opal_buffer_t *sbuf,
                                                     opal_buffer_t *rbuf);
ORTE_DECLSPEC   int orte_grpcomm_base_set_proc_attr(const char *attr_name,
                                                    const void *data,
                                                    size_t size);
ORTE_DECLSPEC   int orte_grpcomm_base_get_proc_attr(const orte_process_name_t proc,
                                                    const char * attribute_name, void **val, 
                                                    size_t *size);
ORTE_DECLSPEC   int orte_grpcomm_base_peer_modex(bool modex_db);
ORTE_DECLSPEC   int orte_grpcomm_base_modex_unpack( opal_buffer_t* rbuf, bool modex_db);
ORTE_DECLSPEC   int orte_grpcomm_base_full_modex(opal_list_t *procs, bool modex_db);
ORTE_DECLSPEC   int orte_grpcomm_base_purge_proc_attrs(void);
ORTE_DECLSPEC   int orte_grpcomm_base_modex_init(void);
ORTE_DECLSPEC   void orte_grpcomm_base_modex_finalize(void);
ORTE_DECLSPEC   int orte_grpcomm_base_pack_modex_entries(opal_buffer_t *buf, bool *modex_reqd);
ORTE_DECLSPEC   int orte_grpcomm_base_update_modex_entries(orte_process_name_t *proc_name,
                                                           opal_buffer_t *rbuf);
ORTE_DECLSPEC   int orte_grpcomm_base_load_modex_data(orte_process_name_t *proc, char *attribute_name,
                                                      void *data, int num_bytes);

/* Tuned collectives */
ORTE_DECLSPEC void orte_grpcomm_base_coll_recv(int status, orte_process_name_t* sender,
                                               opal_buffer_t* buffer, orte_rml_tag_t tag,
                                               void* cbdata);
ORTE_DECLSPEC int orte_grpcomm_base_allgather(opal_buffer_t *sendbuf, opal_buffer_t *recvbuf, int32_t num_entries,
                                              orte_jobid_t jobid, orte_vpid_t np, orte_vpid_t *vpids);


#endif /* ORTE_DISABLE_FULL_SUPPORT */

END_C_DECLS
#endif
