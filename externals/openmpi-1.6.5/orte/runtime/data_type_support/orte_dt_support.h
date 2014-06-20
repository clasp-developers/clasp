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

#ifndef ORTE_DT_SUPPORT_H
#define ORTE_DT_SUPPORT_H

/*
 * includes
 */
#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#include "opal/dss/dss_types.h"
#include "orte/mca/grpcomm/grpcomm_types.h"
#include "orte/mca/odls/odls_types.h"
#include "orte/mca/plm/plm_types.h"
#include "orte/mca/rmaps/rmaps_types.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/iof/iof_types.h"

#include "orte/runtime/orte_globals.h"


BEGIN_C_DECLS

/** Data type compare functions */
int orte_dt_compare_std_cntr(orte_std_cntr_t *value1, orte_std_cntr_t *value2, opal_data_type_t type);
int orte_dt_compare_name(orte_process_name_t *value1,
                         orte_process_name_t *value2,
                         opal_data_type_t type);
int orte_dt_compare_jobid(orte_jobid_t *value1,
                          orte_jobid_t *value2,
                          opal_data_type_t type);
int orte_dt_compare_vpid(orte_vpid_t *value1,
                         orte_vpid_t *value2,
                         opal_data_type_t type);
#if !ORTE_DISABLE_FULL_SUPPORT
int orte_dt_compare_job(orte_job_t *value1, orte_job_t *value2, opal_data_type_t type);
int orte_dt_compare_node(orte_node_t *value1, orte_node_t *value2, opal_data_type_t type);
int orte_dt_compare_proc(orte_proc_t *value1, orte_proc_t *value2, opal_data_type_t type);
int orte_dt_compare_app_context(orte_app_context_t *value1, orte_app_context_t *value2, opal_data_type_t type);
int orte_dt_compare_exit_code(orte_exit_code_t *value1,
                                    orte_exit_code_t *value2,
                                    opal_data_type_t type);
int orte_dt_compare_node_state(orte_node_state_t *value1,
                                     orte_node_state_t *value2,
                                     orte_node_state_t type);
int orte_dt_compare_proc_state(orte_proc_state_t *value1,
                                     orte_proc_state_t *value2,
                                     orte_proc_state_t type);
int orte_dt_compare_job_state(orte_job_state_t *value1,
                                    orte_job_state_t *value2,
                                    orte_job_state_t type);
int orte_dt_compare_map(orte_job_map_t *value1, orte_job_map_t *value2, opal_data_type_t type);
int orte_dt_compare_tags(orte_rml_tag_t *value1, 
                         orte_rml_tag_t *value2, 
                         opal_data_type_t type);
int orte_dt_compare_daemon_cmd(orte_daemon_cmd_flag_t *value1, orte_daemon_cmd_flag_t *value2, opal_data_type_t type);
int orte_dt_compare_grpcomm_mode(orte_grpcomm_mode_t *value1, orte_grpcomm_mode_t *value2, opal_data_type_t type);
int orte_dt_compare_iof_tag(orte_iof_tag_t *value1, orte_iof_tag_t *value2, opal_data_type_t type);
#endif

/** Data type copy functions */
int orte_dt_copy_std_cntr(orte_std_cntr_t **dest, orte_std_cntr_t *src, opal_data_type_t type);
int orte_dt_copy_name(orte_process_name_t **dest, orte_process_name_t *src, opal_data_type_t type);
int orte_dt_copy_jobid(orte_jobid_t **dest, orte_jobid_t *src, opal_data_type_t type);
int orte_dt_copy_vpid(orte_vpid_t **dest, orte_vpid_t *src, opal_data_type_t type);
#if !ORTE_DISABLE_FULL_SUPPORT
int orte_dt_copy_job(orte_job_t **dest, orte_job_t *src, opal_data_type_t type);
int orte_dt_copy_node(orte_node_t **dest, orte_node_t *src, opal_data_type_t type);
int orte_dt_copy_proc(orte_proc_t **dest, orte_proc_t *src, opal_data_type_t type);
int orte_dt_copy_app_context(orte_app_context_t **dest, orte_app_context_t *src, opal_data_type_t type);
int orte_dt_copy_proc_state(orte_proc_state_t **dest, orte_proc_state_t *src, opal_data_type_t type);
int orte_dt_copy_job_state(orte_job_state_t **dest, orte_job_state_t *src, opal_data_type_t type);
int orte_dt_copy_node_state(orte_node_state_t **dest, orte_node_state_t *src, opal_data_type_t type);
int orte_dt_copy_exit_code(orte_exit_code_t **dest, orte_exit_code_t *src, opal_data_type_t type);
int orte_dt_copy_map(orte_job_map_t **dest, orte_job_map_t *src, opal_data_type_t type);
int orte_dt_copy_tag(orte_rml_tag_t **dest, 
                           orte_rml_tag_t *src, 
                           opal_data_type_t type);
int orte_dt_copy_daemon_cmd(orte_daemon_cmd_flag_t **dest, orte_daemon_cmd_flag_t *src, opal_data_type_t type);
int orte_dt_copy_grpcomm_mode(orte_grpcomm_mode_t **dest, orte_grpcomm_mode_t *src, opal_data_type_t type);
int orte_dt_copy_iof_tag(orte_iof_tag_t **dest, orte_iof_tag_t *src, opal_data_type_t type);
#endif

/** Data type pack functions */
int orte_dt_pack_std_cntr(opal_buffer_t *buffer, const void *src,
                            int32_t num_vals, opal_data_type_t type);
int orte_dt_pack_jobid(opal_buffer_t *buffer, const void *src,
                            int32_t num_vals, opal_data_type_t type);
int orte_dt_pack_name(opal_buffer_t *buffer, const void *src,
                      int32_t num_vals, opal_data_type_t type);
int orte_dt_pack_jobid(opal_buffer_t *buffer, const void *src,
                       int32_t num_vals, opal_data_type_t type);
int orte_dt_pack_vpid(opal_buffer_t *buffer, const void *src,
                      int32_t num_vals, opal_data_type_t type);
#if !ORTE_DISABLE_FULL_SUPPORT
int orte_dt_pack_job(opal_buffer_t *buffer, const void *src,
                     int32_t num_vals, opal_data_type_t type);
int orte_dt_pack_node(opal_buffer_t *buffer, const void *src,
                      int32_t num_vals, opal_data_type_t type);
int orte_dt_pack_proc(opal_buffer_t *buffer, const void *src,
                      int32_t num_vals, opal_data_type_t type);
int orte_dt_pack_app_context(opal_buffer_t *buffer, const void *src,
                                   int32_t num_vals, opal_data_type_t type);
int orte_dt_pack_exit_code(opal_buffer_t *buffer, const void *src,
                                 int32_t num_vals, opal_data_type_t type);
int orte_dt_pack_node_state(opal_buffer_t *buffer, const void *src,
                                  int32_t num_vals, opal_data_type_t type);
int orte_dt_pack_proc_state(opal_buffer_t *buffer, const void *src,
                                  int32_t num_vals, opal_data_type_t type);
int orte_dt_pack_job_state(opal_buffer_t *buffer, const void *src,
                                 int32_t num_vals, opal_data_type_t type);
int orte_dt_pack_map(opal_buffer_t *buffer, const void *src,
                             int32_t num_vals, opal_data_type_t type);
int orte_dt_pack_tag(opal_buffer_t *buffer, 
                           const void *src,
                           int32_t num_vals, 
                           opal_data_type_t type);
int orte_dt_pack_daemon_cmd(opal_buffer_t *buffer, const void *src,
                          int32_t num_vals, opal_data_type_t type);
int orte_dt_pack_grpcomm_mode(opal_buffer_t *buffer, const void *src,
                              int32_t num_vals, opal_data_type_t type);
int orte_dt_pack_iof_tag(opal_buffer_t *buffer, const void *src, int32_t num_vals,
                         opal_data_type_t type);
#endif

/** Data type print functions */
int orte_dt_std_print(char **output, char *prefix, void *src, opal_data_type_t type);
int orte_dt_print_name(char **output, char *prefix, orte_process_name_t *name, opal_data_type_t type);
int orte_dt_print_job(char **output, char *prefix, orte_job_t *src, opal_data_type_t type);
#if !ORTE_DISABLE_FULL_SUPPORT
int orte_dt_print_node(char **output, char *prefix, orte_node_t *src, opal_data_type_t type);
int orte_dt_print_proc(char **output, char *prefix, orte_proc_t *src, opal_data_type_t type);
int orte_dt_print_app_context(char **output, char *prefix, orte_app_context_t *src, opal_data_type_t type);
int orte_dt_print_map(char **output, char *prefix, orte_job_map_t *src, opal_data_type_t type);
#endif

/** Data type release functions */
#if !ORTE_DISABLE_FULL_SUPPORT
void orte_dt_std_obj_release(opal_dss_value_t *value);
#endif
void orte_dt_std_release(opal_dss_value_t *value);

/** Data type size functions */
int orte_dt_std_size(size_t *size, void *src, opal_data_type_t type);
#if !ORTE_DISABLE_FULL_SUPPORT
int orte_dt_size_job(size_t *size, orte_job_t *src, opal_data_type_t type);
int orte_dt_size_node(size_t *size, orte_node_t *src, opal_data_type_t type);
int orte_dt_size_proc(size_t *size, orte_proc_t *src, opal_data_type_t type);
int orte_dt_size_app_context(size_t *size, orte_app_context_t *src, opal_data_type_t type);
int orte_dt_size_map(size_t *size, orte_job_map_t *src, opal_data_type_t type);
#endif

/** Data type unpack functions */
int orte_dt_unpack_std_cntr(opal_buffer_t *buffer, void *dest,
                        int32_t *num_vals, opal_data_type_t type);
int orte_dt_unpack_name(opal_buffer_t *buffer, void *dest,
                        int32_t *num_vals, opal_data_type_t type);
int orte_dt_unpack_jobid(opal_buffer_t *buffer, void *dest,
                         int32_t *num_vals, opal_data_type_t type);
int orte_dt_unpack_vpid(opal_buffer_t *buffer, void *dest,
                        int32_t *num_vals, opal_data_type_t type);
#if !ORTE_DISABLE_FULL_SUPPORT
int orte_dt_unpack_job(opal_buffer_t *buffer, void *dest,
                       int32_t *num_vals, opal_data_type_t type);
int orte_dt_unpack_node(opal_buffer_t *buffer, void *dest,
                        int32_t *num_vals, opal_data_type_t type);
int orte_dt_unpack_proc(opal_buffer_t *buffer, void *dest,
                        int32_t *num_vals, opal_data_type_t type);
int orte_dt_unpack_app_context(opal_buffer_t *buffer, void *dest,
                                     int32_t *num_vals, opal_data_type_t type);
int orte_dt_unpack_exit_code(opal_buffer_t *buffer, void *dest,
                                   int32_t *num_vals, opal_data_type_t type);
int orte_dt_unpack_node_state(opal_buffer_t *buffer, void *dest,
                                    int32_t *num_vals, opal_data_type_t type);
int orte_dt_unpack_proc_state(opal_buffer_t *buffer, void *dest,
                                    int32_t *num_vals, opal_data_type_t type);
int orte_dt_unpack_job_state(opal_buffer_t *buffer, void *dest,
                                   int32_t *num_vals, opal_data_type_t type);
int orte_dt_unpack_map(opal_buffer_t *buffer, void *dest,
                               int32_t *num_vals, opal_data_type_t type);
int orte_dt_unpack_tag(opal_buffer_t *buffer,
                             void *dest,
                             int32_t *num_vals,
                             opal_data_type_t type);
int orte_dt_unpack_daemon_cmd(opal_buffer_t *buffer, void *dest,
                            int32_t *num_vals, opal_data_type_t type);
int orte_dt_unpack_grpcomm_mode(opal_buffer_t *buffer, void *dest,
                              int32_t *num_vals, opal_data_type_t type);
int orte_dt_unpack_iof_tag(opal_buffer_t *buffer, void *dest, int32_t *num_vals,
                           opal_data_type_t type);
#endif

END_C_DECLS

#endif
